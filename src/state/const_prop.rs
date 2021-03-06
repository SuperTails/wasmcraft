use wasmparser::Type;

use crate::mir::{Instr, MirBasicBlock, RegOrConst};

use super::{RegFile, Stack, StateError, StateResult, eval_i32_op, OptAction};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PropWord {
    Unknown,
    Exact(i32),
}

impl PropWord {
    pub fn map<F>(self, f: F) -> Self
        where F: FnOnce(i32) -> i32,
    {
        match self {
            PropWord::Unknown => PropWord::Unknown,
            PropWord::Exact(v) => PropWord::Exact(f(v)),
        }
    }
}

impl Default for PropWord {
    fn default() -> Self {
        PropWord::Unknown
    }
}

pub(crate) struct ConstProp<'a> {
    block: &'a MirBasicBlock,

    registers: RegFile<PropWord>,
    stack: Stack<PropWord>,

    locals: Vec<(PropWord, PropWord)>,

    actions: Vec<OptAction>,

    pc: usize,
}

fn read(v: StateResult<PropWord>) -> StateResult<PropWord> {
    if let Err(StateError::UninitRead(_)) = v {
        Ok(PropWord::Unknown)
    } else {
        v
    }
}

impl<'a> ConstProp<'a> {
    pub fn new(block: &'a MirBasicBlock, op_stack: &crate::OpStack) -> Self {
        let mut stack = Stack::new();
        for t in op_stack.0.iter() {
            match t {
                Type::I32 => stack.push_i32(PropWord::Unknown),
                Type::I64 => stack.push_i64_pair(PropWord::Unknown, PropWord::Unknown),
                _ => todo!("{:?}", t)
            }
        }

        ConstProp {
            block,
            registers: RegFile::new(),
            stack,
            locals: Vec::new(),
            actions: Vec::new(),
            pc: 0,
        }
    }

    pub fn run(&mut self) {
        while !self.step().unwrap() {}
    }

    // Returns true when halted
    pub fn step(&mut self) -> StateResult<bool> {
        let instr = &self.block.instrs[self.pc];

        match instr {
            Instr::Comment(_) => {},
            Instr::Tellraw(_) => {},
            Instr::SetTurtleCoord(_, _) => {},
            Instr::SetTurtleBlock(_) => {},

            Instr::TurtleGet(reg) => {
                self.registers.set_half(reg.as_lo(), PropWord::Unknown);
            }

            Instr::Drop => { self.stack.pop_value(); },

            Instr::PopI32Into(reg) => {
                let val = self.stack.pop_i32();
                self.registers.set_half(reg.as_lo(), val);

                if let PropWord::Exact(v) = val {
                    self.actions.push(OptAction::Replace {
                        id: self.pc,
                        instr: vec![Instr::Drop, Instr::SetConst(reg.as_lo(), v)],
                    })
                }
            },
            Instr::PopI64Into(reg) => {
                let val = self.stack.pop_i64_pair();
                self.registers.set_pair(*reg, val);

                if let (PropWord::Exact(lo), PropWord::Exact(hi)) = val {
                    self.actions.push(OptAction::Replace {
                        id: self.pc,
                        instr: vec![Instr::Drop, Instr::SetConst(reg.as_lo(), lo), Instr::SetConst(reg.as_hi(), hi)],
                    })
                }
            }

            Instr::PushI32From(reg) => {
                let val = read(self.registers.get_half(reg.as_lo()))?;
                self.stack.push_i32(val);
            },
            Instr::PushI64From(reg) => {
                let lo = read(self.registers.get_half(reg.as_lo()))?;
                let hi = read(self.registers.get_half(reg.as_hi()))?;
                self.stack.push_i64_pair(lo, hi);
            },
            Instr::PushI32Const(value) => {
                self.stack.push_i32(PropWord::Exact(*value));
            },
            Instr::PushI64Const(value) => {
                let lo = PropWord::Exact(*value as i32);
                let hi = PropWord::Exact((*value >> 32) as i32);

                self.stack.push_i64_pair(lo, hi);
            },
            Instr::PushReturnAddress(_) => {
                self.stack.push_i32(PropWord::Unknown);
            }
            &Instr::SetConst(reg, value) => {
                self.registers.set_half(reg, PropWord::Exact(value));
            },
            Instr::Copy { dst, src } => {
                let val = read(self.registers.get_half(*src))?;
                self.registers.set_half(*dst, val);
            },

            &Instr::LoadLocalI32(reg, idx) => {
                if self.locals.len() <= idx as usize {
                    self.locals.resize(idx as usize + 1, (PropWord::Unknown, PropWord::Unknown))
                }

                self.registers.set_half(reg.as_lo(), self.locals[idx as usize].0)
            }
            &Instr::StoreLocalI32(reg, idx) => {
                if self.locals.len() <= idx as usize {
                    self.locals.resize(idx as usize + 1, (PropWord::Unknown, PropWord::Unknown))
                }

                self.locals[idx as usize].0 = read(self.registers.get_half(reg.as_lo()))?;
            }
            &Instr::LoadLocalI64(reg, idx) => {
                if self.locals.len() <= idx as usize {
                    self.locals.resize(idx as usize + 1, (PropWord::Unknown, PropWord::Unknown))
                }

                self.registers.set_half(reg.as_lo(), self.locals[idx as usize].0);
                self.registers.set_half(reg.as_hi(), self.locals[idx as usize].1);
            }
            &Instr::StoreLocalI64(reg, idx) => {
                if self.locals.len() <= idx as usize {
                    self.locals.resize(idx as usize + 1, (PropWord::Unknown, PropWord::Unknown))
                }

                self.locals[idx as usize].0 = read(self.registers.get_half(reg.as_lo()))?;
                self.locals[idx as usize].1 = read(self.registers.get_half(reg.as_hi()))?;
            }
            Instr::PushFrame(_) => {
                self.locals = Vec::new();
            }
            Instr::PopFrame(_) => {
                self.locals = Vec::new();
            }

            Instr::SetMemPtr(r) => {
                if let RegOrConst::Reg(reg) = r {
                    if let PropWord::Exact(v) = read(self.registers.get_half(*reg))? {
                        self.actions.push(OptAction::Replace {
                            id: self.pc,
                            instr: vec![Instr::SetMemPtr(RegOrConst::Const(v))]
                        })
                    }
                }
            }

            Instr::LoadI32(reg, _) |
            Instr::LoadI32_8U(reg, _) | 
            Instr::LoadI32_8S(reg, _) |
            Instr::LoadI32_16U(reg, _) |
            Instr::LoadI32_16S(reg, _) => self.registers.set_half(reg.as_lo(), PropWord::Unknown),
            Instr::StoreI32(_, _) |
            Instr::StoreI32_8(_, _) |
            Instr::StoreI32_16(_, _) => {},

            &Instr::I32Op { dst, lhs, op, rhs } => {
                let l = read(self.registers.get_half(lhs))?;

                let r = match rhs {
                    RegOrConst::Reg(r) => read(self.registers.get_half(r))?,
                    RegOrConst::Const(c) => PropWord::Exact(c),
                };

                if let PropWord::Exact(r) = r {
                    if op == "+=" {
                        self.actions.push(OptAction::Replace {
                            id: self.pc,
                            instr: vec![Instr::AddI32Const(lhs, r)],
                        })
                    } else if op == "-=" {
                        self.actions.push(OptAction::Replace {
                            id: self.pc,
                            instr: vec![Instr::AddI32Const(lhs, r.wrapping_neg())],
                        })
                    } else if op != "/=" && op != "remu" && op != "rems" {
                        self.actions.push(OptAction::Replace {
                            id: self.pc,
                            instr: vec![Instr::I32Op { dst, lhs, op, rhs: r.into() }]
                        });
                    } else {
                        println!("{} {}", op, rhs);
                    }
                }

                if let (PropWord::Exact(l), PropWord::Exact(r)) = (l, r) {
                    let d = eval_i32_op(lhs, rhs, l, op, r, &mut self.registers);
                    self.registers.set_half(dst, PropWord::Exact(d));
                } else {
                    self.registers.set_half(dst, PropWord::Unknown)
                }
            }
            &Instr::I32Eqz { val, cond } => {
                let v = read(self.registers.get_half(val.as_lo()))?;
                if let PropWord::Exact(v) = v {
                    self.registers.set_half(cond.as_lo(), PropWord::Exact((v == 0) as i32))
                } else {
                    self.registers.set_half(cond.as_lo(), PropWord::Unknown);
                }
            } 
            &Instr::AddI32Const(reg, val) => {
                let v = read(self.registers.get_half(reg))?;
                self.registers.set_half(reg, v.map(|v| v.wrapping_add(val)));
            },
            Instr::I32Extend8S(reg) => {
                let v = read(self.registers.get_half(reg.as_lo()))?;
                self.registers.set_half(reg.as_lo(), v.map(|val| val as i8 as i32));
            },
            Instr::I32Extend16S(reg) => {
                let v = read(self.registers.get_half(reg.as_lo()))?;
                self.registers.set_half(reg.as_lo(), v.map(|val| val as i16 as i32));
            }, 

            Instr::I32Popcnt(reg) => {
                let v = read(self.registers.get_half(*reg))?;
                self.registers.set_half(*reg, v.map(|val| val.count_ones() as i32));
            },
            Instr::I32Ctz(reg) => {
                let v = read(self.registers.get_half(reg.as_lo()))?;
                self.registers.set_half(reg.as_lo(), v.map(|val| val.trailing_zeros() as i32));
            }
            Instr::I32Clz(reg) => {
                let v = read(self.registers.get_half(reg.as_lo()))?;
                self.registers.set_half(reg.as_lo(), v.map(|val| val.leading_zeros() as i32));
            },
            
            Instr::SelectI32 { dst_reg, true_reg, false_reg, cond_reg } => {
                let cond = read(self.registers.get_half(cond_reg.as_lo()))?;

                if let PropWord::Exact(cond) = cond {
                    let result = if cond != 0 {
                        read(self.registers.get_half(true_reg.as_lo()))?
                    } else {
                        read(self.registers.get_half(false_reg.as_lo()))?
                    };

                    self.registers.set_half(dst_reg.as_lo(), result);
                } else {
                    self.registers.set_half(dst_reg.as_lo(), PropWord::Unknown);
                }
            }

            i => {
                // TODO:
                println!("Stopping const prop at {:?}", i);
                return Ok(true);
            }

            /*
            Instr::I64Add { dst, lhs, rhs } => todo!(),
            Instr::I64DivS { dst, lhs, rhs } => todo!(),
            Instr::I64DivU { dst, lhs, rhs } => todo!(),
            Instr::I64RemS { dst, lhs, rhs } => todo!(),
            Instr::I64RemU { dst, lhs, rhs } => todo!(),
            Instr::I64Rotl { dst, lhs, rhs } => todo!(),
            Instr::I64Rotr { dst, lhs, rhs } => todo!(),
            Instr::I64Eq { dst, lhs, invert, rhs } => todo!(),
            Instr::I64UComp { dst, lhs, op, rhs } => todo!(),
            Instr::I64SComp { dst, lhs, op, rhs } => todo!(),
            Instr::I64Shl { dst, lhs, rhs } => todo!(),
            Instr::I64ShrU { dst, lhs, rhs } => todo!(),
            Instr::I64ShrS { dst, lhs, rhs } => todo!(),

            Instr::I32MulTo64 { dst, lhs, rhs } => todo!(),
            Instr::I64Clz { dst, src } => todo!(),
            Instr::I64Ctz { dst, src } => todo!(),
            Instr::I64ExtendI32S { dst, src } => todo!(),
            Instr::I64ExtendI32U(_) => todo!(),
            Instr::I64Eqz { val, cond } => todo!(),

            Instr::StoreRow(_) => todo!(),
            Instr::Unreachable => todo!(),
            */
        }

        self.pc += 1;

        Ok(self.pc == self.block.instrs.len())
    }
}

pub(crate) fn get_actions(basic_block: &MirBasicBlock, op_stack: &crate::OpStack) -> Vec<OptAction> {
    let mut prop = ConstProp::new(basic_block, op_stack);
    prop.run();
    /*println!("LOCALS: {:?}", prop.locals);
    println!("GLOBALS: {:?}", prop.globals);
    panic!("{:?}", prop.actions);*/
    prop.actions
}