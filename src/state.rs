use crate::{Instr, BasicBlock, Label, Register, GlobalList, MemoryList, MemoryType, Axis};
use std::{collections::HashMap, convert::TryInto};

struct Frame {
    data: Vec<(i32, i32)>,
}

impl Frame {
	pub fn new(local_count: u32) -> Self {
		Frame { data: vec![(0, 0); local_count as usize] }
	}
}

#[derive(Debug)]
enum Value {
	I32(i32),
    Any(i32, i32),
}

pub(crate) struct State {
	pub pc: (usize, usize),

	bbs: Vec<BasicBlock<Instr>>,

	frames: Vec<Frame>,

	stack: Vec<Value>,

    memory: Vec<[u8; 65536]>,

    pub registers: HashMap<Register, (i32, i32)>,

    pub globals: Vec<(i32, i32)>,

    global_ptr: u32,

    local_ptr: u32,

    memory_ptr: u32,

    turtle: (i32, i32, i32),
}

fn set_reg_i32(registers: &mut HashMap<Register, (i32, i32)>, reg: Register, value: i32) {
    registers.entry(reg).or_insert((0, 0)).0 = value;
}

fn set_reg_i64(registers: &mut HashMap<Register, (i32, i32)>, reg: Register, value: (i32, i32)) {
    registers.insert(reg, value);
}

impl State {
	pub fn new(bbs: Vec<BasicBlock<Instr>>, globals: &GlobalList, mem: &MemoryList) -> Self {
        let mut memory = Vec::new();
        for mem in mem.memory.iter() {
            match mem {
                MemoryType::M32 { limits, shared } => {
                    assert!(!*shared);
                    // FIXME: Memory size
                    for _ in 0..limits.maximum.unwrap_or(limits.initial) {
                        memory.push([0; 65536]);
                    }
                }
                _ => todo!(),
            }
        }

		State {
			bbs,
			pc: (0, 0),
			frames: Vec::new(),
			stack: Vec::new(),
            registers: HashMap::new(),
            local_ptr: 0,
            global_ptr: 0,
            memory_ptr: 0,
            globals: vec![(0, 0); globals.globals.len()],
            memory,
            turtle: (0, 0, 0),
		}
	}

    pub fn enter(&mut self, idx: usize) {
        self.stack.push(Value::I32(-1));

		self.pc = (idx, 0);
    }

	pub fn call(&mut self, idx: usize) {
        self.enter(idx);
		loop {
			if self.step() { break }
		}
	}

    pub fn get_pc(&self, label: &Label) -> usize {
        self.bbs.iter().enumerate().find(|(_, b)| {
            &b.label == label
        }).unwrap_or_else(|| {
            eprintln!("Failed to find {:?}", label);
            panic!();
        }).0
    }

    fn get_reg_any(&self, reg: &Register) -> (i32, i32) {
        *self.registers.get(reg).unwrap()
    }

    pub fn is_halted(&self) -> bool {
        self.pc.1 == self.bbs[self.pc.0].instrs.len()
    }

	/// Returns true when it ends
	pub fn step(&mut self) -> bool {
		use Instr::*;

		let mut incr_pc = true;

		let bb = &self.bbs[self.pc.0];

		let instr = &bb.instrs[self.pc.1];

        println!("{:?}", instr);

		match instr {
			PushFrame(l) => {
				self.frames.push(Frame::new(*l));
			}
			PopFrame(l) => {
				let f = self.frames.pop().unwrap();
                assert_eq!(f.data.len(), *l as usize);
			}
			PushReturnAddress(r) => {
                let idx = self.get_pc(r);

				self.stack.push(Value::I32(idx as i32));
			}
            PushI64Const(c) => {
                todo!()
            }
            PushI32Const(c) => {
                self.stack.push(Value::I32(*c));
            }

            SetTurtleCoord(r, axis) => {
                let v = self.get_reg_any(r).0;
                match axis {
                    Axis::X => self.turtle.0 = v,
                    Axis::Y => self.turtle.1 = v,
                    Axis::Z => self.turtle.2 = v,
                }
            }
            SetTurtleBlock(r) => {
                let v = self.get_reg_any(r).0;
                println!("TODO: Set block {} at {:?}", v, self.turtle);
            }

            SetLocalPtr(l) => {
                self.local_ptr = *l;
            }
            LoadLocalI64(r) => {
                let f = self.frames.last().unwrap();
                let v = f.data[self.local_ptr as usize];
                set_reg_i64(&mut self.registers, *r, v);
            }
            StoreLocalI64(r) => {
                let f = self.frames.last_mut().unwrap();
                let v = *self.registers.get(r).unwrap();
                f.data[self.local_ptr as usize] = v;
            }
            LoadLocalI32(r) => {
                let f = self.frames.last().unwrap();
                let v = f.data[self.local_ptr as usize];
                set_reg_i32(&mut self.registers, *r, v.0);
            }
            StoreLocalI32(r) => {
                let f = self.frames.last_mut().unwrap();
                let v = *self.registers.get(r).unwrap();
                f.data[self.local_ptr as usize].0 = v.0;
            }

            I64ExtendI32S { .. } => {
                todo!()
            }
            ResetFrames => {
                todo!()
            }

            SetMemPtr(r) => {
                let v = self.get_reg_any(r).0;
                assert!(v >= 0, "{:?}", v);
                self.memory_ptr = v as u32;
            }
            StoreI32(r, _a) => {
                let v = self.registers.get(r).unwrap().0;
                // FIXME: LE
                let v = v.to_be_bytes();

                let page = &mut self.memory[self.memory_ptr as usize / 65536];
                page[self.memory_ptr as usize % 65536..][..4].copy_from_slice(&v);
            }
            StoreI32_16(r, _a) => {
                let v = self.registers.get(r).unwrap().0 as u16;
                // FIXME: LE
                let v = v.to_be_bytes();

                let page = &mut self.memory[self.memory_ptr as usize / 65536];
                page[self.memory_ptr as usize % 65536..][..2].copy_from_slice(&v);
            }
            StoreI32_8(r, _a) => {
                let v = self.registers.get(r).unwrap().0 as u8;
                // FIXME: LE
                let v = v.to_be_bytes();

                let page = &mut self.memory[self.memory_ptr as usize / 65536];
                page[self.memory_ptr as usize % 65536..][..1].copy_from_slice(&v);
            }
            LoadI32(r, _a) => {
                let page = &mut self.memory[self.memory_ptr as usize / 65536];
                
                let v = page[self.memory_ptr as usize % 65536..][..4].try_into().unwrap();
                // FIXME: LE
                let v = i32::from_be_bytes(v);
                set_reg_i32(&mut self.registers, *r, v);
            }
            LoadI32_16U(r, _a) => {
                let page = &mut self.memory[self.memory_ptr as usize / 65536];
                
                let v = page[self.memory_ptr as usize % 65536..][..2].try_into().unwrap();
                // FIXME: LE
                let v = u16::from_be_bytes(v) as u32 as i32;
                set_reg_i32(&mut self.registers, *r, v);
            }
            LoadI32_8U(r, _a) => {
                let page = &mut self.memory[self.memory_ptr as usize / 65536];
                
                let v = page[self.memory_ptr as usize % 65536..][..1].try_into().unwrap();
                // FIXME: LE
                let v = u8::from_be_bytes(v) as u32 as i32;
                set_reg_i32(&mut self.registers, *r, v);
            }



            SetGlobalPtr(v) => {
                self.global_ptr = *v;
            }
            LoadGlobalI64(r) => {
                let v = self.globals[self.global_ptr as usize];
                set_reg_i64(&mut self.registers, *r, v);
            }
            StoreGlobalI64(r) => {
                let v = self.get_reg_any(r);
                self.globals[self.global_ptr as usize] = v;
            }
            LoadGlobalI32(r) => {
                let v = self.globals[self.global_ptr as usize].0;
                set_reg_i32(&mut self.registers, *r, v);
            }
            StoreGlobalI32(r) => {
                let v = self.get_reg_any(r).0;
                self.globals[self.global_ptr as usize].0 = v;
            }

            PushValueFrom(r) => {
                let (l, h) = self.registers.get(r).unwrap();

                self.stack.push(Value::Any(*l, *h));
            }

            PushI32From(r) => {
                let v = self.get_reg_any(r).0;
                self.stack.push(Value::I32(v));
            }

            PopI32Into(r) => {
                let v = pop_i32_into(&mut self.stack);

                self.registers.entry(*r).or_insert((0, 0)).0 = v;
            }
            PopValueInto(r) => {
                let v = pop_any_into(&mut self.stack);

                self.registers.insert(*r, v);
            }

            Drop => {
                let _ = pop_any_into(&mut self.stack);
            }

			Branch(l) => {
                self.pc = (self.get_pc(l), 0);
				incr_pc = false;
			}
            BranchIf { t_name, f_name, cond } => {
                let c = self.get_reg_any(cond).0;
                let target = if c != 0 {
                    t_name
                } else {
                    f_name
                };

                self.pc = (self.get_pc(target), 0);
                incr_pc = false;
            }
            BranchTable { reg, targets, default } => {
                let dest: usize = self.get_reg_any(reg).0.try_into().unwrap();

                let dest = if let Some(target) = targets.get(dest) {
                    target
                } else if let Some(default) = default {
                    default
                } else {
                    panic!("Branch out of bounds and no default target: {}", dest);
                };

                self.pc = (self.get_pc(dest), 0);
                incr_pc = false;
            }

            I32Op { dst, lhs, op, rhs } => {
                let l = self.get_reg_any(lhs).0;
                let r = self.get_reg_any(rhs).0;

                let d = match *op {
                    "+=" => l + r,
                    "-=" => l - r,
                    "*=" => l * r,
                    "/=" => l / r,
                    "&=" => l & r,
                    "|=" => l | r,
                    "^=" => l ^ r,
                    "shl" => l << r,
                    "shru" => ((l as u32) >> r) as i32,
                    "==" => (l == r) as i32,
                    "!=" => (l != r) as i32,
                    "lts" => (l <  r) as i32,
                    "les" => (l <= r) as i32,
                    "gts" => (l >  r) as i32,
                    "ges" => (l >= r) as i32,
                    "leu" => ((l as u32) <= (r as u32)) as i32,
                    "ltu" => ((l as u32) <  (r as u32)) as i32,
                    "geu" => ((l as u32) >= (r as u32)) as i32,
                    "gtu" => ((l as u32) >  (r as u32)) as i32,
                    o => todo!("{:?}", o),
                };

                self.registers.entry(*dst).or_insert((0, 0)).0 = d;
            }
            I32Eqz { val, cond } => {
                let v = self.get_reg_any(val).0;

                let c = (v == 0) as i32;

                self.registers.entry(*cond).or_insert((0, 0)).0 = c;
            }
            AddI32Const(r, rhs) => {
                let mut lhs = self.get_reg_any(r);
                lhs.0 += *rhs;
                self.registers.insert(*r, lhs);
            }

            DynBranch(w, t) => {
                let a = self.registers.get(w).unwrap().0;

                if let Some(t) = t {
                    todo!("{:?}", t)
                } else {
                    if a == -1 {
                        return true;
                    }

                    self.pc = (a as usize, 0);

                    println!("Branched to {} {:?}", a, self.pc);

                    incr_pc = false;
                }
            }

            Unreachable => {
                println!("UNREACHABLE AAAAAA");
                return true;
            }

			Comment(_) => {},
            Tellraw(t) => {
                // TODO:
                println!("{}", t);
            }
		}

		if incr_pc {
			self.pc.1 += 1;
		}

        self.is_halted()
	}
}

fn pop_i32_into(stack: &mut Vec<Value>) -> i32 {
    let a = stack.pop().unwrap();
    match a {
        Value::Any(v, _) => v,
        Value::I32(v) => v,
    }
}

fn pop_any_into(stack: &mut Vec<Value>) -> (i32, i32) {
    let a = stack.pop().unwrap();
    match a {
        Value::Any(a, b) => (a, b),
        Value::I32(a) => (a, 0),
    }
}