use crate::{Axis, BasicBlock, BranchTarget, CodeFuncIdx, GlobalList, HalfRegister, Instr, Label, MemoryList, MemoryType, Register, WasmValue, eval_init_expr};
use crate::{BRANCH_CONV, BranchConv};
use std::{collections::HashMap, convert::TryInto};
use wasmparser::Type;
use std::convert::TryFrom;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StateError {
    UninitRead(HalfRegister),
}

type StateResult<T> = Result<T, StateError>;

struct Frame<T> {
    data: Vec<(T, T)>,
}

impl<T: Default + Clone> Frame<T> {
	pub fn new(local_count: u32) -> Self {
		Frame { data: vec![(T::default(), T::default()); local_count as usize] }
	}
}

#[derive(Debug)]
enum Value {
	I32(i32),
    I64(i32, i32),
}

pub(crate) struct Table {
    pub elements: Vec<Option<CodeFuncIdx>>,
}

pub(crate) struct RegFile<T>(pub HashMap<HalfRegister, T>);

impl<T: Copy> RegFile<T> {
    pub fn new() -> Self {
        RegFile(HashMap::new())
    }

    pub fn get_half(&self, reg: HalfRegister) -> StateResult<T> {
        self.0.get(&reg).copied().ok_or(StateError::UninitRead(reg))
    }

    pub fn get_pair(&self, reg: Register) -> StateResult<(T, T)> {
        let lo = self.get_half(reg.as_lo())?;
        let hi = self.get_half(reg.as_hi())?;

        Ok((lo, hi))
    }

    pub fn set_half(&mut self, reg: HalfRegister, value: T) {
        self.0.insert(reg, value);
    }

    pub fn set_pair(&mut self, reg: Register, value: (T, T)) {
        self.set_half(reg.as_lo(), value.0);
        self.set_half(reg.as_hi(), value.1);
    }

    pub fn clobber_half(&mut self, reg: HalfRegister) {
        self.0.remove(&reg);
    }

    pub fn clobber_pair(&mut self, reg: Register) {
        self.clobber_half(reg.as_lo());
        self.clobber_half(reg.as_hi());
    }
}

impl RegFile<i32> {
    pub fn get_i32(&self, reg: Register) -> StateResult<i32> {
        self.get_half(reg.as_lo())
    }

    pub fn get_i64(&self, reg: Register) -> StateResult<i64> {
        let (lo, hi) = self.get_pair(reg)?;

        Ok((lo as u32 as i64) | ((hi as i64) << 32))
    }

    pub fn get_typed(&self, reg: Register, ty: Type) -> StateResult<WasmValue> {
        match ty {
            Type::I32 => Ok(WasmValue::I32(self.get_i32(reg)?)),
            Type::I64 => Ok(WasmValue::I64(self.get_i64(reg)?)),
            _ => todo!("{:?}", ty),
        }
    }

    pub fn set_i32(&mut self, reg: Register, value: i32) {
        self.set_half(reg.as_lo(), value)
    }

    pub fn set_i64(&mut self, reg: Register, value: i64) {
        let lo = value as i32;
        let hi = (value >> 32) as i32;
        self.set_pair(reg, (lo, hi));
    }
}

pub(crate) struct Pc(pub Vec<(usize, usize)>);

impl Pc {
    pub fn jump(&mut self, label: &Label, bbs: &[BasicBlock<Instr>]) {
        let pos = (State::get_pc(bbs, label), 0);
        self.jump_to(pos);
    }

    pub fn jump_to(&mut self, pos: (usize, usize)) {
        if BRANCH_CONV == BranchConv::Direct {
            self.incr();
            self.0.push(pos);
        } else {
            *self.0.last_mut().unwrap() = pos;
        }
    }

    pub fn incr(&mut self) {
        self.0.last_mut().unwrap().1 += 1;
    }

    pub fn unwind(&mut self, bbs: &[BasicBlock<Instr>]) {
        while let Some(last_pc) = self.0.last() {
            if last_pc.1 == bbs[last_pc.0].instrs.len() {
                println!("Popping {:?}", last_pc);
                self.0.pop();
            } else {
                break
            }
        }
    }
}

#[derive(Debug)]
struct Stack(Vec<Value>);

impl Stack {
    pub fn new() -> Self {
        Stack(Vec::new())
    }

    pub fn pop_i32(&mut self) -> i32 {
        let a = self.pop_value();
        match a {
            Value::I32(v) => v,
            Value::I64(_, _) => panic!(),
        }
    }

    pub fn pop_i64_pair(&mut self) -> (i32, i32) {
        let a = self.pop_value();
        match a {
            Value::I64(lo, hi) => (lo, hi),
            Value::I32(_) => panic!(),
        }
    }

    pub fn pop_i64(&mut self) -> i64 {
        let (lo, hi) = self.pop_i64_pair();
        ((hi as i64) << 32) | (lo as u32 as i64)
    }

    pub fn pop_ty(&mut self, ty: Type) -> (i32, i32) {
        let a = self.pop_value();
        match (a, ty) {
            (Value::I64(a, b), Type::I64) => (a, b),
            (Value::I32(a), Type::I32) => (a, 0),
            _ => panic!(),
        }
    }

    pub fn pop_value(&mut self) -> Value {
        self.0.pop().unwrap()
    }

    pub fn push_ty(&mut self, ty: Type, value: (i32, i32)) {
        match ty {
            Type::I32 => self.0.push(Value::I32(value.0)),
            Type::I64 => self.0.push(Value::I64(value.0, value.1)),
            _ => todo!(),
        }
    }

    pub fn push_i64(&mut self, value: i64) {
        let lo = value as i32;
        let hi = (value >> 32) as i32;
        self.push_i64_pair(lo, hi);
    }

    pub fn push_i64_pair(&mut self, lo: i32, hi: i32) {
        self.push_value(Value::I64(lo, hi))
    }

    pub fn push_i32(&mut self, value: i32) {
        self.push_value(Value::I32(value))
    }

    pub fn push_value(&mut self, value: Value) {
        self.0.push(value);
    }
}

struct GlobalData(Vec<(i32, i32)>);

impl GlobalData {
    pub fn new(global_list: &GlobalList) -> Self {
        let mut data = GlobalData(vec![(0, 0); global_list.globals.len()]);

        for (i, global) in global_list.globals.iter().enumerate() {
            data.set_lo(i, eval_init_expr(global.init_expr));
        }

        data
    }

    pub fn get_pair(&self, index: usize) -> (i32, i32) {
        self.0[index]
    }

    pub fn get_lo(&self, index: usize) -> i32 {
        self.get_pair(index).0
    }

    pub fn set_pair(&mut self, index: usize, value: (i32, i32)) {
        self.0[index] = value;
    }

    pub fn set_lo(&mut self, index: usize, value: i32) {
        self.0[index].0 = value;
    }
}

pub(crate) struct State {
	pub pc: Pc,

	pub bbs: Vec<BasicBlock<Instr>>,

	frames: Vec<Frame<i32>>,

	stack: Stack,

    tables: Vec<Table>,

    pub memory: Vec<[u8; 65536]>,

    pub registers: RegFile<i32>,

    globals: GlobalData,

    global_ptr: u32,

    local_ptr: u32,

    memory_ptr: u32,

    turtle: (i32, i32, i32),
}

impl State {
	pub fn new(bbs: Vec<BasicBlock<Instr>>, globals: &GlobalList, mem: &MemoryList, tables: Vec<Table>) -> Self {
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
			pc: Pc(vec![(0, 0)]),
			frames: Vec::new(),
			stack: Stack::new(),
            registers: RegFile::new(),
            globals: GlobalData::new(globals),
            local_ptr: 0,
            global_ptr: 0,
            memory_ptr: 0,
            memory,
            turtle: (0, 0, 0),
            tables,
		}
	}

    pub fn enter(&mut self, idx: usize) {
        self.stack.push_i32(-1);

		self.pc = Pc(vec![(idx, 0)]);
    }

    pub fn get_pc(bbs: &[BasicBlock<Instr>], label: &Label) -> usize {
        bbs.iter().enumerate().find(|(_, b)| {
            &b.label == label
        }).unwrap_or_else(|| {
            eprintln!("Failed to find {:?}", label);
            panic!();
        }).0
    }

    pub fn is_halted(&self) -> bool {
        self.pc.0.is_empty()
    }

    fn take_branch(target: &BranchTarget, stack: &mut Stack, registers: &mut RegFile<i32>, pc: &mut Pc, bbs: &[BasicBlock<Instr>]) {
        let mut params = Vec::new();
        for ty in target.ty.iter().rev() {
            params.push(stack.pop_ty(*ty));
        }
        params.reverse();

        // FIXME:
        for ((idx, ty), v) in target.ty.iter().enumerate().zip(params.iter()) {
            match ty {
                Type::I32 => {
                    registers.set_i32(Register::Return(idx as u32), v.0);
                }
                Type::I64 => {
                    registers.set_pair(Register::Return(idx as u32), *v);
                }
                _ => todo!(),
            }
        }

        for _ in 0..target.to_pop {
            stack.pop_value();
        }

        pc.jump(&target.label, bbs);

        for (param, ty) in params.into_iter().zip(target.ty.iter()) {
            stack.push_ty(*ty, param);
        }
    }

	/// Returns true when it ends
	pub fn step(&mut self) -> StateResult<bool> {
		use Instr::*;

		let mut incr_pc = true;

        let last_pc = *self.pc.0.last().unwrap();

		let bb = &self.bbs[last_pc.0];

		let instr = &bb.instrs[last_pc.1];

        println!("RUNNING {:?}", instr);

		match instr {
			PushFrame(l) => {
				self.frames.push(Frame::new(*l));
			}
			PopFrame(l) => {
				let f = self.frames.pop().unwrap();
                assert_eq!(f.data.len(), *l as usize);
			}
			PushReturnAddress(r) => {
                let idx = Self::get_pc(&self.bbs, r);

				self.stack.push_i32(idx as i32);
			}
            PushI64Const(c) => {
                self.stack.push_i64(*c);
            }
            PushI32Const(c) => {
                self.stack.push_i32(*c);
            }

            &Copy { dst, src } => {
                let v = self.registers.get_half(src)?;
                self.registers.set_half(dst, v);
            }

            SetTurtleCoord(r, axis) => {
                let v = self.registers.get_i32(*r)?;
                match axis {
                    Axis::X => self.turtle.0 = v,
                    Axis::Y => self.turtle.1 = v,
                    Axis::Z => self.turtle.2 = v,
                }
            }
            SetTurtleBlock(r) => {
                let v = self.registers.get_i32(*r)?;
                println!("TODO: Set block {} at {:?}", v, self.turtle);
            }
            TurtleGet(_r) => todo!(),

            SetLocalPtr(l) => {
                self.local_ptr = *l;
            }
            LoadLocalI64(r) => {
                let f = self.frames.last().unwrap();
                let v = f.data[self.local_ptr as usize];
                println!("Loading {:?}", v);
                self.registers.set_pair(*r, v);
            }
            StoreLocalI64(r) => {
                let f = self.frames.last_mut().unwrap();
                let v = self.registers.get_pair(*r)?;
                println!("Storing {:?}", v);
                f.data[self.local_ptr as usize] = v;
            }
            LoadLocalI32(r) => {
                let f = self.frames.last().unwrap();
                let v = f.data[self.local_ptr as usize].0;
                self.registers.set_i32(*r, v);
            }
            StoreLocalI32(r) => {
                let f = self.frames.last_mut().unwrap();
                let v = self.registers.get_i32(*r)?;
                f.data[self.local_ptr as usize].0 = v;
            }

            &I32MulTo64 { dst, lhs, rhs } => {
                let l = self.registers.get_half(lhs)? as u32 as u64;
                let r = self.registers.get_half(rhs)? as u32 as u64;
                let d = l * r;
                println!("l: {:#X}, r: {:#X}, d: {:#X}", l, r, d);
                self.registers.set_i64(dst, d as i64);
            }

            &I64ExtendI32S { dst, src } => {
                let v = self.registers.get_i32(src)? as i64;
                self.registers.set_i64(dst, v);
            }
            &I64ExtendI32U(reg) => {
                let v = self.registers.get_i32(reg)? as u32 as u64 as i64;
                self.registers.set_i64(reg, v);
            }

            &I64UComp { dst, lhs, op, rhs } => {
                let l = self.registers.get_i64(lhs)? as u64;
                let r = self.registers.get_i64(rhs)? as u64;

                let d = match op {
                    crate::Relation::LessThan => l < r,
                    crate::Relation::LessThanEq => l <= r,
                    crate::Relation::GreaterThan => l > r,
                    crate::Relation::GreaterThanEq => l >= r,
                };

                // TODO: This should be a part of the instruction, probably?
                self.registers.clobber_pair(Register::Work(3));
                self.registers.clobber_pair(Register::Work(4));
                self.registers.clobber_pair(Register::Work(5));
                self.registers.clobber_pair(Register::Work(6));

                println!("l: {:#X}, r: {:#X}", l, r);
                self.registers.set_i32(dst, d as i32);
            }
            &I64SComp { dst, lhs, op, rhs } => {
                let l = self.registers.get_i64(lhs)?;
                let r = self.registers.get_i64(rhs)?;

                let d = match op {
                    crate::Relation::LessThan => l < r,
                    crate::Relation::LessThanEq => l <= r,
                    crate::Relation::GreaterThan => l > r,
                    crate::Relation::GreaterThanEq => l >= r,
                };

                println!("l: {:#X}, r: {:#X}", l, r);
                self.registers.set_i32(dst, d as i32);
            }


            ResetFrames => {
                todo!()
            }

            &SetConst(r, v) => {
                self.registers.set_half(r, v);
            }

            &SetMemPtr(r) => {
                let v = self.registers.get_i32(r)?;
                assert!(v >= 0, "{:?}", v);
                self.memory_ptr = v as u32;
            }
            &StoreI32(r, _a) => {
                let v = self.registers.get_i32(r)?;
                let v = v.to_le_bytes();

                let page = &mut self.memory[self.memory_ptr as usize / 65536];
                page[self.memory_ptr as usize % 65536..][..4].copy_from_slice(&v);
            }
            &StoreRow(r) => {
                let v = self.registers.get_i32(r)?;
                let v = v.to_le_bytes();

                let page = &mut self.memory[self.memory_ptr as usize / 65536];
                for i in 0..8 {
                    page[self.memory_ptr as usize % 65536 + 4 * i..][..4].copy_from_slice(&v);
                }
            }
            &StoreI32_16(r, _a) => {
                let v = self.registers.get_i32(r)? as u16;
                let v = v.to_le_bytes();

                let page = &mut self.memory[self.memory_ptr as usize / 65536];
                page[self.memory_ptr as usize % 65536..][..2].copy_from_slice(&v);
            }
            &StoreI32_8(r, _a) => {
                let v = self.registers.get_i32(r)? as u8;
                let v = v.to_le_bytes();

                let page = &mut self.memory[self.memory_ptr as usize / 65536];
                page[self.memory_ptr as usize % 65536..][..1].copy_from_slice(&v);
            }
            &LoadI32(r, _a) => {
                let page = &mut self.memory[self.memory_ptr as usize / 65536];
                
                let v = page[self.memory_ptr as usize % 65536..][..4].try_into().unwrap();
                let v = i32::from_le_bytes(v);
                self.registers.set_i32(r, v);
            }
            &LoadI32_16S(r, _a) => {
                let page = &mut self.memory[self.memory_ptr as usize / 65536];
                
                let v = page[self.memory_ptr as usize % 65536..][..2].try_into().unwrap();
                let v = u16::from_le_bytes(v) as i16 as i32;
                self.registers.set_i32(r, v);
            }
            &LoadI32_16U(r, _a) => {
                let page = &mut self.memory[self.memory_ptr as usize / 65536];
                
                let v = page[self.memory_ptr as usize % 65536..][..2].try_into().unwrap();
                let v = u16::from_le_bytes(v) as u32 as i32;
                self.registers.set_i32(r, v);
            }
            &LoadI32_8S(r, _a) => {
                let page = &mut self.memory[self.memory_ptr as usize / 65536];
                
                let v = page[self.memory_ptr as usize % 65536..][..1].try_into().unwrap();
                let v = u8::from_le_bytes(v) as i8 as i32;
                self.registers.set_i32(r, v);
            }
            &LoadI32_8U(r, _a) => {
                let page = &mut self.memory[self.memory_ptr as usize / 65536];
                
                let v = page[self.memory_ptr as usize % 65536..][..1].try_into().unwrap();
                let v = u8::from_le_bytes(v) as u32 as i32;
                self.registers.set_i32(r, v);
            }



            SetGlobalPtr(v) => {
                self.global_ptr = *v;
            }
            &LoadGlobalI64(r) => {
                let v = self.globals.get_pair(self.global_ptr as usize);
                self.registers.set_pair(r, v);
            }
            &StoreGlobalI64(r) => {
                let v = self.registers.get_pair(r)?;
                self.globals.set_pair(self.global_ptr as usize, v);
            }
            &LoadGlobalI32(r) => {
                let v = self.globals.get_lo(self.global_ptr as usize);
                self.registers.set_i32(r, v);
            }
            &StoreGlobalI32(r) => {
                let v = self.registers.get_i32(r)?;
                self.globals.set_lo(self.global_ptr as usize, v);
            }

            &PushI32From(r) => {
                let v = self.registers.get_i32(r)?;
                self.stack.push_i32(v);
            }
            &PushI64From(r) => {
                let v = self.registers.get_i64(r)?;
                self.stack.push_i64(v);
            }
            &PopI32Into(r) => {
                let v = self.stack.pop_i32();

                self.registers.set_i32(r, v);
            }
            &PopI64Into(r) => {
                let v = self.stack.pop_i64();

                self.registers.set_i64(r, v);
            }

            Drop => {
                let _ = self.stack.pop_value();
            }
            &SelectI32 { dst_reg, true_reg, false_reg, cond_reg } => {
                let c = self.registers.get_i32(cond_reg)?;
                let t = self.registers.get_i32(true_reg)?;
                let f = self.registers.get_i32(false_reg)?;

                let result = if c != 0 { t } else { f };

                self.registers.set_i32(dst_reg, result);
            }
            &SelectI64 { dst_reg, true_reg, false_reg, cond_reg } => {
                let c = self.registers.get_i32(cond_reg)?;
                let t = self.registers.get_i64(true_reg)?;
                let f = self.registers.get_i64(false_reg)?;

                let result = if c != 0 { t } else { f };

                self.registers.set_i64(dst_reg, result);
            }
            &I64Eq { dst, lhs, invert, rhs } => {
                let l = self.registers.get_i64(lhs)?;
                let r = self.registers.get_i64(rhs)?;
                let d = if invert {
                    l != r
                } else {
                    l == r
                };

                self.registers.set_i32(dst, d as i32);
            }

            Branch(target) => {
                Self::take_branch(target, &mut self.stack, &mut self.registers, &mut self.pc, &self.bbs);
                incr_pc = false;
            }
            BranchIf { t_name, f_name, cond } => {
                let c = self.registers.get_i32(*cond)?;
                let target = if c != 0 {
                    t_name
                } else {
                    f_name
                };

                Self::take_branch(target, &mut self.stack, &mut self.registers, &mut self.pc, &self.bbs);
                incr_pc = false;
            }
            BranchTable { reg, targets, default } => {
                let dest = self.registers.get_i32(*reg)?;

                let target = if let Ok(dest) = usize::try_from(dest) {
                    if let Some(target) = targets.get(dest) {
                        target
                    } else if let Some(default) = default {
                        default
                    } else {
                        panic!("Branch out of bounds and no default target: {}", dest);
                    }
                } else if let Some(default) = default {
                    default
                } else {
                    panic!("Branch out of bounds and no default target: {}", dest);
                };

                Self::take_branch(target, &mut self.stack, &mut self.registers, &mut self.pc, &self.bbs);
                incr_pc = false;
            }

            &I32Ctz(reg) => {
                let mut val = self.registers.get_i32(reg)?;
                val = val.trailing_zeros() as i32;
                self.registers.set_i32(reg, val);
            }
            &I32Clz(reg) => {
                let mut val = self.registers.get_i32(reg)?;
                val = val.leading_zeros() as i32;
                self.registers.set_i32(reg, val);
            }
            &I64Ctz { dst, src } => {
                let mut val = self.registers.get_i64(src)?;
                val = val.trailing_zeros() as i64;
                self.registers.set_i64(dst, val);
            }
            &I64Clz { dst, src } => {
                let mut val = self.registers.get_i64(src)?;
                val = val.leading_zeros() as i64;
                self.registers.set_i64(dst, val);
            }
            &I64Add { dst, lhs, rhs } => {
                let l = self.registers.get_i64(lhs)?;
                let r = self.registers.get_i64(rhs)?;
                let d = l.wrapping_add(r);
                println!("l: {:#X}, r: {:#X}, d: {:#X}", l, r, d);
                self.registers.set_i64(dst, d);
            }
            &I64Shl { dst, lhs, rhs } => {
                let l = self.registers.get_i64(lhs)?;
                let r = self.registers.get_i64(rhs)?;
                let d = l.wrapping_shl(r as u32);
                self.registers.set_i64(dst, d);
            }
            &I64ShrU { dst, lhs, rhs } => {
                let l = self.registers.get_i64(lhs)? as u64;
                let r = self.registers.get_i64(rhs)? as u64;
                let d = l.wrapping_shr(r as u32);
                println!("l, r, d: {} {} {}", l, r, d);
                self.registers.set_i64(dst, d as i64);
            }
            &I64DivU { dst, lhs, rhs } => {
                let l = self.registers.get_i64(lhs)? as u64;
                let r = self.registers.get_i64(rhs)? as u64;
                let d = l / r;
                self.registers.set_i64(dst, d as i64);

                for i in 0..6 {
                    self.registers.clobber_pair(Register::Work(i));
                }
            }
            &I64DivS { dst, lhs, rhs } => {
                let l = self.registers.get_i64(lhs)?;
                let r = self.registers.get_i64(rhs)?;
                let d = l.wrapping_div(r);
                self.registers.set_i64(dst, d);

                for i in 0..6 {
                    self.registers.clobber_pair(Register::Work(i));
                }
            }
            &I64RemS { dst, lhs, rhs } => {
                let l = self.registers.get_i64(lhs)?;
                let r = self.registers.get_i64(rhs)?;
                let d = l.wrapping_rem(r);
                self.registers.set_i64(dst, d);

                for i in 0..6 {
                    self.registers.clobber_pair(Register::Work(i));
                }
            }
            &I64RemU { dst, lhs, rhs } => {
                let l = self.registers.get_i64(lhs)? as u64;
                let r = self.registers.get_i64(rhs)? as u64;
                let d = l % r;
                self.registers.set_i64(dst, d as i64);

                for i in 0..6 {
                    self.registers.clobber_pair(Register::Work(i));
                }
            }

            &I64Rotl { dst, lhs, rhs } => {
                let l = self.registers.get_i64(lhs)?;
                let r = self.registers.get_i64(rhs)?;
                let d = l.rotate_left((r as u32) % 64);
                self.registers.set_i64(dst, d);
               
            }
            &I64Rotr { dst, lhs, rhs } => {
                let l = self.registers.get_i64(lhs)?;
                let r = self.registers.get_i64(rhs)?;
                let d = l.rotate_right((r as u32) % 64);
                self.registers.set_i64(dst, d);
            }

            &I64ShrS { dst, lhs, rhs } => {
                let l = self.registers.get_i64(lhs)?;
                let r = self.registers.get_i64(rhs)?;
                let d = l.wrapping_shr(r as u32);
                self.registers.set_i64(dst, d);
            }
            &I32Op { dst, lhs, op, rhs } => {
                let l = self.registers.get_half(lhs)?;
                let r = self.registers.get_half(rhs)?;

                let d = match op {
                    "+=" => l.wrapping_add(r),
                    "-=" => l.wrapping_sub(r),
                    "*=" => l.wrapping_mul(r),
                    "/=" => {
                        self.registers.clobber_half(lhs);
                        self.registers.clobber_half(rhs);
                        l.wrapping_div(r)
                    },
                    "/=u" => ((l as u32) / (r as u32)) as i32,
                    "remu" => ((l as u32) % (r as u32)) as i32,
                    "rems" => l.wrapping_rem(r),
                    "&=" => l & r,
                    "|=" => l | r,
                    "^=" => l ^ r,
                    "shl" => l.wrapping_shl(r as u32),
                    "shru" => ((l as u32).wrapping_shr(r as u32)) as i32,
                    "shrs" => {
                        self.registers.clobber_half(rhs);
                        l.wrapping_shr(r as u32)
                    },
                    "rotl" => l.rotate_left((r as u32) % 32),
                    "rotr" => l.rotate_right((r as u32) % 32),
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

                println!("{} {} {}", l, r, d);

                self.registers.set_half(dst, d);
            }
            &I32Popcnt(reg) => {
                let v = self.registers.get_half(reg)?;
                self.registers.set_half(reg, v.count_ones() as i32);
            }
            &I32Extend8S(reg) => {
                let v = self.registers.get_i32(reg)?;
                self.registers.set_i32(reg, v as i8 as i32);
            }
            &I32Extend16S(reg) => {
                let v = self.registers.get_i32(reg)?;
                self.registers.set_i32(reg, v as i16 as i32);
            }
            &I32Eqz { val, cond } => {
                let v = self.registers.get_i32(val)?;
                let c = (v == 0) as i32;
                self.registers.set_i32(cond, c);
            }
            &I64Eqz { val, cond } => {
                let v = self.registers.get_i64(val)?;
                let c = (v == 0) as i32;
                self.registers.set_i32(cond, c);
            }
            &AddI32Const(r, rhs) => {
                let mut lhs = self.registers.get_half(r)?;
                lhs = lhs.wrapping_add(rhs);
                self.registers.set_half(r, lhs);
            }

            &DynBranch(w, t) => {
                let a = self.registers.get_i32(w)?;

                let dest_idx = if let Some(t) = t {
                    let table = &self.tables[t as usize];

                    let func_idx = table.elements[a as usize].unwrap();

                    self.bbs.iter().enumerate().find(|(_, bb)| bb.label.func_idx == func_idx).unwrap().0
                } else {
                    if a == -1 {
                        self.pc.incr();
                        self.pc.unwind(&self.bbs);
                        return Ok(true);
                    }

                    a as usize
                };

                self.pc.jump_to((dest_idx, 0));
                incr_pc = false;

                println!("Branched to {} {:?}", dest_idx, last_pc);
            }

            Unreachable => {
                println!("UNREACHABLE AAAAAA");
                return Ok(true);
            }
			Comment(_) => {},
            Tellraw(t) => {
                // TODO:
                println!("{}", t);
            }
		}

		if incr_pc {
            self.pc.incr();
		}

        self.pc.unwind(&self.bbs);

        Ok(self.is_halted())
	}
}