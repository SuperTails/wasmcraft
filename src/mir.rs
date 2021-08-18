use std::collections::HashMap;

use crate::{BRANCH_CONV, BasicBlock, BranchConv, CodeFuncIdx, HalfRegister, Label, OpStack, RealOpStack, Register, get_entry_point, get_local_ty, wasm::{FunctionList, MemoryList, TypeList, WasmFile}};
use wasmparser::{FuncType, MemoryImmediate, Operator, Type, TypeDef, TypeOrFuncType};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Relation {
    GreaterThan,
    LessThan,
    GreaterThanEq,
    LessThanEq,
}

#[derive(Debug, Clone)]
pub struct BranchTarget {
    pub label: Label,
    pub to_pop: usize,
    pub ty: Box<[Type]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Axis {
    X,
    Y,
    Z,
}

#[derive(Debug, Clone)]
pub enum Instr {
    Comment(String),
    Tellraw(String),

    SetTurtleCoord(Register, Axis),
    SetTurtleBlock(Register),
    TurtleGet(Register),

    PushI32From(Register),
    PushI64From(Register),
    PushI32Const(i32),
    PushI64Const(i64),
    PushReturnAddress(Label),

    SetConst(HalfRegister, i32),
    Copy { dst: HalfRegister, src: HalfRegister },

    Branch(BranchTarget),
    // Index into table, index of table
    // (None represents the wasmcraft-specific table)
    DynBranch(Register, Option<u32>),
    BranchIf { t_name: BranchTarget, f_name: BranchTarget, cond: Register },
    BranchTable { reg: Register, targets: Vec<BranchTarget>, default: Option<BranchTarget> },

    I64Add { dst: Register, lhs: Register, rhs: Register },
    I64DivS { dst: Register, lhs: Register, rhs: Register },
    I64DivU { dst: Register, lhs: Register, rhs: Register },
    I64RemS { dst: Register, lhs: Register, rhs: Register },
    I64RemU { dst: Register, lhs: Register, rhs: Register },
    I64Rotl { dst: Register, lhs: Register, rhs: Register },
    I64Rotr { dst: Register, lhs: Register, rhs: Register },

    I64Eq { dst: Register, lhs: Register, invert: bool, rhs: Register },
    I64UComp { dst: Register, lhs: Register, op: Relation, rhs: Register },
    I64SComp { dst: Register, lhs: Register, op: Relation, rhs: Register },

    I64Shl { dst: Register, lhs: Register, rhs: Register },
    I64ShrU { dst: Register, lhs: Register, rhs: Register },
    I64ShrS { dst: Register, lhs: Register, rhs: Register },

    I32MulTo64 { dst: Register, lhs: HalfRegister, rhs: HalfRegister },

    I32Op { dst: HalfRegister, lhs: HalfRegister, op: &'static str, rhs: HalfRegister },

    I32Extend8S(Register),
    I32Extend16S(Register),
    I32Popcnt(HalfRegister),
    I32Ctz(Register),
    I32Clz(Register),
    I64Clz { dst: Register, src: Register },
    I64Ctz { dst: Register, src: Register },

    PopI32Into(Register),
    PopI64Into(Register),

    LoadGlobalI32(Register),
    StoreGlobalI32(Register),
    LoadGlobalI64(Register),
    StoreGlobalI64(Register),

    LoadLocalI32(Register),
    StoreLocalI32(Register),
    LoadLocalI64(Register),
    StoreLocalI64(Register),

    /// reg, align
    /// reg = *memptr
    LoadI32(Register, u8),
    LoadI32_8U(Register, u8),
    LoadI32_8S(Register, u8),
    LoadI32_16U(Register, u8),
    LoadI32_16S(Register, u8),
    /// reg, align
    /// *memptr = reg
    StoreI32(Register, u8),
    StoreI32_8(Register, u8),
    StoreI32_16(Register, u8),

    /// Stores 8 consecutive words
    StoreRow(Register),

    AddI32Const(HalfRegister, i32),
    I64ExtendI32S { dst: Register, src: Register },
    I64ExtendI32U(Register),


    I32Eqz { val: Register, cond: Register },
    I64Eqz { val: Register, cond: Register },

    SetMemPtr(Register),
    SetGlobalPtr(u32),
    SetLocalPtr(u32),

    ResetFrames,
    PushFrame(u32),
    PopFrame(u32),

    Drop,
    SelectI32 { dst_reg: Register, true_reg: Register, false_reg: Register, cond_reg: Register },
    SelectI64 { dst_reg: Register, true_reg: Register, false_reg: Register, cond_reg: Register },

    Unreachable,
}

impl Instr {
    pub fn pop_into(reg: Register, ty: Type) -> Self {
        match ty {
            Type::I32 => Instr::PopI32Into(reg),
            Type::I64 => Instr::PopI64Into(reg),
            _ => todo!("{:?}", ty)
        }
    }

    pub fn push_from(reg: Register, ty: Type) -> Self {
        match ty {
            Type::I32 => Instr::PushI32From(reg),
            Type::I64 => Instr::PushI64From(reg),
            _ => todo!("{:?}", ty)
        }
    }

    pub fn store_local(reg: Register, ty: Type) -> Self {
        match ty {
            Type::I32 => Instr::StoreLocalI32(reg),
            Type::I64 => Instr::StoreLocalI64(reg),
            _ => todo!("{:?}", ty)
        }
    }

    pub fn load_local(reg: Register, ty: Type) -> Self {
        match ty {
            Type::I32 => Instr::LoadLocalI32(reg),
            Type::I64 => Instr::LoadLocalI64(reg),
            _ => todo!("{:?}", ty)
        }
    }

    pub fn store_global(reg: Register, ty: Type) -> Self {
        match ty {
            Type::I32 => Instr::StoreGlobalI32(reg),
            Type::I64 => Instr::StoreGlobalI64(reg),
            _ => todo!("{:?}", ty)
        }
    }

    pub fn load_global(reg: Register, ty: Type) -> Self {
        match ty {
            Type::I32 => Instr::LoadGlobalI32(reg),
            Type::I64 => Instr::LoadGlobalI64(reg),
            _ => todo!("{:?}", ty)
        }
    }

    pub fn set_i64_const(reg: Register, val: i64) -> [Self; 2] {
        [
            Instr::SetConst(reg.as_lo(), val as i32),
            Instr::SetConst(reg.as_hi(), (val >> 32) as i32)
        ]
    }

    pub fn i64_neg(reg: Register) -> Vec<Self> {
        let neg_one = Register::Work(3).as_lo();

        let pos_one = Register::Work(4);

        let temp = Register::Work(5);

        assert_ne!(reg, neg_one.0);
        assert_ne!(reg, pos_one);
        assert_ne!(reg, temp);

        vec![
            Instr::SetConst(neg_one, -1),
            Instr::SetConst(pos_one.as_lo(), 1),
            Instr::SetConst(pos_one.as_hi(), 0),

            // Bitwise invert the low part
            Instr::I32Op { dst: reg.as_lo(), lhs: reg.as_lo(), op: "*=", rhs: neg_one },
            Instr::AddI32Const(reg.as_lo(), -1),

            // Bitwise invert the high part
            Instr::I32Op { dst: reg.as_hi(), lhs: reg.as_hi(), op: "*=", rhs: neg_one },
            Instr::AddI32Const(reg.as_hi(), -1),

            Instr::I64Add { dst: temp, lhs: reg, rhs: pos_one },
            Instr::Copy { dst: reg.as_lo(), src: temp.as_lo() },
            Instr::Copy { dst: reg.as_hi(), src: temp.as_hi() },
        ]
    }

    pub fn i64_mul(dst: Register, lhs: Register, rhs: Register) -> Vec<Self> {
        assert_ne!(dst, lhs);
        assert_ne!(dst, rhs);

        let lo_product = Register::Work(3);

        assert!(!matches!(dst, Register::Work(3 | 4 | 5)));
        assert!(!matches!(lhs, Register::Work(3 | 4 | 5)));
        assert!(!matches!(rhs, Register::Work(3 | 4 | 5)));

        vec![
            // dst = 0
            Instr::SetConst(dst.as_lo(), 0),
            Instr::SetConst(dst.as_hi(), 0),

            // lo_product = (lhs as i64) * (rhs as i64)
            Instr::I32MulTo64 { dst: lo_product, lhs: lhs.as_lo(), rhs: rhs.as_lo() },

            // mid_1 = lhs_lo = lhs_lo * rhs_hi
            Instr::I32Op { dst: lhs.as_lo(), lhs: lhs.as_lo(), op: "*=", rhs: rhs.as_hi() },
            // mid_2 = lhs_hi = lhs_hi * rhs_lo
            Instr::I32Op { dst: lhs.as_hi(), lhs: lhs.as_hi(), op: "*=", rhs: rhs.as_lo() },

            // dst[0] = lo_product[0]
            Instr::I32Op { dst: dst.as_lo(), lhs: dst.as_lo(), op: "+=", rhs: lo_product.as_lo() },

            // dst[1] = lo_product[1]
            Instr::I32Op { dst: dst.as_hi(), lhs: dst.as_hi(), op: "+=", rhs: lo_product.as_hi() },

            // dst[1] += mid_1
            // dst[2] += mid_2
            Instr::I32Op { dst: dst.as_hi(), lhs: dst.as_hi(), op: "+=", rhs: lhs.as_lo() },
            Instr::I32Op { dst: dst.as_hi(), lhs: dst.as_hi(), op: "+=", rhs: lhs.as_hi() },
        ]
    }


    /*
    pub fn modify_stack(&self, op_stack: &mut OpStack, global_list: &GlobalList) {
        use Instr::*;

        match self {
            Comment(_) => {},
            Tellraw(_) => {},
            SetTurtleCoord(_, _) => {},
            SetTurtleBlock(_) => {},
            SetMemPtr(_) => {},
            PushValueFrom(_) => op_stack.push_value(),
            PushI32From(_) => op_stack.push_i32(),
            PushI64FromSplit { lo, hi } => op_stack.push_i64(),
            PushI32Const(_) => op_stack.push_i32(),
            PushI64Const(_) => op_stack.push_i64(),
            PushReturnAddress(_) => op_stack.push_i32(),
            Branch(_) => {},
            DynBranch(_, _) => {},
            BranchIf { t_name, f_name, cond } => {},
            BranchTable { reg, targets, default } => {},
            I32Op { dst, lhs, op, rhs } => {},
            PopValueInto(_) => op_stack.pop_value(),
            PopI32Into(_) => op_stack.pop_i32(),
            LoadGlobalI32(_) => {},
            StoreGlobalI32(_) => {},
            LoadGlobalI64(_) => {},
            StoreGlobalI64(_) => {},
            LoadLocalI32(_) => {},
            StoreLocalI32(_) => {},
            LoadLocalI64(_) => {},
            StoreLocalI64(_) => {},
            LoadI32(_, _) => {},
            LoadI32_8U(_, _) => {},
            LoadI32_16U(_, _) => {},
            StoreI32(_, _) => {},
            StoreI32_8(_, _) => {},
            StoreI32_16(_, _) => {},
            StoreRow(_) => {},
            AddI32Const(_, _) => {},
            I64ExtendI32S { dst, src } => {},
            I32Eqz { val, cond } => {},
            SetGlobalPtr(_) => {},
            SetLocalPtr(_) => {},
            ResetFrames => {},
            PushFrame(_) => {},
            PopFrame(_) => {},
            Drop => op_stack.pop_value(),
            SelectI32 { dst_reg, true_reg, false_reg, cond_reg } => {},
            Unreachable => {},
        }
    }
    */


}

pub fn get_reachable_blocks(body: &[BasicBlock<Instr>]) -> Vec<bool> {
    let mut reachable = vec![false; body.len()];
    let mut to_visit = vec![0];

    while let Some(block) = to_visit.pop() {
        if reachable[block] {
            // This block has already been visited
            continue
        }

        reachable[block] = true;
        for idx in get_next_blocks(&body[block]) {
            to_visit.push(idx);
        }
    }

    reachable
}

// FIXME: This is broken
pub fn get_next_blocks(basic_block: &BasicBlock<Instr>) -> Vec<usize> {
    let body = get_body_instrs(basic_block);
    let term = get_term_instrs(basic_block);

    let mut return_addr = None;

    for instr in body.iter() {
        match instr {
            Instr::PushReturnAddress(l) => {
                // The return address is considered to be on the callee's stack
                assert!(return_addr.is_none());
                return_addr = Some(l);
            }
            Instr::Branch(_) => todo!(),
            Instr::BranchIf { .. } => todo!(),
            Instr::BranchTable { .. } => todo!(),
            Instr::DynBranch(..) => todo!(),
            _ => {}
        }
    }

    term.iter().flat_map(|t| {
        match t {
            Instr::Branch(t) => {
                vec![&t.label]
            }
            Instr::BranchIf { t_name, f_name, .. } => {
                vec![&t_name.label, &f_name.label]
            }
            Instr::BranchTable { targets, default, .. } => {
                let mut result = Vec::new();

                result.extend(targets.iter().map(|t| &t.label));
                result.extend(default.iter().map(|t| &t.label));

                result
            }
            Instr::DynBranch(..) => {
                return_addr.into_iter().collect()
            }
            _ => todo!("{:?}", t)
        }
    }).map(|t| {
        if t.func_idx != basic_block.label.func_idx {
            return_addr.unwrap().idx
        } else {
            t.idx
        }
    }).collect()
}

pub fn get_first_term_instr(basic_block: &BasicBlock<Instr>) -> usize {
    // TODO: Internal branches
    basic_block.instrs.iter().enumerate().find_map(|(idx, instr)| {
        match instr {
            Instr::Branch(_) => {
                Some(idx)
            }
            Instr::BranchIf { .. } => {
                Some(idx)
            }
            Instr::BranchTable { .. } => {
                Some(idx)
            }
            Instr::DynBranch(_, _) => {
                Some(idx)
            }
            _ => None
        }
    }).unwrap_or(basic_block.instrs.len())
}

pub fn get_body_instrs(basic_block: &BasicBlock<Instr>) -> &[Instr] {
    let idx = get_first_term_instr(basic_block);
    &basic_block.instrs[..idx]
}

pub fn get_term_instrs(basic_block: &BasicBlock<Instr>) -> &[Instr] {
    let idx = get_first_term_instr(basic_block);
    &basic_block.instrs[idx..]
}

pub fn get_next_state(basic_block: &BasicBlock<Instr>, entry_state: RealOpStack) -> Vec<(usize, RealOpStack)> {
    let body = get_body_instrs(basic_block);
    let term = get_term_instrs(basic_block);

    let mut state = entry_state;

    let mut return_addr = None;

    for instr in body.iter() {
        match instr {
            Instr::PushReturnAddress(l) => {
                // The return address is considered to be on the callee's stack
                assert!(return_addr.is_none());
                return_addr = Some(l);
            }
            Instr::Drop => { state.pop_value(); },
            Instr::PushI32Const(_) => state.push_i32(),
            Instr::PushI64Const(_) => state.push_i64(),
            Instr::PushI32From(..) => state.push_i32(),
            Instr::PushI64From(..) => state.push_i64(),
            Instr::PopI32Into(..) => { state.pop_i32(); },
            Instr::PopI64Into(..) => { state.pop_i64(); },
            Instr::Branch(t) => {
                for ty in t.ty.iter() {
                    state.pop_ty(*ty);                   
                }

                for _ in 0..t.to_pop {
                    state.pop_value();
                }

                if t.label.func_idx != basic_block.label.func_idx {
                    state.reify_all();
                }
            },
            Instr::BranchIf { .. } => todo!(),
            Instr::BranchTable { .. } => todo!(),
            Instr::DynBranch(..) => {
                state.reify_all();
            }, 
            _ => {}
        }
    }

    let mut result = HashMap::new();

    let next_blocks = term.iter().flat_map(|t| {
        match t {
            Instr::Branch(t) => {
                vec![t.clone()]
            }
            Instr::BranchIf { t_name, f_name, .. } => {
                vec![t_name.clone(), f_name.clone()]
            }
            Instr::BranchTable { targets, default, .. } => {
                let mut result = Vec::new();

                result.extend(targets.iter().cloned());
                result.extend(default.iter().cloned());

                result
            }
            Instr::DynBranch(..) => {
                if let Some(return_addr) = return_addr {
                    vec![BranchTarget {
                        label: return_addr.clone(),
                        to_pop: 0,
                        ty: Box::new([]),
                    }]
                } else {
                    Vec::new()
                }
            }
            _ => todo!("{:?}", t)
        }
    }).map(|t| {
        let mut state = state.clone();

        for ty in t.ty.iter().rev() {
            state.pop_ty(*ty);                   
        }

        for _ in 0..t.to_pop {
            state.pop_value();
        }

        if t.label.func_idx != basic_block.label.func_idx {
            state.reify_all();
        }

        for ty in t.ty.iter() {
            state.push_ty(*ty);
        }

        if t.label.func_idx != basic_block.label.func_idx {
            (return_addr.unwrap().idx, state)
        } else {
            (t.label.idx, state)
        }
    });

    for next_block in next_blocks {
        if let Some(existing) = result.get(&next_block.0) {
            assert_eq!(existing, &next_block.1);
        } else {
            result.insert(next_block.0, next_block.1);
        }
    }

    result.into_iter().collect()
}

pub struct FuncBodyStream {
    func_idx: CodeFuncIdx,
    pub basic_blocks: Vec<BasicBlock<Instr>>,
    reachable: Vec<bool>,
    bb_index: usize,
    depth: usize,

    op_stack: OpStack,
    flow_stack: FlowStack,
}

impl FuncBodyStream {
    pub fn new(func_ty: TypeOrFuncType, types: &TypeList, func_idx: CodeFuncIdx) -> Self {
        let op_stack = if BRANCH_CONV == BranchConv::Direct { OpStack(Vec::new()) } else { OpStack(vec![Type::I32]) };
        
        let entry_block = BasicBlock::new(func_idx, 0, op_stack.clone());

        let mut exit_tys = if BRANCH_CONV == BranchConv::Direct { Vec::new() } else { vec![Type::I32] };
        exit_tys.extend(Vec::from(types.get_output_tys(func_ty)));

        let exit_block = BasicBlock::new(func_idx, 1, OpStack(exit_tys));

        FuncBodyStream {
            func_idx,
            // 0 is always the entry point, 1 is always the exit point
            basic_blocks: vec![entry_block, exit_block],
            reachable: vec![true, true],
            bb_index: 0,
            depth: 0,
            op_stack,
            flow_stack: FlowStack::new(func_ty, types, Label::new(func_idx, 1)),
        }
    }

    fn setup_epilogue(&mut self, local_count: u32, returns: &[Type]) {
        let old_bb_idx = self.bb_index;
        self.bb_index = 1;

        self.push_instr(Instr::Comment(" Pop frame".to_string()));
        self.push_instr(Instr::PopFrame(local_count));

        // Save return value so we can get to the 
        // return address
        self.push_instr(Instr::Comment(" Save return values".to_string()));
        for (idx, ty) in returns.iter().enumerate().rev() {
            self.push_instr(Instr::pop_into(Register::Return(idx as u32), *ty));
            self.op_stack.pop_ty(*ty);
        }

        match BRANCH_CONV {
            BranchConv::Grid | BranchConv::Chain | BranchConv::Loop => {
                self.push_instr(Instr::Comment(" Pop return address".to_string()));
                // Pop return address
                let reg = Register::Work(0);
                self.op_stack.pop_i32();
                self.push_instr(Instr::PopI32Into(reg));
                self.push_instr(Instr::DynBranch(reg, None));
            }
            BranchConv::Direct => {}
        }

        self.bb_index = old_bb_idx;
    }

    pub fn get_i32_dst(&mut self, lhs: Register, op: &str, _rhs: Register) -> Register {
        match op {
            "+=" | "-=" | "*=" | "%=" |
            "&=" | "|=" | "^=" | "shl" | "shru" | "shrs" | "rotl" | "rotr" => {
                lhs
            }
            "gts" | "ges" | "les" | "lts" | "==" | "!=" |
            "gtu" | "geu" | "leu" | "ltu" | "/=" | "/=u" | "remu" | "rems" => {
                Register::Work(2)
            }
            _ => {
                todo!("{:?}", op)
            }
        }

    }

    pub fn make_i32_binop(&mut self, op: &'static str) {
        let rhs = Register::Work(1);
        let lhs = Register::Work(0);

        self.push_instr(Instr::PopI32Into(rhs));
        self.op_stack.pop_i32();
        self.push_instr(Instr::PopI32Into(lhs));
        self.op_stack.pop_i32();
        let dst = self.get_i32_dst(lhs, op, rhs);

        self.push_instr(Instr::I32Op { dst: dst.as_lo(), lhs: lhs.as_lo(), op, rhs: rhs.as_lo() });

        self.push_instr(Instr::PushI32From(dst));
        self.op_stack.push_i32();
    }

    pub fn push_instr(&mut self, instr: Instr) {
        self.basic_blocks[self.bb_index].instrs.push(instr);
    }

    pub fn next_basic_block(&mut self) {
        self.bb_index = self.allocate_block(self.op_stack.clone());
    }

    pub fn mark_unreachable(&mut self) {
        self.reachable[self.bb_index] = false;
    }

    pub fn reachable(&self) -> bool {
        self.reachable[self.bb_index]
    }

    pub fn setup_arguments(&mut self, local_count: u32, types: &TypeList, function_list: &FunctionList) {
        self.push_instr(Instr::PushFrame(local_count));

        let ty = function_list.get_function_type(self.func_idx, types);

        for (param_idx, param) in ty.params.iter().enumerate() {
            self.push_instr(Instr::Comment(format!("#   Parameter {}", param_idx)));

            let reg = Register::Param(param_idx as u32);

            self.push_instr(Instr::SetLocalPtr(param_idx as u32));

            self.push_instr(Instr::store_local(reg, *param));
        }
    }

    /// `storer` is an `Instr` variant that takes the register to load from and the expected alignment
    pub fn store_i32_sized<S>(&mut self, storer: S, memarg: MemoryImmediate, _memory_list: &MemoryList)
        where S: FnOnce(Register, u8) -> Instr
    {
        assert_eq!(memarg.memory, 0);

        let dreg = Register::Work(1);
        let areg = Register::Work(0);

        self.push_instr(Instr::PopI32Into(dreg));
        self.op_stack.pop_i32();
        self.push_instr(Instr::PopI32Into(areg));
        self.op_stack.pop_i32();
        self.push_instr(Instr::AddI32Const(areg.as_lo(), memarg.offset as i32));
        self.push_instr(Instr::SetMemPtr(areg));

        self.push_instr(storer(dreg, memarg.align));
    }

    pub fn store_i32(&mut self, memarg: MemoryImmediate, memory_list: &MemoryList) {
        self.store_i32_sized(Instr::StoreI32, memarg, memory_list)
    }

    pub fn store_i32_8(&mut self, memarg: MemoryImmediate, memory_list: &MemoryList) {
        self.store_i32_sized(Instr::StoreI32_8, memarg, memory_list)
    }

    pub fn store_i32_16(&mut self, memarg: MemoryImmediate, memory_list: &MemoryList) {
        self.store_i32_sized(Instr::StoreI32_16, memarg, memory_list)
    }

    pub fn load_i32_sized<I>(&mut self, memarg: MemoryImmediate, _memory_list: &MemoryList, instr: I)
        where I: FnOnce(Register, u8) -> Instr,
    {
        assert_eq!(memarg.memory, 0);

        let reg = Register::Work(0);

        self.push_instr(Instr::PopI32Into(reg));
        self.op_stack.pop_i32();
        self.push_instr(Instr::AddI32Const(reg.as_lo(), memarg.offset as i32));
        self.push_instr(Instr::SetMemPtr(reg));

        self.push_instr(instr(reg, memarg.align));
        self.push_instr(Instr::PushI32From(reg));
        self.op_stack.push_i32();
    }

    pub fn load_i64_sized_u<I>(&mut self, memarg: MemoryImmediate, _memory_list: &MemoryList, instr: I)
        where I: FnOnce(Register, u8) -> Instr,
    {
        assert_eq!(memarg.memory, 0);

        let reg = Register::Work(0);

        self.push_instr(Instr::PopI32Into(reg));
        self.op_stack.pop_i32();
        self.push_instr(Instr::AddI32Const(reg.as_lo(), memarg.offset as i32));
        self.push_instr(Instr::SetMemPtr(reg));

        self.push_instr(Instr::SetConst(reg.as_hi(), 0));
        self.push_instr(instr(reg, memarg.align));
        self.push_instr(Instr::PushI64From(reg));
        self.op_stack.push_i64();
    }

    pub fn load_i64_sized_s<I>(&mut self, memarg: MemoryImmediate, _memory_list: &MemoryList, instr: I)
        where I: FnOnce(Register, u8) -> Instr,
    {
        assert_eq!(memarg.memory, 0);

        let reg = Register::Work(0);

        self.push_instr(Instr::PopI32Into(reg));
        self.op_stack.pop_i32();
        self.push_instr(Instr::AddI32Const(reg.as_lo(), memarg.offset as i32));
        self.push_instr(Instr::SetMemPtr(reg));

        self.push_instr(instr(reg, memarg.align));
        self.push_instr(Instr::I64ExtendI32S { dst: reg, src: reg });
        self.push_instr(Instr::PushI64From(reg));
        self.op_stack.push_i64();
    }


    pub fn load_i32(&mut self, memarg: MemoryImmediate, memory_list: &MemoryList) {
        self.load_i32_sized(memarg, memory_list, Instr::LoadI32)
    }

    pub fn load_i32_8u(&mut self, memarg: MemoryImmediate, memory_list: &MemoryList) {
        self.load_i32_sized(memarg, memory_list, Instr::LoadI32_8U)
    }

    pub fn load_i32_8s(&mut self, memarg: MemoryImmediate, memory_list: &MemoryList) {
        self.load_i32_sized(memarg, memory_list, Instr::LoadI32_8S)
    }

    pub fn load_i32_16u(&mut self, memarg: MemoryImmediate, memory_list: &MemoryList) {
        self.load_i32_sized(memarg, memory_list, Instr::LoadI32_16U)
    }

    pub fn load_i32_16s(&mut self, memarg: MemoryImmediate, memory_list: &MemoryList) {
        self.load_i32_sized(memarg, memory_list, Instr::LoadI32_16S)
    }


    pub fn local_set(&mut self, local_index: u32, locals: &[(u32, Type)]) {
        let reg = Register::Work(0);

        self.push_instr(Instr::SetLocalPtr(local_index));

        let ty = get_local_ty(locals, local_index);

        self.push_instr(Instr::pop_into(reg, ty));
        self.op_stack.pop_ty(ty);
        self.push_instr(Instr::store_local(reg, ty));
    }

    /// Grid calling convention:
    /// 
    /// The caller:
    /// Pops values from the stack into the parameter registers in the expected order.
    /// Pushes the return address to the stack.
    /// Branches to the callee
    pub fn static_call(&mut self, function_index: CodeFuncIdx, types: &TypeList, function_list: &FunctionList) {
        // Pop values from the stack to use as the arguments
        let ty = function_list.get_function_type(function_index, types);
        for (i, param) in ty.params.iter().enumerate().rev() {
            self.push_instr(Instr::pop_into(Register::Param(i as u32), *param));
            self.op_stack.pop_ty(*param);
        }

        match BRANCH_CONV {
            BranchConv::Grid | BranchConv::Chain | BranchConv::Loop => {
                // Push return address
                self.push_instr(Instr::Comment("  Push return address".to_string()));
                self.push_instr(Instr::PushReturnAddress(Label::new(self.func_idx, self.basic_blocks.len())));

            }
            BranchConv::Direct => {}
        }

        let target = BranchTarget {
            label: Label::new(function_index, 0),
            to_pop: 0,
            ty: Box::new([]),
        };

        // Jump to function
        self.push_instr(Instr::Branch(target));
    }

    pub fn dyn_call(&mut self, table_index: u32, ty: &FuncType) {
        // Pop function index
        self.push_instr(Instr::PopI32Into(Register::Work(0)));
        self.op_stack.pop_i32();

        // Pop values from the stack to use as the arguments
        for (i, param) in ty.params.iter().enumerate().rev() {
            self.push_instr(Instr::pop_into(Register::Param(i as u32), *param));
            self.op_stack.pop_ty(*param);
        }

        match BRANCH_CONV {
            BranchConv::Grid | BranchConv::Loop | BranchConv::Chain => {
                // Push return address
                self.push_instr(Instr::Comment("  Push return address".to_string()));
                self.push_instr(Instr::PushReturnAddress(Label::new(self.func_idx, self.basic_blocks.len())));
            }
            BranchConv::Direct => {}
        }

        // Jump to function
        self.push_instr(Instr::DynBranch(Register::Work(0), Some(table_index)));
    }

    pub fn get_target(&self, relative_depth: u32) -> BranchTarget {
        let entry = &self.flow_stack.0[self.flow_stack.0.len() - 1 - relative_depth as usize];
        let to_pop = self.op_stack.0.len() - entry.target_tys.len() - entry.stack_size;
        BranchTarget {
            label: entry.label.clone().unwrap(),
            to_pop,
            ty: entry.target_tys.clone(),
        }
    }

    fn allocate_block(&mut self, op_stack: OpStack) -> usize {
        let idx = self.basic_blocks.len();
        self.basic_blocks.push(BasicBlock::new(self.func_idx, idx, op_stack));
        self.reachable.push(true);
        idx
    }

    /// dst, lhs, rhs
    pub fn make_i64_binop<F>(&mut self, func: F)
        where F: FnOnce(Register, Register, Register) -> Vec<Instr>
    {
        let lhs = Register::Work(0);
        let rhs = Register::Work(1);
        let dst = Register::Work(2);
        self.op_stack.pop_i64();
        self.push_instr(Instr::PopI64Into(rhs));
        self.op_stack.pop_i64();
        self.push_instr(Instr::PopI64Into(lhs));

        for i in func(dst, lhs, rhs) {
            self.push_instr(i);
        }

        self.op_stack.push_i64();
        self.push_instr(Instr::PushI64From(dst));
    }

    pub fn visit_operator(&mut self, o: Operator, local_count: u32, locals: &[(u32, Type)], wasm_file: &WasmFile) {
        if self.basic_blocks.is_empty() {
            self.next_basic_block();
        }

        assert_eq!(self.depth + 1, self.flow_stack.0.len());

        self.push_instr(Instr::Comment(format!("{:?}", o)));

        println!("{:?}", o);

        if !self.reachable() && !matches!(o, If { .. } | Else { .. } | End | Block { .. } | Loop { .. }) {
            return;
        }

        use Operator::*;
        match o {
            Call { function_index } => { 
                let function_index = CodeFuncIdx(function_index);

                let imports = &wasm_file.imports;

                if imports.is_named_func(function_index, "turtle_x") {
                    self.op_stack.pop_i32();
                    self.push_instr(Instr::PopI32Into(Register::Work(0)));
                    self.push_instr(Instr::SetTurtleCoord(Register::Work(0), Axis::X))
                } else if imports.is_named_func(function_index, "turtle_y") {
                    self.op_stack.pop_i32();
                    self.push_instr(Instr::PopI32Into(Register::Work(0)));
                    self.push_instr(Instr::SetTurtleCoord(Register::Work(0), Axis::Y))
                } else if imports.is_named_func(function_index, "turtle_z") {
                    self.op_stack.pop_i32();
                    self.push_instr(Instr::PopI32Into(Register::Work(0)));
                    self.push_instr(Instr::SetTurtleCoord(Register::Work(0), Axis::Z))
                } else if imports.is_named_func(function_index, "turtle_set") {
                    self.op_stack.pop_i32();
                    self.push_instr(Instr::PopI32Into(Register::Work(0)));
                    self.push_instr(Instr::SetTurtleBlock(Register::Work(0)));
                } else if imports.is_named_func(function_index, "turtle_get") {
                    self.push_instr(Instr::TurtleGet(Register::Work(0)));
                    self.push_instr(Instr::PushI32From(Register::Work(0)));
                    self.op_stack.push_i32();
                } else {
                    self.push_instr(Instr::Comment(format!("#   wasm:{}", get_entry_point(function_index))));

                    self.static_call(function_index, &wasm_file.types, &wasm_file.functions);

                    match BRANCH_CONV {
                        BranchConv::Grid | BranchConv::Chain | BranchConv::Loop => {
                            self.next_basic_block();
                        }
                        BranchConv::Direct => {}
                    }

                    let f = wasm_file.functions.get_function_type(function_index, &wasm_file.types);
                    for (idx, ty) in f.returns.iter().enumerate() {
                        self.push_instr(Instr::push_from(Register::Return(idx as u32), *ty));
                        self.op_stack.push_ty(*ty);
                    }
                }
            }
            CallIndirect { index, table_index } => {
                let ty = &wasm_file.types.types[index as usize];
                let ty = if let TypeDef::Func(f) = ty {
                    f
                } else {
                    unreachable!()
                };

                self.dyn_call(table_index, ty);

                match BRANCH_CONV {
                    BranchConv::Grid | BranchConv::Chain | BranchConv::Loop => {
                        self.next_basic_block();
                    }
                    BranchConv::Direct => {}
                }

                for (idx, ty) in ty.returns.iter().enumerate() {
                    self.push_instr(Instr::push_from(Register::Return(idx as u32), *ty));
                    self.op_stack.push_ty(*ty);
                }

                //self.basic_blocks.push(BasicBlock::new(self.basic_blocks.len()));
            }
            Return => {

                /*
                for l in 0..local_count {
                    let reg = Register::Work(l);
                    self.push_instr(Instr::SetLocalPtr(l));
                    self.push_instr(Instr::LoadLocalI32(reg));

                    let mut msg = r#"[{"text":"local "#.to_string();
                    msg.push_str(&l.to_string());
                    msg.push_str(r#": "},"#);
                    
                    msg.push_str(r#"{"score":{"name":""#);
                    msg.push_str(&reg.get_lo());
                    msg.push_str(r#"","objective":"reg"}}]"#);

                    self.push_instr(Instr::Tellraw(msg));
                }
                */

                self.visit_operator(Operator::Br { relative_depth: self.depth as u32 }, local_count, locals, wasm_file);
            }

            GlobalGet { global_index } => { 
                self.push_instr(Instr::SetGlobalPtr(global_index));

                let reg = Register::Work(0);
                // FIXME:
                let ty = wasm_file.globals.globals[global_index as usize].ty.content_type;

                self.push_instr(Instr::load_global(reg, ty));
                self.push_instr(Instr::push_from(reg, ty));
                self.op_stack.push_ty(ty);
            }
            GlobalSet { global_index } => {
                let reg = Register::Work(0);
                // FIXME:
                let ty = wasm_file.globals.globals[global_index as usize].ty.content_type;

                self.push_instr(Instr::SetGlobalPtr(global_index));

                self.push_instr(Instr::pop_into(reg, ty));
                self.op_stack.pop_ty(ty);

                self.push_instr(Instr::store_global(reg, ty));
            }

            LocalSet { local_index } => {
                self.local_set(local_index, locals);
            }
            LocalGet { local_index } => {
                self.push_instr(Instr::SetLocalPtr(local_index));

                let reg = Register::Work(0);
                let ty = get_local_ty(locals, local_index);

                self.push_instr(Instr::load_local(reg, ty));
                self.push_instr(Instr::push_from(reg, ty));
                self.op_stack.push_ty(ty);
            }

            I64Const { value } => {
                self.push_instr(Instr::PushI64Const(value));
                self.op_stack.push_i64();
            }
            I32Const { value } => {
                self.push_instr(Instr::PushI32Const(value));
                self.op_stack.push_i32();
            }

            I32Store { memarg } => {
                self.store_i32(memarg, &wasm_file.memory);
            }
            I32Store8 { memarg } => {
                self.store_i32_8(memarg, &wasm_file.memory);
            }
            I32Store16 { memarg } => {
                self.store_i32_16(memarg, &wasm_file.memory);
            }
            I32Load { memarg } => {
                self.load_i32(memarg, &wasm_file.memory);
            }
            I32Load8U { memarg } => {
                self.load_i32_8u(memarg, &wasm_file.memory);
            }
            I32Load8S { memarg } => {
                self.load_i32_8s(memarg, &wasm_file.memory);
            }
            I32Load16U { memarg } => {
                self.load_i32_16u(memarg, &wasm_file.memory);
            }
            I32Load16S { memarg } => {
                self.load_i32_16s(memarg, &wasm_file.memory);
            }
            I64Store { memarg } => {
                let dreg_hi = Register::Work(2);
                let dreg_lo = Register::Work(1);
                let areg = Register::Work(0);

                self.push_instr(Instr::PopI64Into(dreg_lo));
                self.op_stack.pop_i64();

                // TODO: Remove
                self.push_instr(Instr::Copy { dst: dreg_hi.as_lo(), src: dreg_lo.as_hi() });

                self.push_instr(Instr::PopI32Into(areg));
                self.op_stack.pop_i32();
                self.push_instr(Instr::AddI32Const(areg.as_lo(), memarg.offset as i32));
                self.push_instr(Instr::SetMemPtr(areg));
                self.push_instr(Instr::StoreI32(dreg_lo, memarg.align));
                self.push_instr(Instr::AddI32Const(areg.as_lo(), 4));
                self.push_instr(Instr::SetMemPtr(areg));
                self.push_instr(Instr::StoreI32(dreg_hi, memarg.align));
            }
            I64Load { memarg } => {
                let areg = Register::Work(0);
                
                let dreg_lo = Register::Work(1);
                let dreg_hi = Register::Work(2);

                self.push_instr(Instr::PopI32Into(areg));
                self.op_stack.pop_i32();

                self.push_instr(Instr::AddI32Const(areg.as_lo(), memarg.offset as i32));
                self.push_instr(Instr::SetMemPtr(areg));
                self.push_instr(Instr::LoadI32(dreg_lo, memarg.align));
                self.push_instr(Instr::AddI32Const(areg.as_lo(), 4));
                self.push_instr(Instr::SetMemPtr(areg));
                self.push_instr(Instr::LoadI32(dreg_hi, memarg.align));

                // TODO: Remove
                self.push_instr(Instr::Copy { dst: dreg_lo.as_hi(), src: dreg_hi.as_lo() });

                self.push_instr(Instr::PushI64From(dreg_lo));

                self.op_stack.push_i64();
            }
            I64Load8U { memarg } => {
                self.load_i64_sized_u(memarg, &wasm_file.memory, Instr::LoadI32_8U);
            }
            I64Load16U { memarg } => {
                self.load_i64_sized_u(memarg, &wasm_file.memory, Instr::LoadI32_16U);
            }
            I64Load32U { memarg } => {
                self.load_i64_sized_u(memarg, &wasm_file.memory, Instr::LoadI32);
            }
            I64Load8S { memarg } => {
                self.load_i64_sized_s(memarg, &wasm_file.memory, Instr::LoadI32_8S);
            }
            I64Load16S { memarg } => {
                self.load_i64_sized_s(memarg, &wasm_file.memory, Instr::LoadI32_16S);
            }
            I64Load32S { memarg } => {
                self.load_i64_sized_s(memarg, &wasm_file.memory, Instr::LoadI32);
            }

            End => {
                let needs_else = self.flow_stack.0.last().unwrap().else_block.is_some();
                if needs_else {
                    self.visit_operator(Operator::Else, local_count, locals, wasm_file);
                }

                let entry = self.flow_stack.0.pop().unwrap();

                let _label = entry.label.unwrap();

                /*let (label, needs_block) = if let Some(label) = entry.label {
                    (label, false)
                } else {
                    todo!()
                    // FIXME: is it this one or the previous one?
                    let label = entry.label.unwrap_or_else(|| Label::new(self.func_idx, self.basic_blocks.len()));
                    (label, true)
                };*/

                if self.reachable() {
                    for ty in entry.outputs.iter().rev().copied() {
                        self.op_stack.pop_ty(ty);
                    }

                    assert_eq!(self.op_stack.0.len(), entry.stack_size);
                } else {
                    self.op_stack.0.truncate(entry.stack_size);
                }

                for ty in entry.outputs.iter().copied() {
                    self.op_stack.push_ty(ty);
                }

                let target = BranchTarget {
                    label: Label::new(self.func_idx, entry.next_block),
                    to_pop: 0,
                    ty: entry.target_tys,
                };

                self.push_instr(Instr::Branch(target));

                self.bb_index = entry.next_block;

                if self.depth == 0 {
                    self.setup_epilogue(local_count, &entry.outputs);
                } else {
                    self.depth -= 1;
                }

            }

            I32Ctz => {
                self.op_stack.pop_i32();
                self.push_instr(Instr::PopI32Into(Register::Work(0)));
                self.push_instr(Instr::I32Ctz(Register::Work(0)));
                self.push_instr(Instr::PushI32From(Register::Work(0)));
                self.op_stack.push_i32();
            }
            I32Clz => {
                self.op_stack.pop_i32();
                self.push_instr(Instr::PopI32Into(Register::Work(0)));
                self.push_instr(Instr::I32Clz(Register::Work(0)));
                self.push_instr(Instr::PushI32From(Register::Work(0)));
                self.op_stack.push_i32();
            }

            I64Clz => {
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(Register::Work(0)));
                self.push_instr(Instr::I64Clz { dst: Register::Work(1), src: Register::Work(0) });
                self.push_instr(Instr::PushI64From(Register::Work(1)));
                self.op_stack.push_i64();
            }
            I64Ctz => {
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(Register::Work(0)));
                self.push_instr(Instr::I64Ctz { dst: Register::Work(1), src: Register::Work(0) });
                self.push_instr(Instr::PushI64From(Register::Work(1)));
                self.op_stack.push_i64();
            }
            I64Popcnt => {
                let reg = Register::Work(0);

                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(reg));

                self.push_instr(Instr::I32Popcnt(reg.as_lo()));
                self.push_instr(Instr::I32Popcnt(reg.as_hi()));
                self.push_instr(Instr::I32Op { dst: reg.as_lo(), lhs: reg.as_lo(), op: "+=", rhs: reg.as_hi() });
                self.push_instr(Instr::SetConst(reg.as_hi(), 0));

                self.op_stack.push_i64();
                self.push_instr(Instr::PushI64From(reg));
            }

            I64Add => {
                self.make_i64_binop(|dst, lhs, rhs| {
                    vec![Instr::I64Add { dst, lhs, rhs }]
                });
            }
            I64Sub => {
                self.make_i64_binop(|dst, lhs, rhs| {
                    let mut instrs = Vec::new();
                    for i in Instr::i64_neg(rhs) {
                        instrs.push(i);
                    }
                    instrs.push(Instr::I64Add { dst, lhs, rhs });
                    instrs
                });
            }
            I64Mul => {
                self.make_i64_binop(|dst, lhs, rhs| {
                    Instr::i64_mul(dst, lhs, rhs)
                });
            }
            I64DivS => {
                let lhs = Register::Param(0);
                let rhs = Register::Param(1);
                let dst = Register::Return(0);
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(rhs));
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(lhs));

                self.push_instr(Instr::I64DivS { dst, lhs, rhs });

                self.op_stack.push_i64();
                self.push_instr(Instr::PushI64From(dst));
            }
            I64DivU => {
                let lhs = Register::Param(0);
                let rhs = Register::Param(1);
                let dst = Register::Return(0);
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(rhs));
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(lhs));

                self.push_instr(Instr::I64DivU { dst, lhs, rhs });

                self.op_stack.push_i64();
                self.push_instr(Instr::PushI64From(dst));
            }
            I64RemS => {
                let lhs = Register::Param(0);
                let rhs = Register::Param(1);
                let dst = Register::Return(0);
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(rhs));
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(lhs));

                self.push_instr(Instr::I64RemS { dst, lhs, rhs });

                self.op_stack.push_i64();
                self.push_instr(Instr::PushI64From(dst));
            }
            I64RemU => {
                let lhs = Register::Param(0);
                let rhs = Register::Param(1);
                let dst = Register::Return(0);
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(rhs));
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(lhs));

                self.push_instr(Instr::I64RemU { dst, lhs, rhs });

                self.op_stack.push_i64();
                self.push_instr(Instr::PushI64From(dst));
            }

            I64Shl => {
                self.make_i64_binop(|dst, lhs, rhs| {
                    vec![Instr::I64Shl { dst, lhs, rhs }]
                });
            }
            I64ShrU => {
                self.make_i64_binop(|dst, lhs, rhs| {
                    vec![
                        Instr::I64ShrU { dst, lhs, rhs },
                    ]
                })
            }
            I64ShrS => {
                self.make_i64_binop(|dst, lhs, rhs| {
                    vec![
                        Instr::I64ShrS { dst, lhs, rhs },
                    ]
                })
            }
            I64Rotl => {
                self.make_i64_binop(|dst, lhs, rhs| {
                    vec![
                        Instr::I64Rotl { dst, lhs, rhs },
                    ]
                })
            }
            I64Rotr => {
                self.make_i64_binop(|dst, lhs, rhs| {
                    vec![
                        Instr::I64Rotr { dst, lhs, rhs }
                    ]
                })
            }

            I64Or => {
                self.make_i64_binop(|dst, lhs, rhs| {
                    vec![
                        Instr::I32Op { dst: dst.as_lo(), lhs: lhs.as_lo(), op: "|=", rhs: rhs.as_lo() },
                        Instr::I32Op { dst: dst.as_hi(), lhs: lhs.as_hi(), op: "|=", rhs: rhs.as_hi() },
                    ]
                });
            }
            I64Xor => {
                self.make_i64_binop(|dst, lhs, rhs| {
                    vec![
                        Instr::I32Op { dst: dst.as_lo(), lhs: lhs.as_lo(), op: "^=", rhs: rhs.as_lo() },
                        Instr::I32Op { dst: dst.as_hi(), lhs: lhs.as_hi(), op: "^=", rhs: rhs.as_hi() },
                    ]
                });
            }
            I64And => {
                self.make_i64_binop(|dst, lhs, rhs| {
                    vec![
                        Instr::I32Op { dst: dst.as_lo(), lhs: lhs.as_lo(), op: "&=", rhs: rhs.as_lo() },
                        Instr::I32Op { dst: dst.as_hi(), lhs: lhs.as_hi(), op: "&=", rhs: rhs.as_hi() },
                    ]
                });
            }
            I64Eq => {
                let lhs = Register::Work(0);
                let rhs = Register::Work(1);
                let dst = Register::Work(2);
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(rhs));
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(lhs));
                self.push_instr(Instr::I64Eq { dst, lhs, invert: false, rhs });
                self.op_stack.push_i32();
                self.push_instr(Instr::PushI32From(dst));
            }
            I64Ne => {
                let lhs = Register::Work(0);
                let rhs = Register::Work(1);
                let dst = Register::Work(2);
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(rhs));
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(lhs));
                self.push_instr(Instr::I64Eq { dst, lhs, invert: true, rhs });
                self.op_stack.push_i32();
                self.push_instr(Instr::PushI32From(dst));
            }

            I64GtS => {
                let lhs = Register::Work(0);
                let rhs = Register::Work(1);
                let dst = Register::Work(2);
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(rhs));
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(lhs));
                self.push_instr(Instr::I64SComp { dst, lhs, op: Relation::GreaterThan, rhs });
                self.op_stack.push_i32();
                self.push_instr(Instr::PushI32From(dst));
            }
            I64GeS => {
                let lhs = Register::Work(0);
                let rhs = Register::Work(1);
                let dst = Register::Work(2);
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(rhs));
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(lhs));
                self.push_instr(Instr::I64SComp { dst, lhs, op: Relation::GreaterThanEq, rhs });
                self.op_stack.push_i32();
                self.push_instr(Instr::PushI32From(dst));
            }

            I64LtS => {
                let lhs = Register::Work(0);
                let rhs = Register::Work(1);
                let dst = Register::Work(2);
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(rhs));
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(lhs));
                self.push_instr(Instr::I64SComp { dst, lhs, op: Relation::LessThan, rhs });
                self.op_stack.push_i32();
                self.push_instr(Instr::PushI32From(dst));
            }
            I64LeS => {
                let lhs = Register::Work(0);
                let rhs = Register::Work(1);
                let dst = Register::Work(2);
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(rhs));
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(lhs));
                self.push_instr(Instr::I64SComp { dst, lhs, op: Relation::LessThanEq, rhs });
                self.op_stack.push_i32();
                self.push_instr(Instr::PushI32From(dst));
            }



            I64LtU => {
                let lhs = Register::Work(0);
                let rhs = Register::Work(1);
                let dst = Register::Work(2);
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(rhs));
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(lhs));
                self.push_instr(Instr::I64UComp { dst, lhs, op: Relation::LessThan, rhs });
                self.op_stack.push_i32();
                self.push_instr(Instr::PushI32From(dst));
            }
            I64LeU => {
                let lhs = Register::Work(0);
                let rhs = Register::Work(1);
                let dst = Register::Work(2);
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(rhs));
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(lhs));
                self.push_instr(Instr::I64UComp { dst, lhs, op: Relation::LessThanEq, rhs });
                self.op_stack.push_i32();
                self.push_instr(Instr::PushI32From(dst));

            }
            I64GtU => {
                let lhs = Register::Work(0);
                let rhs = Register::Work(1);
                let dst = Register::Work(2);
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(rhs));
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(lhs));
                self.push_instr(Instr::I64UComp { dst, lhs, op: Relation::GreaterThan, rhs });
                self.op_stack.push_i32();
                self.push_instr(Instr::PushI32From(dst));
            }
            I64GeU => {
                let lhs = Register::Work(0);
                let rhs = Register::Work(1);
                let dst = Register::Work(2);
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(rhs));
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(lhs));
                self.push_instr(Instr::I64UComp { dst, lhs, op: Relation::GreaterThanEq, rhs });
                self.op_stack.push_i32();
                self.push_instr(Instr::PushI32From(dst));
            }

            I32Add => {
                self.make_i32_binop("+=");
            }
            I32Sub => {
                self.make_i32_binop("-=");
            }
            I32Mul => {
                self.make_i32_binop("*=");
            }
            I32DivS => {
                self.make_i32_binop("/=");
            }
            I32DivU => {
                self.make_i32_binop("/=u");
            }
            I32RemS => {
                self.make_i32_binop("rems");
            }
            I32RemU => {
                self.make_i32_binop("remu");
            }
            I32And => {
                self.make_i32_binop("&=");
            }
            I32Or  => {
                self.make_i32_binop("|=");
            }
            I32Xor => {
                self.make_i32_binop("^=");
            }
            I32Shl => {
                self.make_i32_binop("shl");
            }
            I32Eq => {
                self.make_i32_binop("==");
            }
            I32Ne => {
                self.make_i32_binop("!=");
            }
            I32ShrU => {
                self.make_i32_binop("shru");
            }
            I32ShrS => {
                self.make_i32_binop("shrs");
            }
            I32Rotl => {
                self.make_i32_binop("rotl");
            }
            I32Rotr => {
                self.make_i32_binop("rotr");
            }

            I32Popcnt => {
                let reg = Register::Work(0);
                self.push_instr(Instr::PopI32Into(reg));
                self.op_stack.pop_i32();
                self.push_instr(Instr::I32Popcnt(reg.as_lo()));
                self.push_instr(Instr::PushI32From(reg));
                self.op_stack.push_i32();
            }
            I32Extend8S => {
                let reg = Register::Work(0);
                self.push_instr(Instr::PopI32Into(reg));
                self.op_stack.pop_i32();
                self.push_instr(Instr::I32Extend8S(reg));
                self.push_instr(Instr::PushI32From(reg));
                self.op_stack.push_i32();
            }
            I32Extend16S => {
                let reg = Register::Work(0);
                self.push_instr(Instr::PopI32Into(reg));
                self.op_stack.pop_i32();
                self.push_instr(Instr::I32Extend16S(reg));
                self.push_instr(Instr::PushI32From(reg));
                self.op_stack.push_i32();
            }



            LocalTee { local_index } => {
                let reg = Register::Work(0);
                let ty = get_local_ty(locals, local_index);

                self.push_instr(Instr::pop_into(reg, ty));
                self.op_stack.pop_ty(ty);
                self.push_instr(Instr::push_from(reg, ty));
                self.op_stack.push_ty(ty);
                self.push_instr(Instr::push_from(reg, ty));
                self.op_stack.push_ty(ty);

                self.visit_operator(Operator::LocalSet { local_index }, local_count, locals, wasm_file)
            }
            Drop => {
                self.push_instr(Instr::Drop);
                self.op_stack.pop_value();
            }
            Select => {
                let cond_reg = Register::Work(2);
                self.push_instr(Instr::PopI32Into(cond_reg));
                self.op_stack.pop_i32();


                let false_ty = self.op_stack.pop_value();
                let false_reg = Register::Work(1);
                self.push_instr(Instr::pop_into(false_reg, false_ty));

                let true_ty = self.op_stack.pop_value();
                let true_reg = Register::Work(0);
                self.push_instr(Instr::pop_into(true_reg, true_ty));

                if false_ty != true_ty {
                    todo!("{:?} {:?}", false_ty, true_ty)
                }

                match true_ty {
                    Type::I32 => {
                        self.push_instr(Instr::SelectI32 { dst_reg: true_reg, true_reg, false_reg, cond_reg });
                    }
                    Type::I64 => {
                        self.push_instr(Instr::SelectI64 { dst_reg: true_reg, true_reg, false_reg, cond_reg });
                    }
                    _ => todo!("{:?}", true_ty)
                }

                self.push_instr(Instr::push_from(true_reg, true_ty));
                // TODO: can they be different types?
                self.op_stack.push_ty(true_ty);
            }
            Block { ty } => {
                let inputs = wasm_file.types.get_input_tys(ty);
                let outputs = wasm_file.types.get_output_tys(ty);

                println!("Before starting block: {:?}", self.op_stack);

                let target_tys = outputs.clone();

                for ty in inputs.iter().rev().copied() {
                    self.op_stack.pop_ty(ty);
                }

                let mut next_block_stack = self.op_stack.clone();
                for ty in outputs.iter().copied() {
                    next_block_stack.push_ty(ty);
                }

                let next_block = self.allocate_block(next_block_stack);

                let entry = ControlFlowEntry {
                    label: Some(Label::new(self.func_idx, next_block)),
                    stack_size: self.op_stack.0.len(),
                    target_tys,
                    outputs,
                    else_block: None,
                    next_block,
                };
                self.flow_stack.0.push(entry);

                for ty in inputs.iter().copied() {
                    self.op_stack.push_ty(ty);
                }

                self.depth += 1;
            }
            If { ty } => {
                let inputs = wasm_file.types.get_input_tys(ty);
                let outputs = wasm_file.types.get_output_tys(ty);

                let target_tys = outputs.clone();

                // Condition
                let reg = Register::Work(0);
                self.push_instr(Instr::PopI32Into(reg));
                self.op_stack.pop_i32();

                for ty in inputs.iter().rev().copied() {
                    self.op_stack.pop_ty(ty);
                }

                let body_stack = self.op_stack.clone();

                let mut end_stack = body_stack.clone();
                for ty in outputs.iter().copied() {
                    end_stack.push_ty(ty);
                }

                let then_block = self.allocate_block(body_stack.clone());
                let else_block = self.allocate_block(body_stack);
                let next_block = self.allocate_block(end_stack);

                let then_target = BranchTarget {
                    label: Label::new(self.func_idx, then_block),
                    to_pop: 0,
                    ty: inputs.clone(),
                };

                let else_target = BranchTarget {
                    label: Label::new(self.func_idx, else_block),
                    to_pop: 0,
                    ty: inputs.clone(),
                };

                self.push_instr(Instr::BranchIf { t_name: then_target, f_name: else_target, cond: Register::Work(0) });

                let entry = ControlFlowEntry {
                    label: Some(Label::new(self.func_idx, next_block)),
                    stack_size: self.op_stack.0.len(),
                    target_tys,
                    outputs,
                    else_block: Some((else_block, inputs.clone())),
                    next_block,
                };
                self.flow_stack.0.push(entry);

                for ty in inputs.iter().copied() {
                    self.op_stack.push_ty(ty);
                }

                self.depth += 1;

                self.bb_index = then_block;
            }
            Else => {
                let entry = self.flow_stack.0.last_mut().unwrap();

                //let label = entry.label.as_ref().unwrap();
                let (else_block, else_ty) = entry.else_block.take().unwrap();

                // TODO: ???
                if self.reachable[self.bb_index] {
                    for ty in entry.outputs.iter().rev().copied() {
                        self.op_stack.pop_ty(ty);
                    }

                    assert_eq!(self.op_stack.0.len(), entry.stack_size);
                } else {
                    self.op_stack.0.truncate(entry.stack_size);
                }
                

                // Exit from the if-part of the statement
                let target = BranchTarget {
                    label: Label::new(self.func_idx, entry.next_block),
                    to_pop: 0,
                    ty: entry.target_tys.clone(),
                };

                self.push_instr(Instr::Branch(target));

                self.bb_index = else_block;

                // Enter the else-block with the given param types
                for ty in else_ty.iter() {
                    self.op_stack.push_ty(*ty);
                }
            }

            I32Eqz => {
                let val = Register::Work(0);

                self.push_instr(Instr::PopI32Into(val));
                self.op_stack.pop_i32();

                let cond = Register::Work(1);

                self.push_instr(Instr::I32Eqz { val, cond });

                self.push_instr(Instr::PushI32From(cond));
                self.op_stack.push_i32();
            }
            I64Eqz => {
                let val = Register::Work(0);

                self.push_instr(Instr::PopI64Into(val));
                self.op_stack.pop_i64();

                let cond = Register::Work(1);

                self.push_instr(Instr::I64Eqz { val, cond });

                self.push_instr(Instr::PushI32From(cond));
                self.op_stack.push_i32();
            }

            I32GtS => {
                self.make_i32_binop("gts");
            }
            I32GtU => {
                self.make_i32_binop("gtu");
            }
            I32GeS => {
                self.make_i32_binop("ges");
            }
            I32GeU => {
                self.make_i32_binop("geu");
            }
            I32LtS => {
                self.make_i32_binop("lts");
            }
            I32LtU => {
                self.make_i32_binop("ltu");
            }
            I32LeS => {
                self.make_i32_binop("les");
            }
            I32LeU => {
                self.make_i32_binop("leu");
            }

            I32WrapI64 => {
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(Register::Work(0)));
                self.push_instr(Instr::PushI32From(Register::Work(0)));
                self.op_stack.push_i32();
            }

            I64ExtendI32S => {
                let reg = Register::Work(0);

                self.op_stack.pop_i32();
                self.push_instr(Instr::pop_into(reg, Type::I32));
                self.push_instr(Instr::I64ExtendI32S { dst: reg, src: reg });
                self.push_instr(Instr::push_from(reg, Type::I64));
                self.op_stack.push_i64();

            }
            I64Extend32S => {
                let reg = Register::Work(0);

                self.op_stack.pop_i64();
                self.push_instr(Instr::pop_into(reg, Type::I64));
                self.push_instr(Instr::I64ExtendI32S { dst: reg, src: reg });
                self.push_instr(Instr::push_from(reg, Type::I64));
                self.op_stack.push_i64();
            }
            I64Extend16S => {
                let reg = Register::Work(0);

                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(reg));
                self.push_instr(Instr::I32Extend16S(reg));
                self.push_instr(Instr::I64ExtendI32S { dst: reg, src: reg });
                self.push_instr(Instr::PushI64From(reg));
                self.op_stack.push_i64();
            }
            I64Extend8S => {
                let reg = Register::Work(0);

                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(reg));
                self.push_instr(Instr::I32Extend8S(reg));
                self.push_instr(Instr::I64ExtendI32S { dst: reg, src: reg });
                self.push_instr(Instr::PushI64From(reg));
                self.op_stack.push_i64();
            }
            I64ExtendI32U => {
                let reg = Register::Work(0);
                
                self.op_stack.pop_i32();
                self.push_instr(Instr::pop_into(reg, Type::I32));
                self.push_instr(Instr::I64ExtendI32U(reg));
                self.push_instr(Instr::push_from(reg, Type::I64));
                self.op_stack.push_i64();
            }

            Loop { ty } => {
                let target_tys = wasm_file.types.get_input_tys(ty);
                let outputs = wasm_file.types.get_output_tys(ty);

                for ty in target_tys.iter().rev().copied() {
                    self.op_stack.pop_ty(ty);
                }

                let next_block = self.allocate_block(self.op_stack.clone());

                self.flow_stack.0.push(ControlFlowEntry {
                    label: Some(Label::new(self.func_idx, self.basic_blocks.len())),
                    stack_size: self.op_stack.0.len(),
                    target_tys: target_tys.clone(),
                    else_block: None,
                    next_block,
                    outputs,
                });

                for ty in target_tys.iter().copied() {
                    self.op_stack.push_ty(ty);
                }

                let target = BranchTarget {
                    label: Label::new(self.func_idx, self.basic_blocks.len()),
                    to_pop: 0,
                    ty: Box::new([]),
                };

                // Branch to next block
                self.push_instr(Instr::Branch(target));

                self.next_basic_block();

                self.depth += 1;
            }
            Br { relative_depth } => {
                let target = self.get_target(relative_depth);

                self.push_instr(Instr::Branch(target));

                self.next_basic_block();
                self.mark_unreachable();
            }
            BrIf { relative_depth } => {
                let reg = Register::Work(0);

                self.push_instr(Instr::PopI32Into(reg));
                self.op_stack.pop_i32();

                let t_name = self.get_target(relative_depth);
                let next_block = self.allocate_block(self.op_stack.clone());
                let f_name = BranchTarget {
                    label: self.basic_blocks[next_block].label.clone(),
                    to_pop: 0,
                    ty: Box::new([]),
                };

                self.push_instr(Instr::BranchIf { t_name, f_name, cond: reg });

                self.bb_index = next_block;
            }
            BrTable { table } => {
                let reg = Register::Work(0);

                self.op_stack.pop_i32();
                self.push_instr(Instr::PopI32Into(reg));

                let mut default = None;
                let mut targets = Vec::new();

                for (rel_depth, is_default) in table.targets().map(|t| t.unwrap()) {
                    let target = self.get_target(rel_depth);
                    if is_default {
                        assert!(default.is_none());
                        default = Some(target);
                    } else {
                        targets.push(target);
                    }
                }

                self.push_instr(Instr::BranchTable { reg, targets, default });

                self.next_basic_block();
                self.mark_unreachable();
            }
            Nop => {}
            Unreachable => {
                println!("TODO: Unreachable");
                self.mark_unreachable();
            }

            // TODO: Actually implement a non-failing version of this
            MemoryGrow { .. } => {
                self.op_stack.pop_i32();
                self.push_instr(Instr::Drop);
                self.push_instr(Instr::PushI32Const(0xFFFF_FFFF_u32 as i32));
                self.op_stack.push_i32();
            }

            _ => todo!("{:?}", o),
        }
    }
}

#[derive(Debug, Clone)]
struct FlowStack(Vec<ControlFlowEntry>);

impl FlowStack {
    pub fn new(ty: TypeOrFuncType, types: &TypeList, exit_label: Label) -> Self {
        let outputs = types.get_output_tys(ty);
        let target_tys = outputs.clone();

        let stack_size = if BRANCH_CONV == BranchConv::Direct { 0 } else { 1 };

        FlowStack(vec![ControlFlowEntry {
            label: Some(exit_label),
            stack_size,
            target_tys,
            else_block: None,
            outputs,
            next_block: 1,
        }])
    }
}

#[derive(Debug, Clone)]
struct ControlFlowEntry {
    /// The location that should be jumped to when this branch is targeted
    label: Option<Label>,
    /// The size of the stack 'underneath' this block
    stack_size: usize,
    /// The types that should be popped/pushed when this branch is targeted
    target_tys: Box<[Type]>,

    else_block: Option<(usize, Box<[Type]>)>,

    /// The index of the block that immediately follows the End of this block
    next_block: usize,
    /// The types that should be popped/pushed at the End of this block
    outputs: Box<[Type]>,
}

pub fn compile(file: &WasmFile) -> Vec<BasicBlock<Instr>> {
    let mut basic_blocks = Vec::new();

    let mut func_idx = CodeFuncIdx(file.imports.num_funcs() as u32);
    for e in file.bodies.iter() {
        println!("Doing function {}", func_idx.0);

        let func_type = file.functions.get_function_type(func_idx, &file.types);

        let mut locals = Vec::new();
        for param in func_type.params.iter() {
            locals.push((1, *param))
        }

        locals.extend(e.locals.iter().copied());

        let local_count = locals.iter().map(|(c, _)| *c).sum();

        let func_ty = TypeOrFuncType::FuncType(file.functions.functions[func_idx.0 as usize]);

        let mut s = FuncBodyStream::new(func_ty, &file.types, func_idx);

        s.setup_arguments(local_count, &file.types, &file.functions);
        for o in e.operators.iter().cloned() {
            s.visit_operator(o, local_count, &locals, file);
        }
        assert!(s.op_stack.0.is_empty(), "{:?}", s.op_stack);

        let reachable = get_reachable_blocks(&s.basic_blocks);

        for (idx, bb) in s.basic_blocks.into_iter().enumerate() {
            if reachable[idx] {
                basic_blocks.push(bb);
            }
        }

        func_idx.0 += 1;
    }

    basic_blocks
}

