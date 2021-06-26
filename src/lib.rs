mod datapack;
mod state;

use state::State;

use wasmparser::{Export, ExternalKind, FuncType, Global, Import, ImportSectionEntryType, MemoryImmediate, MemoryType, Operator, Parser, Payload, TableType, Type, TypeDef, TypeOrFuncType};

/// Represents wasm's "Function Index Space"
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CodeFuncIdx(u32);

#[derive(Debug, Clone, PartialEq, Eq)]
struct Label {
	func_idx: CodeFuncIdx,
	idx: usize,
}

impl Label {
	pub fn new(func_idx: CodeFuncIdx, idx: usize) -> Self {
		Label { func_idx, idx }
	}
}


#[derive(Debug, Clone)]
/// Every branch destination must be a new Basic Block
pub struct BasicBlock<T> {
    label: Label,
    instrs: Vec<T>,
}

impl<T> BasicBlock<T> {
    fn new(func_idx: CodeFuncIdx, idx: usize) -> Self {
        BasicBlock {
            label: Label { func_idx, idx },
            instrs: Vec::new()
        }
    }
}

impl BasicBlock<Instr> {
    fn lower(&self, globals: &GlobalList, functions: &FunctionList) -> BasicBlock<String> {
        let mut body = Vec::new();

        for i in self.instrs.iter() {
            i.lower(&mut body, &globals, &functions);
        }

        BasicBlock {
            label: self.label.clone(),
            instrs: body,
        }
    }
}

const USE_GRID_CALL: bool = true;

use std::collections::HashMap;

fn find_end(operators: &[Operator], relative_depth: u32) -> usize {
    for (idx, op) in operators.iter().enumerate() {
        if let Operator::End = op {
            return if relative_depth == 0 {
                idx
            } else {
                idx + 1 + find_end(&operators[idx + 1..], relative_depth - 1)
            }
        }
    }

    if relative_depth == 0 {
        operators.len()
    } else {
        panic!("{:?}", relative_depth);
    }
}

struct SplitBBInfo {
    bb_count: usize,
    branches: HashMap<usize, (Label, Option<Label>)>
}

fn split_bbs(operators: &[Operator], func_idx: CodeFuncIdx) -> SplitBBInfo {
    // First we assign each operator its basic block ID
    
    let mut op_bbs = vec![0; operators.len()];
    let mut bbs_id = 0;

    for (bbs, op) in op_bbs.iter_mut().zip(operators.iter()) {
        match op {
            // TODO: More
            Operator::Br { .. } | Operator::BrIf { .. } |
            Operator::Call { .. } | Operator::CallIndirect { .. } |
            Operator::Return => {
                *bbs = bbs_id;
                bbs_id += 1;
            }
            Operator::Loop { .. } | Operator::End => {
                bbs_id += 1;
                *bbs = bbs_id;
            }
            _ => {}
        }
    }

    let mut branches = HashMap::new();

    for (idx, op) in operators.iter().enumerate() {
        match op {
            Operator::Br { relative_depth } => {
                let target = idx + 1 + find_end(&operators[idx + 1..], *relative_depth);
                let target = op_bbs[target];
                branches.insert(idx, (Label::new(func_idx, target), None));
            }
            Operator::BrIf { relative_depth } => {
                let t_target = idx + 1 + find_end(&operators[idx + 1..], *relative_depth);
                let t_target = op_bbs[t_target];
                let f_target = op_bbs[idx + 1];

                branches.insert(idx, (Label::new(func_idx, t_target), Some(Label::new(func_idx, f_target))));
            }
            _ => {}
        }
    }

    SplitBBInfo {
        bb_count: bbs_id + 1,
        branches,
    }
}

#[derive(Debug, Clone)]
enum Instr {
    Comment(String),

    PushValueFrom(Register),
    PushI32From(Register),
    PushI32Const(i32),
    PushI64Const(i64),
    PushReturnAddress(Label),

    Branch(Label),
    // Index into table, index of table
    // (None represents the wasmcraft-specific table)
    DynBranch(Register, Option<u32>),
    BranchIf { t_name: Label, f_name: Label, cond: Register },

    I32Op { dst: Register, lhs: Register, op: &'static str, rhs: Register },

    PopValueInto(Register),
    PopI32Into(Register),

    LoadGlobal(Register),
    StoreGlobal(Register),

    LoadLocal(Register),
    StoreLocal(Register),

    /// reg, align
    /// reg = *memptr
    LoadI32(Register, u8),
    /// reg, align
    /// *memptr = reg
    StoreI32(Register, u8),

    AddI32Const(Register, i32),
    I64ExtendI32S { dst: Register, src: Register },

    I32Eqz { val: Register, cond: Register },

    SetMemPtr(Register),
    SetGlobalPtr(u32),
    SetLocalPtr(u32),

    ResetFrames,
    PushFrame(u32),
    PopFrame(u32),

    Drop,

    Unreachable,
}

impl Instr {
    pub fn incr_stack_ptr_half(&self, body: &mut Vec<String>) {
        body.push("execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~".to_string());
        body.push("scoreboard players add %stackptr wasm 1".to_string());
    }

    pub fn incr_stack_ptr(&self, body: &mut Vec<String>) {
        body.push("execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~2 ~ ~".to_string());
        body.push("scoreboard players add %stackptr wasm 2".to_string());
    }

    pub fn decr_stack_ptr_half(&self, body: &mut Vec<String>) {
        body.push("execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~".to_string());
        body.push("scoreboard players remove %stackptr wasm 1".to_string());
    }

    pub fn decr_stack_ptr(&self, body: &mut Vec<String>) {
        body.push("execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-2 ~ ~".to_string());
        body.push("scoreboard players remove %stackptr wasm 2".to_string());
    }

    pub fn make_i32_op(&self, lhs: Register, op: &str, rhs: Register, body: &mut Vec<String>) -> Register {
        match op {
            "+=" | "-=" | "*=" | "/=" | "%=" => {
                body.push(format!("scoreboard players operation {} reg {} {} reg", lhs.get_lo(), op, rhs.get_lo()));
                lhs
            }
            "&=" => {
                body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs.get_lo()));
                body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs.get_lo()));
                body.push("function intrinsic:and".to_string());
                body.push(format!("scoreboard players operation {} reg = %return%0 reg", lhs.get_lo()));
                lhs
            }
            "|=" => {
                body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs.get_lo()));
                body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs.get_lo()));
                body.push("function intrinsic:or".to_string());
                body.push(format!("scoreboard players operation {} reg = %return%0 reg", lhs.get_lo()));
                lhs
            }
            "^=" => {
                body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs.get_lo()));
                body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs.get_lo()));
                body.push("function intrinsic:xor".to_string());
                body.push(format!("scoreboard players operation {} reg = %return%0 reg", lhs.get_lo()));
                lhs
            }
            "shl" => {
                body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs.get_lo()));
                body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs.get_lo()));
                body.push("function intrinsic:shl".to_string());
                body.push(format!("scoreboard players operation {} reg = %param0%0 reg", lhs.get_lo()));
                lhs
            }
            "gts" | "ges" | "les" | "lts" => {
                let score_op = match op {
                    "gts" => ">",
                    "ges" => ">=",
                    "les" => "<=",
                    "lts" => "<",
                    _ => unreachable!(),
                };

                let condreg = Register::Work(2);

                body.push(format!("scoreboard players set {} reg 0", condreg.get_lo()));
                body.push(format!("execute if score {} reg {} {} reg run scoreboard players set {} reg 1", lhs.get_lo(), score_op, rhs.get_lo(), condreg.get_lo()));
                condreg
            }
            "geu" | "gtu" => {
                let score_op = match op {
                    "gtu" => ">",
                    "geu" => ">=",
                    _ => unreachable!(),
                };

                let condreg = Register::Work(2);

                let l = lhs.get_lo();
                let r = rhs.get_lo();
                let c = condreg.get_lo();

                body.push(format!("scoreboard players set {} reg 0", c));
                body.push(format!("execute if score {} reg matches ..-1 run scoreboard players set {} 1", l, c));
                body.push(format!("execute if score {} reg matches ..-1 run scoreboard players set {} 0", r, c));
                body.push(format!("execute if score {} reg matches ..-1 run execute if score {} reg matches ..-1 run execute if score {} reg {} {} reg run scoreboard players set {} 1", l, r, l, score_op, r, c));
                body.push(format!("execute if score {} reg matches 0.. run execute if score {} reg matches 0.. run execute if score {} reg {} {} reg run scoreboard players set {} 1", l, r, l, score_op, r, c));
               
                condreg
            }
            "leu" | "ltu" => {
                let score_op = match op {
                    "ltu" => "<",
                    "leu" => "<=",
                    _ => unreachable!(),
                };

                let condreg = Register::Work(2);

                let l = lhs.get_lo();
                let r = rhs.get_lo();
                let c = condreg.get_lo();

                body.push(format!("scoreboard players set {} reg 0", c));
                body.push(format!("execute if score {} reg matches ..-1 run scoreboard players set {} 0", l, c));
                body.push(format!("execute if score {} reg matches ..-1 run scoreboard players set {} 1", r, c));
                body.push(format!("execute if score {} reg matches ..-1 run execute if score {} reg matches ..-1 run execute if score {} reg {} {} reg run scoreboard players set {} 1", l, r, l, score_op, r, c));
                body.push(format!("execute if score {} reg matches 0.. run execute if score {} reg matches 0.. run execute if score {} reg {} {} reg run scoreboard players set {} 1", l, r, l, score_op, r, c));
               
                condreg
            }
            _ => {
                println!("TODO: make_i32_op {}", op);

                Register::Work(0)
            }
        }
    }

    pub fn pop_half_into(&self, reg: Register, body: &mut Vec<String>) {
        self.decr_stack_ptr_half(body);
        body.push(format!("execute at @e[tag=stackptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg.get_lo()));
    }

    pub fn push_i64_const(&self, value: i64, body: &mut Vec<String>) {
        body.push(format!("execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value {}", value as i32));
        self.incr_stack_ptr_half(body);
       
        body.push(format!("execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value {}", (value >> 32) as i32));
        self.incr_stack_ptr_half(body);
    }

    pub fn push_half_from(&self, reg: Register, body: &mut Vec<String>) {
        body.push(format!("execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_lo()));
        self.incr_stack_ptr_half(body);
    }

    pub fn lower(&self, body: &mut Vec<String>, global_list: &GlobalList, function_list: &FunctionList) {
        body.push(format!("# {:?}", self));

        use Instr::*;
        match self {
            Comment(s) => {
                body.push(format!("# {}", s));
            }

            SetMemPtr(reg) => {
                body.push(format!("scoreboard players operation %ptr reg = {} reg", reg.get_lo()));
                body.push("function intrinsic:setptr".to_string());
            }
            &SetGlobalPtr(global_index) => {
                let pos = global_list.get_offset(global_index);
                body.push(format!("execute as @e[tag=globalptr] run tp @s {}", pos));
            }

            &PopI32Into(reg) => {           
                self.decr_stack_ptr(body);
                body.push(format!("execute at @e[tag=stackptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg.get_lo()));
            }
            &PopValueInto(reg) => {
                self.decr_stack_ptr_half(body);
                body.push(format!("execute at @e[tag=stackptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg.get_hi()));

                self.decr_stack_ptr_half(body);
                body.push(format!("execute at @e[tag=stackptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg.get_lo()));
            }

            LoadLocal(reg) => {
                body.push(format!("execute at @e[tag=localptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg.get_lo()));
                body.push(format!("execute at @e[tag=localptr] store result score {} reg run data get block ~ ~ ~1 RecordItem.tag.Memory 1", reg.get_hi()));
            }
            StoreLocal(reg) => {
                body.push(format!("execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_lo()));
                body.push(format!("execute at @e[tag=localptr] store result block ~ ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_hi()));
            }

            &SetLocalPtr(local_index) => {
                body.push(format!("execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~{} 0 1", -(local_index as i32) - 1 ));
            }

            &AddI32Const(reg, value) => {
                use std::cmp::Ordering::*;
                match value.cmp(&0) {
                    Greater => body.push(format!("scoreboard players add {} reg {}", reg.get_lo(), value)),
                    Less => body.push(format!("scoreboard players remove {} reg {}", reg.get_lo(), -value)),
                    Equal => {},
                }
            }
            &I64ExtendI32S { dst, src } => {
                if dst != src {
                    body.push(format!("scoreboard players operation {} reg = {} reg", dst.get_lo(), src.get_lo()));
                }
                body.push(format!("execute if score {} reg matches ..-1 run scoreboard players set {} -1", src.get_lo(), dst.get_hi()));
                body.push(format!("execute if score {} reg matches 0.. run scoreboard players set {} 0", src.get_lo(), dst.get_hi()));
            }

            I32Eqz { val, cond } => {
                body.push(format!("scoreboard players set {} reg 0", cond.get_lo()));
                body.push(format!("execute if score {} reg matches 0..0 run scoreboard players set {} reg 1", val.get_lo(), cond.get_lo()));
            }
            &I32Op { dst, lhs, op, rhs } => {
                let d = self.make_i32_op(lhs, op, rhs, body);
                assert_eq!(d, dst);
            }

            PushI32From(reg) => {
                body.push(format!("execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_lo()));
                self.incr_stack_ptr(body);
            }
            &PushValueFrom(reg) => {
                body.push(format!("execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_lo()));
                self.incr_stack_ptr_half(body);

                body.push(format!("execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_hi()));
                self.incr_stack_ptr_half(body);
            }
            PushI32Const(value) => {
                body.push(format!("execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value {}", value));
                self.incr_stack_ptr(body);
            }
            &PushI64Const(value) => {
                self.push_i64_const(value, body);
            }
            PushReturnAddress(label) => {
                let return_name = function_list.get_block(&label);

                body.push(format!("execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value !BLOCKIDX!{}!", return_name));
                self.incr_stack_ptr(body);
            }

            Branch(entry) => {
                let entry = function_list.get_block(entry);

                body.push(format!("#   Jump to {}", entry));
                body.push(format!("setblock !BLOCKPOS!{}! minecraft:redstone_block destroy", entry));
            }
            &DynBranch(reg, table_idx) => {
                assert_eq!(reg, Register::Work(0));

                if let Some(i) = table_idx {
                    body.push(format!("function wasm:dyn_branch{}", i));
                } else {
                    body.push("function wasm:dyn_branch".to_string());
                }
            }
            BranchIf { t_name, f_name, cond } => {
                let t_name = function_list.get_block(t_name);
                let f_name = function_list.get_block(f_name);
                
                body.push(format!("execute unless score {} reg matches 0..0 run setblock !BLOCKPOS!{}! minecraft:redstone_block destroy", cond.get_lo(), t_name));
                body.push(format!("execute if score {} reg matches 0..0 run setblock !BLOCKPOS!{}! minecraft:redstone_block destroy", cond.get_lo(), f_name));
            }

            LoadGlobal(reg) => {
                body.push(format!("execute at @e[tag=globalptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg.get_lo()));
                body.push(format!("execute at @e[tag=globalptr] store result score {} reg run data get block ~ ~ ~1 RecordItem.tag.Memory 1", reg.get_hi()));
            }
            StoreGlobal(reg) => {
                body.push(format!("execute at @e[tag=globalptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_lo()));
                body.push(format!("execute at @e[tag=globalptr] store result block ~ ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_hi()));
            }

            &LoadI32(dst, align) => {
                if align % 4 == 0 {
                    body.push(format!("execute at @e[tag=memoryptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", dst.get_lo()));
                } else {
                    body.push("function intrinsic:load_word_unaligned".to_string());
                    body.push(format!("scoreboard players operation {} reg = %return%0 reg", dst.get_lo()));
                }
            }
            &StoreI32(src, _align) => {
                /*
                if align % 4 == 0 {
                    body.push(format!("execute at @e[tag=memoryptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", src.get_lo()));
                } else {
                    body.push(format!("scoreboard players operation %param0%0 reg = {} reg", src.get_lo()));
                    body.push("function intrinsic:store_word_unaligned".to_string());
                }
                */
                body.push(format!("scoreboard players operation %param0%0 reg = {} reg", src.get_lo()));
                body.push("function intrinsic:store_word".to_string());
            }

            ResetFrames => {
                body.push("execute as @e[tag=frameptr] run tp @s 0 0 1".to_string());
                body.push("scoreboard players set %frameptr wasm 0".to_string());
            }
            &PushFrame(local_count) => {
                if local_count != 0 {
                    body.push(format!("# Push frame with {} locals", local_count));
                    body.push(format!("execute at @e[tag=frameptr] run fill ~ ~ ~ ~{} ~ ~1 minecraft:jukebox{{RecordItem:{{id:\"minecraft:stone\",Count:1b,tag:{{Memory:1}}}}}}", local_count - 1));
                    body.push(format!("execute as @e[tag=frameptr] at @e[tag=frameptr] run tp @s ~{} ~ ~", local_count));
                }
            }
            &PopFrame(local_count) => {
                if local_count != 0 {
                    body.push(format!("execute as @e[tag=frameptr] at @e[tag=frameptr] run tp @s ~-{} ~ ~", local_count));
                    body.push(format!("execute at @e[tag=frameptr] run fill ~ ~ ~ ~{} ~ ~1 minecraft:air", local_count - 1));
                }
            }

            Drop => {
                self.decr_stack_ptr(body);
            }

            Unreachable => {
                body.push("tellraw @a \"ENTERED UNREACHABLE CODE\"".to_string());
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Register {
    Work(u32),
    Param(u32),
    Return,
}

impl Register {
    pub fn get_lo(&self) -> String {
        match self {
            Register::Work(i) => {
                format!("%work%{}%lo", i)
            }
            Register::Param(i) => {
                format!("%param%{}%lo", i)
            }
            Register::Return => {
                "%return%lo".to_string()
            }
        }
    }

    pub fn get_hi(&self) -> String {
        match self {
            Register::Work(i) => {
                format!("%work%{}%hi", i)
            }
            Register::Param(i) => {
                format!("%param%{}%hi", i)
            }
            Register::Return => {
                "%return%hi".to_string()
            }
        }
    }
}

struct FuncBodyStream {
    basic_blocks: Vec<BasicBlock<Instr>>,
    depth: usize,
}

impl FuncBodyStream {
    pub fn new() -> Self {
        FuncBodyStream {
            basic_blocks: Vec::new(),
            depth: 0,
        }
    }

    pub fn get_i32_dst(&mut self, lhs: Register, op: &str, rhs: Register) -> Register {
        match op {
            "+=" | "-=" | "*=" | "/=" | "%=" |
            "&=" | "|=" | "^=" | "shl" => {
                lhs
            }
            "gts" | "ges" | "les" | "lts" |
            "geu" | "gtu" | "leu" | "ltu" => {
                Register::Work(2)
            }
            _ => {
                todo!()
            }
        }

    }

    pub fn set_memory_ptr(&mut self, memarg: MemoryImmediate, memory_list: &MemoryList) {
        //let offset = memory_list.get_offset(memarg.memory) + memarg.offset;
        //self.push_instr(format!("tp @e[tag=memoryptr] {} 0 0", offset));
        println!("TODO: set_memory_ptr {:?}", memarg);
    }

    pub fn make_i32_binop(&mut self, op: &'static str) {
        let rhs = Register::Work(1);
        let lhs = Register::Work(0);

        self.push_instr(Instr::PopI32Into(rhs));
        self.push_instr(Instr::PopI32Into(lhs));
        let dst = self.get_i32_dst(lhs, op, rhs);
        self.push_instr(Instr::I32Op { dst, lhs, op, rhs });
        self.push_instr(Instr::PushI32From(dst));
    }

    pub fn push_instr(&mut self, instr: Instr) {
        self.basic_blocks.last_mut().unwrap().instrs.push(instr);
    }

    pub fn setup_arguments(&mut self, function_index: CodeFuncIdx, local_count: u32, types: &TypeList, function_list: &FunctionList, locals: &[(u32, Type)]) {
        if self.basic_blocks.is_empty() {
            self.basic_blocks.push(BasicBlock::new(function_index, 0));

            self.push_instr(Instr::Comment(format!("Push frame {}", local_count)));
            self.push_instr(Instr::PushFrame(local_count));
        } else {
            panic!()
        }

        let ty = function_list.get_function_type(function_index, types);

        for (param_idx, _param) in ty.params.iter().enumerate() {
            self.push_instr(Instr::Comment(format!("#   Parameter {}", param_idx)));

            let reg = Register::Param(param_idx as u32);

            self.push_instr(Instr::SetLocalPtr(param_idx as u32));
            self.push_instr(Instr::StoreLocal(reg));
        }
    }

    pub fn store_i32(&mut self, memarg: MemoryImmediate, memory_list: &MemoryList) {
        let dreg = Register::Work(1);
        let areg = Register::Work(0);

        self.push_instr(Instr::PopI32Into(dreg));
        self.push_instr(Instr::PopI32Into(areg));
        self.push_instr(Instr::AddI32Const(areg, memarg.offset as i32));
        self.push_instr(Instr::SetMemPtr(areg));

        self.push_instr(Instr::StoreI32(dreg, memarg.align));
    }

    pub fn load_i32(&mut self, memarg: MemoryImmediate, memory_list: &MemoryList) {
        let reg = Register::Work(0);

        self.push_instr(Instr::PopI32Into(reg));
        self.push_instr(Instr::AddI32Const(reg, memarg.offset as i32));
        self.push_instr(Instr::SetMemPtr(reg));

        self.push_instr(Instr::LoadI32(reg, memarg.align));
        self.push_instr(Instr::PushI32From(reg));
    }

    pub fn local_set(&mut self, local_index: u32) {
        let reg = Register::Work(0);
        self.push_instr(Instr::PopValueInto(reg));
        self.push_instr(Instr::SetLocalPtr(local_index));
        self.push_instr(Instr::StoreLocal(reg));
    }

    /// Grid calling convention:
    /// 
    /// The caller:
    /// Pops values from the stack into the parameter registers in the expected order.
    /// Pushes the return address to the stack.
    /// Branches to the callee
    pub fn static_call(&mut self, this_func_index: CodeFuncIdx, function_index: CodeFuncIdx, types: &TypeList, function_list: &FunctionList) {
        if USE_GRID_CALL {
            // Pop values from the stack to use as the arguments
            let ty = function_list.get_function_type(function_index, types);
            for (i, param) in ty.params.iter().enumerate() {
                match param {
                    // TODO: Replace once I fix the loading at the beginning of the callee
                    /*
                    Type::I32 => {
                        self.push_instr(Instr::PopI32Into(Register::Param(ty.params.len() as u32 - 1 - i as u32)));
                    }
                    */
                    _ => self.push_instr(Instr::PopValueInto(Register::Param(ty.params.len() as u32 - 1 - i as u32))),
                }
            }

            // Push return address
            self.push_instr(Instr::Comment("  Push return address".to_string()));
            self.push_instr(Instr::PushReturnAddress(Label::new(this_func_index, self.basic_blocks.len())));

            // Jump to function
            self.push_instr(Instr::Branch(Label::new(function_index, 0)));
        } else {
            todo!()
        }
    }

    pub fn dyn_call(&mut self, this_func_index: CodeFuncIdx, table_index: u32, ty: &FuncType) {
        if USE_GRID_CALL {
            // Pop function index
            self.push_instr(Instr::PopValueInto(Register::Work(0)));

            // Pop values from the stack to use as the arguments
            for (i, param) in ty.params.iter().enumerate() {
                match param {
                    // TODO: Replace once I fix the loading at the beginning of the callee
                    /*
                    Type::I32 => {
                        self.push_instr(Instr::PopI32Into(Register::Param(ty.params.len() as u32 - 1 - i as u32)));
                    }
                    */
                    _ => self.push_instr(Instr::PopValueInto(Register::Param(ty.params.len() as u32 - 1 - i as u32)))
                }
            }

            // Push return address
            self.push_instr(Instr::Comment("  Push return address".to_string()));
            self.push_instr(Instr::PushReturnAddress(Label::new(this_func_index, self.basic_blocks.len())));

            // Jump to function
            self.push_instr(Instr::DynBranch(Register::Work(0), Some(table_index)));
        } else {
            todo!()
        }
    }


    pub fn visit_operator(&mut self, o: Operator, local_count: u32, func_idx: CodeFuncIdx, types: &TypeList, function_list: &FunctionList, memory_list: &MemoryList, global_list: &GlobalList, branches: Option<&(Label, Option<Label>)>) {
        if self.basic_blocks.is_empty() {
            self.basic_blocks.push(BasicBlock::new(func_idx, 0));
        }

        self.push_instr(Instr::Comment(format!("{:?}", o)));

        use Operator::*;
        match o {
            Call { function_index } => { 
                let function_index = CodeFuncIdx(function_index);

                self.push_instr(Instr::Comment(format!("#   wasm:{}", function_list.get_entry_point(function_index))));

                self.static_call(func_idx, function_index, types, function_list);

                self.basic_blocks.push(BasicBlock::new(func_idx, self.basic_blocks.len()));

                let f = function_list.get_function_type(function_index, types);
                assert!(f.returns.len() <= 1);

                if f.returns.len() == 1 {
                    match f.returns[0] {
                        Type::I32 => {
                            // TODO:
                            // self.push_instr(r#"tellraw @a [{"text":"return val is "},{"score":{"name":"%work%1%lo","objective":"reg"}}]"#.to_string());
                            self.push_instr(Instr::PushI32From(Register::Return));
                        }
                        _ => todo!(),
                    }
                }
            }
            CallIndirect { index, table_index } => {
                let ty = &types.types[index as usize];
                let ty = if let TypeDef::Func(f) = ty {
                    f
                } else {
                    unreachable!()
                };

                self.dyn_call(func_idx, table_index, ty);

                self.basic_blocks.push(BasicBlock::new(func_idx, self.basic_blocks.len()));
                assert!(ty.returns.len() <= 1);

                if ty.returns.len() == 1 {
                    match ty.returns[0] {
                        Type::I32 => {
                            // TODO:
                            // self.push_instr(r#"tellraw @a [{"text":"return val is "},{"score":{"name":"%work%1%lo","objective":"reg"}}]"#.to_string());
                            self.push_instr(Instr::PushI32From(Register::Return));
                        }
                        _ => todo!(),
                    }
                }

                //self.basic_blocks.push(BasicBlock::new(self.basic_blocks.len()));
            }
            Return => {
                self.push_instr(Instr::Comment(" Pop frame".to_string()));
                self.push_instr(Instr::PopFrame(local_count));

                // Save return value so we can get to the 
                // return address
                let f = function_list.get_function_type(func_idx, types);
                self.push_instr(Instr::Comment(" Save return value".to_string()));
                assert!(f.returns.len() <= 1);

                if f.returns.len() == 1 {
                    self.push_instr(Instr::PopI32Into(Register::Return));
                }

                self.push_instr(Instr::Comment(" Pop return address".to_string()));
                // Pop return address
                let reg = Register::Work(0);
                self.push_instr(Instr::PopI32Into(reg));
                self.push_instr(Instr::DynBranch(reg, None));

                self.basic_blocks.push(BasicBlock::new(func_idx, self.basic_blocks.len()));
            }

            GlobalGet { global_index } => { 
                self.push_instr(Instr::SetGlobalPtr(global_index));

                let reg = Register::Work(0);
                self.push_instr(Instr::LoadGlobal(reg));

                self.push_instr(Instr::PushValueFrom(reg));
            }
            GlobalSet { global_index } => {
                let reg = Register::Work(0);

                self.push_instr(Instr::SetGlobalPtr(global_index));
                self.push_instr(Instr::PopValueInto(reg));
                self.push_instr(Instr::StoreGlobal(reg));
            }

            LocalSet { local_index } => {
                self.local_set(local_index);
            }
            LocalGet { local_index } => {
                self.push_instr(Instr::SetLocalPtr(local_index));

                let reg = Register::Work(0);
                self.push_instr(Instr::LoadLocal(reg));
                self.push_instr(Instr::PushValueFrom(reg));
            }

            I64Const { value } => {
                self.push_instr(Instr::PushI64Const(value));
            }
            I32Const { value } => {
                self.push_instr(Instr::PushI32Const(value));
            }

            I32Store { memarg } => {
                self.store_i32(memarg, memory_list);
            }
            I32Load { memarg } => {
                self.load_i32(memarg, memory_list);
            }
            I64Store { memarg } => {
                let mut hi_memarg = memarg;
                hi_memarg.offset += 4;

                self.store_i32(hi_memarg, memory_list);
                self.store_i32(memarg, memory_list);
            }
            I64Load { memarg } => {
                let mut hi_memarg = memarg;
                hi_memarg.offset += 4;

                self.load_i32(memarg, memory_list);
                self.load_i32(hi_memarg, memory_list);
            }

            End => {
                if self.depth == 0 {
                    self.visit_operator(Operator::Return, local_count, func_idx, types, function_list, memory_list, global_list, branches)
                } else {
                    // Branch to next block
                    self.push_instr(Instr::Branch(Label::new(func_idx, self.basic_blocks.len())));

                    self.basic_blocks.push(BasicBlock::new(func_idx, self.basic_blocks.len()));

                    self.depth -= 1;
                }
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

            LocalTee { local_index } => {
                let reg = Register::Work(0);
                self.push_instr(Instr::PopValueInto(reg));
                self.push_instr(Instr::PushValueFrom(reg));
                self.push_instr(Instr::PushValueFrom(reg));
                self.visit_operator(Operator::LocalSet { local_index }, local_count, func_idx, types, function_list, memory_list, global_list, branches)
            }
            Drop => {
                self.push_instr(Instr::Drop);
            }

            Block { ty } => {
                if ty != TypeOrFuncType::Type(Type::EmptyBlockType) {
                    todo!("Block {:?}", ty);
                }

                self.depth += 1;
            }

            I32Eqz => {
                let val = Register::Work(0);

                self.push_instr(Instr::PopI32Into(val));

                let cond = Register::Work(1);

                self.push_instr(Instr::I32Eqz { val, cond });

                self.push_instr(Instr::PushI32From(cond));
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

            I64ExtendI32S => {
                let reg = Register::Work(0);

                self.push_instr(Instr::PopValueInto(reg));
                self.push_instr(Instr::I64ExtendI32S { dst: reg, src: reg });
                self.push_instr(Instr::PushValueFrom(reg));
            }

            Loop { ty } => {
                self.basic_blocks.push(BasicBlock::new(func_idx, self.basic_blocks.len()));

                println!("TODO: Loop {:?}", ty);

                self.depth += 1;
            }
            Br { .. } => {
                let (target, o) = branches.unwrap();
                assert!(o.is_none());

                self.push_instr(Instr::Branch(Label::new(func_idx, target.idx)));

                self.basic_blocks.push(BasicBlock::new(func_idx, self.basic_blocks.len()));
            }
            BrIf { .. } => {
                let reg = Register::Work(0);

                self.push_instr(Instr::PopValueInto(reg));

                let (t_target, f_target) = branches.unwrap();
                let f_target = f_target.as_ref().unwrap();

                let t_name = Label::new(func_idx, t_target.idx);
                let f_name = Label::new(func_idx, f_target.idx);

                self.push_instr(Instr::BranchIf { t_name, f_name, cond: reg });

                self.basic_blocks.push(BasicBlock::new(func_idx, self.basic_blocks.len()));
            }
            Nop => {}
            Unreachable => {
                println!("TODO: Unreachable")
            }

            _ => todo!("{:?}", o),
        }
    }
}

struct TypeList<'a> {
    types: Vec<TypeDef<'a>>,
}

impl<'a> TypeList<'a> {
    fn new() -> Self {
        TypeList {
            types: Vec::new()
        }
    }

    fn add_type(&mut self, ty: TypeDef<'a>) {
        self.types.push(ty);
    }
}

#[derive(Debug, Clone)]
struct FunctionList {
    functions: Vec<u32>,
}

impl FunctionList {
    pub fn new() -> Self {
        FunctionList {
            functions: Vec::new(),
        }
    }

    pub fn add_function(&mut self, function: u32) {
        self.functions.push(function)
    }

    pub fn get_function_type<'a>(&self, function: CodeFuncIdx, types: &'a TypeList) -> &'a FuncType {
        let ty = self.functions[function.0 as usize];
        if let TypeDef::Func(f) = &types.types[ty as usize] {
            f
        } else {
            unreachable!()
        }
    }

    pub fn get_entry_point(&self, function: CodeFuncIdx) -> String {
        self.get_block(&Label::new(function, 0))
    }

    pub fn get_block(&self, label: &Label) -> String {
        format!("__wasm{}_{}", label.func_idx.0, label.idx)
    }
}

struct MemoryList {
    memory: Vec<MemoryType>,
}

impl MemoryList {
    pub fn new() -> Self {
        MemoryList {
            memory: Vec::new()
        }
    }

    pub fn add_memory(&mut self, memory: MemoryType) {
        self.memory.push(memory);
    }

    pub fn get_offset(&self, memarg: MemoryImmediate) -> String {
        // FIXME:
        format!("{} 0 8", memarg.offset)
    }

    pub fn init(&self) -> Vec<String> {
        let mut cmds = Vec::new();

        let mut x_offset = 0;

        for m in self.memory.iter() {
            match m {
                MemoryType::M32 { limits, shared } => {
                    /*if limits.maximum.is_none() {
                        todo!()
                    }

                    if limits.maximum.unwrap() != limits.initial {
                        todo!()
                    }*/

                    // FIXME:

                    for _ in 0..limits.maximum.unwrap_or(limits.initial) {
                        // Web assembly page size is 64KiB
                        // Thus an 8x256x8 area where each block is an i32
                        // makes up exactly one page

                        // Also note that a single fill command can only fill 32768 blocks,
                        // so we'll just do it one at a time for safety
                        cmds.push(format!("fill {} 0 8 {} 255 15 minecraft:air replace", x_offset, x_offset + 8));
                        cmds.push(format!("fill {} 0 8 {} 255 15 minecraft:jukebox{{RecordItem:{{id:\"minecraft:stone\",Count:1b,tag:{{Memory:1}}}}}} replace", x_offset, x_offset + 8));
                        x_offset += 8;
                    }

                }
                MemoryType::M64 { .. } => todo!(),
            }
        }

        cmds
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct BlockPos {
    x: i32,
    y: i32,
    z: i32,
}

impl std::fmt::Display for BlockPos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.x, self.y, self.z)
    }
}

struct GlobalList<'a> {
    globals: Vec<Global<'a>>,
}

impl<'a> GlobalList<'a> {
    pub fn new() -> Self {
        GlobalList {
            globals: Vec::new(),
        }
    }

    pub fn add_global(&mut self, global: Global<'a>) {
        self.globals.push(global);
    }

    pub fn get_offset(&self, idx: u32) -> BlockPos {
        BlockPos { x: idx as i32 * 2, y: 0, z: 3 }
    }

    pub fn init(&self) -> Vec<String> {
        let mut cmds = Vec::new();

        cmds.push(format!("fill 0 0 3 {} 0 4 minecraft:air replace", self.globals.len()));
        cmds.push(format!("fill 0 0 3 {} 0 4 minecraft:jukebox{{RecordItem:{{id:\"minecraft:stone\",Count:1b,tag:{{Memory:0}}}}}} replace", self.globals.len()));

        for (i, global) in self.globals.iter().enumerate() {
            let op = global.init_expr.get_operators_reader()
                .into_iter()
                .map(|o| o.unwrap())
                .collect::<Vec<_>>();

            assert_eq!(op.len(), 2);

            assert!(matches!(op[1], Operator::End));

            match op[0] {
                Operator::I32Const { value } => {
                    cmds.push(format!("data modify block {} 0 3 RecordItem.tag.Memory set value {}", i, value));
                }
                _ => todo!("{:?}", op)
            }
        }

        cmds
    }
}

struct ImportList<'a> {
    imports: Vec<Import<'a>>
}

impl<'a> ImportList<'a> {
    pub fn new() -> Self {
        ImportList { imports: Vec::new() }
    }

    pub fn add_import(&mut self, i: Import<'a>) {
        self.imports.push(i);
    }

    pub fn num_funcs(&self) -> usize {
        self.imports.iter()
            .filter(|i| matches!(i.ty, ImportSectionEntryType::Function(_)))
            .count()
    }
}

struct ExportList<'a> {
    exports: Vec<Export<'a>>,
}

impl<'a> ExportList<'a> {
    pub fn new() -> Self {
        ExportList {
            exports: Vec::new(),
        }
    }

    pub fn add_export(&mut self, export: Export<'a>) {
        self.exports.push(export);
    }

    pub fn get_func(&self, s: &str) -> CodeFuncIdx {
        for (i, e) in self.exports.iter().enumerate() {
            if e.field == s {
                return CodeFuncIdx(e.index);
            }
        }

        panic!("couldn't find {:?}", s);
    }
}

fn save_datapack(mc_functions: Vec<(String, String)>, wasm_file: &WasmFile) {
    let datapack = datapack::Datapack::new();

    let folder_path = std::path::Path::new("../out/");


    datapack.save(folder_path).unwrap();
    for (name, contents) in mc_functions.iter() {
        datapack.write_function(folder_path, "wasm", name, contents).unwrap();
    }

    for i in std::fs::read_dir("./src/intrinsic").unwrap() {
        let i = i.unwrap();
        if i.file_type().unwrap().is_dir() {
            for j in std::fs::read_dir(i.path()).unwrap() {
                let j = j.unwrap();
                if j.file_type().unwrap().is_dir() {
                    todo!()
                } else {
                    let p = i.file_name();
                    let n = j.file_name();
                    let n = format!("{}/{}", p.to_string_lossy(), n.to_string_lossy());
                    let n = &n[..n.len() - ".mcfunction".len()];

                    let contents = std::fs::read_to_string(j.path()).unwrap();
                    datapack.write_function(folder_path, "intrinsic", &n, &contents).unwrap();
                }
            }
        } else {
            let name = i.file_name();
            let name = name.to_string_lossy();
            let name = &name[..name.len() - ".mcfunction".len()];
            let contents = std::fs::read_to_string(i.path()).unwrap();
            datapack.write_function(folder_path, "intrinsic", name, &contents).unwrap();
        }
    }
}

struct TableList {
    tables: Vec<TableType>,
}

impl TableList {
    pub fn new() -> Self {
        TableList { tables: Vec::new() }
    }

    pub fn add_table(&mut self, ty: TableType) {
        dbg!(&ty);
        self.tables.push(ty);
    }
}

struct WasmFile<'a> {
    types: TypeList<'a>,
    globals: GlobalList<'a>,
    memory: MemoryList,
    exports: ExportList<'a>,
    imports: ImportList<'a>,
    tables: TableList,
    functions: FunctionList,
}

pub fn run() {
    let file = std::fs::read("../example.wasm").unwrap();

    let (basic_blocks, wasm_file) = compile(&file);

    //run_ir(&basic_blocks, &wasm_file);

    let mc_functions = assemble(&basic_blocks, &wasm_file);

    for func in mc_functions.iter() {
        println!("F: {:?}", func.0)
    }

    run_commands(&mc_functions, &wasm_file);

    save_datapack(mc_functions, &wasm_file);
}

fn run_commands(mc_functions: &[(String, String)], wasm_file: &WasmFile) {
    use datapack_vm::cir::{Function, FunctionId};

    let mut funcs = mc_functions.iter()
        .map(|(n, c)| {
            // FIXME: Namespace
            let id = FunctionId::new(format!("wasm:{}", n.clone()));

            println!("Converting {}", id.name);

            Function::from_str(id, c).unwrap()
        })
        .collect::<Vec<_>>();
    
    for entry in std::fs::read_dir("./src/intrinsic").unwrap() {
        let entry = entry.unwrap();

        if entry.file_type().unwrap().is_dir() {
            for in_entry in std::fs::read_dir(entry.path()).unwrap() {
                let in_entry = in_entry.unwrap();
                if in_entry.file_type().unwrap().is_dir() {
                    todo!()
                } else {
                    let p = entry.file_name();
                    let n = in_entry.file_name();
                    let n = format!("{}/{}", p.to_string_lossy(), n.to_string_lossy());
                    let n = &n[..n.len() - ".mcfunction".len()];

                    let contents = std::fs::read_to_string(in_entry.path()).unwrap();

                    let id = FunctionId::new(format!("intrinsic:{}", n));
                    funcs.push(Function::from_str(id, &contents).unwrap());
                }
            }
        } else {
            let name = entry.file_name();
            let name = name.to_string_lossy();
            let name = &name[..name.len() - ".mcfunction".len()];

            let contents = std::fs::read_to_string(entry.path()).unwrap();

            let id = FunctionId::new(format!("intrinsic:{}", name));
            funcs.push(Function::from_str(id, &contents).unwrap());
        }
    }


    /*let label = wasm_file.exports.get_func("_start");
    let idx = mc_functions.iter().enumerate().find(|(_, (n, _))| {
            n == &format!("__wasm{}_0", label.0)
        }).unwrap_or_else(|| {
            eprintln!("Failed to find {:?}", label);
            panic!();
        }).0;
    */

    let idx = mc_functions.iter().enumerate().find(|(_, (n, _))| {
            n == "setup"
        }).unwrap_or_else(|| {
            eprintln!("Failed to find {:?}", "setup");
            panic!();
        }).0;

    let mut i = datapack_vm::Interpreter::new_wasm(funcs, idx, "");

    i.run_to_end().unwrap();

    let idx = mc_functions.iter().enumerate().find(|(_, (n, _))| {
            n == "_start"
        }).unwrap_or_else(|| {
            eprintln!("Failed to find {:?}", "_start");
            panic!();
        }).0;

    i.set_pos(idx);

    i.run_to_end().unwrap();

    let holder = "%return%lo".to_string();
    let obj = "reg".to_string();
    dbg!(i.scoreboard.get(&datapack_vm::cir::ScoreHolder::new(holder).unwrap(), &obj));
}

fn compile(file: &[u8]) -> (Vec<BasicBlock<Instr>>, WasmFile) {
    let mut types = TypeList::new();
    let mut globals = GlobalList::new();
    let mut memory = MemoryList::new();
    let mut exports = ExportList::new();
    let mut imports = ImportList::new();
    let mut tables = TableList::new();

    let mut func_reader = None;

    let mut codes = Vec::new();

    for payload in Parser::new(0).parse_all(&file) {
        let payload = payload.unwrap();
        match payload {
            Payload::Version { .. } => {
                println!("========= Module");
            }
            Payload::ImportSection(i) => {
                for import in i {
                    let import = import.unwrap();
                    imports.add_import(import);
                }
            }
            Payload::ExportSection(e) => {
                for export in e {
                    let export = export.unwrap();
                    exports.add_export(export);
                }
            }
            Payload::TypeSection(t) => {
                for ty in t {
                    let ty = ty.unwrap();

                    types.add_type(ty);
                }
            }
            Payload::GlobalSection(g) => {
                for global in g {
                    let global = global.unwrap();

                    globals.add_global(global);
                }
            }
            Payload::MemorySection(m) => {
                for mem in m {
                    let mem = mem.unwrap();

                    memory.add_memory(mem);
                }
            }
            Payload::FunctionSection(f) => {
                assert!(func_reader.is_none());
                func_reader = Some(f);
            }
            Payload::CodeSectionEntry(e) => {
                codes.push(e);
            }
            Payload::TableSection(t) => {
                for table in t {
                    let table = table.unwrap();
                    tables.add_table(table);
                }
            }
            _other => {
                println!("{:?}", _other);
            }
        }
    }

    // Add stub imported functions
    // TODO: Figure out how to do this

    let mut functions = FunctionList::new(); 

    for f in imports.imports.iter() {
        if let ImportSectionEntryType::Function(func) = f.ty {
            functions.add_function(func)
        }
    }


    let func_reader = func_reader.unwrap();
    for func in func_reader {
        let func = func.unwrap();

        functions.add_function(func);
    }


    let mut basic_blocks = Vec::new();

    for i in 0..imports.num_funcs() {
        let mut bb = BasicBlock::new(CodeFuncIdx(i as u32), 0);
        bb.instrs.push(Instr::Unreachable);
        basic_blocks.push(bb);
    }


    let mut func_idx = CodeFuncIdx(imports.num_funcs() as u32);
    for e in codes {
        println!("Doing function {}", func_idx.0);

        let operators = e.get_operators_reader().unwrap()
            .into_iter()
            .map(|o| o.unwrap())
            .collect::<Vec<_>>();
        
        let split_bb_info = split_bbs(&operators, func_idx);

        let mut local_count = functions.get_function_type(func_idx, &types).params.len() as u32;

        let locals = e.get_locals_reader().unwrap()
            .into_iter()
            .map(|l| l.unwrap())
            .inspect(|l| local_count += l.0)
            .collect::<Vec<_>>();
            
        let mut s = FuncBodyStream::new();

        s.setup_arguments(func_idx, local_count, &types, &functions, &locals);
        for (idx, o) in operators.into_iter().enumerate() {
            let labels = split_bb_info.branches.get(&idx);
            s.visit_operator(o, local_count, func_idx, &types, &functions, &memory, &globals, labels);
        }

        /*
        for b in s.basic_blocks.iter() {
            println!("{:?}", b.label);
            for i in b.instrs.iter() {
                println!("\t{}", i);
            }
        }
        */

        assert_eq!(s.basic_blocks.len(), split_bb_info.bb_count);

        /*
        println!("===== FUNC BODY =====");
        for bb in s.basic_blocks.iter() {
            println!("  --- {:?} ---", bb.label);
            for i in bb.instrs.iter() {
                println!("    {}", i);
            }
        }
        */

        for bb in s.basic_blocks {
            basic_blocks.push(bb);
        }

        func_idx.0 += 1;
    }

    /*
    for bb in basic_blocks.iter() {
        for instr in bb.instrs.iter() {
            println!("{:?}", instr)
        }
        println!();
    }
    */

    let wasm_file = WasmFile { functions, memory, globals, exports, imports, types, tables };

    (basic_blocks, wasm_file)
}

fn run_ir(basic_blocks: &[BasicBlock<Instr>], wasm_file: &WasmFile) {
    let mut state = State::new(basic_blocks.to_owned(), &wasm_file.globals, &wasm_file.memory);

    for (i, global) in wasm_file.globals.globals.iter().enumerate() {
        let op = global.init_expr.get_operators_reader()
            .into_iter()
            .map(|o| o.unwrap())
            .collect::<Vec<_>>();

        assert_eq!(op.len(), 2);

        assert!(matches!(op[1], Operator::End));

        match op[0] {
            Operator::I32Const { value } => {
                state.globals[i].0 = value;
            }
            _ => todo!("{:?}", op)
        }
    }

    /*state.call(state.get_pc(&Label::new(exports.get_func("__wasm_call_ctors"), 0)));

    println!("=============");

    state.call(state.get_pc(&Label::new(exports.get_func("main"), 0)));*/

    state.call(state.get_pc(&Label::new(wasm_file.exports.get_func("_start"), 0)));

    println!("{:?}", state.registers.get(&Register::Return));
}

fn assemble(basic_blocks: &[BasicBlock<Instr>], wasm_file: &WasmFile) -> Vec<(String, String)> {
    let mut mc_functions = Vec::new();

    for bb in basic_blocks {
        /*
        bb.instrs.insert(0, r#"tellraw @a [{"text":"stackptr is "},{"score":{"name":"%stackptr","objective":"wasm"}}]"#.to_string());
        */

        let mut new_block = bb.lower(&wasm_file.globals, &wasm_file.functions);
        let name = wasm_file.functions.get_block(&new_block.label);

        if USE_GRID_CALL {
            new_block.instrs.insert(0, "setblock ~ ~1 ~ minecraft:air".to_string());
        }

        let contents = new_block.instrs.join("\n");

        mc_functions.push((name, contents));
    }

    do_fixups(&mut mc_functions);

    let mut setup = "\
    # Set up scoreboard\n\
    scoreboard objectives remove wasm\n\
    scoreboard objectives add wasm dummy\n\
    # scoreboard objectives setdisplay sidebar wasm\n\
    \n\
    scoreboard objectives remove reg\n\
    scoreboard objectives add reg dummy\n\
    \n\
    # Remove old armor stand pointers\n\
    kill @e[tag=memoryptr]\n\
    kill @e[tag=localptr]\n\
    kill @e[tag=frameptr]\n\
    kill @e[tag=stackptr]\n\
    kill @e[tag=globalptr]\n\
    \n\
    # Add armor stand pointers\n\
    summon minecraft:armor_stand 0 0 8 {Marker:1b,Tags:[\"memoryptr\"],CustomName:'\"memoryptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 1 {Marker:1b,Tags:[\"localptr\"],CustomName:'\"localptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 1 {Marker:1b,Tags:[\"frameptr\"],CustomName:'\"frameptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 0 {Marker:1b,Tags:[\"stackptr\"],CustomName:'\"stackptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 3 {Marker:1b,Tags:[\"globalptr\"],CustomName:'\"globalptr\"',CustomNameVisible:1b}\n\
    
    scoreboard players set %stackptr wasm 0
    scoreboard players set %frameptr wasm 0
    
    scoreboard players set %%2 reg 2
    scoreboard players set %%4 reg 4
    scoreboard players set %%8 reg 8
    scoreboard players set %%16 reg 16
    scoreboard players set %%SIXTEEN reg 16
    scoreboard players set %%256 reg 256
    scoreboard players set %%65536 reg 65536
    scoreboard players set %%16777216 reg 16777216 
    ".to_string();

    setup.push_str("\n# Make stack\n");
    setup.push_str("fill 0 0 0 30 0 0 minecraft:air replace\n");
    setup.push_str("fill 0 0 0 30 0 0 minecraft:jukebox{RecordItem:{id:\"minecraft:stone\",Count:1b,tag:{Memory:1}}} replace\n");

    setup.push_str("\n# Make memory\n");
    for mem_cmd in wasm_file.memory.init() {
        setup.push_str(&mem_cmd);
        setup.push('\n');
    }

    setup.push_str("\n# Make globals\n");
    for global_cmd in wasm_file.globals.init() {
        setup.push_str(&global_cmd);
        setup.push('\n');
    }

    if USE_GRID_CALL {
        setup.push_str("\n# Make commands\n");
        for (idx, (name, _)) in mc_functions.iter().enumerate() {
            let cmd = format!("setblock {} 0 -1 minecraft:air replace", idx);
            setup.push_str(&cmd);
            setup.push('\n');
            let cmd = format!("setblock {} 0 -1 minecraft:command_block{{Command:\"function wasm:{}\"}} replace", idx, name);
            setup.push_str(&cmd);
            setup.push('\n');
        }
    }

    mc_functions.push(("setup".to_string(), setup));

    let mut dyn_branch = String::new();
    for (idx, (name, _)) in mc_functions.iter().enumerate() {
        // FIXME: Reg0
        let b = format!("# wasm:{}\nexecute if score %work%0%lo reg matches {}..{} run setblock {} 1 -1 minecraft:redstone_block destroy\n", name, idx, idx, idx);
        dyn_branch.push_str(&b);
    }

    // FIXME:
    let e = format!("execute unless score %work%0%lo reg matches 0..{} run ", mc_functions.len() - 1);
    dyn_branch.push_str(&e);
    dyn_branch.push_str(r#"tellraw @a [{"text":"Attempt to branch to invalid function "},{"score":{"name":"%work%0%lo","objective":"reg"}}]"#);

    mc_functions.push(("dyn_branch".to_string(), dyn_branch));


    for export in wasm_file.exports.exports.iter() {
        if let ExternalKind::Function = export.kind {
            let index = CodeFuncIdx(export.index);

            // FIXME: Pos
            let name = wasm_file.functions.get_entry_point(index);
            let idx = mc_functions.iter().enumerate().find(|(_, (n, _))| {
                n == &name
            }).unwrap().0;

            let mut f = FuncBodyStream::new();
            f.basic_blocks.push(BasicBlock::new(CodeFuncIdx(0), 0));
            f.push_instr(Instr::PushI32Const(-1));
            if export.field == "main" {
                f.push_instr(Instr::PushI32Const(0));
                f.push_instr(Instr::PushI32Const(0));
            }

            assert_eq!(f.basic_blocks.len(), 1);

            let mut body = Vec::new();
            for i in f.basic_blocks[0].instrs.iter_mut() {
                i.lower(&mut body, &wasm_file.globals, &wasm_file.functions);
            }
            body.push(format!("setblock {} 1 -1 minecraft:redstone_block destroy", idx));

            let content = body.join("\n");

            mc_functions.push((export.field.to_string(), content));
        }
    }


    mc_functions
}

fn do_fixups(mc_functions: &mut Vec<(String, String)>) {
    // Apply fixups
    for func_idx in 0..mc_functions.len() {
        while let Some(start) = mc_functions[func_idx].1.find('!') {
            let middle = mc_functions[func_idx].1[start + 1..].find('!').unwrap() + start + 1;
            let end = mc_functions[func_idx].1[middle + 1..].find('!').unwrap() + middle + 1;

            let fixup_type = &mc_functions[func_idx].1[start + 1..middle];
            let fixup_value = &mc_functions[func_idx].1[middle + 1..end];

            match fixup_type {
                "BLOCKIDX" => {
                    let idx = mc_functions.iter().enumerate().find(|(_, (n, _))| {
                        n == fixup_value
                    }).unwrap_or_else(|| {
                        eprintln!("Failed to find {:?}", fixup_value);
                        eprintln!("Functions are:");
                        for (n, _) in mc_functions.iter() {
                            eprintln!("{:?}", n);
                        }
                        panic!();
                    }).0;

                    let idx = idx.to_string();

                    mc_functions[func_idx].1.replace_range(start..=end, &idx);
                }
                "BLOCKPOS" => {
                    let idx = mc_functions.iter().enumerate().find(|(_, (n, _))| {
                        n == fixup_value
                    }).unwrap().0;

                    // FIXME:
                    let pos = format!("{} 1 -1", idx);

                    mc_functions[func_idx].1.replace_range(start..=end, &pos);
                }
                s => {
                    todo!("{:?}", s)
                }
            }
        }
    }
}