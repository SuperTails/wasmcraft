mod datapack;

use wasmparser::{Parser, Payload, Operator, GlobalType, Global, MemoryType,
    MemoryImmediate, TypeDef, Type, TypeOrFuncType, Export, ExternalKind, FuncType};

#[derive(Debug)]
struct Label(usize);

/// Every branch destination must be a new Basic Block
struct BasicBlock<T> {
    label: Label,
    instrs: Vec<T>,
}

impl<T> BasicBlock<T> {
    pub fn new(l: usize) -> Self {
        BasicBlock {
            label: Label(l),
            instrs: Vec::new()
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

fn split_bbs(operators: &[Operator]) -> SplitBBInfo {
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
                branches.insert(idx, (Label(target), None));
            }
            Operator::BrIf { relative_depth } => {
                let t_target = idx + 1 + find_end(&operators[idx + 1..], *relative_depth);
                let t_target = op_bbs[t_target];
                let f_target = op_bbs[idx + 1];

                branches.insert(idx, (Label(t_target), Some(Label(f_target))));
            }
            _ => {}
        }
    }

    SplitBBInfo {
        bb_count: bbs_id + 1,
        branches,
    }
}

struct FuncBodyStream {
    basic_blocks: Vec<BasicBlock<String>>,
    depth: usize,
}

impl FuncBodyStream {
    pub fn new() -> Self {
        FuncBodyStream {
            basic_blocks: Vec::new(),
            depth: 0,
        }
    }

    pub fn set_memory_ptr(&mut self, memarg: MemoryImmediate, memory_list: &MemoryList) {
        //let offset = memory_list.get_offset(memarg.memory) + memarg.offset;
        //self.push_instr(format!("tp @e[tag=memoryptr] {} 0 0", offset));
        println!("TODO: set_memory_ptr {:?}", memarg);
    }

    pub fn set_global_ptr(&mut self, global_index: u32, is_hi: bool, global_list: &GlobalList) {
        let pos = global_list.get_offset(global_index, is_hi);
        self.push_instr(format!("execute as @e[tag=globalptr] run tp @s {}", pos));
    }

    pub fn reset_frame_ptr(&mut self) {
        self.push_instr("execute as @e[tag=frameptr] run tp @s 0 0 1".to_string());
        self.push_instr("scoreboard players set %frameptr wasm 0".to_string());
    }

    pub fn push_frame(&mut self, local_count: u32) {
        if local_count != 0 {
            self.push_instr(format!("# Push frame with {} locals", local_count));
            self.push_instr(format!("execute at @e[tag=frameptr] run fill ~ ~ ~ ~{} ~ ~1 minecraft:jukebox{{RecordItem:{{id:\"minecraft:stone\",Count:1b,tag:{{Memory:1}}}}}}", local_count - 1));
            self.push_instr(format!("execute as @e[tag=frameptr] at @e[tag=frameptr] run tp @s ~{} ~ ~", local_count));
        }
    }

    pub fn pop_frame(&mut self, local_count: u32) {
        if local_count != 0 {
            self.push_instr(format!("execute as @e[tag=frameptr] at @e[tag=frameptr] run tp @s ~-{} ~ ~", local_count));
            self.push_instr(format!("execute at @e[tag=frameptr] run fill ~ ~ ~ ~{} ~ ~1 air", local_count - 1));
        }
    }

    pub fn set_local_ptr(&mut self, local_index: u32, is_hi: bool) {
        let z = if is_hi { 2 } else { 1 };
        self.push_instr(format!("execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~{} 0 {}", local_index, z));
    }

    pub fn pop_value_into(&mut self, register_idx: u32) {
        let reg_hi = self.regname_hi(register_idx);
        self.decr_stack_ptr_half();
        self.push_instr(format!("execute at @e[tag=stackptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg_hi));

        let reg_lo = self.regname_lo(register_idx);
        self.decr_stack_ptr_half();
        self.push_instr(format!("execute at @e[tag=stackptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg_lo));
    }

    pub fn push_value_from(&mut self, register_idx: u32) {
        let reg_lo = self.regname_lo(register_idx);
        self.push_instr(format!("execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg_lo));
        self.incr_stack_ptr_half();

        let reg_hi = self.regname_hi(register_idx);
        self.push_instr(format!("execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg_hi));
        self.incr_stack_ptr_half();
    }

    pub fn incr_stack_ptr_half(&mut self) {
        self.push_instr("execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~".to_string());
        self.push_instr("scoreboard players add %stackptr wasm 1".to_string());
    }

    pub fn incr_stack_ptr(&mut self) {
        self.push_instr("execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~2 ~ ~".to_string());
        self.push_instr("scoreboard players add %stackptr wasm 2".to_string());
    }

    pub fn decr_stack_ptr_half(&mut self) {
        self.push_instr("execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~".to_string());
        self.push_instr("scoreboard players remove %stackptr wasm 1".to_string());
    }

    pub fn decr_stack_ptr(&mut self) {
        self.push_instr("execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-2 ~ ~".to_string());
        self.push_instr("scoreboard players remove %stackptr wasm 2".to_string());
    }

    pub fn pop_half_into(&mut self, register_idx: u32) {
        let reg = self.regname_i32(register_idx);
        self.decr_stack_ptr_half();
        self.push_instr(format!("execute at @e[tag=stackptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg));
    }

    pub fn pop_i32_into(&mut self, register_idx: u32) {
        let reg = self.regname_i32(register_idx);
        self.decr_stack_ptr();
        self.push_instr(format!("execute at @e[tag=stackptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg));
    }

    pub fn push_i64_const(&mut self, value: i64) {
        self.push_instr(format!("execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value {}", value as i32));
        self.incr_stack_ptr_half();
       
        self.push_instr(format!("execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value {}", (value >> 32) as i32));
        self.incr_stack_ptr_half();
    }

    pub fn push_i32_const(&mut self, value: i32) {
        self.push_instr(format!("execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value {}", value));
        self.incr_stack_ptr();
    }

    pub fn push_i32_from(&mut self, register_idx: u32) {
        let reg = self.regname_i32(register_idx);
        self.push_instr(format!("execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg));
        self.incr_stack_ptr();
    }

    pub fn push_half_from(&mut self, register_idx: u32) {
        let reg = self.regname_i32(register_idx);
        self.push_instr(format!("execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg));
        self.incr_stack_ptr_half();
    }

    pub fn make_i32_op(&mut self, lhs_idx: u32, op: &str, rhs_idx: u32) -> u32 {
        let lhs = self.regname_i32(lhs_idx);
        let rhs = self.regname_i32(rhs_idx);

        match op {
            "+=" | "-=" | "*=" | "/=" | "%=" => {
                self.push_instr(format!("scoreboard players operation {} reg {} {} reg", lhs, op, rhs));
                0
            }
            "&=" => {
                self.push_instr(format!("scoreboard players operation %param0%0 reg = {} reg", lhs));
                self.push_instr(format!("scoreboard players operation %param1%0 reg = {} reg", rhs));
                self.push_instr("function intrinsic:and".to_string());
                self.push_instr(format!("scoreboard players operation {} reg = %return%0 reg", lhs));
                0
            }
            "|=" => {
                self.push_instr(format!("scoreboard players operation %param0%0 reg = {} reg", lhs));
                self.push_instr(format!("scoreboard players operation %param1%0 reg = {} reg", rhs));
                self.push_instr("function intrinsic:or".to_string());
                self.push_instr(format!("scoreboard players operation {} reg = %return%0 reg", lhs));
                0
            }
            "^=" => {
                self.push_instr(format!("scoreboard players operation %param0%0 reg = {} reg", lhs));
                self.push_instr(format!("scoreboard players operation %param1%0 reg = {} reg", rhs));
                self.push_instr("function intrinsic:xor".to_string());
                self.push_instr(format!("scoreboard players operation {} reg = %return%0 reg", lhs));
                0
            }
            "shl" => {
                self.push_instr(format!("scoreboard players operation %param0%0 reg = {} reg", lhs));
                self.push_instr(format!("scoreboard players operation %param1%0 reg = {} reg", rhs));
                self.push_instr("function intrinsic:shl".to_string());
                self.push_instr(format!("scoreboard players operation {} reg = %param0%0 reg", lhs));
                0
            }
            "gts" | "ges" | "les" | "lts" => {
                let score_op = match op {
                    "gts" => ">",
                    "ges" => ">=",
                    "les" => "<=",
                    "lts" => "<",
                    _ => unreachable!(),
                };

                let condreg = self.regname_i32(2);
                self.push_instr(format!("scoreboard players set {} reg 0", condreg));
                self.push_instr(format!("execute if score {} reg {} {} reg run scoreboard players set {} reg 1", lhs, score_op, rhs, condreg));
                2
            }
            "geu" | "gtu" => {
                let score_op = match op {
                    "gtu" => ">",
                    "geu" => ">=",
                    _ => unreachable!(),
                };

                let condreg = self.regname_i32(2);

                self.push_instr(format!("scoreboard players set {} reg 0", condreg));
                self.push_instr(format!("execute if score {} reg matches ..-1 run scoreboard players set {} 1", lhs, condreg));
                self.push_instr(format!("execute if score {} reg matches ..-1 run scoreboard players set {} 0", rhs, condreg));
                self.push_instr(format!("execute if score {} reg matches ..-1 run execute if score {} reg matches ..-1 run execute if score {} reg {} {} reg run scoreboard players set {} 1", lhs, rhs, lhs, score_op, rhs, condreg));
                self.push_instr(format!("execute if score {} reg matches 0.. run execute if score {} reg matches 0.. run execute if score {} reg {} {} reg run scoreboard players set {} 1", lhs, rhs, lhs, score_op, rhs, condreg));
               
                2
            }
            "leu" | "ltu" => {
                let score_op = match op {
                    "ltu" => "<",
                    "leu" => "<=",
                    _ => unreachable!(),
                };

                let condreg = self.regname_i32(2);

                self.push_instr(format!("scoreboard players set {} reg 0", condreg));
                self.push_instr(format!("execute if score {} reg matches ..-1 run scoreboard players set {} 0", lhs, condreg));
                self.push_instr(format!("execute if score {} reg matches ..-1 run scoreboard players set {} 1", rhs, condreg));
                self.push_instr(format!("execute if score {} reg matches ..-1 run execute if score {} reg matches ..-1 run execute if score {} reg {} {} reg run scoreboard players set {} 1", lhs, rhs, lhs, score_op, rhs, condreg));
                self.push_instr(format!("execute if score {} reg matches 0.. run execute if score {} reg matches 0.. run execute if score {} reg {} {} reg run scoreboard players set {} 1", lhs, rhs, lhs, score_op, rhs, condreg));
               
                2
            }
            _ => {
                println!("TODO: make_i32_op {}", op);
                0
            }
        }

    }

    pub fn make_i32_binop(&mut self, op: &str) {
        self.pop_i32_into(1);
        self.pop_i32_into(0);
        let out = self.make_i32_op(0, op, 1);
        self.push_i32_from(out);
    }

    pub fn regname_i32(&self, idx: u32) -> String {
        self.regname_lo(idx)
    }

    pub fn regname_lo(&self, idx: u32) -> String {
        format!("%work%{}%lo", idx)
    }

    pub fn regname_hi(&self, idx: u32) -> String {
        format!("%work%{}%hi", idx)
    }

    pub fn push_instr(&mut self, instr: String) {
        self.basic_blocks.last_mut().unwrap().instrs.push(instr);
    }

    pub fn setup_arguments(&mut self, function_index: u32, local_count: u32, types: &TypeList, function_list: &FunctionList, locals: &[(u32, Type)]) {
        if self.basic_blocks.is_empty() {
            self.basic_blocks.push(BasicBlock::new(0));

            self.push_frame(local_count);
        } else {
            panic!()
        }

        let ty = function_list.get_function_type(function_index, types);

        for (param_idx, _param) in ty.params.iter().enumerate() {
            self.push_instr(format!("#   Parameter {}", param_idx));

            self.set_local_ptr(param_idx as u32, true);

            let reg = self.regname_lo(ty.params.len() as u32 - 1 + 2 - param_idx as u32);
            self.push_instr(format!("execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg));

            self.set_local_ptr(param_idx as u32, false);

            let reg = self.regname_hi(ty.params.len() as u32 - 1 + 2 - param_idx as u32);
            self.push_instr(format!("execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg));
        }
    }

    pub fn store_i32(&mut self, memarg: MemoryImmediate, memory_list: &MemoryList) {
        if memarg.offset % 4 == 0 {
            self.pop_i32_into(1);
            self.pop_i32_into(0);

            let dreg = self.regname_i32(1);

            let reg = self.regname_i32(0);
            self.push_instr(format!("scoreboard players add {} reg {}", reg, memarg.offset));

            if memarg.align == 4 {
                let setptr = [
                    "scoreboard players operation %%ptr reg = %work%0%lo reg",
                    "scoreboard players operation %%ptr reg /= %%4 reg",
                    "scoreboard players operation %z reg = %%ptr reg",
                    "scoreboard players operation %z reg %= %%SIXTEEN reg",
                    "scoreboard players operation %%ptr reg /= %%SIXTEEN reg",
                    "scoreboard players operation %y reg = %%ptr reg",
                    "scoreboard players operation %y reg %= %%SIXTEEN reg",
                    "scoreboard players operation %%ptr reg /= %%SIXTEEN reg",
                    "scoreboard players operation %x reg = %%ptr reg",
                    "execute as @e[tag=memoryptr] store result entity @s Pos[0] double 1 run scoreboard players get %x reg",
                    "execute as @e[tag=memoryptr] store result entity @s Pos[1] double 1 run scoreboard players get %y reg",
                    "execute as @e[tag=memoryptr] store result entity @s Pos[2] double 1 run scoreboard players get %z reg",
                    "execute as @e[tag=memoryptr] at @e[tag=memoryptr] run tp @s ~ ~ ~8",
                ];

                for s in std::array::IntoIter::new(setptr) {
                    self.push_instr(s.to_string())
                }

                let reg = self.regname_i32(1);
                self.push_instr(format!("execute at @e[tag=memoryptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg));
            /*
            } else if memarg.align == 2 {
                // FIXME: Improve
                self.push_instr(format!("scoreboard players operation %ptr reg = {} reg", reg));
                self.push_instr(format!("scoreboard players operation %param0%0 reg = {} reg", dreg));
                self.push_instr("function intrinsic:store_word_unaligned".to_string());
            */
            } else {
                self.push_instr(format!("scoreboard players operation %ptr reg = {} reg", reg));
                self.push_instr(format!("scoreboard players operation %param0%0 reg = {} reg", dreg));
                self.push_instr("function intrinsic:store_word_unaligned".to_string());
            }
        } else {
            todo!()
        }
    }

    pub fn load_i32(&mut self, memarg: MemoryImmediate, memory_list: &MemoryList) {
        if memarg.offset % 4 == 0 {
            self.pop_i32_into(0);
            let reg = self.regname_i32(0);
            self.push_instr(format!("scoreboard players add {} reg {}", reg, memarg.offset));

            let setptr = [
                "scoreboard players operation %%ptr reg = %work%0%lo reg",
                "scoreboard players operation %%ptr reg /= %%4 reg",
                "scoreboard players operation %z reg = %%ptr reg",
                "scoreboard players operation %z reg %= %%SIXTEEN reg",
                "scoreboard players operation %%ptr reg /= %%SIXTEEN reg",
                "scoreboard players operation %y reg = %%ptr reg",
                "scoreboard players operation %y reg %= %%SIXTEEN reg",
                "scoreboard players operation %%ptr reg /= %%SIXTEEN reg",
                "scoreboard players operation %x reg = %%ptr reg",
                "execute as @e[tag=memoryptr] store result entity @s Pos[0] double 1 run scoreboard players get %x reg",
                "execute as @e[tag=memoryptr] store result entity @s Pos[1] double 1 run scoreboard players get %y reg",
                "execute as @e[tag=memoryptr] store result entity @s Pos[2] double 1 run scoreboard players get %z reg",
                "execute as @e[tag=memoryptr] at @e[tag=memoryptr] run tp @s ~ ~ ~8",
            ];

            for s in std::array::IntoIter::new(setptr) {
                self.push_instr(s.to_string())
            }

            /*
            let off = memory_list.get_offset(memarg);
            self.push_instr(format!("execute as @e[tag=memoryptr] run tp @s {}", off));

            let reg = self.regname_i32(0);
            */
            self.push_instr(format!("execute at @e[tag=memoryptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg));

            self.push_i32_from(0);
        } else {
            todo!()
        }
    }

    pub fn local_set(&mut self, local_index: u32) {
        self.set_local_ptr(local_index, true);

        self.pop_half_into(0);

        let reg = self.regname_lo(0);
        self.push_instr(format!("execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg));

        self.set_local_ptr(local_index, false);

        self.pop_half_into(0);

        let reg = self.regname_lo(0);
        self.push_instr(format!("execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg));
    }

    pub fn static_call(&mut self, this_func_index: u32, function_index: u32, types: &TypeList, function_list: &FunctionList) {
        let entry = function_list.get_entry_point(function_index);

        let return_name = function_list.get_block(this_func_index, self.basic_blocks.len() as u32);

        if USE_GRID_CALL {
            let ty = function_list.get_function_type(function_index, types);
            for (i, param) in ty.params.iter().enumerate() {
                self.pop_value_into(2 + i as u32);
            }

            // Push return address
            self.push_instr(format!("#   Push return address {}", return_name));
            self.push_instr(format!("execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value !BLOCKIDX!{}!", return_name));
            self.incr_stack_ptr();

            // Jump to function
            self.push_instr(format!("#   Jump to {}", entry));
            self.push_instr(format!("setblock !BLOCKPOS!{}! minecraft:redstone_block destroy", entry));
        } else {
            todo!()
        }
    }

    pub fn visit_operator(&mut self, o: Operator, local_count: u32, func_idx: u32, types: &TypeList, function_list: &FunctionList, memory_list: &MemoryList, global_list: &GlobalList, branches: Option<&(Label, Option<Label>)>) {
        if self.basic_blocks.is_empty() {
            self.basic_blocks.push(BasicBlock::new(0));
        }

        self.push_instr(format!("# {:?}", o));

        use Operator::*;
        match o {
            Call { function_index } => { 
                self.push_instr(format!("#   wasm:{}", function_list.get_entry_point(function_index)));

                self.static_call(func_idx, function_index, types, function_list);

                self.basic_blocks.push(BasicBlock::new(self.basic_blocks.len()));

                let f = function_list.get_function_type(function_index, types);
                assert!(f.returns.len() <= 1);

                if f.returns.len() == 1 {
                    self.push_instr(r#"tellraw @a [{"text":"return val is "},{"score":{"name":"%work%1%lo","objective":"reg"}}]"#.to_string());
                    self.push_i32_from(1);
                }
            }
            CallIndirect { index, table_index } => {
                let ty = &types.types[index as usize];

                todo!("TODO: Dynamic call {:?} {}", ty, table_index);

                //self.basic_blocks.push(BasicBlock::new(self.basic_blocks.len()));
            }
            Return => {
                self.push_instr("#   Pop frame".to_string());
                self.pop_frame(local_count);

                // Save return value so we can get to the 
                // return address
                let f = function_list.get_function_type(func_idx, types);
                self.push_instr("#   Save return value".to_string());
                assert!(f.returns.len() <= 1);

                if f.returns.len() == 1 {
                    self.pop_i32_into(1);
                }

                self.push_instr("#   Pop return address".to_string());
                // Pop return address
                self.pop_i32_into(0);
                self.push_instr("function wasm:dyn_branch".to_string());

                self.basic_blocks.push(BasicBlock::new(self.basic_blocks.len()));
            }

            GlobalGet { global_index } => { 
                self.set_global_ptr(global_index, false, global_list);

                let reg = self.regname_lo(0);
                self.push_instr(format!("execute at @e[tag=globalptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg));

                self.push_half_from(0);

                self.set_global_ptr(global_index, true, global_list);

                let reg = self.regname_lo(0);
                self.push_instr(format!("execute at @e[tag=globalptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg));

                self.push_half_from(0);
            }
            GlobalSet { global_index } => {
                self.set_global_ptr(global_index, true, global_list);

                self.pop_half_into(0);

                let reg = self.regname_lo(0);
                self.push_instr(format!("execute at @e[tag=globalptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg));


                self.set_global_ptr(global_index, false, global_list);

                self.pop_half_into(0);

                let reg = self.regname_lo(0);
                self.push_instr(format!("execute at @e[tag=globalptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg));

            }

            LocalSet { local_index } => {
                self.local_set(local_index);
            }
            LocalGet { local_index } => {
                self.set_local_ptr(local_index, false);

                let reg = self.regname_lo(0);
                self.push_instr(format!("execute at @e[tag=stackptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg));

                self.push_half_from(0);

                self.set_local_ptr(local_index, true);

                let reg = self.regname_lo(0);
                self.push_instr(format!("execute at @e[tag=stackptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg));

                self.push_half_from(0);
            }

            I64Const { value } => {
                self.push_i64_const(value);
            }
            I32Const { value } => {
                self.push_i32_const(value);
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
                    let next = function_list.get_block(func_idx, self.basic_blocks.len() as u32);
                    self.push_instr(format!("#   Jump to {}", next));
                    self.push_instr(format!("setblock !BLOCKPOS!{}! minecraft:redstone_block destroy", next));

                    self.basic_blocks.push(BasicBlock::new(self.basic_blocks.len()));

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
                self.pop_value_into(0);
                self.push_value_from(0);
                self.push_value_from(0);
                self.visit_operator(Operator::LocalSet { local_index }, local_count, func_idx, types, function_list, memory_list, global_list, branches)
            }
            Drop => {
                self.decr_stack_ptr();
            }

            Block { ty } => {
                if ty != TypeOrFuncType::Type(Type::EmptyBlockType) {
                    todo!("Block {:?}", ty);
                }

                self.depth += 1;
            }

            I32Eqz => {
                self.pop_i32_into(0);
                let valreg = self.regname_i32(0); 
                let condreg = self.regname_i32(1);
                self.push_instr(format!("scoreboard players set {} reg 0", condreg));
                self.push_instr(format!("execute if score {} reg matches 0..0 run scoreboard players set {} reg 1", valreg, condreg));
                self.push_i32_from(1);
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
                self.pop_value_into(0);
                let lo_reg = self.regname_i32(0);
                let hi_reg = self.regname_hi(0);
                self.push_instr(format!("execute if score {} reg matches ..-1 run scoreboard players set {} -1", lo_reg, hi_reg));
                self.push_instr(format!("execute if score {} reg matches 0.. run scoreboard players set {} 0", lo_reg, hi_reg));
                self.push_value_from(0);
            }

            Loop { ty } => {
                self.basic_blocks.push(BasicBlock::new(self.basic_blocks.len()));

                println!("TODO: Loop {:?}", ty);

                self.depth += 1;
            }
            BrIf { .. } => {
                // FIXME: CONDITION STACK
                self.pop_value_into(0);
                let r = self.regname_i32(0);

                let (t_target, f_target) = branches.unwrap();
                let f_target = f_target.as_ref().unwrap();

                let t_name = function_list.get_block(func_idx, t_target.0 as u32);
                let f_name = function_list.get_block(func_idx, f_target.0 as u32);

                self.push_instr(format!("execute unless score {} reg matches 0..0 run function wasm:{}", r, t_name));
                self.push_instr(format!("execute if score {} reg matches 0..0 run function wasm:{}", r, f_name));

                self.basic_blocks.push(BasicBlock::new(self.basic_blocks.len()));
            }
            Nop => {}

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

    pub fn get_function_type<'a>(&self, function: u32, types: &'a TypeList) -> &'a FuncType {
        let ty = self.functions[function as usize];
        if let TypeDef::Func(f) = &types.types[ty as usize] {
            f
        } else {
            unreachable!()
        }
    }

    pub fn get_entry_point(&self, function: u32) -> String {
        self.get_block(function, 0)
    }

    pub fn get_block(&self, function: u32, block: u32) -> String {
        format!("__wasm{}_{}", function, block)
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
                    if limits.maximum.is_none() {
                        todo!()
                    }

                    if limits.maximum.unwrap() != limits.initial {
                        todo!()
                    }

                    for _ in 0..limits.initial {
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

struct GlobalList {
    globals: Vec<GlobalType>,
}

impl GlobalList {
    pub fn new() -> Self {
        GlobalList {
            globals: Vec::new(),
        }
    }

    pub fn add_global(&mut self, global: Global) {
        self.globals.push(global.ty);
    }

    pub fn get_offset(&self, idx: u32, is_hi: bool) -> String {
        let z = if is_hi { 4 } else { 3 };
        format!("{} 0 {}", idx * 2, z)
    }

    pub fn init(&self) -> Vec<String> {
        let mut cmds = Vec::new();

        cmds.push(format!("fill 0 0 3 {} 0 4 air replace", self.globals.len()));
        cmds.push(format!("fill 0 0 3 {} 0 4 minecraft:jukebox{{RecordItem:{{id:\"minecraft:stone\",Count:1b,tag:{{Memory:1}}}}}} replace", self.globals.len()));

        cmds
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
}

fn main() {
    let mut types = TypeList::new();
    let mut functions = FunctionList::new(); 
    let mut globals = GlobalList::new();
    let mut memory = MemoryList::new();
    let mut exports = ExportList::new();

    memory.add_memory(MemoryType::M32 {
        limits: wasmparser::ResizableLimits {
            initial: 1,
            maximum: Some(1),
        },
        shared: false,
    });

    let mut codes = Vec::new();

    let mut func_idx = 0;

    let mut mc_functions = Vec::new();

    let file = std::fs::read("../example.wasm").unwrap();
    for payload in Parser::new(0).parse_all(&file) {
        let payload = payload.unwrap();
        match payload {
            Payload::Version { .. } => {
                println!("========= Module");
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
                let func_sec_offset = f.original_position();

                for func in f {
                    let func = func.unwrap();

                    functions.add_function(func);
                }
            }
            Payload::CodeSectionEntry(e) => {
                codes.push(e);
            }
            Payload::TableSection(t) => {
                for table in t {
                    let table = table.unwrap();
                    dbg!(table);
                }
            }
            _other => {
                println!("{:?}", _other);
            }
        }
    }

    for e in codes {
        println!("Doing function {}", func_idx);

        let operators = e.get_operators_reader().unwrap()
            .into_iter()
            .map(|o| o.unwrap())
            .collect::<Vec<_>>();
        
        let split_bb_info = split_bbs(&operators);

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

        for bb in s.basic_blocks.iter_mut() {
            let name = functions.get_block(func_idx, bb.label.0 as u32);

            bb.instrs.insert(0, r#"tellraw @a [{"text":"stackptr is "},{"score":{"name":"%stackptr","objective":"wasm"}}]"#.to_string());

            if USE_GRID_CALL {
                bb.instrs.insert(0, "setblock ~ ~1 ~ minecraft:air".to_string());
            }

            let mut contents = String::new();
            for i in bb.instrs.iter() {
                contents.push_str(i);
                contents.push('\n');
            }

            mc_functions.push((name, contents));
        }

        func_idx += 1;
    }

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

    let datapack = datapack::Datapack::new();

    let folder_path = std::path::Path::new("../out/");

    let mut setup = "\
    # Set up scoreboard\n\
    scoreboard objectives remove wasm\n\
    scoreboard objectives add wasm dummy\n\
    scoreboard objectives setdisplay sidebar wasm\n\
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
    scoreboard players set %%SIXTEEN reg 16
    ".to_string();

    setup.push_str("\n# Make stack\n");
    setup.push_str("fill 0 0 0 30 0 0 minecraft:air replace\n");
    setup.push_str("fill 0 0 0 30 0 0 minecraft:jukebox{RecordItem:{id:\"minecraft:stone\",Count:1b,tag:{Memory:1}}} replace\n");

    setup.push_str("\n# Make memory\n");
    for mem_cmd in memory.init() {
        setup.push_str(&mem_cmd);
        setup.push('\n');
    }

    setup.push_str("\n# Make globals\n");
    for global_cmd in globals.init() {
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

    let mut dyn_branch = String::new();
    for (idx, (name, _)) in mc_functions.iter().enumerate() {
        // FIXME: Reg0
        let b = format!("# wasm:{}\nexecute if score %work%0%lo reg matches {}..{} run setblock {} 1 -1 minecraft:redstone_block destroy\n", name, idx, idx, idx);
        dyn_branch.push_str(&b);
    }

    let e = format!("execute unless score %work%0%lo reg matches 0..{} run ", mc_functions.len() - 1);
    dyn_branch.push_str(&e);
    dyn_branch.push_str(r#"tellraw @a [{"text":"Attempt to branch to invalid function "},{"score":{"name":"%work%0%lo","objective":"reg"}}]"#);

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

    datapack.write_function(folder_path, "wasm", "setup", &setup).unwrap();

    datapack.write_function(folder_path, "wasm", "dyn_branch", &dyn_branch).unwrap();

    for export in exports.exports.iter() {
        if let ExternalKind::Function = export.kind {
            // FIXME: Pos
            let name = functions.get_entry_point(export.index);
            let idx = mc_functions.iter().enumerate().find(|(_, (n, _))| {
                n == &name
            }).unwrap().0;

            let mut f = FuncBodyStream::new();
            f.basic_blocks.push(BasicBlock::new(0));
            f.push_i32_const(-1);
            if export.field == "main" {
                f.push_i32_const(0);
                f.push_i32_const(0);
            }
            f.push_instr(format!("setblock {} 1 -1 minecraft:redstone_block destroy", idx));

            let mut content = String::new();

            assert_eq!(f.basic_blocks.len(), 1);

            for i in f.basic_blocks[0].instrs.iter() {
                content.push_str(i);
                content.push('\n');
            }

            datapack.write_function(folder_path, "wasm", export.field, &content).unwrap();
        }
    }
}
