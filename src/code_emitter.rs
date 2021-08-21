use std::collections::BTreeSet;

use crate::{BRANCH_CONV, BranchConv, BranchTarget, CodeFuncIdx, HalfRegister, INTRINSIC_COUNTS, Label, OpStack, RealOpStack, Register, StateInfo, get_block_name, get_entry_point, get_intrinsic_counts, get_stack_states, get_tables};
use crate::wasm::WasmFile;
use crate::mir::{FuncBodyStream, Instr, MirBasicBlock, RegOrConst, Relation, Terminator};
use wasmparser::{ExternalKind, Type, TypeOrFuncType};

pub struct ConstantPool(BTreeSet<i32>);

impl ConstantPool {
    pub fn new() -> Self {
        ConstantPool(BTreeSet::new())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn init_cmds(mut self) -> Vec<String> {
        self.0.insert(-1);
        self.0.insert(0);
        for i in 0..32 {
            self.0.insert(1 << i);
        }

        let mut result = vec!["scoreboard players set %%SIXTEEN reg 16".to_string()];

        for value in self.0 {
            result.push(format!("scoreboard players set %%{} reg {}", value, value));
        }

        result
    }
}


pub struct CodeEmitter<'a> {
    pub body: Vec<String>,

    op_stack: RealOpStack,

    insert_sync: bool,

    bb_idx: Option<usize>,

    label: Option<Label>,

    sync_instr_idx: usize,

    state_info: Option<&'a StateInfo>,

    constant_pool: &'a mut ConstantPool,
}

impl<'a> CodeEmitter<'a> {
    pub fn new(bb_idx: Option<usize>, use_virt_stack: bool, insert_sync: bool, state_info: Option<&'a StateInfo>, label: Option<Label>, constant_pool: &'a mut ConstantPool) -> Self {
        if bb_idx.is_none() {
            assert!(!use_virt_stack);
            assert!(!insert_sync);
        }

        let mut op_stack;
        if let Some(state_info) = state_info {
            op_stack = state_info.entry.clone();
            // FIXME:
            op_stack.use_virtual = use_virt_stack;
        } else {
            op_stack = RealOpStack::new(use_virt_stack);
        }

        let mut emitter = CodeEmitter {
            body: Vec::new(),
            op_stack,
            bb_idx,
            sync_instr_idx: 0,
            insert_sync,
            state_info,
            label,
            constant_pool
        };
        
        emitter.emit_sync();
        
        emitter
    }

    pub fn finalize(mut self) -> Vec<String> {
        if self.insert_sync {
            self.body.pop();
        }

        if BRANCH_CONV == BranchConv::Direct {
            self.body.push("scoreboard players set %%taken wasm 1".to_string());
        }

        self.body
    }

    pub fn emit_all(basic_block: &MirBasicBlock, bb_idx: Option<usize>, use_virt_stack: bool, insert_sync: bool, state_info: Option<&'a StateInfo>, constant_pool: &'a mut ConstantPool) -> Vec<String> {
        println!("\nSTARTING {:?}", basic_block.label);
        println!("{:?}", state_info);

        // The real stack has 1 entry: the return address
        let mut emitter = Self::new(bb_idx, use_virt_stack, insert_sync, state_info, Some(basic_block.label.clone()), constant_pool);
        for i in basic_block.instrs.iter() {
            emitter.emit(i);
        }
        emitter.emit_terminator(&basic_block.terminator);
        emitter.finalize()
    }

    fn emit_sync(&mut self) {
        if self.insert_sync {
            let bb_idx = self.bb_idx.unwrap();
            self.body.push(format!("# !INTERPRETER: SYNC {} {}", bb_idx, self.sync_instr_idx));
        }

        self.sync_instr_idx += 1;
    }

    pub fn emit_drop(&mut self, count: usize) {
        // TODO: Optimize
        for _ in 0..count {
            let was_real = self.op_stack.pop_value().1;

            if was_real {
                Self::decr_stack_ptr(&mut self.body);
            }
        }
    }

    pub fn emit(&mut self, instr: &Instr) {
        //println!("Emitting {:?}", instr);
        
        self.body.push(format!("# {:?}", instr));

        self.emit_inner(instr);

        self.emit_sync();
    }

    pub fn emit_terminator(&mut self, term: &Terminator) {
        match term {
            Terminator::Branch(target) => {
                self.jump_begin();
                self.branch(target);
                self.jump_end();
            }
            Terminator::Call { func_idx, return_addr: _ } => {
                let target = BranchTarget {
                    label: Label::new(*func_idx, 0),
                    to_pop: 0,
                    ty: Box::new([]),
                };

                self.jump_begin();
                self.branch(&target);
                self.jump_end();
            }
            &Terminator::DynBranch(reg, table_idx) => {
                // TODO: Maybe use a real register for this
                assert_eq!(reg, Register::Temp(0));

                self.emit_stack_save();

                if let Some(i) = table_idx {
                    self.body.push(format!("function wasm:dyn_branch{}", i));
                } else {
                    self.body.push("function wasm:dyn_branch".to_string());
                }
            }
            Terminator::BranchIf { t_name, f_name, cond } => {
                self.branch_begin(cond.as_lo());
                self.branch_arm("execute if score %%taken wasm matches 0 unless score %%tempcond reg matches 0..0 run ", Some(t_name));
                self.branch_arm("execute if score %%taken wasm matches 0 if score %%tempcond reg matches 0..0 run ", Some(f_name));
                self.branch_end();
            }
            Terminator::BranchTable { reg, targets, default } => {
                let targets = targets.iter().map(Some);

                self.lower_branch_table(*reg, targets, default.as_ref());
            }
            Terminator::Halt => {}
            t => todo!("{:?}", t)
        }

        self.emit_sync();
    }

    pub fn emit_inner(&mut self, instr: &Instr) {
        let blocks = [
            "minecraft:air",
            "minecraft:cobblestone",
            "minecraft:granite",
            "minecraft:andesite",
            "minecraft:diorite",
            "minecraft:lapis_block",
            "minecraft:iron_block",
            "minecraft:gold_block",
            "minecraft:diamond_block",
            "minecraft:redstone_block",
        ];

        use Instr::*;
        match instr {
            Comment(s) => {
                self.body.push(format!("# {}", s));
            }
            Tellraw(s) => {
                self.body.push(format!("tellraw @a {}", s));
            }

            SetConst(reg, v) => {
                self.body.push(format!("scoreboard players set {} reg {}", reg.get(), v));
            }

            SetTurtleCoord(reg, axis) => {
                self.body.push(format!("execute as @e[tag=turtle] store result entity @s Pos[{}] double 1 run scoreboard players get {} reg", *axis as u32, reg.get_lo()));
            }
            SetTurtleBlock(reg) => {
                for (idx, block) in blocks.iter().enumerate() {
                    self.body.push(format!("execute at @e[tag=turtle] if score {} reg matches {}..{} run setblock ~ ~ ~ {} destroy\n", reg.get_lo(), idx, idx, block));
                }

                let mut s = format!("execute unless score {} reg matches 0..{} run ", reg.get_lo(), blocks.len() - 1);
                s.push_str(r#"tellraw @a [{"text":"Attempt to set invalid block"},{"score":{"name":""#);
                s.push_str(&reg.get_lo());
                s.push_str(r#"","objective":"reg"}}]"#);
                self.body.push(s);
            }
            TurtleGet(reg) => {
                self.body.push(format!("scoreboard players set {} reg -1", reg.as_lo()));
                for (i, b) in blocks.iter().enumerate() {
                    self.body.push(format!("execute at @e[tag=turtle] if block ~ ~ ~ {} run scoreboard players set {} reg {}", b, reg.get_lo(), i));
                }
            }

            Call(i) => {
                self.emit_stack_save();

                let name = get_entry_point(*i);
                self.body.push(format!("function wasm:{}", name));
            }
            DynCall(_, _) => todo!(),

            Copy { dst, src } => {
                if dst != src {
                    self.body.push(format!("scoreboard players operation {} reg = {} reg", dst, src));
                }
            }

            SetMemPtr(val) => {
                match val {
                    RegOrConst::Reg(reg) => {
                        self.body.push(format!("scoreboard players operation %ptr reg = {} reg", reg));
                        self.body.push("function intrinsic:setptr".to_string());
                    }
                    RegOrConst::Const(c) => {
                        self.body.push(format!("scoreboard players set %ptr reg {}", *c));

                        let z = (*c / 4) % 8 + 8;
                        let y = (*c / (4 * 8)) % 256;
                        let x = (*c / (4 * 8 * 256)) % 8;
                        self.body.push(format!("execute as @e[tag=memoryptr] run tp @s {} {} {}", x, y, z));
                    }
                } 
            }
            &PopI32Into(reg) => {           
                self.emit_pop_i32_into(reg);
            }
            &PopI64Into(reg) => {
                self.emit_pop_i64_into(reg);
            }

            LoadLocalI64(reg) => {
                self.body.push(format!("execute at @e[tag=localptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg.get_lo()));
                self.body.push(format!("execute at @e[tag=localptr] store result score {} reg run data get block ~ ~ ~1 RecordItem.tag.Memory 1", reg.get_hi()));
            }
            StoreLocalI64(reg) => {
                self.body.push(format!("execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_lo()));
                self.body.push(format!("execute at @e[tag=localptr] store result block ~ ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_hi()));
            }
            LoadLocalI32(reg) => {
                self.body.push(format!("execute at @e[tag=localptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg.get_lo()));
            }
            StoreLocalI32(reg) => {
                self.body.push(format!("execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_lo()));
            }


            &SetLocalPtr(local_index) => {
                self.body.push(format!("execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~{} 0 1", -(local_index as i32) - 1 ));
            }

            &AddI32Const(reg, value) => {
                use std::cmp::Ordering::*;
                match value.cmp(&0) {
                    Greater => self.body.push(format!("scoreboard players add {} reg {}", reg, value)),
                    Less => self.body.push(format!("scoreboard players remove {} reg {}", reg, -value)),
                    Equal => {},
                }
            }
            &I64ExtendI32S { dst, src } => {
                if dst != src {
                    self.body.push(format!("scoreboard players operation {} reg = {} reg", dst.get_lo(), src.get_lo()));
                }
                self.body.push(format!("execute if score {} reg matches ..-1 run scoreboard players set {} reg -1", src.get_lo(), dst.get_hi()));
                self.body.push(format!("execute if score {} reg matches 0.. run scoreboard players set {} reg 0", src.get_lo(), dst.get_hi()));
            }
            I64ExtendI32U(reg) => {
                self.body.push(format!("scoreboard players set {} reg 0", reg.get_hi()));
            }

            I32MulTo64 { dst, lhs, rhs } => {
                self.body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs));
                self.body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs));
                self.body.push("function intrinsic:mul_32_to_64".to_string());
                self.body.push(format!("scoreboard players operation {} reg = %return%0 reg", dst.as_lo()));
                self.body.push(format!("scoreboard players operation {} reg = %return%1 reg", dst.as_hi()));
            }

            I64Rotl { dst, lhs, rhs } => {
                self.body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs.as_lo()));
                self.body.push(format!("scoreboard players operation %param0%1 reg = {} reg", lhs.as_hi()));

                self.body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs.as_lo()));
                self.body.push("scoreboard players operation %param1%0 reg %= %%64 reg".to_string());

                self.body.push("function intrinsic:rotl_64".to_string());

                self.body.push(format!("scoreboard players operation {} reg = %param0%0 reg", dst.as_lo()));
                self.body.push(format!("scoreboard players operation {} reg = %param0%1 reg", dst.as_hi()));
            }
            I64Rotr { dst, lhs, rhs } => {
                self.body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs.as_lo()));
                self.body.push(format!("scoreboard players operation %param0%1 reg = {} reg", lhs.as_hi()));

                self.body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs.as_lo()));
                self.body.push("scoreboard players operation %param1%0 reg %= %%64 reg".to_string());

                self.body.push("function intrinsic:rotr_64".to_string());

                self.body.push(format!("scoreboard players operation {} reg = %param0%0 reg", dst.as_lo()));
                self.body.push(format!("scoreboard players operation {} reg = %param0%1 reg", dst.as_hi()));
            }

            I64Shl { dst, lhs, rhs } => {
                self.body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs.as_lo()));
                self.body.push(format!("scoreboard players operation %param0%1 reg = {} reg", lhs.as_hi()));

                self.body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs.as_lo()));
                self.body.push("scoreboard players operation %param1%0 reg %= %%64 reg".to_string());

                self.body.push("function intrinsic:shl_64".to_string());

                self.body.push(format!("scoreboard players operation {} reg = %param0%0 reg", dst.as_lo()));
                self.body.push(format!("scoreboard players operation {} reg = %param0%1 reg", dst.as_hi()));
            }
            I64ShrU { dst, lhs, rhs } => {
                self.body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs.as_lo()));
                self.body.push(format!("scoreboard players operation %param0%1 reg = {} reg", lhs.as_hi()));

                self.body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs.as_lo()));
                self.body.push("scoreboard players operation %param1%0 reg %= %%64 reg".to_string());

                self.body.push("function intrinsic:lshr_i64".to_string());

                self.body.push(format!("scoreboard players operation {} reg = %param0%0 reg", dst.as_lo()));
                self.body.push(format!("scoreboard players operation {} reg = %param0%1 reg", dst.as_hi()));
            }
            I64ShrS { dst, lhs, rhs } => {
                self.body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs.as_lo()));
                self.body.push(format!("scoreboard players operation %param0%1 reg = {} reg", lhs.as_hi()));

                self.body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs.as_lo()));
                self.body.push("scoreboard players operation %param1%0 reg %= %%64 reg".to_string());

                self.body.push("function intrinsic:ashr_i64".to_string());

                self.body.push(format!("scoreboard players operation {} reg = %param0%0 reg", dst.as_lo()));
                self.body.push(format!("scoreboard players operation {} reg = %param0%1 reg", dst.as_hi()));
            }
            &I64DivS { dst, lhs, rhs } => {
                assert_eq!(dst, Register::Return(0));
                assert_eq!(lhs, Register::Param(0));
                assert_eq!(rhs, Register::Param(1));

                self.body.push("function intrinsic:i64_sdiv".to_string());
            }
            &I64DivU { dst, lhs, rhs } => {
                assert_eq!(dst, Register::Return(0));
                assert_eq!(lhs, Register::Param(0));
                assert_eq!(rhs, Register::Param(1));

                self.body.push("function intrinsic:i64_udiv".to_string());
            }
            &I64RemS { dst, lhs, rhs } => {
                assert_eq!(dst, Register::Return(0));
                assert_eq!(lhs, Register::Param(0));
                assert_eq!(rhs, Register::Param(1));

                self.body.push("function intrinsic:i64_srem".to_string());
            }
            &I64RemU { dst, lhs, rhs } => {
                assert_eq!(dst, Register::Return(0));
                assert_eq!(lhs, Register::Param(0));
                assert_eq!(rhs, Register::Param(1));

                self.body.push("function intrinsic:i64_urem".to_string());
            }

            I64Add { dst, lhs, rhs } => {
                let carry = Register::Temp(10).as_lo();

                assert_ne!(*dst, carry.0);
                assert_ne!(*lhs, carry.0);
                assert_ne!(*rhs, carry.0);
                assert_ne!(dst, lhs);
                assert_ne!(dst, rhs);
                assert_ne!(lhs, rhs);

                self.body.push(format!("scoreboard players operation {} reg = {} reg", dst.get_lo(), lhs.get_lo()));
                self.body.push(format!("scoreboard players operation {} reg = {} reg", dst.get_hi(), lhs.get_hi()));

                self.body.push(format!("scoreboard players operation {} reg += {} reg", dst.get_lo(), rhs.get_lo()));
                self.body.push(format!("scoreboard players operation {} reg += {} reg", dst.get_hi(), rhs.get_hi()));

                /*
                    Carrying:

                    if lhs < 0 && rhs < 0 {
                        true
                    } else if lhs < 0 && rhs >= 0 {
                        lhs + rhs >= 0
                    } else if lhs >= 0 && rhs < 0 {
                        lhs + rhs >= 0
                    } else {
                        false
                    }
                
                */

                self.body.push(format!("scoreboard players set {} reg 0", carry));
                self.body.push(format!("execute if score {} reg matches ..-1 if score {} reg matches ..-1 run scoreboard players set {} reg 1", lhs.get_lo(), rhs.get_lo(), carry));
                self.body.push(format!("execute if score {} reg matches ..-1 if score {} reg matches 0.. if score {} reg matches 0.. run scoreboard players set {} reg 1", lhs.get_lo(), rhs.get_lo(), dst.get_lo(), carry));
                self.body.push(format!("execute if score {} reg matches 0.. if score {} reg matches ..-1 if score {} reg matches 0.. run scoreboard players set {} reg 1", lhs.get_lo(), rhs.get_lo(), dst.get_lo(), carry));

                self.body.push(format!("scoreboard players operation {} reg += {} reg", dst.get_hi(), carry));
            }

            &I64Eq { dst, lhs, invert, rhs } => {
                let dst = dst.as_lo();

                self.body.push(format!("scoreboard players set {} reg 1", dst));
                self.body.push(format!("execute unless score {} reg = {} reg run scoreboard players set {} reg 0", lhs.as_lo(), rhs.as_lo(), dst));
                self.body.push(format!("execute unless score {} reg = {} reg run scoreboard players set {} reg 0", lhs.as_hi(), rhs.as_hi(), dst));

                if invert {
                    self.body.push(format!("execute store success score {} reg if score {} reg matches 0..0", dst, dst));
                }
            }
            &I64UComp { dst, mut lhs, mut op, mut rhs } => {
                /*
                    if lhs_hi ltu rhs_hi:
                        return true
                    if lhs_hi gtu rhs_hi:
                        return false
                    if lhs_hi == rhs_hi:
                        return x_lo ltu y_lo
                */

                if op == Relation::GreaterThan {
                    std::mem::swap(&mut lhs, &mut rhs);
                    op = Relation::LessThan;
                } else if op == Relation::GreaterThanEq {
                    std::mem::swap(&mut lhs, &mut rhs);
                    op = Relation::LessThanEq;
                }

                assert!(!matches!(lhs, Register::Temp(3 | 4 | 5 | 6)));
                assert!(!matches!(rhs, Register::Temp(3 | 4 | 5 | 6)));
                assert!(!matches!(dst, Register::Temp(3 | 4 | 5 | 6)));

                assert_ne!(dst, lhs);
                assert_ne!(dst, rhs);

                let hi_is_lesser = Register::Temp(3).as_lo();
                let hi_is_greater = Register::Temp(4).as_lo();
                let hi_is_equal = Register::Temp(5).as_lo();

                let lo_is_lesser = Register::Temp(6).as_lo();

                self.make_i32_op(lhs.as_hi(), "ltu", rhs.as_hi().into(), hi_is_lesser);
                self.make_i32_op(lhs.as_hi(), "gtu", rhs.as_hi().into(), hi_is_greater);
                self.make_i32_op(lhs.as_hi(), "==", rhs.as_hi().into(), hi_is_equal);
                self.make_i32_op(lhs.as_lo(), "ltu", rhs.as_lo().into(), lo_is_lesser);

                self.body.push(format!("execute if score {} reg matches 1.. run scoreboard players set {} reg 1", hi_is_lesser, dst.as_lo()));
                self.body.push(format!("execute if score {} reg matches 1.. run scoreboard players set {} reg 0", hi_is_greater, dst.as_lo()));
                self.body.push(format!("execute if score {} reg matches 1.. run scoreboard players operation {} reg = {} reg", hi_is_equal, dst.as_lo(), lo_is_lesser));

                if op == Relation::LessThanEq {
                    self.body.push(format!("execute if score {} reg = {} reg if score {} reg = {} reg run scoreboard players set {} reg 1", lhs.as_lo(), rhs.as_lo(), lhs.as_hi(), rhs.as_hi(), dst.as_lo()));
                } else {
                    assert_eq!(op, Relation::LessThan);
                }

            }
            &I64SComp { dst, mut lhs, mut op, mut rhs } => {
                /*
                    if lhs_hi ltu rhs_hi:
                        return true
                    if lhs_hi gtu rhs_hi:
                        return false
                    if lhs_hi == rhs_hi:
                        return x_lo ltu y_lo
                */

                if op == Relation::GreaterThan {
                    std::mem::swap(&mut lhs, &mut rhs);
                    op = Relation::LessThan;
                } else if op == Relation::GreaterThanEq {
                    std::mem::swap(&mut lhs, &mut rhs);
                    op = Relation::GreaterThan;
                }

                /*
                    As written out normally:

                    if lhs_hi < rhs_hi {
                        true
                    } else if lhs_hi > rhs_hi {
                        false
                    } else {
                        (lhs_lo as u32) < (rhs_lo as u32)
                    }
                */


                assert_ne!(dst, lhs);
                assert_ne!(dst, rhs);

                let op = if op == Relation::LessThan {
                    "ltu"
                } else {
                    "leu"
                };

                // dst = (lhs_lo as u32) < (rhs_lo as u32);
                self.make_i32_op(lhs.as_lo(), op, rhs.as_lo().into(), dst.as_lo());
                // if lhs_hi < rhs_hi { dst = true }
                self.body.push(format!("execute if score {} reg < {} reg run scoreboard players set {} reg 1", lhs.as_hi(), rhs.as_hi(), dst.as_lo()));
                // if lhs_hi > rhs_hi { dst = false }
                self.body.push(format!("execute if score {} reg > {} reg run scoreboard players set {} reg 0", lhs.as_hi(), rhs.as_hi(), dst.as_lo()));
            }

            I32Eqz { val, cond } => {
                self.body.push(format!("scoreboard players set {} reg 0", cond.get_lo()));
                self.body.push(format!("execute if score {} reg matches 0..0 run scoreboard players set {} reg 1", val.get_lo(), cond.get_lo()));
            }
            I64Eqz { val, cond } => {
                self.body.push(format!("scoreboard players set {} reg 1", cond.get_lo()));
                self.body.push(format!("execute unless score {} reg matches 0..0 run scoreboard players set {} reg 0", val.get_lo(), cond.get_lo()));
                self.body.push(format!("execute unless score {} reg matches 0..0 run scoreboard players set {} reg 0", val.get_hi(), cond.get_lo()));
            }
            &I32Op { dst, lhs, op, rhs } => {
                self.make_i32_op(lhs, op, rhs, dst);
            }
            I32Extend8S(reg) => {
                self.body.push(format!("scoreboard players operation {} reg %= %%256 reg", reg.as_lo()));
                self.body.push(format!("execute if score {} reg matches 128..255 run scoreboard players remove {} reg 256", reg.as_lo(), reg.as_lo()))
            }
            I32Extend16S(reg) => {
                self.body.push(format!("scoreboard players operation {} reg %= %%65536 reg", reg.as_lo()));
                self.body.push(format!("execute if score {} reg matches 32768..65535 run scoreboard players remove {} reg 65536", reg.as_lo(), reg.as_lo()))
            }
            I32Popcnt(reg) => {
                self.body.push(format!("scoreboard players operation %param0%0 reg = {} reg", reg));
                self.body.push("function intrinsic:popcnt".to_string());
                self.body.push(format!("scoreboard players operation {} reg = %return%0 reg", reg));
            }
            I32Ctz(reg) => {
                self.body.push(format!("scoreboard players operation %param0%0 reg = {} reg", reg.get_lo()));
                self.body.push("function intrinsic:ctz".to_string());
                self.body.push(format!("scoreboard players operation {} reg = %return%0 reg", reg.get_lo()));
            }
            I32Clz(reg) => {
                self.body.push(format!("scoreboard players operation %param0%0 reg = {} reg", reg.get_lo()));
                self.body.push("function intrinsic:clz".to_string());
                self.body.push(format!("scoreboard players operation {} reg = %return%0 reg", reg.get_lo()));
            }
            I64Clz { src, dst } => {
                assert_ne!(src, dst);

                self.body.push(format!("execute if score {} reg matches 0..0 run scoreboard players operation %param0%0 reg = {} reg", src.get_hi(), src.get_lo()));
                self.body.push(format!("execute unless score {} reg matches 0..0 run scoreboard players operation %param0%0 reg = {} reg", src.get_hi(), src.get_hi()));
                self.body.push("function intrinsic:clz".to_string());
                self.body.push(format!("scoreboard players operation {} reg = %return%0 reg", dst.get_lo()));
                self.body.push(format!("execute if score {} reg matches 0..0 run scoreboard players add {} reg 32", src.get_hi(), dst.get_lo()));
                self.body.push(format!("scoreboard players set {} reg 0", dst.get_hi()));
            }
            I64Ctz { src, dst } => {
                assert_ne!(src, dst);

                self.body.push(format!("execute if score {} reg matches 0..0 run scoreboard players operation %param0%0 reg = {} reg", src.get_lo(), src.get_hi()));
                self.body.push(format!("execute unless score {} reg matches 0..0 run scoreboard players operation %param0%0 reg = {} reg", src.get_lo(), src.get_lo()));
                self.body.push("function intrinsic:ctz".to_string());
                self.body.push(format!("scoreboard players operation {} reg = %return%0 reg", dst.get_lo()));
                self.body.push(format!("execute if score {} reg matches 0..0 run scoreboard players add {} reg 32", src.get_lo(), dst.get_lo()));
                self.body.push(format!("scoreboard players set {} reg 0", dst.get_hi()));
            }

            &PushI32From(reg) => {
                self.emit_push_i32_from(reg);
            }
            &PushI64From(reg) => {
                self.emit_push_i64_from(reg);
            }
            &PushI32Const(value) => {
                self.emit_push_i32_const(value);
            }
            &PushI64Const(value) => {
                self.emit_push_i64_const(value);
            }
            PushReturnAddress(label) => {
                let return_name = get_block_name(label);

                self.emit_stack_save();

                self.body.push(format!("execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value !BLOCKIDX!{}!", return_name));
                Self::incr_stack_ptr(&mut self.body);

                // TODO: ????
                //self.op_stack.push_i32();
            }

            &LoadI32(dst, _align) => {
                self.body.push("function intrinsic:load_word".to_string());
                self.body.push(format!("scoreboard players operation {} reg = %return%0 reg", dst.get_lo()));
            }
            LoadI32_8U(dst, _align) => {
                self.body.push("function intrinsic:load_byte".to_string());
                self.body.push(format!("scoreboard players operation {} reg = %param0%0 reg", dst.get_lo()));
                // TODO: Determine if load_byte actually returns a byte
                self.body.push(format!("scoreboard players operation {} reg = %param0%0 reg", dst.get_lo()));
            }
            LoadI32_8S(dst, _align) => {
                self.body.push("function intrinsic:load_byte".to_string());
                self.body.push(format!("scoreboard players operation {} reg = %param0%0 reg", dst.get_lo()));
                // TODO: Determine if load_byte actually returns a byte
                self.body.push(format!("execute if score {} reg matches 128..255 run scoreboard players add {} reg -256", dst.get_lo(), dst.get_lo()));
            }
            LoadI32_16U(dst, _align) => {
                // TODO:
                self.body.push("function intrinsic:load_halfword_unaligned".to_string());
                self.body.push(format!("scoreboard players operation {} reg = %return%0 reg", dst.get_lo()));
                // TODO: Determine if load_halfword actually returns a halfword
                self.body.push(format!("scoreboard players operation {} reg = %return%0 reg", dst.get_lo()));
            }
            LoadI32_16S(dst, _align) => {
                // TODO:
                self.body.push("function intrinsic:load_halfword_unaligned".to_string());
                self.body.push(format!("scoreboard players operation {} reg = %return%0 reg", dst.get_lo()));
                // TODO: Determine if load_halfword actually returns a halfword
                self.body.push(format!("execute if score {} reg matches 32768..65535 run scoreboard players add {} reg -65536", dst.get_lo(), dst.get_lo()));
            }
            &StoreI32(src, _align) => {
                self.body.push(format!("scoreboard players operation %param0%0 reg = {} reg", src.get_lo()));
                self.body.push("function intrinsic:store_word".to_string());
            }
            &StoreI32_8(src, _align) => {
                self.body.push(format!("scoreboard players operation %param2%0 reg = {} reg", src.get_lo()));
                self.body.push("function intrinsic:store_byte".to_string());
            }
            &StoreI32_16(src, _align) => {
                self.body.push(format!("scoreboard players operation %param0%0 reg = {} reg", src.get_lo()));
                // TODO:
                self.body.push("function intrinsic:store_halfword_unaligned".to_string());
            }

            StoreRow(src) => {
                for i in 0..8 {
                    self.body.push(format!("execute at @e[tag=memoryptr] store result block ~ ~ ~{} RecordItem.tag.Memory int 1 run scoreboard players get {} reg", i, src.get_lo()))
                }
            }

            &PushFrame(local_count) => {
                if local_count != 0 {
                    self.body.push(format!("# Push frame with {} locals", local_count));
                    self.body.push(format!("execute at @e[tag=frameptr] run fill ~ ~ ~ ~{} ~ ~1 minecraft:jukebox{{RecordItem:{{id:\"minecraft:stone\",Count:1b,tag:{{Memory:0}}}}}}", local_count - 1));
                    self.body.push(format!("execute as @e[tag=frameptr] at @e[tag=frameptr] run tp @s ~{} ~ ~", local_count));
                }
            }
            &PopFrame(local_count) => {
                if local_count != 0 {
                    self.body.push(format!("execute as @e[tag=frameptr] at @e[tag=frameptr] run tp @s ~-{} ~ ~", local_count));
                    self.body.push(format!("execute at @e[tag=frameptr] run fill ~ ~ ~ ~{} ~ ~1 minecraft:air", local_count - 1));
                }
            }

            Drop => {
                self.emit_drop(1);
            }
            SelectI32 { dst_reg, true_reg, false_reg, cond_reg } => {
                assert_ne!(dst_reg, false_reg);
                assert_ne!(dst_reg, cond_reg);

                self.body.push(format!("scoreboard players operation {} reg = {} reg", dst_reg.get_lo(), true_reg.get_lo()));
                //self.body.push(format!("scoreboard players operation {} reg = {} reg", dst_reg.get_hi(), true_reg.get_hi()));

                self.body.push(format!("execute if score {} reg matches 0..0 run scoreboard players operation {} reg = {} reg", cond_reg.get_lo(), dst_reg.get_lo(), false_reg.get_lo()));
                //self.body.push(format!("execute if score {} reg matches 0..0 run scoreboard players operation {} reg = {} reg", cond_reg.get_lo(), dst_reg.get_hi(), false_reg.get_hi()));
            }
            SelectI64 { dst_reg, true_reg, false_reg, cond_reg } => {
                assert_ne!(dst_reg, false_reg);
                assert_ne!(dst_reg, cond_reg);

                self.body.push(format!("scoreboard players operation {} reg = {} reg", dst_reg.get_lo(), true_reg.get_lo()));
                self.body.push(format!("scoreboard players operation {} reg = {} reg", dst_reg.get_hi(), true_reg.get_hi()));

                self.body.push(format!("execute if score {} reg matches 0..0 run scoreboard players operation {} reg = {} reg", cond_reg.get_lo(), dst_reg.as_lo(), false_reg.as_lo()));
                self.body.push(format!("execute if score {} reg matches 0..0 run scoreboard players operation {} reg = {} reg", cond_reg.get_lo(), dst_reg.as_hi(), false_reg.as_hi()));
            }

            Unreachable => {
                self.body.push("tellraw @a \"ENTERED UNREACHABLE CODE\"".to_string());
            }
        }
    }

    pub fn push_i32_from(reg: Register, body: &mut Vec<String>) {
        body.push(format!("execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_lo()));
        Self::incr_stack_ptr(body);
    }

    pub fn push_i64_from(reg: Register, body: &mut Vec<String>) {
        body.push(format!("execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_lo()));
        Self::incr_stack_ptr_half(body);

        body.push(format!("execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_hi()));
        Self::incr_stack_ptr_half(body);
    }

    pub fn pop_i32_into(reg: Register, body: &mut Vec<String>) {
        Self::decr_stack_ptr(body);
        body.push(format!("execute at @e[tag=stackptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg.get_lo()));
    }

    pub fn pop_i64_into(reg: Register, body: &mut Vec<String>) {
        Self::decr_stack_ptr_half(body);
        body.push(format!("execute at @e[tag=stackptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg.get_hi()));

        Self::decr_stack_ptr_half(body);
        body.push(format!("execute at @e[tag=stackptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg.get_lo()));
    }

    pub fn emit_push_i32_const(&mut self, value: i32) {
        if self.op_stack.use_virtual {
            let idx = self.op_stack.stack.0.len();

            self.op_stack.push_i32();

            let stack_reg = Register::Stack(idx as u32).as_lo();
            self.body.push(format!("scoreboard players set {} reg {}", stack_reg, value));
        } else {
            self.body.push(format!("execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value {}", value));
            Self::incr_stack_ptr(&mut self.body);

            self.op_stack.push_i32();
        }
    }

    pub fn emit_push_i64_const(&mut self, value: i64) {
        if self.op_stack.use_virtual {
            let idx = self.op_stack.stack.0.len();

            self.op_stack.push_i64();

            let stack_reg_lo = Register::Stack(idx as u32).as_lo();
            let stack_reg_hi = Register::Stack(idx as u32).as_hi();
            self.body.push(format!("scoreboard players set {} reg {}", stack_reg_lo, value as i32));
            self.body.push(format!("scoreboard players set {} reg {}", stack_reg_hi, (value >> 32) as i32));
        } else {
            Self::push_i64_const(value, &mut self.body);

            self.op_stack.push_i64();
        }
    }

    pub fn emit_push_from(&mut self, reg: Register, ty: Type) {
        match ty {
            Type::I32 => self.emit_push_i32_from(reg),
            Type::I64 => self.emit_push_i64_from(reg),
            _ => todo!("{:?}", ty)
        }
    }

    pub fn emit_push_i32_from(&mut self, reg: Register) {
        if self.op_stack.use_virtual {
            let idx = self.op_stack.stack.0.len();

            self.op_stack.push_i32();

            // This is just a register copy
            let stack_reg = Register::Stack(idx as u32).as_lo();
            self.body.push(format!("scoreboard players operation {} reg = {} reg", stack_reg, reg.as_lo()));
        } else {
            self.op_stack.push_i32();

            Self::push_i32_from(reg, &mut self.body);
        }
    }

    pub fn emit_push_i64_from(&mut self, reg: Register) {
        if self.op_stack.use_virtual {
            let idx = self.op_stack.stack.0.len();

            self.op_stack.push_i64();

            // This is just a register copy
            let stack_reg_lo = Register::Stack(idx as u32).as_lo();
            let stack_reg_hi = Register::Stack(idx as u32).as_hi();
            self.body.push(format!("scoreboard players operation {} reg = {} reg", stack_reg_lo, reg.as_lo()));
            self.body.push(format!("scoreboard players operation {} reg = {} reg", stack_reg_hi, reg.as_hi()));
        } else {
            self.op_stack.push_i64();

            Self::push_i64_from(reg, &mut self.body);
        }
    }

    pub fn emit_pop_into(&mut self, reg: Register, ty: Type) {
        match ty {
            Type::I32 => self.emit_pop_i32_into(reg),
            Type::I64 => self.emit_pop_i64_into(reg),
            _ => todo!("{:?}", ty),
        }
    }

    pub fn emit_pop_i32_into(&mut self, reg: Register) {
        let is_real = self.op_stack.pop_i32();

        let idx = self.op_stack.stack.0.len();

        if is_real {
            // We are popping a value off of the real stack
            Self::pop_i32_into(reg, &mut self.body);
        } else {
            // This is just a register copy
            let stack_reg = Register::Stack(idx as u32).as_lo();
            self.body.push(format!("scoreboard players operation {} reg = {} reg", reg.as_lo(), stack_reg));
        }
    }

    pub fn emit_pop_i64_into(&mut self, reg: Register) {
        let is_real = self.op_stack.pop_i64();

        let idx = self.op_stack.stack.0.len();

        if is_real {
            // We are popping a value off of the real stack
            Self::pop_i64_into(reg, &mut self.body);
        } else {
            // This is just a register copy
            let stack_reg_lo = Register::Stack(idx as u32).as_lo();
            let stack_reg_hi = Register::Stack(idx as u32).as_hi();
            self.body.push(format!("scoreboard players operation {} reg = {} reg", reg.as_lo(), stack_reg_lo));
            self.body.push(format!("scoreboard players operation {} reg = {} reg", reg.as_hi(), stack_reg_hi));
        }
    }

    pub fn emit_real_stack_resize(&mut self, target_size: usize) {
        use std::cmp::Ordering;

        match self.op_stack.real_size.cmp(&target_size) {
            Ordering::Less => self.emit_stack_save_to(target_size),
            Ordering::Equal => {}
            Ordering::Greater => todo!(),
        }
    }

    pub fn emit_stack_save_to(&mut self, target_size: usize) {
        assert!(self.op_stack.real_size <= target_size);
        for idx in self.op_stack.real_size..target_size {
            let reg = Register::Stack(idx as u32);
            let ty = self.op_stack.stack.0[idx];

            Self::lower_push_from(reg, ty, &mut self.body);
        }

        self.op_stack.real_size = target_size;
    }

    pub fn emit_stack_save(&mut self) {
        self.emit_stack_save_to(self.op_stack.stack.0.len());
    }

    pub fn incr_stack_ptr_half(body: &mut Vec<String>) {
        body.push("execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~".to_string());
        body.push("scoreboard players add %stackptr wasm 1".to_string());
    }

    pub fn incr_stack_ptr(body: &mut Vec<String>) {
        body.push("execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~2 ~ ~".to_string());
        body.push("scoreboard players add %stackptr wasm 2".to_string());
    }

    pub fn decr_stack_ptr_half(body: &mut Vec<String>) {
        body.push("execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~".to_string());
        body.push("scoreboard players remove %stackptr wasm 1".to_string());
    }

    pub fn decr_stack_ptr(body: &mut Vec<String>) {
        body.push("execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-2 ~ ~".to_string());
        body.push("scoreboard players remove %stackptr wasm 2".to_string());
    }

    pub fn make_i32_op(&mut self, lhs: HalfRegister, mut op: &str, mut rhs: RegOrConst, dst: HalfRegister) {
        if let RegOrConst::Const(c) = rhs {
            self.constant_pool.0.insert(c);
        }

        match op {
            "+=" | "-=" | "*=" | "%=" => {
                self.body.push(format!("scoreboard players operation {} reg {} {} reg", lhs, op, rhs));
                assert_eq!(lhs, dst);
            }
            "/=" => {
                assert_ne!(lhs, dst);
                if let RegOrConst::Reg(r) = rhs {
                    assert_ne!(r, dst);
                } else {
                    panic!()
                }

                // Minecraft division always rounds towards negative infinity, so we need to correct for that

                let rem = Register::Temp(21).as_lo();

                self.body.push(format!("scoreboard players operation {} reg = {} reg", rem, lhs));
                self.body.push(format!("scoreboard players operation {} reg %= {} reg ", rem, rhs));

                self.body.push(format!("scoreboard players operation {} reg = {} reg", dst, lhs));
                self.body.push(format!("scoreboard players operation {} reg /= {} reg", dst, rhs));

                self.body.push(format!("execute if score {} reg matches ..-1 if score {} reg matches 0.. unless score {} reg matches 0..0 run scoreboard players add {} reg 1", lhs, rhs, rem, dst));
                self.body.push(format!("execute if score {} reg matches ..-1 if score {} reg matches 0.. unless score {} reg matches 0..0 run scoreboard players add {} reg 1", rhs, lhs, rem, dst));
            }
            "/=u" => {
                let d1 = Register::Temp(30).as_lo();
                let r1 = Register::Temp(31).as_lo();
                let d2 = Register::Temp(32).as_lo();
                let r2 = Register::Temp(33).as_lo();
                let d3 = Register::Temp(34).as_lo();
                let is_gtu = Register::Temp(35).as_lo();
                let lhs_lo = Register::Temp(36).as_lo();

                assert_ne!(lhs, dst);
                if let RegOrConst::Reg(r) = rhs {
                    assert_ne!(r, dst);
                }

                // let mut dst = 0;
                self.body.push(format!("scoreboard players set {} reg 0", dst));

                // if lhs >= 0 && rhs >= 0 { dst = lhs / rhs }
                self.body.push(format!("execute if score {} reg matches 0.. if score {} reg matches 0.. run scoreboard players operation {} reg = {} reg", lhs, rhs, dst, lhs));
                self.body.push(format!("execute if score {} reg matches 0.. if score {} reg matches 0.. run scoreboard players operation {} reg /= {} reg", lhs, rhs, dst, rhs));

                // is_gtu = (lhs as u32) >= (rhs as u32)
                self.make_i32_op(lhs, "geu", rhs, is_gtu);
                // if lhs < 0 && rhs < 0 && is_gtu { dst = 1 }
                self.body.push(format!("execute if score {} reg matches ..-1 if score {} reg matches ..-1 if score {} reg matches 1..1 run scoreboard players set {} reg 1", lhs, rhs, is_gtu, dst));

                // lhs_lo = lhs & 0x7F
                self.body.push(format!("scoreboard players operation {} reg = {} reg", lhs_lo, lhs));
                self.body.push(format!("scoreboard players operation {} reg += %%-2147483648 reg", lhs_lo));

                // d1 = lhs_lo / rhs
                self.body.push(format!("scoreboard players operation {} reg = {} reg", d1, lhs_lo));
                self.body.push(format!("scoreboard players operation {} reg /= {} reg", d1, rhs));
                // r1 = lhs_lo % rhs
                self.body.push(format!("scoreboard players operation {} reg = {} reg", r1, lhs_lo));
                self.body.push(format!("scoreboard players operation {} reg %= {} reg", r1, rhs));

                // d2 = i32::MAX / rhs
                self.body.push(format!("scoreboard players set {} reg {}", d2, i32::MAX));
                self.body.push(format!("scoreboard players operation {} reg /= {} reg", d2, rhs));
                // r2 = i32::MAX % rhs
                self.body.push(format!("scoreboard players set {} reg {}", r2, i32::MAX));
                self.body.push(format!("scoreboard players operation {} reg %= {} reg", r2, rhs));

                // r1 += r2
                self.body.push(format!("scoreboard players operation {} reg += {} reg", r1, r2));
                // r1 += 1
                self.body.push(format!("scoreboard players add {} reg 1", r1));

                // d3 = r1 / rhs
                self.body.push(format!("scoreboard players operation {} reg = {} reg", d3, r1));
                self.body.push(format!("scoreboard players operation {} reg /= {} reg", d3, rhs));

                // d1 += d2
                self.body.push(format!("scoreboard players operation {} reg += {} reg", d1, d2));
                // d1 += d3
                self.body.push(format!("scoreboard players operation {} reg += {} reg", d1, d3));

                self.body.push(format!("execute if score {} reg matches ..-1 if score {} reg matches 0.. run scoreboard players operation {} reg = {} reg", lhs, rhs, dst, d1));
            }
            "remu" => {
                assert_ne!(lhs, dst);
                if let RegOrConst::Reg(r) = rhs {
                    assert_ne!(r, dst);
                }

                self.make_i32_op(lhs, "/=u", rhs, dst);

                // lhs - dst * rhs

                self.body.push(format!("scoreboard players operation {} reg *= {} reg", dst, rhs));
                self.body.push(format!("scoreboard players operation {} reg *= %%-1 reg", dst));
                self.body.push(format!("scoreboard players operation {} reg += {} reg", dst, lhs));
            }
            "rems" => {
                assert_ne!(lhs, dst);
                if let RegOrConst::Reg(r) = rhs {
                    assert_ne!(r, dst);
                }

                self.make_i32_op(lhs, "/=", rhs, dst);

                // lhs - dst * rhs

                self.body.push(format!("scoreboard players operation {} reg *= {} reg", dst, rhs));
                self.body.push(format!("scoreboard players operation {} reg *= %%-1 reg", dst));
                self.body.push(format!("scoreboard players operation {} reg += {} reg", dst, lhs));
            }

            "&=" => {
                self.body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs));
                self.body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs));
                self.body.push("function intrinsic:and".to_string());
                self.body.push(format!("scoreboard players operation {} reg = %return%0 reg", dst));
            }
            "|=" => {
                self.body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs));
                self.body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs));
                self.body.push("function intrinsic:or".to_string());
                self.body.push(format!("scoreboard players operation {} reg = %return%0 reg", dst));
            }
            "^=" => {
                self.body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs));
                self.body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs));
                self.body.push("function intrinsic:xor".to_string());
                self.body.push(format!("scoreboard players operation {} reg = %return%0 reg", dst));
            }
            "shl" => {
                match rhs {
                    RegOrConst::Reg(rhs) => {
                        self.body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs));
                        self.body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs));
                        self.body.push("scoreboard players operation %param1%0 reg %= %%32 reg".to_string());
                        self.body.push("function intrinsic:shl".to_string());
                        self.body.push(format!("scoreboard players operation {} reg = %param0%0 reg", dst));
                    }
                    RegOrConst::Const(mut rhs) => {
                        if dst != lhs {
                            self.body.push(format!("scoreboard players operation {} reg = {} reg", dst, lhs));
                        }

                        rhs %= 32;

                        if rhs != 0 {
                            self.body.push(format!("scoreboard players operation {} reg *= %%{} reg", lhs, 1 << rhs));
                        }
                    }
                }
            }
            "shru" => {
                self.body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs));
                self.body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs));
                self.body.push("scoreboard players operation %param1%0 reg %= %%32 reg".to_string());
                self.body.push("function intrinsic:lshr".to_string());
                self.body.push(format!("scoreboard players operation {} reg = %param0%0 reg", dst));
            }
            "shrs" => {
                match rhs {
                    RegOrConst::Reg(r) => {
                        assert_ne!(r, dst);

                        self.body.push(format!("scoreboard players operation {} reg %= %%32 reg", rhs));

                        if dst != lhs {
                            self.body.push(format!("scoreboard players operation {} reg = {} reg", dst, lhs));
                        }

                        for i in 1..31 {
                            self.body.push(format!("execute if score {} reg matches {}..{} run scoreboard players operation {} reg /= %%{} reg", rhs, i, i, dst, 1 << i))
                        }
                        self.body.push(format!("execute if score {} reg matches 31..31 run execute store success score {} reg if score {} reg matches ..-1", rhs, dst, dst));
                        self.body.push(format!("execute if score {} reg matches 31..31 run scoreboard players operation {} reg *= %%-1 reg", rhs, dst));
                    }
                    RegOrConst::Const(rhs) => {
                        let rhs = rhs % 32;

                        if dst != lhs {
                            self.body.push(format!("scoreboard players operation {} reg = {} reg", dst, lhs));
                        }

                        use std::cmp::Ordering;

                        match rhs.cmp(&31) {
                            Ordering::Less => {
                                self.body.push(format!("scoreboard players operation {} reg /= %%{} reg", dst, 1 << rhs));
                            }
                            Ordering::Equal => {
                                self.body.push(format!("execute store success score {} reg if score {} reg matches ..-1", dst, dst));
                                self.body.push(format!("scoreboard players operation {} reg *= %%-1 reg", dst));
                            }
                            Ordering::Greater => unreachable!(),
                        }
                    }
                }
            }
            "rotl" => {
                self.body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs));
                self.body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs));
                self.body.push("scoreboard players operation %param1%0 reg %= %%32 reg".to_string());
                self.body.push("function intrinsic:rotl".to_string());
                self.body.push(format!("scoreboard players operation {} reg = %param0%0 reg", dst));
            }
            "rotr" => {
                self.body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs));
                self.body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs));
                self.body.push("scoreboard players operation %param1%0 reg %= %%32 reg".to_string());
                self.body.push("function intrinsic:rotr".to_string());
                self.body.push(format!("scoreboard players operation {} reg = %param0%0 reg", dst));
            }

            "gts" | "ges" | "les" | "lts" | "==" => {
                let score_op = match op {
                    "gts" => ">",
                    "ges" => ">=",
                    "les" => "<=",
                    "lts" => "<",
                    "==" => "=",
                    "!=" => "<>",
                    _ => unreachable!(),
                };

                self.body.push(format!("execute store success score {} reg if score {} reg {} {} reg", dst, lhs, score_op, rhs));
            }
            "!=" => {
                self.body.push(format!("execute store success score {} reg unless score {} reg = {} reg", dst, lhs, rhs));
            }
            "geu" | "gtu" | "leu" | "ltu" => {
                let mut lhs = RegOrConst::Reg(lhs);

                if op == "geu" {
                    op = "leu";
                    std::mem::swap(&mut lhs, &mut rhs);
                } else if op == "gtu" {
                    op = "ltu";
                    std::mem::swap(&mut lhs, &mut rhs);
                }

                let score_op = match op {
                    "ltu" => "<",
                    "leu" => "<=",
                    _ => unreachable!(),
                };

                /*
                    dst = false
                    if lhs < 0 && rhs >= 0 { reg = false }
                    if lhs >= 0 && rhs < 0 { reg = true }
                    if lhs < 0 && rhs < 0 && lhs < rhs { reg = true }
                    if lhs >= 0 && rhs >= 0 && lhs < rhs { reg = true }
                */

                if let RegOrConst::Reg(l) = lhs {
                    assert_ne!(l, dst);
                }
                if let RegOrConst::Reg(r) = rhs {
                    assert_ne!(r, dst);
                }

                self.body.push(format!("scoreboard players set {} reg 0", dst));
                self.body.push(format!("execute if score {} reg matches ..-1 if score {} reg matches 0.. run scoreboard players set {} reg 0", lhs, rhs, dst));
                self.body.push(format!("execute if score {} reg matches 0.. if score {} reg matches ..-1 run scoreboard players set {} reg 1", lhs, rhs, dst));
                self.body.push(format!("execute if score {} reg matches ..-1 if score {} reg matches ..-1 if score {} reg {} {} reg run scoreboard players set {} reg 1", lhs, rhs, lhs, score_op, rhs, dst));
                self.body.push(format!("execute if score {} reg matches 0.. if score {} reg matches 0.. if score {} reg {} {} reg run scoreboard players set {} reg 1", lhs, rhs, lhs, score_op, rhs, dst));
            }
            _ => {
                todo!("TODO: make_i32_op {}", op);
            }
        }
    }

    pub fn push_i64_const(value: i64, body: &mut Vec<String>) {
        body.push(format!("execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value {}", value as i32));
        Self::incr_stack_ptr_half(body);
       
        body.push(format!("execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value {}", (value >> 32) as i32));
        Self::incr_stack_ptr_half(body);
    }

    fn add_condition(code: &mut [String], cond: &str)  {
        for c in code.iter_mut() {
            if !c.starts_with('#') {
                *c = format!("{}{}", cond, *c);
            }
        }
    }

    fn branch_begin(&mut self, cond: HalfRegister) {
        // TODO: This is only necessary for the direct convention
        //if BRANCH_CONV == BranchConv::Direct {
        self.body.push("scoreboard players set %%taken wasm 0".to_string());
        //}

        self.body.push(format!("scoreboard players operation %%tempcond reg = {} reg", cond));

        self.jump_begin();
    }

    fn branch_arm(&mut self, cond: &str, target: Option<&BranchTarget>) {
        let start_idx = self.body.len();

        if let Some(target) = target {
            self.branch(target)
        } else {
            self.halt()
        };

        Self::add_condition(&mut self.body[start_idx..], cond);
    }

    fn branch_end(&mut self) {
        self.jump_end();
    }

    fn jump_begin(&mut self) {
        match BRANCH_CONV {
            BranchConv::Grid | BranchConv::Direct | BranchConv::Loop => {},
            BranchConv::Chain => {
                // Update command count
                self.body.push("scoreboard players add %cmdcount wasm !COMMANDCOUNT!!".to_string());

                // Reset next command to break the chain
                //"execute if score %cmdcount wasm >= %maxcmdcount wasm at @e[tag=nextchain] run data modify block ~ ~ ~ Command set value \"\"".to_string(),
                self.body.push("execute if score %cmdcount wasm >= %maxcmdcount wasm at @e[tag=nextchain] run setblock ~ ~ ~ minecraft:air".to_string());
                // Set nextchain to the temporary chain block
                self.body.push("execute if score %cmdcount wasm >= %maxcmdcount wasm as @e[tag=nextchain] run tp @s 1 1 -1".to_string());
            }
        }
    }

    fn jump_end(&mut self) {
        match BRANCH_CONV {
            BranchConv::Grid | BranchConv::Direct | BranchConv::Loop => {}
            BranchConv::Chain => {
                // Activate start block
                self.body.push("execute if score %cmdcount wasm >= %maxcmdcount wasm run setblock 0 1 -1 minecraft:redstone_block replace".to_string());

                // Set nextchain to this block
                self.body.push("execute if score %cmdcount wasm < %maxcmdcount wasm as @e[tag=nextchain] run tp @s ~ ~ ~".to_string());
            }
        }
    }

    fn halt(&mut self) {
        match BRANCH_CONV {
            BranchConv::Grid | BranchConv::Loop | BranchConv::Direct => {}
            BranchConv::Chain => {
                //"execute at @e[tag=nextchain] run data modify block ~ ~ ~ Command set value \"\"".to_string()
                self.body.push("execute at @e[tag=nextchain] run setblock ~ ~ ~ minecraft:air".to_string())
            }
        }
    }

    fn jump(&mut self, entry: &Label) {
        if let Some(label) = &self.label {
            if label.func_idx != entry.func_idx {
                self.emit_stack_save();
            } else if let Some(info) = self.state_info {
                let state = &info.exit.iter().find(|(idx, _state)| *idx == entry.idx).unwrap().1;
                assert_eq!(self.op_stack.stack, state.stack);
                self.emit_real_stack_resize(state.real_size);
            }
        }

        let entry = get_block_name(entry);

        self.body.push(format!("#   Jump to {}", entry));
        match BRANCH_CONV {
            BranchConv::Grid => {
                self.body.push(format!("setblock !BLOCKPOS!{}! minecraft:redstone_block destroy", entry));
            }
            BranchConv::Chain => {
                self.body.push(format!("execute at @e[tag=nextchain] run data modify block ~ ~ ~ Command set value \"function wasm:{}\"", entry));
            }
            BranchConv::Direct => {
                self.body.push(format!("function wasm:{}", entry));
            }
            c => todo!("{:?}", c)
        }
    }

    fn branch(&mut self, target: &BranchTarget) {
        let old_stack = self.op_stack.clone();

        let entry = get_block_name(&target.label);

        self.body.push(format!("#   Branch to {}", entry));
        for (idx, ty) in target.ty.iter().enumerate().rev() {
            // TODO: Should I use the return register?
            self.emit_pop_into(Register::Return(idx as u32), *ty);
        }

        self.emit_drop(target.to_pop);

        for (idx, ty) in target.ty.iter().enumerate() {
            self.emit_push_from(Register::Return(idx as u32), *ty);
        }

        self.jump(&target.label);

        self.op_stack = old_stack;
    }


    pub fn lower_branch_table<'b, I>(&mut self, reg: Register, targets: I, default: Option<&BranchTarget>)
        where I: ExactSizeIterator<Item=Option<&'b BranchTarget>>,
    {
        let num_targets = targets.len();

        if num_targets == 0 {
            self.jump_begin();
            self.branch(default.as_ref().unwrap());
            self.jump_end();
        } else {
            self.branch_begin(reg.as_lo());

            for (idx, target) in targets.enumerate() {
                let cond = format!("execute if score %%taken wasm matches 0 if score %%tempcond reg matches {}..{} run ", idx, idx);

                self.branch_arm(&cond, target);
            }

            let cond = format!("execute if score %%taken wasm matches 0 unless score %%tempcond reg matches 0..{} run ", num_targets - 1);
            self.branch_arm(&cond, default);

            self.branch_end();
        }
    }

    pub fn lower_push_from(reg: Register, ty: Type, body: &mut Vec<String>) {
        match ty {
            Type::I32 => Self::push_i32_from(reg, body),
            Type::I64 => Self::push_i64_from(reg, body),
            _ => todo!("{:?}", ty)
        }
    }

}

pub fn assemble(basic_blocks: &[MirBasicBlock], file: &WasmFile, insert_sync: bool) -> Vec<(String, String)> {
    let mut mc_functions = Vec::new();

    println!("Started getting stack states");

    let stack_states = get_stack_states(basic_blocks);

    println!("Done getting stack states");

    let mut pool = ConstantPool::new();

    for (bb_idx, bb) in basic_blocks.iter().enumerate() {
        let mut new_block = bb.lower(bb_idx, insert_sync, Some(&stack_states[bb_idx]), &mut pool);
        let name = get_block_name(&new_block.label);

        //new_block.instrs.insert(0, r#"tellraw @a [{"text":"stackptr is "},{"score":{"name":"%stackptr","objective":"wasm"}}]"#.to_string());

        match BRANCH_CONV {
            BranchConv::Grid | BranchConv::Loop => {
                new_block.instrs.insert(0, "setblock ~ ~1 ~ minecraft:air".to_string());
            }
            BranchConv::Direct | BranchConv::Chain => {}
        }


        let contents = new_block.instrs.join("\n");

        mc_functions.push((name, contents));
    }

    let setup = create_setup_function(&mc_functions, file, pool);
    mc_functions.push(("setup".to_string(), setup));

    let dyn_branch = create_return_jumper(basic_blocks);
    mc_functions.push(("dyn_branch".to_string(), dyn_branch));

    if BRANCH_CONV == BranchConv::Chain {
        let setup_chain = create_chain_setup_function();
        mc_functions.push(("setupchain".to_string(), setup_chain));
    }

    let tables = get_tables(file, basic_blocks);
    for (idx, table) in tables.iter().enumerate() {
        let dyn_branch = create_dyn_jumper(table);
        mc_functions.push((format!("dyn_branch{}", idx), dyn_branch));
    }

    for export in file.exports.exports.iter() {
        if let ExternalKind::Function = export.kind {
            let index = CodeFuncIdx(export.index);

            let content = create_caller_function(&mc_functions, index, file);

            mc_functions.push((export.field.to_string(), content));
        }
    }

    do_fixups(&mut mc_functions);

    mc_functions
}

fn create_return_jumper(basic_blocks: &[MirBasicBlock]) -> String {
    let targets = basic_blocks.iter()
        .map(|bb| BranchTarget {
            label: bb.label.clone(),
            to_pop: 0,
            ty: Box::new([]),
        })
        .collect::<Vec<_>>();
    
    let targets = targets.iter().map(Some);

    let mut pool = ConstantPool::new();

    let mut emitter = CodeEmitter::new(None, false, false, None, None, &mut pool);
    emitter.lower_branch_table(Register::Temp(0), targets, None);
    let dyn_branch = emitter.finalize();

    assert!(pool.is_empty());

    dyn_branch.join("\n")
}

fn create_dyn_jumper(table: &crate::state::Table) -> String {
    let targets = table.elements.iter().map(|elem| {
        elem.map(|elem| {
            BranchTarget {
                label: Label::new(elem, 0),
                to_pop: 0,
                ty: Box::new([]),
            }
        })
    }).collect::<Vec<_>>();

    let targets = targets.iter().map(|t| t.as_ref());

    let mut pool = ConstantPool::new();

    let mut emitter = CodeEmitter::new(None, false, false, None, None, &mut pool);
    emitter.lower_branch_table(Register::Temp(0), targets, None);
    let dyn_branch = emitter.finalize();

    assert!(pool.is_empty());
    
    dyn_branch.join("\n")
}

fn do_fixups(mc_functions: &mut Vec<(String, String)>) {
    let intrinsic_counts = INTRINSIC_COUNTS.get_or_init(get_intrinsic_counts);

    // Apply fixups
    for func_idx in 0..mc_functions.len() {

        loop {
            let start = mc_functions[func_idx].1.find("!BLOCKIDX")
                .or_else(|| mc_functions[func_idx].1.find("!BLOCKPOS"))
                .or_else(|| mc_functions[func_idx].1.find("!COMMANDCOUNT"));

            let start = if let Some(start) = start {
                start
            } else {
                break;
            };

            let middle = mc_functions[func_idx].1[start + 1..].find('!').unwrap() + start + 1;
            let end = mc_functions[func_idx].1[middle + 1..].find('!').unwrap() + middle + 1;

            let fixup_type = &mc_functions[func_idx].1[start + 1..middle];
            let fixup_value = &mc_functions[func_idx].1[middle + 1..end];

            match fixup_type {
                "BLOCKIDX" => {
                    let idx = get_entry_index_named(fixup_value, mc_functions);
                    let idx = idx.to_string();

                    mc_functions[func_idx].1.replace_range(start..=end, &idx);
                }
                "BLOCKPOS" => {
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

                    let pos = block_pos_from_idx(idx, true);

                    mc_functions[func_idx].1.replace_range(start..=end, &pos);
                }
                "COMMANDCOUNT" => {
                    assert!(fixup_value.is_empty());

                    let body = &mc_functions[func_idx].1;

                    let mut count = body.lines().count();

                    for call in body.match_indices("function intrinsic:") {
                        let line = body[call.0..].lines().next().unwrap();

                        let func_name = line.strip_prefix("function intrinsic:").unwrap();

                        count += intrinsic_counts.get(func_name).copied().unwrap_or(1_000);
                    }

                    mc_functions[func_idx].1.replace_range(start..=end, &count.to_string());
                }
                s => {
                    todo!("{:?}", s)
                }
            }
        }
    }
}

/*fn get_entry_index(func: CodeFuncIdx, mc_functions: &[(String, String)]) -> usize {
    let name = get_block_name(&Label::new(func, 0));
    get_entry_index_named(&name, mc_functions)
}*/

fn get_entry_index_named(func: &str, mc_functions: &[(String, String)]) -> usize {
    mc_functions.iter().enumerate().find(|(_, (n, _))| {
        n == func
    }).unwrap_or_else(|| {
        eprintln!("Failed to find {:?}", func);
        eprintln!("Functions are:");
        for (n, _) in mc_functions.iter() {
            eprintln!("{:?}", n);
        }
        panic!();
    }).0
}

fn block_pos_from_idx(idx: usize, redstone: bool) -> String {
    let x = (idx as i32) % 48;
    let y = if redstone { 1 } else { 0 };
    let z = -1 - ((idx as i32) / 48);

    format!("{} {} {}", x, y, z)
}

fn create_setup_function(mc_functions: &[(String, String)], file: &WasmFile, pool: ConstantPool) -> String {
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
    kill @e[tag=turtle]\n\
    kill @e[tag=nextchain]\n\
    \n\
    # Add armor stand pointers\n\
    summon minecraft:armor_stand 0 0 8 {Marker:1b,Tags:[\"memoryptr\"],CustomName:'\"memoryptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 1 {Marker:1b,Tags:[\"localptr\"],CustomName:'\"localptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 1 {Marker:1b,Tags:[\"frameptr\"],CustomName:'\"frameptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 0 {Marker:1b,Tags:[\"stackptr\"],CustomName:'\"stackptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 -2 {Marker:1b,Tags:[\"turtle\"],CustomName:'\"turtle\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 1 1 -1 {Marker:1b,Tags:[\"nextchain\"],CustomName:'\"nextchain\"',CustomNameVisible:1b}\n\
    
    scoreboard players set %stackptr wasm 0
    scoreboard players set %frameptr wasm 0
    ".to_string();

    setup.push_str("\n# Make constants\n");
    for line in pool.init_cmds() {
       setup.push_str(&line);
       setup.push('\n');
    }
    
    setup.push_str("\n# Make stack\n");
    setup.push_str("fill 0 0 0 50 0 0 minecraft:air replace\n");
    setup.push_str("fill 0 0 0 50 0 0 minecraft:jukebox{RecordItem:{id:\"minecraft:stone\",Count:1b,tag:{Memory:0}}} replace\n");
    
    setup.push_str("\n# Make memory\n");
    for mem_cmd in file.memory.init() {
        setup.push_str(&mem_cmd);
        setup.push('\n');
    }

    setup.push_str("\n# Init memory\n");
    for data_cmd in file.data.init() {
        setup.push_str(&data_cmd);
        setup.push('\n');
    }

    setup.push_str("\n# Make globals\n");
    for global_cmd in file.globals.init() {
        setup.push_str(&global_cmd);
        setup.push('\n');
    }

    match BRANCH_CONV {
        BranchConv::Grid => {
            setup.push_str("\n# Make commands\n");
            for (idx, (name, _)) in mc_functions.iter().enumerate() {
                let lo_pos = block_pos_from_idx(idx, false);
                let hi_pos = block_pos_from_idx(idx, true);
                // FIXME: Pos
                let cmd = format!("fill {} {} minecraft:air replace", lo_pos, hi_pos);
                setup.push_str(&cmd);
                setup.push('\n');
                let cmd = format!("setblock {} minecraft:command_block{{Command:\"function wasm:{}\"}} replace", lo_pos, name);
                setup.push_str(&cmd);
                setup.push('\n');
            }
        }
        BranchConv::Chain => {
            setup.push_str("\n# Make commands\n");

            setup.push_str("fill 0 0 -1 3 1 -1 minecraft:air replace\n");

            // Start block
            setup.push_str("setblock 0 0 -1 minecraft:command_block[facing=east]{Command:\"function wasm:setupchain\"} replace\n");
            // Temporary block
            setup.push_str("setblock 1 1 -1 minecraft:chain_command_block[facing=east,conditional=false]{UpdateLastExecution:0b,auto:1b} replace\n");
        }
        BranchConv::Direct => {}
        bc => todo!("{:?}", bc)
    }

    setup
}

fn create_chain_setup_function() -> String {
    let mut setup_chain = String::new();

    // Reset activation
    setup_chain.push_str("setblock ~ ~1 ~ minecraft:air replace\n");
    // Initialize the command limit
    setup_chain.push_str("execute store result score %maxcmdcount wasm run gamerule maxCommandChainLength\n");
    setup_chain.push_str("scoreboard players operation %maxcmdcount wasm /= %%32 reg\n");
    // Reset the command counter
    setup_chain.push_str("scoreboard players set %cmdcount wasm 0\n");
    // Setup the next chain pointer
    setup_chain.push_str("execute as @e[tag=nextchain] run tp @s 2 0 -1\n");

    // Replace the first chain block with the temporary chain block
    setup_chain.push_str("setblock ~1 ~ ~ minecraft:air replace\n");
    setup_chain.push_str("clone ~1 ~1 ~ ~1 ~1 ~ ~1 ~ ~\n");

    // Second chain block
    setup_chain.push_str("setblock 2 0 -1 minecraft:air replace\n");
    setup_chain.push_str("setblock 2 0 -1 minecraft:chain_command_block[facing=west,conditional=false]{UpdateLastExecution:0b,auto:1b} replace\n");

    setup_chain
}

fn create_caller_function(mc_functions: &[(String, String)], index: CodeFuncIdx, file: &WasmFile) -> String {
    let name = crate::get_entry_point(index);
    let idx = mc_functions.iter().enumerate().find(|(_, (n, _))| {
        n == &name
    }).unwrap().0;

    let func_ty = file.functions.functions[index.0 as usize];

    let mut f = FuncBodyStream::new(TypeOrFuncType::FuncType(func_ty), &file.types, index);
    f.basic_blocks.push(MirBasicBlock::new(CodeFuncIdx(0), 0, OpStack::new()));

    if BRANCH_CONV != BranchConv::Direct {
        f.push_instr(Instr::PushI32Const(-1));
    }

    // FIXME:
    /*if export.field == "main" {
        f.push_instr(Instr::PushI32Const(0));
        f.push_instr(Instr::PopI32Into(Register::Param(0)));
        f.push_instr(Instr::PushI32Const(0));
        f.push_instr(Instr::PopI32Into(Register::Param(1)));
    }*/

    //assert_eq!(f.basic_blocks.len(), 1);

    let mut pool = ConstantPool::new();

    let mut body = CodeEmitter::emit_all(&f.basic_blocks[0], None, false, false, None, &mut pool);

    assert!(pool.is_empty());

    let pos = block_pos_from_idx(idx, true);

    match BRANCH_CONV {
        BranchConv::Grid => {
            body.push(format!("setblock {} minecraft:redstone_block destroy", pos));
        }
        BranchConv::Chain => {
            body.push(format!("data modify block 1 1 -1 Command set value \"function wasm:{}\"", name));
            body.push("setblock 0 1 -1 minecraft:redstone_block destroy".to_string());
        }
        BranchConv::Direct => {
            let name = &mc_functions[idx].0;
            body.push(format!("function wasm:{}", name));
        }
        bc => todo!("{:?}", bc)
    }

    body.join("\n")
}