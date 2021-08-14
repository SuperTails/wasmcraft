mod datapack;
mod state;
mod sexpr;

use datapack_common::functions::{Command, Function, Objective, ScoreHolder};
use datapack_common::functions::FunctionIdent as FunctionId;

use datapack_vm::interpreter::Interpreter;
use std::convert::TryInto;
use std::path::{Path, PathBuf};
use std::str;
use once_cell::sync::OnceCell;
use std::collections::{HashMap, BTreeMap};

use state::State;

use wasmparser::{Data, DataKind, Element, ElementItem, ElementKind, Export, ExternalKind, FuncType, Global, Import, ImportSectionEntryType, InitExpr, MemoryImmediate, MemoryType, Operator, Parser, Payload, TableType, Type, TypeDef, TypeOrFuncType};

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
    op_stack: OpStack,
    label: Label,
    instrs: Vec<T>,
}

impl<T> BasicBlock<T> {
    fn new(func_idx: CodeFuncIdx, idx: usize, op_stack: OpStack) -> Self {
        BasicBlock {
            op_stack,
            label: Label { func_idx, idx },
            instrs: Vec::new()
        }
    }
}

//  Assume the MIR and the CMD are in sync.
//
//  If the next instruction is normal:
//      Step the MIR
//      There must be a sync point immediately following the instruction (ish).
//      Run the CMD to a sync point.

//  If the next instruction is a branch in direct mode:
//      Step the MIR.
//      The source instruction must be followed by a sync point.
//      The target block must begin with a sync point.
//      Run the CMD to a sync point.
//  If the next instruction is the last in direct mode:
//      Step the MIR and unwind *one* frame, and incr the pc when you get there.
//      The last instruction must *not* be followed by a sync point.
//      We know when we get back there will be a sync point.
//      Run the CMD to a sync point.

//  If the next instruction is a branch in indirect mode:
//      Step the MIR.
//      The source instruction must *not* be followed by a sync point.
//      The target block must begin with a sync point.
//      Run the CMD to a sync point.
//  If the next instruction is the last in indirect mode:
//      Step the MIR and unwind *one* frame, and incr the pc when you get there.
//      The last instruction must *not* be followed by a sync point.
//      Run the CMD to a sync point, because we know the beginning of the target block has one.

impl BasicBlock<Instr> {
    fn lower(&self, globals: &GlobalList, bb_idx: usize, insert_sync: bool, state_info: Option<&StateInfo>) -> BasicBlock<String> {
        // TODO: Should I use a virtual stack always?
        let instrs = CodeEmitter::emit_all(self, globals, Some(bb_idx), true, insert_sync, state_info);

        BasicBlock {
            op_stack: self.op_stack.clone(),
            label: self.label.clone(),
            instrs,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BranchConv {
    Grid,
    Loop,
    Chain,
    Direct,
}

const BRANCH_CONV: BranchConv = BranchConv::Chain;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Axis {
    X,
    Y,
    Z,
}

#[derive(Debug, Clone)]
struct BranchTarget {
    label: Label,
    to_pop: usize,
    ty: Box<[Type]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Half {
    Hi,
    Lo,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct HalfRegister(Register, Half);

impl HalfRegister {
    pub fn get(&self) -> String {
        self.to_string()
    }


    fn split_half(w: &str) -> Result<(&str, Half), String> {
        if let Some(w) = w.strip_suffix("%lo") {
            Ok((w, Half::Lo))
        } else if let Some(w) = w.strip_suffix("%hi") {
            Ok((w, Half::Hi))
        } else {
            Err(format!("invalid register {}", w))
        }
    }
}

impl std::fmt::Display for HalfRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.1 {
            Half::Hi => write!(f, "{}", self.0.get_hi()),
            Half::Lo => write!(f, "{}", self.0.get_lo()),
        }
    }
}

impl str::FromStr for HalfRegister {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        /*
        let split_half = |w: &str| {
            if let Some(w) = w.strip_suffix("%lo") {
                Ok((w, false))
            } else if let Some(w) = w.strip_suffix("%hi") {
                Ok((w, true))
            } else {
                Err(format!("invalid register {}", s))
            }
        };
        */

        let (s, is_hi) = Self::split_half(s)?;
        
        if let Some(s) = s.strip_prefix("%work%") {
            let idx = s.parse::<u32>().map_err(|e| e.to_string())?;
            Ok(Self(Register::Work(idx), is_hi))
        } else if let Some(s) = s.strip_prefix("%param%") {
            let idx = s.parse::<u32>().map_err(|e| e.to_string())?;
            Ok(Self(Register::Param(idx), is_hi))
        } else if let Some(s) = s.strip_prefix("%return%") {
            let idx = s.parse::<u32>().map_err(|e| e.to_string())?;
            Ok(Self(Register::Return(idx), is_hi))
        } else if let Some(s) = s.strip_prefix("%stack%") {
            let idx = s.parse::<u32>().map_err(|e| e.to_string())?;
            Ok(Self(Register::Stack(idx), is_hi))
        } else {
            Err(format!("invalid register {}", s))
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Relation {
    GreaterThan,
    LessThan,
    GreaterThanEq,
    LessThanEq,
}

#[derive(Debug, Clone)]
enum Instr {
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

struct CodeEmitter<'a> {
    pub body: Vec<String>,

    pub op_stack: OpStack,
    pub real_stack_size: usize,

    use_virt_stack: bool,
    insert_sync: bool,

    bb_idx: Option<usize>,

    label: Option<Label>,

    sync_instr_idx: usize,

    state_info: Option<&'a StateInfo>,
}

impl<'a> CodeEmitter<'a> {
    pub fn new(bb_idx: Option<usize>, use_virt_stack: bool, insert_sync: bool, state_info: Option<&'a StateInfo>, label: Option<Label>) -> Self {
        if bb_idx.is_none() {
            assert!(!use_virt_stack);
            assert!(!insert_sync);
        }

        let op_stack;
        let real_stack_size;
        if let Some(state_info) = state_info {
            op_stack = state_info.entry.op_stack.clone();
            real_stack_size = state_info.entry.real_size;
        } else {
            op_stack = OpStack::new();
            real_stack_size = 0;
        }

        let mut emitter = CodeEmitter {
            body: Vec::new(),
            op_stack,
            real_stack_size,
            bb_idx,
            sync_instr_idx: 0,
            use_virt_stack,
            insert_sync,
            state_info,
            label,
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

    pub fn emit_all(basic_block: &BasicBlock<Instr>, global_list: &GlobalList, bb_idx: Option<usize>, use_virt_stack: bool, insert_sync: bool, state_info: Option<&'a StateInfo>) -> Vec<String> {
        println!("\nSTARTING {:?}", basic_block.label);
        println!("{:?}", state_info);

        // The real stack has 1 entry: the return address
        let mut emitter = Self::new(bb_idx, use_virt_stack, insert_sync, state_info, Some(basic_block.label.clone()));
        for i in basic_block.instrs.iter() {
            emitter.emit(i, global_list);
        }
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
            if self.real_stack_size == self.op_stack.0.len() {
                self.real_stack_size -= 1;

                Self::decr_stack_ptr(&mut self.body);
            }

            self.op_stack.pop_value();
        }
    }

    pub fn emit(&mut self, instr: &Instr, global_list: &GlobalList) {
        println!("Emitting {:?}", instr);
        
        self.body.push(format!("# {:?}", instr));

        self.emit_inner(instr, global_list);

        self.emit_sync();
    }

    pub fn emit_inner(&mut self, instr: &Instr, global_list: &GlobalList) {
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

            Copy { dst, src } => {
                if dst != src {
                    self.body.push(format!("scoreboard players operation {} reg = {} reg", dst, src));
                }
            }

            SetMemPtr(reg) => {
                self.body.push(format!("scoreboard players operation %ptr reg = {} reg", reg.get_lo()));
                self.body.push("function intrinsic:setptr".to_string());
            }
            &SetGlobalPtr(global_index) => {
                let pos = global_list.get_offset(global_index);
                self.body.push(format!("execute as @e[tag=globalptr] run tp @s {}", pos));
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
                let carry = Register::Work(10).as_lo();

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
                self.body.push(format!("execute store success score {} reg if score {} reg = {} reg if score {} reg = {} reg", dst.as_lo(), lhs.as_lo(), rhs.as_lo(), lhs.as_hi(), rhs.as_hi()));

                if invert {
                    self.body.push(format!("execute store success score {} reg if score {} reg matches 0..0", dst.as_lo(), dst.as_lo()));
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

                assert!(!matches!(lhs, Register::Work(3 | 4 | 5 | 6)));
                assert!(!matches!(rhs, Register::Work(3 | 4 | 5 | 6)));
                assert!(!matches!(dst, Register::Work(3 | 4 | 5 | 6)));

                assert_ne!(dst, lhs);
                assert_ne!(dst, rhs);

                let hi_is_lesser = Register::Work(3).as_lo();
                let hi_is_greater = Register::Work(4).as_lo();
                let hi_is_equal = Register::Work(5).as_lo();

                let lo_is_lesser = Register::Work(6).as_lo();

                self.make_i32_op(lhs.as_hi(), "ltu", rhs.as_hi(), hi_is_lesser);
                self.make_i32_op(lhs.as_hi(), "gtu", rhs.as_hi(), hi_is_greater);
                self.make_i32_op(lhs.as_hi(), "==", rhs.as_hi(), hi_is_equal);
                self.make_i32_op(lhs.as_lo(), "ltu", rhs.as_lo(), lo_is_lesser);

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
                self.make_i32_op(lhs.as_lo(), op, rhs.as_lo(), dst.as_lo());
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
                let return_name = get_block(&label);

                self.emit_stack_save();

                self.body.push(format!("execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value !BLOCKIDX!{}!", return_name));
                Self::incr_stack_ptr(&mut self.body);

                // TODO: ????
                //self.op_stack.push_i32();
            }

            Branch(target) => {
                self.jump_begin();
                self.branch(target);
                self.jump_end();
            }
            &DynBranch(reg, table_idx) => {
                assert_eq!(reg, Register::Work(0));

                self.emit_stack_save();

                if let Some(i) = table_idx {
                    self.body.push(format!("function wasm:dyn_branch{}", i));
                } else {
                    self.body.push("function wasm:dyn_branch".to_string());
                }
            }
            BranchIf { t_name, f_name, cond } => {
                self.body.push(format!("#   {:?}", instr));

                self.branch_begin(cond.as_lo());
                self.branch_arm("execute if score %%taken wasm matches 0 unless score %%tempcond reg matches 0..0 run ", Some(t_name));
                self.branch_arm("execute if score %%taken wasm matches 0 if score %%tempcond reg matches 0..0 run ", Some(f_name));
                self.branch_end();
            }
            BranchTable { reg, targets, default } => {
                let targets = targets.iter().map(Some);

                self.lower_branch_table(*reg, targets, default.as_ref());
            }

            LoadGlobalI64(reg) => {
                self.body.push(format!("execute at @e[tag=globalptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg.get_lo()));
                self.body.push(format!("execute at @e[tag=globalptr] store result score {} reg run data get block ~ ~ ~1 RecordItem.tag.Memory 1", reg.get_hi()));
            }
            LoadGlobalI32(reg) => {
                self.body.push(format!("execute at @e[tag=globalptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg.get_lo()));
            }
            StoreGlobalI64(reg) => {
                self.body.push(format!("execute at @e[tag=globalptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_lo()));
                self.body.push(format!("execute at @e[tag=globalptr] store result block ~ ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_hi()));
            }
            StoreGlobalI32(reg) => {
                self.body.push(format!("execute at @e[tag=globalptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_lo()));
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

            ResetFrames => {
                self.body.push("execute as @e[tag=frameptr] run tp @s 0 0 1".to_string());
                self.body.push("scoreboard players set %frameptr wasm 0".to_string());
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
        if self.use_virt_stack {
            assert!(self.op_stack.0.len() >= self.real_stack_size);

            let idx = self.op_stack.0.len();

            self.op_stack.push_i32();

            let stack_reg = Register::Stack(idx as u32).as_lo();
            self.body.push(format!("scoreboard players set {} reg {}", stack_reg, value));
        } else {
            assert_eq!(self.op_stack.0.len(), self.real_stack_size);

            self.body.push(format!("execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value {}", value));
            Self::incr_stack_ptr(&mut self.body);

            self.op_stack.push_i32();
            self.real_stack_size += 1;
        }
    }

    pub fn emit_push_i64_const(&mut self, value: i64) {
        if self.use_virt_stack {
            assert!(self.op_stack.0.len() >= self.real_stack_size);

            let idx = self.op_stack.0.len();

            self.op_stack.push_i64();

            let stack_reg_lo = Register::Stack(idx as u32).as_lo();
            let stack_reg_hi = Register::Stack(idx as u32).as_hi();
            self.body.push(format!("scoreboard players set {} reg {}", stack_reg_lo, value as i32));
            self.body.push(format!("scoreboard players set {} reg {}", stack_reg_hi, (value >> 32) as i32));
        } else {
            assert_eq!(self.op_stack.0.len(), self.real_stack_size);

            Self::push_i64_const(value, &mut self.body);

            self.op_stack.push_i64();
            self.real_stack_size += 1;
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
        if self.use_virt_stack {
            assert!(self.op_stack.0.len() >= self.real_stack_size, "{:?} {}", self.op_stack, self.real_stack_size);

            let idx = self.op_stack.0.len();

            self.op_stack.push_i32();

            // This is just a register copy
            let stack_reg = Register::Stack(idx as u32).as_lo();
            self.body.push(format!("scoreboard players operation {} reg = {} reg", stack_reg, reg.as_lo()));
        } else {
            assert!(self.op_stack.0.len() == self.real_stack_size);

            self.real_stack_size += 1;
            self.op_stack.push_i32();

            Self::push_i32_from(reg, &mut self.body);
        }
    }

    pub fn emit_push_i64_from(&mut self, reg: Register) {
        if self.use_virt_stack {
            assert!(self.op_stack.0.len() >= self.real_stack_size);

            let idx = self.op_stack.0.len();

            self.op_stack.push_i64();

            // This is just a register copy
            let stack_reg_lo = Register::Stack(idx as u32).as_lo();
            let stack_reg_hi = Register::Stack(idx as u32).as_hi();
            self.body.push(format!("scoreboard players operation {} reg = {} reg", stack_reg_lo, reg.as_lo()));
            self.body.push(format!("scoreboard players operation {} reg = {} reg", stack_reg_hi, reg.as_hi()));
        } else {
            assert!(self.op_stack.0.len() == self.real_stack_size);

            self.real_stack_size += 1;
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
        assert!(self.op_stack.0.len() >= self.real_stack_size);

        let is_real_stack = self.op_stack.0.len() == self.real_stack_size;

        self.op_stack.pop_i32();

        let idx = self.op_stack.0.len();

        if !self.use_virt_stack {
            assert!(is_real_stack);
        }

        if is_real_stack {
            // We are popping a value off of the real stack
            Self::pop_i32_into(reg, &mut self.body);
            self.real_stack_size -= 1;
        } else {
            // This is just a register copy
            let stack_reg = Register::Stack(idx as u32).as_lo();
            self.body.push(format!("scoreboard players operation {} reg = {} reg", reg.as_lo(), stack_reg));
        }
    }

    pub fn emit_pop_i64_into(&mut self, reg: Register) {
        assert!(self.op_stack.0.len() >= self.real_stack_size);

        let is_real_stack = self.op_stack.0.len() == self.real_stack_size;

        self.op_stack.pop_i64();

        let idx = self.op_stack.0.len();

        if !self.use_virt_stack {
            assert!(is_real_stack);
        }

        if is_real_stack {
            // We are popping a value off of the real stack
            Self::pop_i64_into(reg, &mut self.body);
            self.real_stack_size -= 1;
        } else {
            // This is just a register copy
            let stack_reg_lo = Register::Stack(idx as u32).as_lo();
            let stack_reg_hi = Register::Stack(idx as u32).as_hi();
            self.body.push(format!("scoreboard players operation {} reg = {} reg", reg.as_lo(), stack_reg_lo));
            self.body.push(format!("scoreboard players operation {} reg = {} reg", reg.as_hi(), stack_reg_hi));
        }
    }

    pub fn emit_real_stack_resize(&mut self, target_size: usize) {
        if self.real_stack_size < target_size {
            self.emit_stack_save_to(target_size);
        } else if self.real_stack_size > target_size {
            todo!()
        }
    }

    pub fn emit_stack_save_to(&mut self, target_size: usize) {
        assert!(self.real_stack_size <= target_size);
        for idx in self.real_stack_size..target_size {
            let reg = Register::Stack(idx as u32);
            let ty = self.op_stack.0[idx];

            Self::lower_push_from(reg, ty, &mut self.body);
            self.real_stack_size += 1;
        }
    }

    pub fn emit_stack_save(&mut self) {
        self.emit_stack_save_to(self.op_stack.0.len());
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

    pub fn make_i32_op(&mut self, mut lhs: HalfRegister, mut op: &str, mut rhs: HalfRegister, dst: HalfRegister) {
        match op {
            "+=" | "-=" | "*=" | "%=" => {
                self.body.push(format!("scoreboard players operation {} reg {} {} reg", lhs, op, rhs));
                assert_eq!(lhs, dst);
            }
            "/=" => {
                // Minecraft division always rounds towards negative infinity, so we need to correct for that
                assert_ne!(lhs, dst);
                assert_ne!(rhs, dst);

                let rem = Register::Work(21).as_lo();

                self.body.push(format!("scoreboard players operation {} reg = {} reg", rem, lhs));
                self.body.push(format!("scoreboard players operation {} reg %= {} reg ", rem, rhs));

                self.body.push(format!("scoreboard players operation {} reg = {} reg", dst, lhs));
                self.body.push(format!("scoreboard players operation {} reg /= {} reg", dst, rhs));

                self.body.push(format!("execute if score {} reg matches ..-1 if score {} reg matches 0.. unless score {} reg matches 0..0 run scoreboard players add {} reg 1", lhs, rhs, rem, dst));
                self.body.push(format!("execute if score {} reg matches ..-1 if score {} reg matches 0.. unless score {} reg matches 0..0 run scoreboard players add {} reg 1", rhs, lhs, rem, dst));
            }
            "/=u" => {
                let d1 = Register::Work(30).as_lo();
                let r1 = Register::Work(31).as_lo();
                let d2 = Register::Work(32).as_lo();
                let r2 = Register::Work(33).as_lo();
                let d3 = Register::Work(34).as_lo();
                let is_gtu = Register::Work(35).as_lo();
                let lhs_lo = Register::Work(36).as_lo();

                assert_ne!(lhs, dst);
                assert_ne!(rhs, dst);

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
                assert_ne!(rhs, dst);

                self.make_i32_op(lhs, "/=u", rhs, dst);

                // lhs - dst * rhs

                self.body.push(format!("scoreboard players operation {} reg *= {} reg", dst, rhs));
                self.body.push(format!("scoreboard players operation {} reg *= %%-1 reg", dst));
                self.body.push(format!("scoreboard players operation {} reg += {} reg", dst, lhs));
            }
            "rems" => {
                assert_ne!(lhs, dst);
                assert_ne!(rhs, dst);

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
                self.body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs));
                self.body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs));
                self.body.push("scoreboard players operation %param1%0 reg %= %%32 reg".to_string());
                self.body.push("function intrinsic:shl".to_string());
                self.body.push(format!("scoreboard players operation {} reg = %param0%0 reg", dst));
            }
            "shru" => {
                self.body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs));
                self.body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs));
                self.body.push("scoreboard players operation %param1%0 reg %= %%32 reg".to_string());
                self.body.push("function intrinsic:lshr".to_string());
                self.body.push(format!("scoreboard players operation {} reg = %param0%0 reg", dst));
            }
            "shrs" => {
                assert_ne!(dst, rhs);

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

                assert_ne!(lhs, dst);
                assert_ne!(rhs, dst);

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

    pub fn pop_half_into(reg: Register, body: &mut Vec<String>) {
        Self::decr_stack_ptr_half(body);
        body.push(format!("execute at @e[tag=stackptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg.get_lo()));
    }

    pub fn push_i64_const(value: i64, body: &mut Vec<String>) {
        body.push(format!("execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value {}", value as i32));
        Self::incr_stack_ptr_half(body);
       
        body.push(format!("execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value {}", (value >> 32) as i32));
        Self::incr_stack_ptr_half(body);
    }

    pub fn push_half_from(reg: Register, body: &mut Vec<String>) {
        body.push(format!("execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_lo()));
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
                assert_eq!(self.op_stack, state.op_stack);
                self.emit_real_stack_resize(state.real_size);
            }
        }

        let entry = get_block(entry);

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
        let old_state = self.op_stack.clone();
        let old_size = self.real_stack_size;

        let entry = get_block(&target.label);

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

        self.op_stack = old_state;
        self.real_stack_size = old_size;
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

    pub fn lower_pop_into(reg: Register, ty: Type, body: &mut Vec<String>) {
        match ty {
            Type::I32 => Self::pop_i32_into(reg, body),
            Type::I64 => Self::pop_i64_into(reg, body),
            _ => todo!("{:?}", ty)
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

#[derive(Debug, Clone, PartialEq, Eq)]
struct OpStack(Vec<Type>);

impl OpStack {
    pub fn new() -> Self {
        OpStack(Vec::new())
    }

    pub fn push_i32(&mut self) {
        self.0.push(Type::I32);
    }

    pub fn push_i64(&mut self) {
        self.0.push(Type::I64);
    }

    pub fn push_ty(&mut self, ty: Type) {
        self.0.push(ty);
    }

    pub fn pop_value(&mut self) -> Type {
        self.0.pop().unwrap()
    }

    pub fn pop_i32(&mut self) {
        self.pop_ty(Type::I32)
    }

    pub fn pop_i64(&mut self) {
        self.pop_ty(Type::I64)
    }

    pub fn pop_ty(&mut self, ty: Type) {
        assert_eq!(self.pop_value(), ty);
    }

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Register {
    Work(u32),
    Param(u32),
    Return(u32),
    Stack(u32),
}

impl Register {
    pub fn as_lo(self) -> HalfRegister {
        HalfRegister(self, Half::Lo)
    }

    pub fn as_hi(self) -> HalfRegister {
        HalfRegister(self, Half::Hi)
    }

    pub fn get_lo(&self) -> String {
        match self {
            Register::Work(i) => {
                format!("%work%{}%lo", i)
            }
            Register::Param(i) => {
                format!("%param%{}%lo", i)
            }
            Register::Return(i) => {
                format!("%return%{}%lo", i)
            }
            Register::Stack(i) => {
                format!("%stack%{}%lo", i)
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
            Register::Return(i) => {
                format!("%return%{}%hi", i)
            }
            Register::Stack(i) => {
                format!("%stack%{}%hi", i)
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WasmValue {
    I32(i32),
    I64(i64),
}

impl WasmValue {
    fn ty(self) -> Type {
        match self {
            WasmValue::I32(_) => Type::I32,
            WasmValue::I64(_) => Type::I64,
        }
    }
}

impl From<i32> for WasmValue {
    fn from(v: i32) -> Self {
        WasmValue::I32(v)
    }
}

impl From<i64> for WasmValue {
    fn from(v: i64) -> Self {
        WasmValue::I64(v)
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

fn get_input_tys(ty: TypeOrFuncType, types: &TypeList) -> Box<[Type]> {
     match ty {
        TypeOrFuncType::Type(_) => Vec::new().into_boxed_slice(),
        TypeOrFuncType::FuncType(i) => {
            let ty = &types.types[i as usize];
            if let TypeDef::Func(ty) = ty {
                ty.params.clone()
            } else {
                panic!()
            }
        }
    }
}

fn get_output_tys(ty: TypeOrFuncType, types: &TypeList) -> Box<[Type]> {
    match ty {
        TypeOrFuncType::Type(Type::EmptyBlockType) => Vec::new().into_boxed_slice(),
        TypeOrFuncType::Type(t) => vec![t].into_boxed_slice(),
        TypeOrFuncType::FuncType(i) => {
            let ty = &types.types[i as usize];
            if let TypeDef::Func(ty) = ty {
                ty.returns.clone()
            } else {
                panic!()
            }
        }
    }
}

#[derive(Debug, Clone)]
struct FlowStack(Vec<ControlFlowEntry>);

impl FlowStack {
    pub fn new(ty: TypeOrFuncType, types: &TypeList, exit_label: Label) -> Self {
        let outputs = get_output_tys(ty, types);
        let target_tys = outputs.clone();

        FlowStack(vec![ControlFlowEntry {
            label: Some(exit_label),
            stack_size: 1,
            target_tys,
            else_block: None,
            outputs,
            next_block: 1,
        }])
    }
}

struct FuncBodyStream {
    func_idx: CodeFuncIdx,
    basic_blocks: Vec<BasicBlock<Instr>>,
    reachable: Vec<bool>,
    bb_index: usize,
    depth: usize,

    op_stack: OpStack,
    flow_stack: FlowStack,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct StackState {
    op_stack: OpStack,
    real_size: usize,
}

impl StackState {
    pub fn new() -> Self {
        StackState {
            op_stack: OpStack::new(),
            real_size: 0,
        }
    }
}

fn calc_exit_state(basic_block: &BasicBlock<Instr>, entry_state: StackState) -> StackState {
    let mut state = entry_state;


    state
}

fn get_first_term_instr(basic_block: &BasicBlock<Instr>) -> usize {
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

fn get_body_instrs(basic_block: &BasicBlock<Instr>) -> &[Instr] {
    let idx = get_first_term_instr(basic_block);
    &basic_block.instrs[..idx]
}

fn get_term_instrs(basic_block: &BasicBlock<Instr>) -> &[Instr] {
    let idx = get_first_term_instr(basic_block);
    &basic_block.instrs[idx..]
}

fn get_reachable_blocks(body: &[BasicBlock<Instr>]) -> Vec<bool> {
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

fn get_next_blocks(basic_block: &BasicBlock<Instr>) -> Vec<usize> {
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
            Instr::Branch(t) => todo!(),
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
                return_addr.clone().into_iter().collect()
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

fn get_next_state(basic_block: &BasicBlock<Instr>, entry_state: StackState) -> Vec<(usize, StackState)> {
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
            Instr::Drop => {
                if state.real_size == state.op_stack.0.len() {
                    state.real_size -= 1;
                }

                state.op_stack.pop_value();
            }
            Instr::PushI32Const(_) => state.op_stack.push_i32(),
            Instr::PushI64Const(_) => state.op_stack.push_i64(),
            Instr::PushI32From(..) => state.op_stack.push_i32(),
            Instr::PushI64From(..) => state.op_stack.push_i64(),
            Instr::PopI32Into(..) => {
                if state.real_size == state.op_stack.0.len() {
                    state.real_size -= 1;
                }

                state.op_stack.pop_i32();
            },
            Instr::PopI64Into(..) => {
                if state.real_size == state.op_stack.0.len() {
                    state.real_size -= 1;
                }

                state.op_stack.pop_i64();
            },
            Instr::Branch(t) => {
                for ty in t.ty.iter() {
                    state.op_stack.pop_ty(*ty);                   
                }

                for _ in 0..t.to_pop {
                    state.op_stack.pop_value();
                }

                if t.label.func_idx != basic_block.label.func_idx {
                    state.real_size = state.op_stack.0.len();
                }
            },
            Instr::BranchIf { .. } => todo!(),
            Instr::BranchTable { .. } => todo!(),
            Instr::DynBranch(..) => {
                state.real_size = state.op_stack.0.len();
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
            state.op_stack.pop_ty(*ty);                   
        }

        for _ in 0..t.to_pop {
            state.op_stack.pop_value();
        }

        if state.real_size > state.op_stack.0.len() || t.label.func_idx != basic_block.label.func_idx {
            state.real_size = state.op_stack.0.len();
        }

        for ty in t.ty.iter() {
            state.op_stack.push_ty(*ty);
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

#[derive(Debug, PartialEq, Eq)]
struct StateInfo {
    entry: StackState,
    exit: Vec<(usize, StackState)>,
}

impl StateInfo {
    fn new(basic_block: &BasicBlock<Instr>, entry: StackState) -> Self {
        let exit = get_next_state(basic_block, entry.clone());
        StateInfo { entry, exit }
    }
}

fn find_real_stack_sizes(basic_blocks: &[BasicBlock<Instr>]) -> Vec<StateInfo> {
    let mut states = BTreeMap::new();

    let entry_state = StackState {
        op_stack: OpStack(vec![Type::I32]),
        real_size: 1,
    };

    let start_state = StateInfo::new(&basic_blocks[0], entry_state);

    states.insert(0, start_state);

    let mut to_visit = vec![0];

    while let Some(block) = to_visit.pop() {
        let info = states.get(&block).unwrap();
        let exit = info.exit.clone();

        for (next_idx, next_state) in exit.iter() {
            let next_block = &basic_blocks.iter().find(|bb| bb.label.idx == *next_idx).unwrap();

            let changed = if let Some(prev) = states.get(next_idx) {
                prev.entry != *next_state
            } else {
                true
            };

            if changed {
                let next_info = StateInfo::new(next_block, next_state.clone());
                states.insert(*next_idx, next_info);
                to_visit.push(*next_idx);
            }
        }
    }

    assert_eq!(states.len(), basic_blocks.len());

    basic_blocks.iter().map(|bb| states.remove(&bb.label.idx).unwrap()).collect()
}

impl FuncBodyStream {
    pub fn new(func_ty: TypeOrFuncType, types: &TypeList, func_idx: CodeFuncIdx) -> Self {
        let entry_block = BasicBlock::new(func_idx, 0, OpStack(vec![Type::I32]));

        let mut exit_tys = vec![Type::I32];
        exit_tys.extend(Vec::from(get_output_tys(func_ty, types)));

        let exit_block = BasicBlock::new(func_idx, 1, OpStack(exit_tys));

        FuncBodyStream {
            func_idx,
            // 0 is always the entry point, 1 is always the exit point
            basic_blocks: vec![entry_block, exit_block],
            reachable: vec![true, true],
            bb_index: 0,
            depth: 0,
            op_stack: OpStack(vec![Type::I32]),
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
                let inputs = get_input_tys(ty, &wasm_file.types);
                let outputs = get_output_tys(ty, &wasm_file.types);

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
                let inputs = get_input_tys(ty, &wasm_file.types);
                let outputs = get_output_tys(ty, &wasm_file.types);

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
                let target_tys = get_input_tys(ty, &wasm_file.types);
                let outputs = get_output_tys(ty, &wasm_file.types);

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

            _ => todo!("{:?}", o),
        }
    }
}

fn get_local_ty(mut locals: &[(u32, Type)], mut local_index: u32) -> Type {
    let orig_locals = locals;
    let orig_index = local_index;

    loop {
        if let Some((count, ty)) = locals.first().copied() {
            if count <= local_index {
                local_index -= count;
                locals = &locals[1..];
            } else {
                return ty;
            }
        } else {
            panic!("{:?} {}", orig_locals, orig_index);
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
}

fn get_entry_point(function: CodeFuncIdx) -> String {
    get_block(&Label::new(function, 0))
}

fn get_block(label: &Label) -> String {
    format!("__wasm{}_{}", label.func_idx.0, label.idx)
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
                    if *shared {
                        todo!()
                    }

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
                        cmds.push(format!("fill {} 0 8 {} 255 15 minecraft:jukebox{{RecordItem:{{id:\"minecraft:stone\",Count:1b,tag:{{Memory:0}}}}}} replace", x_offset, x_offset + 8));
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
            let value = eval_init_expr(global.init_expr);
            cmds.push(format!("data modify block {} 0 3 RecordItem.tag.Memory set value {}", i, value));
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

    pub fn is_named_func(&self, code: CodeFuncIdx, name: &str) -> bool {
        let func = self.imports.iter().filter(|i| {
            matches!(i.ty, ImportSectionEntryType::Function(_))
        }).nth(code.0 as usize);

        if let Some(func) = func {
            if func.module == "env" {
                if let Some(field) = func.field {
                    if field == name {
                        return true;  
                    }
                }
            }
        }

        false
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

fn save_datapack(folder_path: &Path, mc_functions: Vec<(String, String)>) {
    let datapack = datapack::Datapack::new();

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

struct FunctionBody<'a> {
    operators: Vec<Operator<'a>>,
    locals: Vec<(u32, Type)>,
}

struct WasmFile<'a> {
    types: TypeList<'a>,
    globals: GlobalList<'a>,
    memory: MemoryList,
    exports: ExportList<'a>,
    imports: ImportList<'a>,
    data: DataList<'a>,
    tables: TableList,
    elements: ElementList<'a>,
    functions: FunctionList,
    bodies: Vec<FunctionBody<'a>>,
}

pub struct RunOptions {
    pub wasm_path: PathBuf,
    pub out_path: Option<PathBuf>,
}

/*
fn link(dst: &mut WasmFile, mut src: WasmFile) {
    let type_offset = dst.types.types.len();

    if !src.exports.exports.is_empty() {
        todo!()
    }
    if !src.globals.globals.is_empty() {
        todo!()
    }
    if !src.memory.memory.is_empty() {
        todo!()
    }
    if !src.imports.imports.is_empty() {
        todo!()
    }
    if !src.data.data.is_empty() {
        todo!()
    }
    if !src.tables.tables.is_empty() {
        todo!()
    }
    if !src.elements.elements.is_empty() {
        todo!()
    }
    if !src.functions.functions.is_empty() {
        todo!()
    }
    if !src.bodies.is_empty() {
        todo!()
    }
}
*/

const VERIFY_OUTPUT: bool = false;

pub fn run(run_options: &RunOptions) {
    let file = std::fs::read(&run_options.wasm_path).unwrap();
    let mut wasm_file = parse_wasm_file(&file);
    link_intrinsics(&mut wasm_file);

    let basic_blocks = compile(&wasm_file);

    let mc_functions = assemble(&basic_blocks, &wasm_file, VERIFY_OUTPUT);

    for func in mc_functions.iter() {
        println!("F: {:?}", func.0)
    }

    let folder_path = run_options.out_path.as_deref().unwrap_or_else(|| Path::new("../out"));

    save_datapack(folder_path, mc_functions.clone());

    if VERIFY_OUTPUT {
        let result = run_and_compare(&basic_blocks, &mc_functions, &wasm_file);

        println!("Returned with {:?}", result);
    }
}

fn run_commands(mc_functions: &[(String, String)], globals: &GlobalList, memory: &MemoryList, exports: &ExportList) -> Interpreter {
    let mut cmd = setup_commands(mc_functions, globals, memory, exports);

    cmd.run_to_end().unwrap();

    cmd
}

fn parse_function(id: FunctionId, contents: &str) -> Function {
    let cmds = contents.lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .map(|l| {
            l.parse::<Command>().unwrap_or_else(|e| {
                panic!("failed to parse {}: {:?}", l, e)
            })
        }).collect::<Vec<_>>();

    Function { id, cmds }
}

fn read_function(path: &Path, id: FunctionId) -> Function {
    let contents = std::fs::read_to_string(path).unwrap();
    parse_function(id, &contents)
}

fn prepare_interp(mc_functions: &[(String, String)]) -> Interpreter {
    let mut funcs = mc_functions.iter()
        .map(|(n, c)| {
            let id = FunctionId {
                namespace: "wasm".into(),
                path: n.clone(),
            };

            println!("Converting {}", id.path);

            parse_function(id, c)
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

                    let id = FunctionId {
                        namespace: "intrinsic".into(),
                        path: n.to_string(),
                    };

                    funcs.push(read_function(&in_entry.path(), id));
                }
            }
        } else {
            let name = entry.file_name();
            let name = name.to_string_lossy();
            let name = &name[..name.len() - ".mcfunction".len()];

            let id = FunctionId {
                namespace: "intrinsic".into(),
                path: name.to_string(),
            };
            
            funcs.push(read_function(&entry.path(), id));
        }
    }

    let idx = mc_functions.iter().enumerate().find(|(_, (n, _))| {
            n == "setup"
        }).unwrap_or_else(|| {
            eprintln!("Failed to find {:?}", "setup");
            panic!();
        }).0;

    let mut i = datapack_vm::Interpreter::new(funcs, idx);

    i.run_to_end().unwrap();

    i
}

fn set_interp_pos(interp: &mut Interpreter, mc_functions: &[(String, String)], name: &str) {
    let idx = mc_functions.iter().enumerate().find(|(_, (n, _))| {
            n == name
        }).unwrap_or_else(|| {
            eprintln!("Failed to find {:?}", name);
            panic!();
        }).0;

    interp.set_pos(idx);
}

fn setup_commands(mc_functions: &[(String, String)], globals: &GlobalList, memory: &MemoryList, exports: &ExportList) -> Interpreter {
    let mut i = prepare_interp(mc_functions);

    set_interp_pos(&mut i, mc_functions, "_start");

    i
}

struct DataList<'a> {
    data: Vec<Data<'a>>,
}

impl<'a> DataList<'a> {
    pub fn new() -> Self {
        DataList { data: Vec::new() }
    }

    pub fn add_data(&mut self, d: Data<'a>) {
        self.data.push(d);
    }

    fn set_word(&self, mut ptr: usize, value: i32) -> String {
        assert_eq!(ptr % 4, 0);
        ptr /= 4;

        let z = (ptr % 8) + 8;
        ptr /= 8;
        let y = ptr % 256;
        ptr /= 256;
        let x = ptr;

        format!("data modify block {} {} {} RecordItem.tag.Memory set value {}", x, y, z, value)
    }

    pub fn init(&self) -> Vec<String> {
        let mut cmds = Vec::new();

        for d in self.data.iter() {
            match d.kind {
                DataKind::Active { memory_index, init_expr } => {
                    assert_eq!(memory_index, 0);

                    let offset = eval_init_expr(init_expr);

                    assert_eq!(offset % 4, 0);

                    // FIXME:
                    let mut data = d.data.to_owned();
                    while data.len() % 4 != 0 {
                        data.push(0);
                    }

                    let mut ptr: usize = offset.try_into().unwrap();
                    for word in data.chunks_exact(4) {
                        let word = word.try_into().unwrap();
                        let word = i32::from_le_bytes(word);

                        cmds.push(self.set_word(ptr, word));

                        ptr += 4;
                    }
                }
                DataKind::Passive => todo!(),
            }
        }

        cmds
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
        self.tables.push(ty);
    }
}

struct ElementList<'a> {
    elements: Vec<Element<'a>>,
}

impl<'a> ElementList<'a> {
    pub fn new() -> Self {
        ElementList { elements: Vec::new() }
    }

    pub fn add_element(&mut self, elem: Element<'a>) {
        self.elements.push(elem);
    }
}

fn parse_wasm_file(file: &[u8]) -> WasmFile {
    let mut types = TypeList::new();
    let mut globals = GlobalList::new();
    let mut memory = MemoryList::new();
    let mut exports = ExportList::new();
    let mut imports = ImportList::new();
    let mut tables = TableList::new();
    let mut data = DataList::new();
    let mut elements = ElementList::new();

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
            Payload::DataSection(d) => {
                for d in d {
                    let d = d.unwrap();
                    data.add_data(d);
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
                let operators = e.get_operators_reader().unwrap()
                    .into_iter()
                    .map(|o| o.unwrap())
                    .collect::<Vec<_>>();
                let locals = e.get_locals_reader().unwrap()
                    .into_iter()
                    .map(|l| l.unwrap())
                    .collect::<Vec<_>>();

                codes.push(FunctionBody { operators, locals });
            }
            Payload::TableSection(t) => {
                for table in t {
                    let table = table.unwrap();
                    tables.add_table(table);
                }
            }
            Payload::ElementSection(e) => {
                for elem in e {
                    let elem = elem.unwrap();
                    elements.add_element(elem);
                }
            }
            _other => {
                println!("{:?}", _other);
            }
        }
    }

    let mut functions = FunctionList::new(); 

    for (i, f) in imports.imports.iter().enumerate() {
        if let ImportSectionEntryType::Function(func) = f.ty {
            functions.add_function(func)
        }

        /*let mut set_turtle_coord = |axis| {
            let mut bb = BasicBlock::new(CodeFuncIdx(i as u32), 0);

            let ret_reg = Register::Work(0);
            bb.instrs = vec![
                Instr::SetTurtleCoord(Register::Param(0), axis),
                // TODO: Dedup
                Instr::Comment(" Pop return address".to_string()),
                Instr::PopI32Into(ret_reg),
                Instr::DynBranch(ret_reg, None),
            ];
            basic_blocks.push(bb);
        };

        if f.module == "env" {
            match f.field {
                Some("turtle_x") => set_turtle_coord(Axis::X),
                Some("turtle_y") => set_turtle_coord(Axis::Y),
                Some("turtle_z") => set_turtle_coord(Axis::Z),
                Some("turtle_set") => {
                    // TODO: Verify parameter types

                    let mut bb = BasicBlock::new(CodeFuncIdx(i as u32), 0);

                    let ret_reg = Register::Work(0);
                    bb.instrs = vec![
                        Instr::SetTurtleBlock(Register::Param(0)),
                        // TODO: Dedup
                        Instr::Comment(" Pop return address".to_string()),
                        Instr::PopI32Into(ret_reg),
                        Instr::DynBranch(ret_reg, None),
                    ];
                    basic_blocks.push(bb);
                }
                Some("store_8") => {
                    // TODO: Verify parameter types

                    let mut bb = BasicBlock::new(CodeFuncIdx(i as u32), 0);

                    let ret_reg = Register::Work(0);
                    bb.instrs = vec![
                        Instr::SetMemPtr(Register::Param(0)),
                        Instr::StoreRow(Register::Param(1)),
                        // TODO: Dedup
                        Instr::Comment(" Pop return address".to_string()),
                        Instr::PopI32Into(ret_reg),
                        Instr::DynBranch(ret_reg, None),
                    ];
                    basic_blocks.push(bb);
                   
                }
                Some("turtle_get") => {
                    basic_blocks.push(BasicBlock::new(CodeFuncIdx(i as u32), 0));
                }
                field => todo!("{:?}", field),
            }
        } else {
            todo!("{:?}", f)
        }*/
    }

    let func_reader = func_reader.unwrap();
    for func in func_reader {
        let func = func.unwrap();

        functions.add_function(func);
    }

    WasmFile { functions, memory, globals, exports, imports, types, tables, data, elements, bodies: codes }
}

fn link_intrinsics(file: &mut WasmFile) {
    /*
    let math = std::fs::read("../math2.wasm").unwrap();
    let math_file = parse_wasm_file(&math);

    link(file, math_file);
    */
}

fn compile(file: &WasmFile) -> Vec<BasicBlock<Instr>> {
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
        for (idx, o) in e.operators.iter().cloned().enumerate() {
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

fn run_ir(basic_blocks: &[BasicBlock<Instr>], file: &WasmFile) -> State {
    let mut state = setup_state(basic_blocks, file);

    loop {
        if state.step() { break }
    }
    
    state

    //state.registers.get(&Register::Return).unwrap().0
}

fn eval_init_expr(expr: InitExpr<'_>) -> i32 {
    let ops = expr.get_operators_reader().into_iter().map(|o| o.unwrap()).collect::<Vec<_>>();

    if let [Operator::I32Const { value }, Operator::End] = &ops[..] {
        *value
    } else {
        todo!("{:?}", ops)
    }
}

fn get_tables(file: &WasmFile, basic_blocks: &[BasicBlock<Instr>]) -> Vec<state::Table> {
    let mut tables = Vec::new();

    for table in file.tables.tables.iter() {
        if table.element_type != Type::FuncRef {
            todo!()
        }

        tables.push(state::Table { elements: vec![None; table.limits.initial as usize] });
    }

    for elem in file.elements.elements.iter() {
        match elem.kind {
            ElementKind::Active { table_index, init_expr } => {
                let offset = eval_init_expr(init_expr);
                let items = elem.items.get_items_reader().unwrap().into_iter().map(|i| i.unwrap());

                let table = &mut tables[table_index as usize];

                for (idx, item) in items.enumerate() {
                    match item {
                        ElementItem::Null(ty) => todo!("{:?}", ty),
                        ElementItem::Func(f) => {
                            table.elements[offset as usize + idx] = Some(CodeFuncIdx(f));
                        }
                    }
                }
            }
            _ => todo!()
        }
    }

    tables
}

fn prepare_state(basic_blocks: &[BasicBlock<Instr>], file: &WasmFile) -> State {
    let tables = get_tables(file, basic_blocks);

    let mut state = State::new(basic_blocks.to_owned(), &file.globals, &file.memory, tables);

    for (i, global) in file.globals.globals.iter().enumerate() {
        state.globals[i].0 = eval_init_expr(global.init_expr);
    }

    for d in file.data.data.iter() {
        match d.kind {
            DataKind::Active { memory_index, init_expr } => {
                assert_eq!(memory_index, 0);

                let offset: usize = eval_init_expr(init_expr).try_into().unwrap();
                
                assert_eq!(offset % 4, 0);

                let page = &mut state.memory[offset / 65536];

                page[offset % 65536..][..d.data.len()].copy_from_slice(d.data);
            }
            DataKind::Passive => todo!(),
        }
    }

    state
}

fn set_state_pos(state: &mut State, exports: &ExportList, name: &str) {
    let idx = State::get_pc(&state.bbs, &Label::new(exports.get_func(name), 0));

    state.enter(idx);
}

fn block_pos_from_idx(idx: usize, redstone: bool) -> String {
    let x = (idx as i32) % 48;
    let y = if redstone { 1 } else { 0 };
    let z = -1 - ((idx as i32) / 48);

    format!("{} {} {}", x, y, z)
}

fn setup_state(basic_blocks: &[BasicBlock<Instr>], file: &WasmFile) -> State {
    let mut state = prepare_state(basic_blocks, file);

    set_state_pos(&mut state, &file.exports, "_start");


    /*state.call(state.get_pc(&Label::new(exports.get_func("__wasm_call_ctors"), 0)));

    println!("=============");

    state.call(state.get_pc(&Label::new(exports.get_func("main"), 0)));*/

    state

}

struct BBGroupBy<'a> {
    list: &'a [BasicBlock<Instr>]
}

impl<'a> Iterator for BBGroupBy<'a> {
    type Item = &'a [BasicBlock<Instr>];

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((head, tail)) = self.list.split_first() {
            let to_match = head.label.func_idx;

            let mut idx = 0;
            for (i, elem) in tail.iter().enumerate() {
                if elem.label.func_idx == to_match {
                    idx = i + 1;
                }
            }

            let result = &self.list[..idx + 1];
            self.list = &tail[idx..];

            Some(result)
        } else {
            None
        }
    }
}

fn get_stack_states(basic_blocks: &[BasicBlock<Instr>]) -> Vec<StateInfo> {
    let mut stack_states = Vec::new();
    
    let iter = BBGroupBy { list: basic_blocks };

    for group in iter {
        stack_states.extend(find_real_stack_sizes(group));
    }

    stack_states
}

fn assemble(basic_blocks: &[BasicBlock<Instr>], file: &WasmFile, insert_sync: bool) -> Vec<(String, String)> {
    let mut mc_functions = Vec::new();

    let stack_states = get_stack_states(basic_blocks);

    for (bb_idx, bb) in basic_blocks.iter().enumerate() {
        let mut new_block = bb.lower(&file.globals, bb_idx, insert_sync, Some(&stack_states[bb_idx]));
        let name = get_block(&new_block.label);

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

    let dyn_branch = create_return_jumper(&basic_blocks);
    mc_functions.push(("dyn_branch".to_string(), dyn_branch));

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
    kill @e[tag=turtle]\n\
    kill @e[tag=nextchain]\n\
    \n\
    # Add armor stand pointers\n\
    summon minecraft:armor_stand 0 0 8 {Marker:1b,Tags:[\"memoryptr\"],CustomName:'\"memoryptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 1 {Marker:1b,Tags:[\"localptr\"],CustomName:'\"localptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 1 {Marker:1b,Tags:[\"frameptr\"],CustomName:'\"frameptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 0 {Marker:1b,Tags:[\"stackptr\"],CustomName:'\"stackptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 3 {Marker:1b,Tags:[\"globalptr\"],CustomName:'\"globalptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 -2 {Marker:1b,Tags:[\"turtle\"],CustomName:'\"turtle\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 1 1 -1 {Marker:1b,Tags:[\"nextchain\"],CustomName:'\"nextchain\"',CustomNameVisible:1b}\n\
    
    scoreboard players set %stackptr wasm 0
    scoreboard players set %frameptr wasm 0
    
    scoreboard players set %%-1 reg -1
    scoreboard players set %%0 reg 0
    scoreboard players set %%1 reg 1
    scoreboard players set %%2 reg 2
    scoreboard players set %%4 reg 4
    scoreboard players set %%8 reg 8
    scoreboard players set %%16 reg 16
    scoreboard players set %%32 reg 32
    scoreboard players set %%64 reg 64
    scoreboard players set %%SIXTEEN reg 16
    scoreboard players set %%256 reg 256
    scoreboard players set %%65536 reg 65536
    scoreboard players set %%16777216 reg 16777216 
    scoreboard players set %%1073741824 reg 1073741824
    scoreboard players set %%-2147483648 reg -2147483648
    ".to_string();

    setup.push_str("\n# Make stack\n");
    setup.push_str("fill 0 0 0 30 0 0 minecraft:air replace\n");
    setup.push_str("fill 0 0 0 30 0 0 minecraft:jukebox{RecordItem:{id:\"minecraft:stone\",Count:1b,tag:{Memory:0}}} replace\n");
    
    setup.push_str("\n# Make condition stack\n");
    setup.push_str("fill 0 3 0 100 3 0 minecraft:air replace\n");
    setup.push_str("fill 0 3 0 100 3 0 minecraft:jukebox{RecordItem:{id:\"minecraft:stone\",Count:1b,tag:{Memory:0}}} replace\n");

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

    mc_functions.push(("setup".to_string(), setup));

    if BRANCH_CONV == BranchConv::Chain {
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

        mc_functions.push(("setupchain".to_string(), setup_chain));
    }

    let tables = get_tables(file, basic_blocks);
    for (idx, table) in tables.iter().enumerate() {
        let dyn_branch = create_dyn_jumper(table);
        mc_functions.push((format!("dyn_branch{}", idx), dyn_branch));
    }

    do_fixups(&mut mc_functions);

    for export in file.exports.exports.iter() {
        if let ExternalKind::Function = export.kind {
            let index = CodeFuncIdx(export.index);

            let name = get_entry_point(index);
            let idx = mc_functions.iter().enumerate().find(|(_, (n, _))| {
                n == &name
            }).unwrap().0;

            let func_ty = file.functions.functions[index.0 as usize];

            let mut f = FuncBodyStream::new(TypeOrFuncType::FuncType(func_ty), &file.types, index);
            f.basic_blocks.push(BasicBlock::new(CodeFuncIdx(0), 0, OpStack::new()));

            if BRANCH_CONV != BranchConv::Direct {
                f.push_instr(Instr::PushI32Const(-1));
            }

            // FIXME:
            if export.field == "main" {
                f.push_instr(Instr::PushI32Const(0));
                f.push_instr(Instr::PopI32Into(Register::Param(0)));
                f.push_instr(Instr::PushI32Const(0));
                f.push_instr(Instr::PopI32Into(Register::Param(1)));
            }

            //assert_eq!(f.basic_blocks.len(), 1);

            let mut body = CodeEmitter::emit_all(&f.basic_blocks[0], &file.globals, None, false, false, None);

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

            let content = body.join("\n");

            mc_functions.push((export.field.to_string(), content));
        }
    }


    mc_functions
}

fn create_return_jumper(basic_blocks: &[BasicBlock<Instr>]) -> String {
    let targets = basic_blocks.iter()
        .map(|bb| BranchTarget {
            label: bb.label.clone(),
            to_pop: 0,
            ty: Box::new([]),
        })
        .collect::<Vec<_>>();
    
    let targets = targets.iter().map(Some);

    let mut emitter = CodeEmitter::new(None, false, false, None, None);
    emitter.lower_branch_table(Register::Work(0), targets, None);
    let dyn_branch = emitter.finalize();

    dyn_branch.join("\n")
}

fn create_dyn_jumper(table: &state::Table) -> String {
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

    let mut emitter = CodeEmitter::new(None, false, false, None, None);
    emitter.lower_branch_table(Register::Work(0), targets, None);
    let dyn_branch = emitter.finalize();
    
    dyn_branch.join("\n")
}

fn get_entry_index(func: CodeFuncIdx, mc_functions: &[(String, String)]) -> usize {
    let name = get_block(&Label::new(func, 0));
    get_entry_index_named(&name, mc_functions)
}

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

fn count_intrinsic(name: &str, params: &[(&str, i32)]) -> usize {
    let globals = GlobalList::new();
    let memory = MemoryList::new();

    // TODO: Dedup
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
    kill @e[tag=turtle]\n\
    kill @e[tag=nextchain]\n\
    \n\
    # Add armor stand pointers\n\
    summon minecraft:armor_stand 0 0 8 {Marker:1b,Tags:[\"memoryptr\"],CustomName:'\"memoryptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 1 {Marker:1b,Tags:[\"localptr\"],CustomName:'\"localptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 1 {Marker:1b,Tags:[\"frameptr\"],CustomName:'\"frameptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 0 {Marker:1b,Tags:[\"stackptr\"],CustomName:'\"stackptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 3 {Marker:1b,Tags:[\"globalptr\"],CustomName:'\"globalptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 -2 {Marker:1b,Tags:[\"turtle\"],CustomName:'\"turtle\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 1 1 -1 {Marker:1b,Tags:[\"nextchain\"],CustomName:'\"nextchain\"',CustomNameVisible:1b}\n\
    
    scoreboard players set %stackptr wasm 0
    scoreboard players set %frameptr wasm 0
    
    scoreboard players set %%-1 reg -1
    scoreboard players set %%0 reg 0
    scoreboard players set %%1 reg 1
    scoreboard players set %%2 reg 2
    scoreboard players set %%4 reg 4
    scoreboard players set %%8 reg 8
    scoreboard players set %%16 reg 16
    scoreboard players set %%32 reg 32
    scoreboard players set %%64 reg 64
    scoreboard players set %%SIXTEEN reg 16
    scoreboard players set %%256 reg 256
    scoreboard players set %%65536 reg 65536
    scoreboard players set %%16777216 reg 16777216 
    scoreboard players set %%1073741824 reg 1073741824
    scoreboard players set %%-2147483648 reg -2147483648
    ".to_string();

    setup.push_str("\n# Make stack\n");
    setup.push_str("fill 0 0 0 30 0 0 minecraft:air replace\n");
    setup.push_str("fill 0 0 0 30 0 0 minecraft:jukebox{RecordItem:{id:\"minecraft:stone\",Count:1b,tag:{Memory:0}}} replace\n");
    
    setup.push_str("\n# Make condition stack\n");
    setup.push_str("fill 0 3 0 100 3 0 minecraft:air replace\n");
    setup.push_str("fill 0 3 0 100 3 0 minecraft:jukebox{RecordItem:{id:\"minecraft:stone\",Count:1b,tag:{Memory:0}}} replace\n");

    let mc_functions = [("setup".to_string(), setup)];

    let mut interp = prepare_interp(&mc_functions);

    let idx = interp.program.iter().enumerate().find(|(_, f)| {
        &*f.id.namespace == "intrinsic" && f.id.path == name
    }).unwrap().0;

    let obj = Objective::new("reg".to_string()).unwrap();

    for (name, val) in params {
        let holder = ScoreHolder::new(name.to_string()).unwrap();
        interp.scoreboard.set(&holder, &obj, *val);
    }

    interp.set_pos(idx);

    interp.run_to_end().unwrap();

    interp.last_commands_run
}

fn get_intrinsic_counts() -> HashMap<&'static str, usize> {
    let mut result = HashMap::new();

    result.insert("and", count_intrinsic("and", &[("%param0%0", 0), ("%param1%0", 0)]));
    result.insert("ashr_i64", count_intrinsic("ashr_i64", &[("%param0%0", -1), ("%param0%1", 1), ("%param1%0", 63)]));
    result.insert("clz", count_intrinsic("clz", &[("%param0%0", 0)]));
    result.insert("ctz", count_intrinsic("ctz", &[("%param0%0", 0)]));

    // TODO: i64_sdiv, i64_srem, i64_udiv, i64_urem

    result.insert("i64_sdiv", count_intrinsic("i64_sdiv", &[("%param%0%lo", -1), ("%param%0%hi", -1), ("%param%1%lo", -1), ("%param%1%hi", -1)]));
    result.insert("i64_srem", count_intrinsic("i64_srem", &[("%param%0%lo", -1), ("%param%0%hi", -1), ("%param%1%lo", -1), ("%param%1%hi", -1)]));
    result.insert("i64_udiv", count_intrinsic("i64_udiv", &[("%param%0%lo", -1), ("%param%0%hi", -1), ("%param%1%lo", -1), ("%param%1%hi", -1)]));
    result.insert("i64_urem", count_intrinsic("i64_urem", &[("%param%0%lo", -1), ("%param%0%hi", -1), ("%param%1%lo", -1), ("%param%1%hi", -1)]));

    // TODO: Memory ops

    result.insert("lshr_i64", count_intrinsic("lshr_i64", &[("%param0%0", -1), ("%param0%1", 1), ("%param1%0", 63)]));
    result.insert("lshr", count_intrinsic("lshr", &[("%param0%0", -1), ("%param1%0", 31)]));

    // TODO: is this worst-case?
    result.insert("mul_32_to_64", count_intrinsic("mul_32_to_64", &[("%param0%0", -1), ("%param1%0", -1)]));

    result.insert("or", count_intrinsic("or", &[("%param0%0", 0), ("%param1%0", 0)]));
    result.insert("popcnt", count_intrinsic("popcnt", &[("%param0%0", -1)]));
    result.insert("rotl_64", count_intrinsic("rotl_64", &[("%param0%0", -1), ("%param0%1", -1), ("%param1%0", 63)]));
    result.insert("rotl", count_intrinsic("rotl", &[("%param0%0", -1), ("%param1%0", 63)]));
    result.insert("rotr_64", count_intrinsic("rotr_64", &[("%param0%0", -1), ("%param0%1", -1), ("%param1%0", 63)]));
    result.insert("rotr", count_intrinsic("rotr", &[("%param0%0", -1), ("%param1%0", 63)]));
    result.insert("shl_64", count_intrinsic("shl_64", &[("%param0%0", -1), ("%param0%1", 1), ("%param1%0", 63)]));
    result.insert("shl", count_intrinsic("shl", &[("%param0%0", -1), ("%param1%0", 31)]));
    result.insert("xor", count_intrinsic("xor",&[("%param0%0", 0), ("%param1%0", -1)]));

    println!("{:?}", result);

    result
}

static INTRINSIC_COUNTS: OnceCell<HashMap<&'static str, usize>> = OnceCell::new();

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

use datapack_vm::interpreter::InterpError;

fn run_and_compare2(mir: &mut State, cmd: &mut Interpreter, return_types: &[Type]) -> Vec<WasmValue> {
    loop {
        let pc = mir.pc.0.last().copied();

        match cmd.run_to_end() {
            Err(InterpError::SyncHit(f, i)) => {
                let top_func = cmd.get_top_func();

                let pc = pc.expect("sync hit but MIR halted");

                if pc == (f, i) {
                    compare_states(&mir, &cmd);

                    println!();
                    mir.step();
                }

                cmd.finish_unwind(top_func);
            }
            Ok(()) => {
                assert!(pc.is_none());

                compare_states(&mir, &cmd);

                break;
            }
            Err(e) => todo!("{:?}", e)
        }
    }

    /*match cmd.run_to_end() {
        Err(InterpError::SyncHit(f, i)) => {
            let top_func = cmd.get_top_func();
            compare_states(&mir, &cmd);
            println!();
            cmd.finish_unwind(top_func);

            if cmd.halted() {
                panic!();
            }
        }
        Ok(()) => {
            panic!();
        }
        Err(e) => todo!("{:?}", e)
    }


    loop {
        //println!("Stepping MIR, pc is {:?}", mir.pc);

        let mir_halted = mir.step();

        match cmd.run_to_end() {
            Err(InterpError::SyncHit(f, i)) => {
                assert_eq!((f, i), *mir.pc.0.last().unwrap());

                let top_func = cmd.get_top_func();
                compare_states(&mir, &cmd);
                println!();
                cmd.finish_unwind(top_func);

                if cmd.halted() {
                    assert!(mir_halted);
                    break;
                }
            }
            Ok(()) => {

                assert!(mir_halted);
                break;
            }
            Err(e) => todo!("{:?}", e)
        }

        assert!(!mir_halted);

        //println!();

    }

    compare_states(&mir, &cmd);*/

    return_types.iter().enumerate()
        .map(|(idx, ty)| {
            mir.registers.get_typed(Register::Return(idx as u32), *ty)
        })
        .collect()
}

fn run_and_compare(basic_blocks: &[BasicBlock<Instr>], mc_functions: &[(String, String)], wasm_file: &WasmFile) -> Vec<WasmValue> {
    let mut mir = setup_state(&basic_blocks, wasm_file);

    let mut cmd = setup_commands(&mc_functions, &wasm_file.globals, &wasm_file.memory, &wasm_file.exports);

    let func_idx = wasm_file.exports.get_func("_start");
    let func_ty = wasm_file.functions.get_function_type(func_idx, &wasm_file.types);

    run_and_compare2(&mut mir, &mut cmd, &func_ty.returns)
}

fn compare_states(mir: &State, cmd: &Interpreter) {
    let mut diffs = Vec::new();

    /*
    for (name, value) in cmd.scoreboard.0.get("reg").unwrap().iter() {
        if let Ok(reg) = name.as_ref().parse::<HalfRegister>() {
            let mir_val = mir.registers.get_half(reg)/*.unwrap_or_else(|| panic!("Uninit reg {:?} (hi:){}", reg, is_hi))*/;

            if mir_val != *value {
                diffs.push((reg, mir_val, *value));
            }
        }
    }
    */

    let obj = cmd.scoreboard.0.get("reg").unwrap();

    for (&reg, &mir_value) in mir.registers.0.iter() {
        let holder = ScoreHolder::new(reg.to_string()).unwrap();
        let cmd_value = obj.get(&holder).copied();

        if Some(mir_value) != cmd_value {
            diffs.push((reg, mir_value, cmd_value));
        }
    }
    
    if !diffs.is_empty() {
        println!("=== Registers ===");
        println!("-- MIR --");
        let mut mir_regs = mir.registers.0.iter().collect::<Vec<_>>();
        mir_regs.sort();
        for (&reg, &mir_value) in mir_regs {
            println!("{} : {}", reg, mir_value);
        }
        println!("-- CMD --");
        let mut cmd_regs = obj.iter()
            .filter_map(|(r, v)| {
                if let Ok(reg) = r.as_ref().parse::<HalfRegister>() {
                    Some((reg, v))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        cmd_regs.sort();
        for (reg, cmd_value) in cmd_regs {
            println!("{} : {}", reg, cmd_value);
        }
        println!();

        println!("=== Diffs === ");
        for (name, mir, cmd) in diffs.iter() {
            println!("{} : {} (mir) vs {:?} (cmd)", name, mir, cmd);
        }
        println!();

        panic!("MIR registers and CMD registers differed");
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use wasmparser::ResizableLimits;
    use std::path::Path;

    fn run_wasm_file(path: &Path) -> i32 {
        use wasmtime::{Engine, Module, Instance, Store};

        let engine = Engine::default();
        let wasm = std::fs::read(path).unwrap();
        let module = Module::new(&engine, wasm).unwrap();
        
        let mut store = Store::new(&engine, 0);
        let instance = Instance::new(&mut store, &module, &[]).unwrap();
        let start_func = instance.get_typed_func::<(), i32, _>(&mut store, "_start").unwrap();
        start_func.call(&mut store, ()).unwrap()
    }

    fn test_whole_program(path: &Path, expected: i32) {
        test_whole_program2(path, (expected, None))
    }

    fn test_whole_program2(path: &Path, expected: (i32, Option<i32>)) {

        let file = std::fs::read(path).unwrap();
        let mut wasm_file = parse_wasm_file(&file);
        link_intrinsics(&mut wasm_file);

        let basic_blocks = compile(&wasm_file);

        let mc_functions = assemble(&basic_blocks, &wasm_file, true);

        for func in mc_functions.iter() {
            println!("F: {:?}", func.0)
        }


        let func_ty = wasm_file.functions.get_function_type(wasm_file.exports.get_func("_start"), &wasm_file.types);

        let result = run_and_compare(&basic_blocks, &mc_functions, &wasm_file);

        match &result[..] {
            [WasmValue::I32(i)] => {
                assert!(expected.1.is_none());
                assert_eq!(*i, expected.0);
            }
            [WasmValue::I64(i)] => {
                let hi = expected.1.unwrap();
                let expected = ((hi as u32 as i64) << 32) | (expected.0 as u32 as i64);
                assert_eq!(*i, expected);
            }
            r => todo!("{:?}", r)
        }

        //assert_eq!(run_wasm_file(path), expected);
    }

    fn test_mir<R, I>(program: Vec<Instr>, expected: R)
        where
            R: IntoIterator<Item=I>,
            I: Into<WasmValue>,
    {
        let mut bb = BasicBlock::new(CodeFuncIdx(0), 0, OpStack::new());
        bb.instrs = program;

        let basic_blocks = [bb];

        let globals = GlobalList::new();
        let mut memory = MemoryList::new();
        memory.add_memory(MemoryType::M32 {
            limits: ResizableLimits {
                initial: 1,
                maximum: Some(1),
            },
            shared: false,
        });
        let mut exports = ExportList::new();
        exports.add_export(Export {
            index: 0,
            field: "_start",
            kind: ExternalKind::Function,
        });
        let data = DataList::new();
        let mut functions = FunctionList::new();
        functions.add_function(0);
        let mut types = TypeList::new();
        types.add_type(TypeDef::Func(FuncType {
            params: Vec::new().into_boxed_slice(),
            returns: vec![Type::I32].into_boxed_slice(),
        }));

        let file = WasmFile {
            types, globals, exports, data, functions, memory, imports: ImportList::new(), tables: TableList::new(), elements: ElementList::new(), bodies: Vec::new(),
        };

        let mc_functions = assemble(&basic_blocks, &file, true);

        let mut mir = setup_state(&basic_blocks, &file);

        let mut cmd = setup_commands(&mc_functions, &file.globals, &file.memory, &file.exports);

        let expected = expected.into_iter().map(|e| e.into()).collect::<Vec<WasmValue>>();

        let return_types = expected.iter().map(|e| e.ty()).collect::<Vec<_>>();

        let actual_vals = run_and_compare2(&mut mir, &mut cmd, &return_types);

        assert_eq!(expected, actual_vals);
    }


    #[test]
    #[ignore]
    fn chip8test() {
        test_whole_program(Path::new("../CHIP-8-Emulator/chip8.wasm"), 0);
    }

    #[test]
    fn while_loop() {
        test_whole_program(Path::new("../while.wasm"), 0);
    }

    #[test]
    fn i64_ucomp() {
        let test_vals = [
            0, -1, 1, i64::MIN, i64::MAX, 1234567898765432123, 42, -34394854589489,
        ];

        for lhs in test_vals.iter().copied() {
            for rhs in test_vals.iter().copied() {
                i64_ucomp_single(lhs, rhs);
            }
        }
    }

    #[test]
    fn i64_scomp() {
        let test_vals = [
            0, -1, 1, i64::MIN, i64::MAX, 1234567898765432123, 42, -34394854589489,
        ];

        for lhs in test_vals.iter().copied() {
            for rhs in test_vals.iter().copied() {
                i64_scomp_single(lhs, rhs);
            }
        }
    }

    fn i64_ucomp_single(lhs: i64, rhs: i64) {
        let expected = (lhs as u64) < (rhs as u64);

        let mut instrs = Vec::new();
        instrs.extend(Instr::set_i64_const(Register::Work(0), lhs));
        instrs.extend(Instr::set_i64_const(Register::Work(1), rhs));
        instrs.push(Instr::I64UComp { dst: Register::Return(0), lhs: Register::Work(0), op: Relation::LessThan, rhs: Register::Work(1) });

        test_mir(instrs, Some(expected as i32));
    }

    fn i64_scomp_single(lhs: i64, rhs: i64) {
        let expected = lhs < rhs;

        let mut instrs = Vec::new();
        instrs.extend(Instr::set_i64_const(Register::Work(0), lhs));
        instrs.extend(Instr::set_i64_const(Register::Work(1), rhs));
        instrs.push(Instr::I64SComp { dst: Register::Return(0), lhs: Register::Work(0), op: Relation::LessThan, rhs: Register::Work(1) });

        test_mir(instrs, Some(expected as i32));
    }


    #[test]
    fn i64_mul2() {
        let test_vals = [
            0, -1, 1, i64::MIN, i64::MAX, 1234567898765432123, 42, -34394854589489,
        ];

        for lhs in test_vals.iter().copied() {
            for rhs in test_vals.iter().copied() {
                i64_mul_single(lhs, rhs);
            }
        }
    }

    #[test]
    fn i64_neg() {
        let test_vals = [
            0, -1, 1, i64::MIN, i64::MAX, 1234567898765432123, 42, -34394854589489,
        ];

        for val in test_vals.iter().copied() {
            i64_neg_single(val);
        }
    }

    fn i64_neg_single(val: i64) {
        let expected = val.wrapping_neg();

        let mut instrs = Vec::new();
        instrs.extend(Instr::set_i64_const(Register::Return(0), val));
        instrs.extend(Instr::i64_neg(Register::Return(0)));

        test_mir(instrs, Some(expected));
    }

    fn i64_mul_single(lhs: i64, rhs: i64) {
        let expected = lhs.wrapping_mul(rhs);

        let mut instrs = Vec::new();
        instrs.extend(Instr::set_i64_const(Register::Work(0), lhs));
        instrs.extend(Instr::set_i64_const(Register::Work(1), rhs));
        instrs.extend(Instr::i64_mul(Register::Return(0), Register::Work(0), Register::Work(1)));

        test_mir(instrs, Some(expected));
    }

    #[test]
    fn i64_udiv() {
        /*for rhs in IntoIterator::into_iter([1, -1, i64::MAX, i64::MIN, 42]) {
            i64_udiv_single(0, rhs);
        }

        for lhs in IntoIterator::into_iter([1, -1, i64::MAX, i64::MIN, 42]) {
            i64_udiv_single(lhs, 1);
        }*/

        i64_udiv_single(-1, 1);
    }

    fn i64_udiv_single(lhs: i64, rhs: i64) {
        let expected = ((lhs as u64) / (rhs as u64)) as i64;

        let mut instrs = Vec::new();
        instrs.extend(Instr::set_i64_const(Register::Param(0), lhs));
        instrs.extend(Instr::set_i64_const(Register::Param(1), rhs));
        instrs.push(Instr::I64DivU { dst: Register::Return(0), lhs: Register::Param(0), rhs: Register::Param(1), });

        test_mir(instrs, Some(expected));
    }

    #[test]
    fn i64_mul() {
        let expected = -5815198268866991520_i64;
        let lo = expected as i32;
        let hi = Some((expected >> 32) as i32);
        test_whole_program2(Path::new("../i64_mul.wasm"), (lo, hi));
    }

    #[test]
    fn stack_elim() {
        test_whole_program(Path::new("./tests/stack_elim.wasm"), 42);
    }

    #[test]
    #[ignore]
    fn block_return() {
        test_whole_program(Path::new("./tests/block_return.wasm"), 24);
    }

    #[test]
    fn nested_loop() {
        test_whole_program(Path::new("./tests/nested_loop.wasm"), 18);
    }

    #[test]
    fn memtest() {
        test_whole_program(Path::new("./tests/memtest.wasm"), 42);
    }

    #[test]
    fn return_42() {
        test_whole_program(Path::new("./tests/return_42.wasm"), 42);
    }

    #[test]
    fn arithmetic() {
        test_whole_program(Path::new("./tests/arithmetic.wasm"), -92);
    }

    #[test]
    fn arithmetic2() {
        test_whole_program(Path::new("./tests/arithmetic2.wasm"), -11);
    }

    #[test]
    fn for_loop() {
        test_whole_program(Path::new("./tests/for_loop.wasm"), 45);
    }

    #[test]
    fn endian() {
        let val1 = 0x01_AB_CD_EF_i32;
        let val1_bts = val1.to_le_bytes();

        let val2 = 0x12_34_56_78_i32;

        let prelude = vec![
            Instr::PushI32Const(4),
            Instr::PopI32Into(Register::Work(0)),
            Instr::SetMemPtr(Register::Work(0)),
            
            Instr::PushI32Const(val1),
            Instr::PopI32Into(Register::Work(1)),
            Instr::StoreI32(Register::Work(1), 2),

            Instr::PushI32Const(8),
            Instr::PopI32Into(Register::Work(0)),
            Instr::SetMemPtr(Register::Work(0)),
            
            Instr::PushI32Const(val2),
            Instr::PopI32Into(Register::Work(1)),
            Instr::StoreI32(Register::Work(1), 2),
        ];

        let mut load_byte = prelude.clone();
        load_byte.extend([
            Instr::PushI32Const(5),
            Instr::PopI32Into(Register::Work(0)),
            Instr::SetMemPtr(Register::Work(0)),

            Instr::LoadI32_8U(Register::Return(0), 0),
        ]);
        test_mir(load_byte, Some(val1_bts[1] as u32 as i32));

        let mut load_halfword_unalign = prelude.clone();
        load_halfword_unalign.extend([
            Instr::PushI32Const(5),
            Instr::PopI32Into(Register::Work(0)),
            Instr::SetMemPtr(Register::Work(0)),

            Instr::LoadI32_16U(Register::Return(0), 0),
        ]);
        test_mir(load_halfword_unalign, Some(u16::from_le_bytes([val1_bts[1], val1_bts[2]]) as u32 as i32));

        let mut load_halfword_align = prelude.clone();
        load_halfword_align.extend([
            Instr::PushI32Const(6),
            Instr::PopI32Into(Register::Work(0)),
            Instr::SetMemPtr(Register::Work(0)),

            Instr::LoadI32_16U(Register::Return(0), 0),
        ]);
        test_mir(load_halfword_align, Some(u16::from_le_bytes([val1_bts[2], val1_bts[3]]) as u32 as i32));

        let mut load_word_unalign = prelude;
        load_word_unalign.extend([
            Instr::PushI32Const(7),
            Instr::PopI32Into(Register::Work(0)),
            Instr::SetMemPtr(Register::Work(0)),

            Instr::LoadI32(Register::Return(0), 0),
           
        ]);
        test_mir(load_word_unalign, Some(0x34_56_78_01_i32));
    }

    #[test]
    fn load_store() {
        let instrs = vec![
            Instr::PushI32Const(4),
            Instr::PopI32Into(Register::Work(0)),
            Instr::SetMemPtr(Register::Work(0)),

            Instr::PushI32Const(42),
            Instr::PopI32Into(Register::Work(1)),
            Instr::StoreI32(Register::Work(1), 2),

            Instr::PushI32Const(4),
            Instr::PopI32Into(Register::Work(0)),
            Instr::SetMemPtr(Register::Work(0)),

            Instr::LoadI32(Register::Return(0), 2),
        ];

        test_mir(instrs, Some(42_i32));
    }

    #[test]
    fn locals() {
        let instrs = vec![
            Instr::PushFrame(2),

            Instr::PushI32Const(42),
            Instr::PopI32Into(Register::Work(0)),
            Instr::SetLocalPtr(0),
            Instr::StoreLocalI32(Register::Work(0)),

            Instr::PushI32Const(27),
            Instr::PopI32Into(Register::Work(0)),
            Instr::SetLocalPtr(1),
            Instr::StoreLocalI32(Register::Work(0)),

            Instr::SetLocalPtr(0),
            Instr::LoadLocalI32(Register::Work(0)),
            Instr::SetLocalPtr(1),
            Instr::LoadLocalI32(Register::Work(1)),

            Instr::I32Op { dst: Register::Work(2).as_lo(), lhs: Register::Work(0).as_lo(), op: "/=", rhs: Register::Work(1).as_lo() },

            Instr::PushI32From(Register::Work(2)),
            Instr::PopI32Into(Register::Return(0)),

            Instr::PopFrame(2),
        ];

        test_mir(instrs, Some(1_i32));
    }

    // TODO: Add a test for division signs

    // TODO: Test I64 loads and stores


    fn load_state(data: &[u8]) -> TestState {
        let mut wasm_file = parse_wasm_file(data);
        link_intrinsics(&mut wasm_file);

        let basic_blocks = compile(&wasm_file);

        let mc_functions = assemble(&basic_blocks, &wasm_file, true);

        let state = prepare_state(&basic_blocks, &wasm_file);
        let interp = prepare_interp(&mc_functions);

        //save_datapack(Path::new("../out"), mc_functions.clone(), &wasm_file);
    
        TestState { interp, state, basic_blocks, mc_functions, wasm_file }
    }

    /*
    fn test_whole_program2(path: &Path, expected: i32) {
        let result = run_and_compare(&basic_blocks, &mc_functions, &wasm_file);

        assert_eq!(result, expected);

        assert_eq!(run_wasm_file(path), expected);
    }
    */


    struct TestState<'a> {
        interp: Interpreter,
        state: State,
        basic_blocks: Vec<BasicBlock<Instr>>,
        mc_functions: Vec<(String, String)>,
        wasm_file: WasmFile<'a>,
    }

    use std::convert::TryFrom;
    use crate::sexpr::SExpr;

    impl TestState<'_> {
        pub fn eval_expr(&mut self, s: &SExpr) -> Vec<WasmValue> {
            match s {
                /*SExpr::Int(i) => {
                    WasmValue::I32(i32::try_from(*i).unwrap())
                }*/
                SExpr::Node { name, params } if name == "i32.const" => {
                    assert_eq!(params.len(), 1);
                    if let SExpr::Int(i) = &params[0] {
                        let i = if *i < 0 {
                            i32::try_from(*i).unwrap()
                        } else {
                            u32::try_from(*i).unwrap() as i32
                        };
                        vec![WasmValue::I32(i)]
                    } else {
                        panic!()
                    }
                }
                SExpr::Node { name, params } if name == "i64.const" => {
                    assert_eq!(params.len(), 1, "{:?}", params);
                    if let SExpr::Int(i) = &params[0] {
                        vec![WasmValue::I64(*i)]
                    } else {
                        panic!()
                    }
                }
                SExpr::Node { name, params } if name == "invoke" => {
                    let name = if let SExpr::String(n) = &params[0] {
                        n
                    } else {
                        panic!()
                    };

                    let args = params[1..].iter().map(|p| self.eval_expr(p)).collect::<Vec<_>>();

                    for (idx, arg) in args.iter().enumerate() {
                        match &arg[..] {
                            [WasmValue::I32(arg)] => {
                                self.state.registers.set_i32(Register::Param(idx as u32), *arg);
                                let holder = ScoreHolder::new(Register::Param(idx as u32).get_lo()).unwrap();
                                let obj = Objective::new("reg".to_string()).unwrap();
                                self.interp.scoreboard.set(&holder, &obj, *arg);
                            }
                            [WasmValue::I64(arg)] => {
                                self.state.registers.set_i64(Register::Param(idx as u32), *arg);
                                let obj = Objective::new("reg".to_string()).unwrap();
                                let holder_lo = ScoreHolder::new(Register::Param(idx as u32).get_lo()).unwrap();
                                let arg_lo = *arg as i32;
                                self.interp.scoreboard.set(&holder_lo, &obj, arg_lo);
                                let holder_hi = ScoreHolder::new(Register::Param(idx as u32).get_hi()).unwrap();
                                let arg_hi = (*arg >> 32) as i32;
                                self.interp.scoreboard.set(&holder_hi, &obj, arg_hi);

                                println!("arg: {}", arg);
                            }
                            _ => todo!()
                        }
                    }

                    set_state_pos(&mut self.state, &self.wasm_file.exports, name);

                    set_interp_pos(&mut self.interp, &self.mc_functions, name);

                    let func_ty = self.wasm_file.functions.get_function_type(self.wasm_file.exports.get_func(name), &self.wasm_file.types);

                    run_and_compare2(&mut self.state, &mut self.interp, &func_ty.returns)
                }
                _ => todo!("{:?}", s)
            }
        }

        pub fn run_check(&mut self, s: &str) {
            let orig_s = s;
            let s: SExpr = s.parse().unwrap();

            match s {
                SExpr::AssertReturn(lhs, rhs) => {
                    println!("\n=========");
                    println!("Assert return {:?} {:?}", lhs, rhs);
                    let l = self.eval_expr(&lhs);

                    let r = rhs.iter().flat_map(|r| self.eval_expr(r)).collect::<Vec<_>>();

                    if l != r {
                        println!("In test {:?}", orig_s);
                        
                        println!("Actual:   {:?}", l);
                        println!("Expected: {:?}", r);
                        panic!();
                    }
                }
                SExpr::AssertTrap(lhs, error) => {
                    println!("TODO: ASSERT TRAP {:?} {:?}", lhs, error);
                }
                SExpr::Node { name, .. } if name == "assert_malformed" => {}
                SExpr::Node { name, .. } if name == "assert_invalid" => {}
                SExpr::Node { name, .. } if name == "assert_exhaustion" => {}
                _ => todo!("{:?}", s)
            }
        }
    }

    include!("../tests/wasm_suite/tests.rs.gen");
}