mod datapack;
mod state;

use datapack_vm::cir::{Function, FunctionId};
use datapack_vm::interpreter::Interpreter;
use std::convert::TryInto;
use std::path::{Path, PathBuf};
use std::str;

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
    fn lower(&self, globals: &GlobalList, insert_sync: bool) -> BasicBlock<String> {
        let mut body = Vec::new();

        if insert_sync {
            body.push(format!("# !INTERPRETER: SYNC -1"));
        }

        for (idx, i) in self.instrs.iter().enumerate() {
            i.lower(&mut body, &globals);
            if insert_sync {
                body.push(format!("# !INTERPRETER: SYNC {}", idx));
            }
        }

        if insert_sync {
            body.pop();
        }

        BasicBlock {
            label: self.label.clone(),
            instrs: body,
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

const BRANCH_CONV: BranchConv = BranchConv::Grid;

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
    PushI64FromSplit { lo: Register, hi: Register },
    PushI64From(Register),
    PushI32Const(i32),
    PushI64Const(i64),
    PushReturnAddress(Label),

    SetConst(HalfRegister, i32),
    Copy { dst: HalfRegister, src: HalfRegister },

    RawBranch(Label),
    Branch(BranchTarget),
    // Index into table, index of table
    // (None represents the wasmcraft-specific table)
    DynBranch(Register, Option<u32>),
    BranchIf { t_name: BranchTarget, f_name: BranchTarget, cond: Register },
    BranchTable { reg: Register, targets: Vec<BranchTarget>, default: Option<BranchTarget> },

    I64Add { dst: Register, lhs: Register, rhs: Register },
    I64DivS { dst: Register, lhs: Register, rhs: Register },
    I64DivU { dst: Register, lhs: Register, rhs: Register },

    I64Eq { dst: Register, lhs: Register, invert: bool, rhs: Register },
    I64UComp { dst: Register, lhs: Register, op: Relation, rhs: Register },
    I64SComp { dst: Register, lhs: Register, op: Relation, rhs: Register },

    I64Shl { dst: Register, lhs: Register, rhs: Register },
    I64ShrU { dst: Register, lhs: Register, rhs: Register },
    I64ShrS { dst: Register, lhs: Register, rhs: Register },

    I32MulTo64 { dst: Register, lhs: HalfRegister, rhs: HalfRegister },

    I32Op { dst: HalfRegister, lhs: HalfRegister, op: &'static str, rhs: HalfRegister },

    I32Ctz(Register),
    I32Clz(Register),
    I64Ctz { dst: Register, src: Register },

    PopValueInto(Register),
    PopI32Into(Register),
    PopI64Into(Register),
    PopI64IntoSplit { hi: Register, lo: Register },

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

fn push_cond(reg: HalfRegister) -> Vec<String> {
    vec![
        format!("scoreboard players operation %%tempcond reg = {} reg", reg),
        "execute at @e[tag=condstackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %%tempcond reg".to_string(),
        "execute at @e[tag=condstackptr] as @e[tag=condstackptr] run tp @s ~1 ~ ~".to_string(),
    ]
}

fn read_cond() -> String {
    format!("execute at @e[tag=condstackptr] store result score %%tempcond reg run data get block ~-1 ~ ~ RecordItem.tag.Memory 1")
}

fn pop_cond() -> String {
    "execute at @e[tag=condstackptr] as @e[tag=condstackptr] run tp @s ~-1 ~ ~".to_string()
}

#[derive(Debug)]
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

    pub fn make_i32_op(&self, mut lhs: HalfRegister, mut op: &str, mut rhs: HalfRegister, body: &mut Vec<String>, dst: HalfRegister) {
        match op {
            "+=" | "-=" | "*=" | "%=" => {
                body.push(format!("scoreboard players operation {} reg {} {} reg", lhs, op, rhs));
                assert_eq!(lhs, dst);
            }
            "/=" => {
                // Minecraft division always rounds towards negative infinity
                assert_ne!(lhs, dst);
                assert_ne!(rhs, dst);

                // tmp = (rhs < 0)
                body.push(format!("execute store success score {} reg if score {} reg matches ..-1", dst, rhs));
                body.push(format!("execute if score {} reg matches 1..1 run scoreboard players operation {} reg *= %%-1 reg", dst, lhs));
                body.push(format!("execute if score {} reg matches 1..1 run scoreboard players operation {} reg *= %%-1 reg", dst, rhs));

                body.push(format!("scoreboard players operation {} reg = {} reg", dst, lhs));
                body.push(format!("execute if score {} reg matches ..-1 run scoreboard players operation {} reg += {} reg", lhs, dst, rhs));
                body.push(format!("execute if score {} reg matches ..-1 run scoreboard players remove {} reg 1", lhs, dst));
                body.push(format!("scoreboard players operation {} reg /= {} reg", dst, rhs));
            }
            "/=u" => {
                let sign = Register::Work(20).as_lo();

                assert_ne!(sign, lhs);
                assert_ne!(sign, rhs);
                assert_ne!(sign, dst);

                assert_ne!(lhs, dst);
                assert_ne!(rhs, dst);

                body.push(format!("scoreboard players set {} reg 1", sign));

                body.push(format!("execute if score {} reg matches ..-1 run scoreboard players operation {} reg *= %%-1 reg", lhs, sign));
                body.push(format!("execute if score {} reg matches ..-1 run scoreboard players operation {} reg *= %%-1 reg", lhs, lhs));

                body.push(format!("execute if score {} reg matches ..-1 run scoreboard players operation {} reg *= %%-1 reg", rhs, sign));
                body.push(format!("execute if score {} reg matches ..-1 run scoreboard players operation {} reg *= %%-1 reg", rhs, rhs));

                body.push(format!("# !INTERPRETER: ASSERT if score {} reg matches 0..", lhs));
                body.push(format!("# !INTERPRETER: ASSERT if score {} reg matches 1..", rhs));

                body.push(format!("scoreboard players operation {} reg = {} reg", dst, lhs));
                body.push(format!("scoreboard players operation {} reg /= {} reg", dst, rhs));

                body.push(format!("scoreboard players operation {} reg *= {} reg", dst, sign));
            }
            "remu" => {
                // FIXME: 

                assert_ne!(lhs, dst);
                assert_ne!(rhs, dst);

                body.push(format!("# !INTERPRETER: ASSERT if score {} reg matches 0..", lhs));
                body.push(format!("# !INTERPRETER: ASSERT if score {} reg matches 1..", rhs));
                body.push(format!("scoreboard players operation {} reg %= {} reg", lhs, rhs));
                body.push(format!("scoreboard players operation {} reg = {} reg", dst, lhs));
            }
            "rems" => {
                // FIXME: 

                assert_ne!(lhs, dst);
                assert_ne!(rhs, dst);

                body.push(format!("# !INTERPRETER: ASSERT if score {} reg matches 0..", lhs));
                body.push(format!("# !INTERPRETER: ASSERT if score {} reg matches 1..", rhs));
                body.push(format!("scoreboard players operation {} reg %= {} reg", lhs, rhs));
                body.push(format!("scoreboard players operation {} reg = {} reg", dst, lhs));
            }

            "&=" => {
                body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs));
                body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs));
                body.push("function intrinsic:and".to_string());
                body.push(format!("scoreboard players operation {} reg = %return%0 reg", dst));
            }
            "|=" => {
                body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs));
                body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs));
                body.push("function intrinsic:or".to_string());
                body.push(format!("scoreboard players operation {} reg = %return%0 reg", dst));
            }
            "^=" => {
                body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs));
                body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs));
                body.push("function intrinsic:xor".to_string());
                body.push(format!("scoreboard players operation {} reg = %return%0 reg", dst));
            }
            "shl" => {
                body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs));
                body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs));
                body.push("function intrinsic:shl".to_string());
                body.push(format!("scoreboard players operation {} reg = %param0%0 reg", dst));
            }
            "shru" => {
                body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs));
                body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs));
                body.push("function intrinsic:lshr".to_string());
                body.push(format!("scoreboard players operation {} reg = %param0%0 reg", dst));
            }
            "shrs" => {
                assert_ne!(dst, rhs);

                if dst != lhs {
                    body.push(format!("scoreboard players operation {} reg = {} reg", dst, lhs));
                }

                for i in 1..31 {
                    body.push(format!("execute if score {} reg matches {}..{} run scoreboard players operation {} reg /= %%{} reg", rhs, i, i, dst, 1 << i))
                }
                body.push(format!("execute if score {} reg matches 32.. run scoreboard players set {} reg 0", rhs, dst));
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

                body.push(format!("execute store success score {} reg if score {} reg {} {} reg", dst, lhs, score_op, rhs));
            }
            "!=" => {
                body.push(format!("execute store success score {} reg unless score {} reg = {} reg", dst, lhs, rhs));
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

                body.push(format!("scoreboard players set {} reg 0", dst));
                body.push(format!("execute if score {} reg matches ..-1 if score {} reg matches 0.. run scoreboard players set {} reg 0", lhs, rhs, dst));
                body.push(format!("execute if score {} reg matches 0.. if score {} reg matches ..-1 run scoreboard players set {} reg 1", lhs, rhs, dst));
                body.push(format!("execute if score {} reg matches ..-1 if score {} reg matches ..-1 if score {} reg {} {} reg run scoreboard players set {} reg 1", lhs, rhs, lhs, score_op, rhs, dst));
                body.push(format!("execute if score {} reg matches 0.. if score {} reg matches 0.. if score {} reg {} {} reg run scoreboard players set {} reg 1", lhs, rhs, lhs, score_op, rhs, dst));
            }
            _ => {
                todo!("TODO: make_i32_op {}", op);
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
            PopI64IntoSplit { hi, lo } => op_stack.pop_i64(),
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

    fn add_condition(code: &mut Vec<String>, cond: &str)  {
        for c in code.iter_mut() {
            if !c.starts_with('#') {
                *c = format!("{}{}", cond, *c);
            }
        }
    }

    fn branch(target: &BranchTarget, global_list: &GlobalList) -> Vec<String> {
        let mut body = Vec::new();

        let entry = get_block(&target.label);

        body.push(format!("#   Branch to {}", entry));
        for (idx, ty) in target.ty.iter().enumerate().rev() {
            // TODO: Should I use the return register?
            Instr::pop_into(Register::Return(idx as u32), *ty).lower(&mut body, global_list);
        }

        // TODO: Optimize
        for _ in 0..target.to_pop {
            Instr::Drop.lower(&mut body, global_list)
        }

        for (idx, ty) in target.ty.iter().enumerate() {
            Instr::push_from(Register::Return(idx as u32), *ty).lower(&mut body, global_list);
        }

        Instr::RawBranch(target.label.clone()).lower(&mut body, global_list);

        body
    }

    pub fn lower(&self, body: &mut Vec<String>, global_list: &GlobalList) {
        body.push(format!("# {:?}", self));

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
        match self {
            Comment(s) => {
                body.push(format!("# {}", s));
            }
            Tellraw(s) => {
                body.push(format!("tellraw @a {}", s));
            }

            SetConst(reg, v) => {
                body.push(format!("scoreboard players set {} reg {}", reg.get(), v));
            }

            SetTurtleCoord(reg, axis) => {
                body.push(format!("execute as @e[tag=turtle] store result entity @s Pos[{}] double 1 run scoreboard players get {} reg", *axis as u32, reg.get_lo()));
            }
            SetTurtleBlock(reg) => {
                for (idx, block) in blocks.iter().enumerate() {
                    body.push(format!("execute at @e[tag=turtle] if score {} reg matches {}..{} run setblock ~ ~ ~ {} destroy\n", reg.get_lo(), idx, idx, block));
                }

                let mut s = format!("execute unless score {} reg matches 0..{} run ", reg.get_lo(), blocks.len() - 1);
                s.push_str(r#"tellraw @a [{"text":"Attempt to set invalid block"},{"score":{"name":""#);
                s.push_str(&reg.get_lo());
                s.push_str(r#"","objective":"reg"}}]"#);
                body.push(s);
            }
            TurtleGet(reg) => {
                body.push(format!("scoreboard players set {} reg -1", reg.as_lo()));
                for (i, b) in blocks.iter().enumerate() {
                    body.push(format!("execute at @e[tag=turtle] if block ~ ~ ~ {} run scoreboard players set {} reg {}", b, reg.get_lo(), i));
                }
            }

            Copy { dst, src } => {
                if dst != src {
                    body.push(format!("scoreboard players operation {} reg = {} reg", dst, src));
                }
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
            &PopI64Into(reg) => {
                self.decr_stack_ptr_half(body);
                body.push(format!("execute at @e[tag=stackptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg.get_hi()));

                self.decr_stack_ptr_half(body);
                body.push(format!("execute at @e[tag=stackptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg.get_lo()));
            }
            PopI64IntoSplit { hi, lo } => {
                self.decr_stack_ptr_half(body);
                body.push(format!("execute at @e[tag=stackptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", hi.get_lo()));

                self.decr_stack_ptr_half(body);
                body.push(format!("execute at @e[tag=stackptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", lo.get_lo()));
            }
            &PopValueInto(reg) => {
                self.decr_stack_ptr_half(body);
                body.push(format!("execute at @e[tag=stackptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg.get_hi()));

                self.decr_stack_ptr_half(body);
                body.push(format!("execute at @e[tag=stackptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg.get_lo()));
            }

            LoadLocalI64(reg) => {
                body.push(format!("execute at @e[tag=localptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg.get_lo()));
                body.push(format!("execute at @e[tag=localptr] store result score {} reg run data get block ~ ~ ~1 RecordItem.tag.Memory 1", reg.get_hi()));
            }
            StoreLocalI64(reg) => {
                body.push(format!("execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_lo()));
                body.push(format!("execute at @e[tag=localptr] store result block ~ ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_hi()));
            }
            LoadLocalI32(reg) => {
                body.push(format!("execute at @e[tag=localptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg.get_lo()));
            }
            StoreLocalI32(reg) => {
                body.push(format!("execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_lo()));
            }


            &SetLocalPtr(local_index) => {
                body.push(format!("execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~{} 0 1", -(local_index as i32) - 1 ));
            }

            &AddI32Const(reg, value) => {
                use std::cmp::Ordering::*;
                match value.cmp(&0) {
                    Greater => body.push(format!("scoreboard players add {} reg {}", reg, value)),
                    Less => body.push(format!("scoreboard players remove {} reg {}", reg, -value)),
                    Equal => {},
                }
            }
            &I64ExtendI32S { dst, src } => {
                if dst != src {
                    body.push(format!("scoreboard players operation {} reg = {} reg", dst.get_lo(), src.get_lo()));
                }
                body.push(format!("execute if score {} reg matches ..-1 run scoreboard players set {} reg -1", src.get_lo(), dst.get_hi()));
                body.push(format!("execute if score {} reg matches 0.. run scoreboard players set {} reg 0", src.get_lo(), dst.get_hi()));
            }
            I64ExtendI32U(reg) => {
                body.push(format!("scoreboard players set {} reg 0", reg.get_hi()));
            }

            I32MulTo64 { dst, lhs, rhs } => {
                body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs));
                body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs));
                body.push("function intrinsic:mul_32_to_64".to_string());
                body.push(format!("scoreboard players operation {} reg = %return%0 reg", dst.as_lo()));
                body.push(format!("scoreboard players operation {} reg = %return%1 reg", dst.as_hi()));
            }

            I64Shl { dst, lhs, rhs } => {
                body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs.as_lo()));
                body.push(format!("scoreboard players operation %param0%1 reg = {} reg", lhs.as_hi()));

                body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs.as_lo()));

                body.push("function intrinsic:shl_64".to_string());

                body.push(format!("scoreboard players operation {} reg = %param0%0 reg", dst.as_lo()));
                body.push(format!("scoreboard players operation {} reg = %param0%1 reg", dst.as_hi()));
            }
            I64ShrU { dst, lhs, rhs } => {
                body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs.as_lo()));
                body.push(format!("scoreboard players operation %param0%1 reg = {} reg", lhs.as_hi()));

                body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs.as_lo()));

                body.push("function intrinsic:lshr_i64".to_string());

                body.push(format!("scoreboard players operation {} reg = %param0%0 reg", dst.as_lo()));
                body.push(format!("scoreboard players operation {} reg = %param0%1 reg", dst.as_hi()));
            }
            I64ShrS { dst, lhs, rhs } => {
                body.push(format!("scoreboard players operation %param0%0 reg = {} reg", lhs.as_lo()));
                body.push(format!("scoreboard players operation %param0%1 reg = {} reg", lhs.as_hi()));

                body.push(format!("scoreboard players operation %param1%0 reg = {} reg", rhs.as_lo()));

                body.push("function intrinsic:ashr_i64".to_string());

                body.push(format!("scoreboard players operation {} reg = %param0%0 reg", dst.as_lo()));
                body.push(format!("scoreboard players operation {} reg = %param0%1 reg", dst.as_hi()));
            }
            &I64DivS { dst, lhs, rhs } => {
                assert_eq!(dst, Register::Return(0));
                assert_eq!(lhs, Register::Param(0));
                assert_eq!(rhs, Register::Param(1));

                body.push("function intrinsic:i64_sdiv".to_string());
            }
            &I64DivU { dst, lhs, rhs } => {
                assert_eq!(dst, Register::Return(0));
                assert_eq!(lhs, Register::Param(0));
                assert_eq!(rhs, Register::Param(1));

                body.push("function intrinsic:i64_udiv".to_string());
            }

            I64Add { dst, lhs, rhs } => {
                let carry = Register::Work(10).as_lo();

                assert_ne!(*dst, carry.0);
                assert_ne!(*lhs, carry.0);
                assert_ne!(*rhs, carry.0);
                assert_ne!(dst, lhs);
                assert_ne!(dst, rhs);
                assert_ne!(lhs, rhs);

                body.push(format!("scoreboard players operation {} reg = {} reg", dst.get_lo(), lhs.get_lo()));
                body.push(format!("scoreboard players operation {} reg = {} reg", dst.get_hi(), lhs.get_hi()));

                body.push(format!("scoreboard players operation {} reg += {} reg", dst.get_lo(), rhs.get_lo()));
                body.push(format!("scoreboard players operation {} reg += {} reg", dst.get_hi(), rhs.get_hi()));

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

                body.push(format!("scoreboard players set {} reg 0", carry));
                body.push(format!("execute if score {} reg matches ..-1 if score {} reg matches ..-1 run scoreboard players set {} reg 1", lhs.get_lo(), rhs.get_lo(), carry));
                body.push(format!("execute if score {} reg matches ..-1 if score {} reg matches 0.. if score {} reg matches 0.. run scoreboard players set {} reg 1", lhs.get_lo(), rhs.get_lo(), dst.get_lo(), carry));
                body.push(format!("execute if score {} reg matches 0.. if score {} reg matches ..-1 if score {} reg matches 0.. run scoreboard players set {} reg 1", lhs.get_lo(), rhs.get_lo(), dst.get_lo(), carry));

                body.push(format!("scoreboard players operation {} reg += {} reg", dst.get_hi(), carry));
            }

            &I64Eq { dst, lhs, invert, rhs } => {
                body.push(format!("execute store success score {} reg if score {} reg = {} reg if score {} reg = {} reg", dst.as_lo(), lhs.as_lo(), rhs.as_lo(), lhs.as_hi(), rhs.as_hi()));

                if invert {
                    body.push(format!("execute store success score {} reg if score {} reg matches 0..0", dst.as_lo(), dst.as_lo()));
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
                    op = Relation::GreaterThan;
                }

                if op != Relation::LessThan {
                    todo!()
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

                Instr::I32Op { dst: hi_is_lesser, lhs: lhs.as_hi(), op: "ltu", rhs: rhs.as_hi() }.lower(body, global_list);
                Instr::I32Op { dst: hi_is_greater, lhs: lhs.as_hi(), op: "gtu", rhs: rhs.as_hi() }.lower(body, global_list);
                Instr::I32Op { dst: hi_is_equal, lhs: lhs.as_hi(), op: "==", rhs: rhs.as_hi() }.lower(body, global_list);
                Instr::I32Op { dst: lo_is_lesser, lhs: lhs.as_lo(), op: "ltu", rhs: rhs.as_lo() }.lower(body, global_list);

                body.push(format!("execute if score {} reg matches 1.. run scoreboard players set {} reg 1", hi_is_lesser, dst.as_lo()));
                body.push(format!("execute if score {} reg matches 1.. run scoreboard players set {} reg 0", hi_is_greater, dst.as_lo()));
                body.push(format!("execute if score {} reg matches 1.. run scoreboard players operation {} reg = {} reg", hi_is_equal, dst.as_lo(), lo_is_lesser));
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

                if op != Relation::LessThan {
                    todo!()
                }
            }

            I32Eqz { val, cond } => {
                body.push(format!("scoreboard players set {} reg 0", cond.get_lo()));
                body.push(format!("execute if score {} reg matches 0..0 run scoreboard players set {} reg 1", val.get_lo(), cond.get_lo()));
            }
            I64Eqz { val, cond } => {
                body.push(format!("scoreboard players set {} reg 1", cond.get_lo()));
                body.push(format!("execute unless score {} reg matches 0..0 run scoreboard players set {} reg 0", val.get_lo(), cond.get_lo()));
                body.push(format!("execute unless score {} reg matches 0..0 run scoreboard players set {} reg 0", val.get_hi(), cond.get_lo()));
            }
            &I32Op { dst, lhs, op, rhs } => {
                self.make_i32_op(lhs, op, rhs, body, dst);
            }
            I32Ctz(reg) => {
                body.push(format!("scoreboard players operation %param0%0 reg = {} reg", reg.get_lo()));
                body.push("function intrinsic:ctz".to_string());
                body.push(format!("scoreboard players operation {} reg = %return%0 reg", reg.get_lo()));
            }
            I32Clz(reg) => {
                body.push(format!("scoreboard players operation %param0%0 reg = {} reg", reg.get_lo()));
                body.push("function intrinsic:clz".to_string());
                body.push(format!("scoreboard players operation {} reg = %return%0 reg", reg.get_lo()));
            }
            I64Ctz { src, dst } => {
                assert_ne!(src, dst);

                body.push(format!("execute if score {} reg matches 0..0 run scoreboard players operation %param0%0 reg = {} reg", src.get_lo(), src.get_hi()));
                body.push(format!("execute unless score {} reg matches 0..0 run scoreboard players operation %param0%0 reg = {} reg", src.get_lo(), src.get_lo()));
                body.push("function intrinsic:ctz".to_string());
                body.push(format!("scoreboard players operation {} reg = %return%0 reg", dst.get_lo()));
                body.push(format!("execute if score {} reg matches 0..0 run scoreboard players add {} reg 32", src.get_lo(), dst.get_lo()));
                body.push(format!("scoreboard players set {} reg 0", dst.get_hi()));
            }

            PushI32From(reg) => {
                body.push(format!("execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_lo()));
                self.incr_stack_ptr(body);
            }
            PushI64From(reg) => {
                body.push(format!("execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_lo()));
                self.incr_stack_ptr_half(body);

                body.push(format!("execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_hi()));
                self.incr_stack_ptr_half(body);
            }
            PushI64FromSplit { lo, hi } => {
                body.push(format!("execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", lo.get_lo()));
                self.incr_stack_ptr_half(body);

                body.push(format!("execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", hi.get_lo()));
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
                let return_name = get_block(&label);

                body.push(format!("execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value !BLOCKIDX!{}!", return_name));
                self.incr_stack_ptr(body);
            }

            RawBranch(entry) => {
                let entry = get_block(entry);

                body.push(format!("#   Jump to {}", entry));
                match BRANCH_CONV {
                    BranchConv::Grid => {
                        body.push(format!("setblock !BLOCKPOS!{}! minecraft:redstone_block destroy", entry));
                    }
                    BranchConv::Direct => {
                        body.push(format!("function wasm:{}", entry));
                    }
                    c => todo!("{:?}", c)
                }
            }
            Branch(target) => {
                body.extend(Instr::branch(target, global_list))
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
                body.push(format!("#   {:?}", self));

                if BRANCH_CONV == BranchConv::Direct {
                    body.extend(push_cond(cond.as_lo()));
                } else {
                    body.push(format!("scoreboard players operation %%tempcond reg = {} reg", cond.as_lo()));
                }

                let mut true_code = Instr::branch(t_name, global_list);
                Instr::add_condition(&mut true_code, &format!("execute unless score %%tempcond reg matches 0..0 run "));
                body.extend(true_code);

                if BRANCH_CONV == BranchConv::Direct {
                    body.push(read_cond());
                }

                let mut false_code = Instr::branch(f_name, global_list);
                Instr::add_condition(&mut false_code, &format!("execute if score %%tempcond reg matches 0..0 run "));
                body.extend(false_code);
                
                if BRANCH_CONV == BranchConv::Direct {
                    body.push(pop_cond());
                }
            }
            BranchTable { reg, targets, default } => {
                if targets.is_empty() {
                    body.extend(Instr::branch(default.as_ref().unwrap(), global_list));
                } else {
                    if BRANCH_CONV == BranchConv::Direct {
                        body.extend(push_cond(reg.as_lo()));
                    } else {
                        body.push(format!("scoreboard players operation %%tempcond reg = {} reg", reg.as_lo()));
                    }

                    for (idx, target) in targets.iter().enumerate() {
                        let mut code = Instr::branch(target, global_list);
                        Instr::add_condition(&mut code, &format!("execute if score %%tempcond reg matches {}..{} run ", idx, idx));
                        body.extend(code);

                        if BRANCH_CONV == BranchConv::Direct {
                            body.push(read_cond());
                        }
                    }

                    if let Some(default) = default {
                        let mut code = Instr::branch(default, global_list);
                        Instr::add_condition(&mut code, &format!("execute unless score %%tempcond reg matches 0..{} run ", targets.len() - 1));
                        body.extend(code);
                    } else {
                        let mut s = format!("execute unless score %%tempcond reg matches 0..{} run ", targets.len() - 1);
                        s.push_str(r#"tellraw @a [{"text":"Attempt to branch to invalid function "},{"score":{"name":"%work%0%lo","objective":"reg"}}]"#);
                        body.push(s);
                    }
                    
                    if BRANCH_CONV == BranchConv::Direct {
                        body.push(pop_cond());
                    }
                }
            }

            LoadGlobalI64(reg) => {
                body.push(format!("execute at @e[tag=globalptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg.get_lo()));
                body.push(format!("execute at @e[tag=globalptr] store result score {} reg run data get block ~ ~ ~1 RecordItem.tag.Memory 1", reg.get_hi()));
            }
            LoadGlobalI32(reg) => {
                body.push(format!("execute at @e[tag=globalptr] store result score {} reg run data get block ~ ~ ~ RecordItem.tag.Memory 1", reg.get_lo()));
            }
            StoreGlobalI64(reg) => {
                body.push(format!("execute at @e[tag=globalptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_lo()));
                body.push(format!("execute at @e[tag=globalptr] store result block ~ ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_hi()));
            }
            StoreGlobalI32(reg) => {
                body.push(format!("execute at @e[tag=globalptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {} reg", reg.get_lo()));
            }

            &LoadI32(dst, _align) => {
                body.push("function intrinsic:load_word".to_string());
                body.push(format!("scoreboard players operation {} reg = %return%0 reg", dst.get_lo()));
            }
            LoadI32_8U(dst, _align) => {
                body.push("function intrinsic:load_byte".to_string());
                body.push(format!("scoreboard players operation {} reg = %param0%0 reg", dst.get_lo()));
                // TODO: Determine if load_byte actually returns a byte
                body.push(format!("scoreboard players operation {} reg = %param0%0 reg", dst.get_lo()));
            }
            LoadI32_8S(dst, _align) => {
                body.push("function intrinsic:load_byte".to_string());
                body.push(format!("scoreboard players operation {} reg = %param0%0 reg", dst.get_lo()));
                // TODO: Determine if load_byte actually returns a byte
                body.push(format!("execute if score {} reg matches 128..255 run scoreboard players add {} reg -256", dst.get_lo(), dst.get_lo()));
            }
            LoadI32_16U(dst, _align) => {
                // TODO:
                body.push("function intrinsic:load_halfword_unaligned".to_string());
                body.push(format!("scoreboard players operation {} reg = %return%0 reg", dst.get_lo()));
                // TODO: Determine if load_halfword actually returns a halfword
                body.push(format!("scoreboard players operation {} reg = %return%0 reg", dst.get_lo()));
            }
            LoadI32_16S(dst, _align) => {
                // TODO:
                body.push("function intrinsic:load_halfword_unaligned".to_string());
                body.push(format!("scoreboard players operation {} reg = %return%0 reg", dst.get_lo()));
                // TODO: Determine if load_halfword actually returns a halfword
                body.push(format!("execute if score {} reg matches 32768..65535 run scoreboard players add {} reg -65536", dst.get_lo(), dst.get_lo()));
            }
            &StoreI32(src, _align) => {
                body.push(format!("scoreboard players operation %param0%0 reg = {} reg", src.get_lo()));
                body.push("function intrinsic:store_word".to_string());
            }
            &StoreI32_8(src, _align) => {
                body.push(format!("scoreboard players operation %param2%0 reg = {} reg", src.get_lo()));
                body.push("function intrinsic:store_byte".to_string());
            }
            &StoreI32_16(src, _align) => {
                body.push(format!("scoreboard players operation %param0%0 reg = {} reg", src.get_lo()));
                // TODO:
                body.push("function intrinsic:store_halfword_unaligned".to_string());
            }

            StoreRow(src) => {
                for i in 0..8 {
                    body.push(format!("execute at @e[tag=memoryptr] store result block ~ ~ ~{} RecordItem.tag.Memory int 1 run scoreboard players get {} reg", i, src.get_lo()))
                }
            }

            ResetFrames => {
                body.push("execute as @e[tag=frameptr] run tp @s 0 0 1".to_string());
                body.push("scoreboard players set %frameptr wasm 0".to_string());
            }
            &PushFrame(local_count) => {
                if local_count != 0 {
                    body.push(format!("# Push frame with {} locals", local_count));
                    body.push(format!("execute at @e[tag=frameptr] run fill ~ ~ ~ ~{} ~ ~1 minecraft:jukebox{{RecordItem:{{id:\"minecraft:stone\",Count:1b,tag:{{Memory:0}}}}}}", local_count - 1));
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
            SelectI32 { dst_reg, true_reg, false_reg, cond_reg } => {
                assert_ne!(dst_reg, false_reg);
                assert_ne!(dst_reg, cond_reg);

                body.push(format!("scoreboard players operation {} reg = {} reg", dst_reg.get_lo(), true_reg.get_lo()));
                //body.push(format!("scoreboard players operation {} reg = {} reg", dst_reg.get_hi(), true_reg.get_hi()));

                body.push(format!("execute if score {} reg matches 0..0 run scoreboard players operation {} reg = {} reg", cond_reg.get_lo(), dst_reg.get_lo(), false_reg.get_lo()));
                //body.push(format!("execute if score {} reg matches 0..0 run scoreboard players operation {} reg = {} reg", cond_reg.get_lo(), dst_reg.get_hi(), false_reg.get_hi()));
            }
            SelectI64 { dst_reg, true_reg, false_reg, cond_reg } => {
                assert_ne!(dst_reg, false_reg);
                assert_ne!(dst_reg, cond_reg);

                body.push(format!("scoreboard players operation {} reg = {} reg", dst_reg.get_lo(), true_reg.get_lo()));
                body.push(format!("scoreboard players operation {} reg = {} reg", dst_reg.get_hi(), true_reg.get_hi()));

                body.push(format!("execute if score {} reg matches 0..0 run scoreboard players operation {} reg = {} reg", cond_reg.get_lo(), dst_reg.as_lo(), false_reg.as_lo()));
                body.push(format!("execute if score {} reg matches 0..0 run scoreboard players operation {} reg = {} reg", cond_reg.get_lo(), dst_reg.as_hi(), false_reg.as_hi()));
            }

            Unreachable => {
                body.push("tellraw @a \"ENTERED UNREACHABLE CODE\"".to_string());
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Register {
    Work(u32),
    Param(u32),
    Return(u32),
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
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WasmValue {
    I32(i32),
    I64(i64),
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
            stack_size: 0,
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

impl FuncBodyStream {
    pub fn new(func_ty: TypeOrFuncType, types: &TypeList, func_idx: CodeFuncIdx) -> Self {
        FuncBodyStream {
            func_idx,
            // 0 is always the entry point, 1 is always the exit point
            basic_blocks: vec![BasicBlock::new(func_idx, 0), BasicBlock::new(func_idx, 1)],
            reachable: vec![true, true],
            bb_index: 0,
            depth: 0,
            op_stack: OpStack::new(),
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
            BranchConv::Grid => {
                self.push_instr(Instr::Comment(" Pop return address".to_string()));
                // Pop return address
                let reg = Register::Work(0);
                self.push_instr(Instr::PopI32Into(reg));
                self.push_instr(Instr::DynBranch(reg, None));
            }
            BranchConv::Direct => {}
            bc => todo!("{:?}", bc)
        }

        self.bb_index = old_bb_idx;
    }

    pub fn get_i32_dst(&mut self, lhs: Register, op: &str, rhs: Register) -> Register {
        match op {
            "+=" | "-=" | "*=" | "%=" |
            "&=" | "|=" | "^=" | "shl" | "shru" | "shrs" => {
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

    pub fn set_memory_ptr(&mut self, memarg: MemoryImmediate, memory_list: &MemoryList) {
        //let offset = memory_list.get_offset(memarg.memory) + memarg.offset;
        //self.push_instr(format!("tp @e[tag=memoryptr] {} 0 0", offset));
        println!("TODO: set_memory_ptr {:?}", memarg);
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
        self.bb_index = self.allocate_block();
    }

    pub fn mark_unreachable(&mut self) {
        self.reachable[self.bb_index] = false;
    }

    pub fn reachable(&self) -> bool {
        self.reachable[self.bb_index]
    }

    pub fn setup_arguments(&mut self, local_count: u32, types: &TypeList, function_list: &FunctionList, locals: &[(u32, Type)]) {
        self.push_instr(Instr::PushFrame(local_count));

        let ty = function_list.get_function_type(self.func_idx, types);

        for (param_idx, param) in ty.params.iter().enumerate() {
            self.push_instr(Instr::Comment(format!("#   Parameter {}", param_idx)));

            let reg = Register::Param(param_idx as u32);

            self.push_instr(Instr::SetLocalPtr(param_idx as u32));

            self.push_instr(Instr::store_local(reg, *param));
        }
    }

    pub fn store_i32(&mut self, memarg: MemoryImmediate, memory_list: &MemoryList) {
        let dreg = Register::Work(1);
        let areg = Register::Work(0);

        self.push_instr(Instr::PopI32Into(dreg));
        self.op_stack.pop_i32();
        self.push_instr(Instr::PopI32Into(areg));
        self.op_stack.pop_i32();
        self.push_instr(Instr::AddI32Const(areg.as_lo(), memarg.offset as i32));
        self.push_instr(Instr::SetMemPtr(areg));

        self.push_instr(Instr::StoreI32(dreg, memarg.align));
    }

    pub fn store_i32_8(&mut self, memarg: MemoryImmediate, memory_list: &MemoryList) {
        let dreg = Register::Work(1);
        let areg = Register::Work(0);

        self.push_instr(Instr::PopI32Into(dreg));
        self.op_stack.pop_i32();
        self.push_instr(Instr::PopI32Into(areg));
        self.op_stack.pop_i32();
        self.push_instr(Instr::AddI32Const(areg.as_lo(), memarg.offset as i32));
        self.push_instr(Instr::SetMemPtr(areg));

        self.push_instr(Instr::StoreI32_8(dreg, memarg.align));
    }

    pub fn store_i32_16(&mut self, memarg: MemoryImmediate, memory_list: &MemoryList) {
        let dreg = Register::Work(1);
        let areg = Register::Work(0);

        self.push_instr(Instr::PopI32Into(dreg));
        self.op_stack.pop_i32();
        self.push_instr(Instr::PopI32Into(areg));
        self.op_stack.pop_i32();
        self.push_instr(Instr::AddI32Const(areg.as_lo(), memarg.offset as i32));
        self.push_instr(Instr::SetMemPtr(areg));

        self.push_instr(Instr::StoreI32_16(dreg, memarg.align));
    }

    pub fn load_i32_sized<I>(&mut self, memarg: MemoryImmediate, memory_list: &MemoryList, instr: I)
        where I: FnOnce(Register, u8) -> Instr,
    {
        let reg = Register::Work(0);

        self.push_instr(Instr::PopI32Into(reg));
        self.op_stack.pop_i32();
        self.push_instr(Instr::AddI32Const(reg.as_lo(), memarg.offset as i32));
        self.push_instr(Instr::SetMemPtr(reg));

        self.push_instr(instr(reg, memarg.align));
        self.push_instr(Instr::PushI32From(reg));
        self.op_stack.push_i32();
    }

    pub fn load_i64_sized_u<I>(&mut self, memarg: MemoryImmediate, memory_list: &MemoryList, instr: I)
        where I: FnOnce(Register, u8) -> Instr,
    {
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

    pub fn load_i64_sized_s<I>(&mut self, memarg: MemoryImmediate, memory_list: &MemoryList, instr: I)
        where I: FnOnce(Register, u8) -> Instr,
    {
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

        // Jump to function
        self.push_instr(Instr::RawBranch(Label::new(function_index, 0)));
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
            BranchConv::Grid => {
                // Push return address
                self.push_instr(Instr::Comment("  Push return address".to_string()));
                self.push_instr(Instr::PushReturnAddress(Label::new(self.func_idx, self.basic_blocks.len())));
            }
            BranchConv::Direct => {}
            bc => todo!("{:?}", bc),
        }

        // Jump to function
        self.push_instr(Instr::DynBranch(Register::Work(0), Some(table_index)));
    }

    pub fn get_target(&self, relative_depth: u32) -> BranchTarget {
        let flow_stack = self.flow_stack.clone();
        let entry = &self.flow_stack.0[self.flow_stack.0.len() - 1 - relative_depth as usize];
        let to_pop = self.op_stack.0.len() - entry.target_tys.len() - entry.stack_size;
        BranchTarget {
            label: entry.label.clone().unwrap(),
            to_pop,
            ty: entry.target_tys.clone(),
        }
    }

    fn allocate_block(&mut self) -> usize {
        let idx = self.basic_blocks.len();
        self.basic_blocks.push(BasicBlock::new(self.func_idx, idx));
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

    pub fn visit_operator(&mut self, o: Operator, local_count: u32, types: &TypeList, function_list: &FunctionList, memory_list: &MemoryList, global_list: &GlobalList, locals: &[(u32, Type)], imports: &ImportList) {
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

                    self.static_call(function_index, types, function_list);

                    match BRANCH_CONV {
                        BranchConv::Grid | BranchConv::Chain | BranchConv::Loop => {
                            self.next_basic_block();
                        }
                        BranchConv::Direct => {}
                    }

                    let f = function_list.get_function_type(function_index, types);
                    for (idx, ty) in f.returns.iter().enumerate() {
                        self.push_instr(Instr::push_from(Register::Return(idx as u32), *ty));
                        self.op_stack.push_ty(*ty);
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

                self.visit_operator(Operator::Br { relative_depth: self.depth as u32 }, local_count, types, function_list, memory_list, global_list, locals, imports);
            }

            GlobalGet { global_index } => { 
                self.push_instr(Instr::SetGlobalPtr(global_index));

                let reg = Register::Work(0);
                // FIXME:
                let ty = global_list.globals[global_index as usize].ty.content_type;

                self.push_instr(Instr::load_global(reg, ty));
                self.push_instr(Instr::push_from(reg, ty));
                self.op_stack.push_ty(ty);
            }
            GlobalSet { global_index } => {
                let reg = Register::Work(0);
                // FIXME:
                let ty = global_list.globals[global_index as usize].ty.content_type;

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
                self.store_i32(memarg, memory_list);
            }
            I32Store8 { memarg } => {
                self.store_i32_8(memarg, memory_list);
            }
            I32Store16 { memarg } => {
                self.store_i32_16(memarg, memory_list);
            }
            I32Load { memarg } => {
                self.load_i32(memarg, memory_list);
            }
            I32Load8U { memarg } => {
                self.load_i32_8u(memarg, memory_list);
            }
            I32Load8S { memarg } => {
                self.load_i32_8s(memarg, memory_list);
            }
            I32Load16U { memarg } => {
                self.load_i32_16u(memarg, memory_list);
            }
            I32Load16S { memarg } => {
                self.load_i32_16s(memarg, memory_list);
            }
            I64Store { memarg } => {
                let dreg_hi = Register::Work(2);
                let dreg_lo = Register::Work(1);
                let areg = Register::Work(0);

                self.push_instr(Instr::PopI64IntoSplit { hi: dreg_hi, lo: dreg_lo });
                self.op_stack.pop_i64();

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

                self.push_instr(Instr::PushI64FromSplit { lo: dreg_lo, hi: dreg_hi });
                self.op_stack.push_i64();
            }
            I64Load8U { memarg } => {
                self.load_i64_sized_u(memarg, memory_list, Instr::LoadI32_8U);
            }
            I64Load16U { memarg } => {
                self.load_i64_sized_u(memarg, memory_list, Instr::LoadI32_16U);
            }
            I64Load32U { memarg } => {
                self.load_i64_sized_u(memarg, memory_list, Instr::LoadI32);
            }
            I64Load8S { memarg } => {
                self.load_i64_sized_s(memarg, memory_list, Instr::LoadI32_8S);
            }
            I64Load16S { memarg } => {
                self.load_i64_sized_s(memarg, memory_list, Instr::LoadI32_16S);
            }
            I64Load32S { memarg } => {
                self.load_i64_sized_s(memarg, memory_list, Instr::LoadI32);
            }

            MemoryGrow { mem, mem_byte } => {
                println!("FIXME: MEMORY GROW");
            }

            End => {
                let needs_else = self.flow_stack.0.last().unwrap().else_block.is_some();
                if needs_else {
                    self.visit_operator(Operator::Else, local_count, types, function_list, memory_list, global_list, locals, imports);
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

            I64Ctz => {
                self.op_stack.pop_i64();
                self.push_instr(Instr::PopI64Into(Register::Work(0)));
                self.push_instr(Instr::I64Ctz { dst: Register::Work(1), src: Register::Work(0) });
                self.push_instr(Instr::PushI64From(Register::Work(1)));
                self.op_stack.push_i64();
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

            LocalTee { local_index } => {
                let reg = Register::Work(0);
                let ty = get_local_ty(locals, local_index);

                self.push_instr(Instr::pop_into(reg, ty));
                self.op_stack.pop_ty(ty);
                self.push_instr(Instr::push_from(reg, ty));
                self.op_stack.push_ty(ty);
                self.push_instr(Instr::push_from(reg, ty));
                self.op_stack.push_ty(ty);

                self.visit_operator(Operator::LocalSet { local_index }, local_count, types, function_list, memory_list, global_list, locals, imports)
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
                let inputs = get_input_tys(ty, types);
                let outputs = get_output_tys(ty, types);

                println!("Before starting block: {:?}", self.op_stack);

                let target_tys = outputs.clone();

                for ty in inputs.iter().rev().copied() {
                    self.op_stack.pop_ty(ty);
                }

                let next_block = self.allocate_block();

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
                let inputs = get_input_tys(ty, types);
                let outputs = get_output_tys(ty, types);

                let target_tys = outputs.clone();

                // Condition
                let reg = Register::Work(0);
                self.push_instr(Instr::PopI32Into(reg));
                self.op_stack.pop_i32();

                for ty in inputs.iter().rev().copied() {
                    self.op_stack.pop_ty(ty);
                }

                let then_block = self.allocate_block();
                let else_block = self.allocate_block();
                let next_block = self.allocate_block();

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
            I64ExtendI32U => {
                let reg = Register::Work(0);
                
                self.op_stack.pop_i32();
                self.push_instr(Instr::pop_into(reg, Type::I32));
                self.push_instr(Instr::I64ExtendI32U(reg));
                self.push_instr(Instr::push_from(reg, Type::I64));
                self.op_stack.push_i64();
            }

            Loop { ty } => {
                let target_tys = get_input_tys(ty, types);
                let outputs = get_output_tys(ty, types);

                for ty in target_tys.iter().rev().copied() {
                    self.op_stack.pop_ty(ty);
                }

                let next_block = self.allocate_block();

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

                // Branch to next block
                self.push_instr(Instr::RawBranch(Label::new(self.func_idx, self.basic_blocks.len())));

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
                let next_block = self.allocate_block();
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

fn save_datapack(folder_path: &Path, mc_functions: Vec<(String, String)>, wasm_file: &WasmFile) {
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

pub fn run(run_options: &RunOptions) {
    let file = std::fs::read(&run_options.wasm_path).unwrap();
    let mut wasm_file = parse_wasm_file(&file);
    link_intrinsics(&mut wasm_file);

    let basic_blocks = compile(&wasm_file);

    let mc_functions = assemble(&basic_blocks, &wasm_file, false);

    for func in mc_functions.iter() {
        println!("F: {:?}", func.0)
    }

    let folder_path = run_options.out_path.as_deref().unwrap_or_else(|| Path::new("../out"));

    save_datapack(folder_path, mc_functions.clone(), &wasm_file);

    //let result = run_and_compare(&basic_blocks, &mc_functions, &wasm_file);

    //println!("Returned with {:?}", result);
}

fn run_commands(mc_functions: &[(String, String)], globals: &GlobalList, memory: &MemoryList, exports: &ExportList) -> Interpreter {
    let mut cmd = setup_commands(mc_functions, globals, memory, exports);

    cmd.run_to_end().unwrap();

    cmd
}

fn prepare_interp(mc_functions: &[(String, String)], globals: &GlobalList, memory: &MemoryList) -> Interpreter {
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
    let mut i = prepare_interp(mc_functions, globals, memory);

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

        s.setup_arguments(local_count, &file.types, &file.functions, &locals);
        for (idx, o) in e.operators.iter().cloned().enumerate() {
            s.visit_operator(o, local_count, &file.types, &file.functions, &file.memory, &file.globals, &locals, &file.imports);
        }
        assert!(s.op_stack.0.is_empty(), "{:?}", s.op_stack);

        for bb in s.basic_blocks {
            basic_blocks.push(bb);
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

fn assemble(basic_blocks: &[BasicBlock<Instr>], file: &WasmFile, insert_sync: bool) -> Vec<(String, String)> {
    let mut mc_functions = Vec::new();

    for bb in basic_blocks {
        let mut new_block = bb.lower(&file.globals, insert_sync);
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
    kill @e[tag=condstackptr]\n\
    \n\
    # Add armor stand pointers\n\
    summon minecraft:armor_stand 0 0 8 {Marker:1b,Tags:[\"memoryptr\"],CustomName:'\"memoryptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 1 {Marker:1b,Tags:[\"localptr\"],CustomName:'\"localptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 1 {Marker:1b,Tags:[\"frameptr\"],CustomName:'\"frameptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 0 {Marker:1b,Tags:[\"stackptr\"],CustomName:'\"stackptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 3 0 {Marker:1b,Tags:[\"condstackptr\"],CustomName:'\"condstackptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 3 {Marker:1b,Tags:[\"globalptr\"],CustomName:'\"globalptr\"',CustomNameVisible:1b}\n\
    summon minecraft:armor_stand 0 0 -2 {Marker:1b,Tags:[\"turtle\"],CustomName:'\"turtle\"',CustomNameVisible:1b}\n\
    
    scoreboard players set %stackptr wasm 0
    scoreboard players set %frameptr wasm 0
    
    scoreboard players set %%-1 reg -1
    scoreboard players set %%0 reg 0
    scoreboard players set %%1 reg 1
    scoreboard players set %%2 reg 2
    scoreboard players set %%4 reg 4
    scoreboard players set %%8 reg 8
    scoreboard players set %%16 reg 16
    scoreboard players set %%SIXTEEN reg 16
    scoreboard players set %%256 reg 256
    scoreboard players set %%65536 reg 65536
    scoreboard players set %%16777216 reg 16777216 
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
        BranchConv::Direct => {}
        bc => todo!("{:?}", bc)
    }

    mc_functions.push(("setup".to_string(), setup));

    let mut dyn_branch = String::new();
    
    if BRANCH_CONV == BranchConv::Direct {
        let cmds = push_cond(Register::Work(0).as_lo()).join("\n");
        dyn_branch.push_str(&cmds);
        dyn_branch.push('\n');
    }

    for (idx, (name, _)) in mc_functions.iter().enumerate() {
        let pos = block_pos_from_idx(idx, true);

        // FIXME: Reg0
        match BRANCH_CONV {
            BranchConv::Grid => {
                let b = format!("# wasm:{}\nexecute if score %work%0%lo reg matches {}..{} run setblock {} minecraft:redstone_block destroy\n", name, idx, idx, pos);
                dyn_branch.push_str(&b);
            }
            BranchConv::Direct => {
                dyn_branch.push_str(&read_cond());
                dyn_branch.push('\n');

                let b = format!("# wasm:{}\nexecute if score %%tempcond reg matches {}..{} run function wasm:{}\n", name, idx, idx, name);
                dyn_branch.push_str(&b);
            }
            bc => todo!("{:?}", bc)
        }
    }

    if BRANCH_CONV == BranchConv::Direct {
        dyn_branch.push_str(&read_cond());
        dyn_branch.push('\n');
        dyn_branch.push_str(&pop_cond());
        dyn_branch.push('\n');
    } else {
        dyn_branch.push_str("scoreboard players operation %%tempcond reg = %work%0%lo reg\n");
    }

    // FIXME:
    let e = format!("execute unless score %%tempcond reg matches 0..{} run ", mc_functions.len() - 1);
    dyn_branch.push_str(&e);
    dyn_branch.push_str(r#"tellraw @a [{"text":"Attempt to branch to invalid function "},{"score":{"name":"%work%0%lo","objective":"reg"}}]"#);
    dyn_branch.push('\n');



    mc_functions.push(("dyn_branch".to_string(), dyn_branch));

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
            f.basic_blocks.push(BasicBlock::new(CodeFuncIdx(0), 0));

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

            let mut body = Vec::new();
            for i in f.basic_blocks[0].instrs.iter_mut() {
                i.lower(&mut body, &file.globals);
            }
            let pos = block_pos_from_idx(idx, true);

            match BRANCH_CONV {
                BranchConv::Grid => {
                    body.push(format!("setblock {} minecraft:redstone_block destroy", pos));
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

fn create_dyn_jumper(table: &state::Table) -> String {
    let mut dyn_branch = String::new();

    if BRANCH_CONV == BranchConv::Direct {
        let cmds = push_cond(Register::Work(0).as_lo()).join("\n");
        dyn_branch.push_str(&cmds);
        dyn_branch.push('\n');
    }

    for (idx, dest) in table.elements.iter().enumerate() {
        if let Some(dest) = dest {
            let label = get_block(&Label::new(*dest, 0));

            match BRANCH_CONV {
                BranchConv::Grid => {
                    // FIXME: Reg0
                    let b = format!("execute if score %work%0%lo reg matches {}..{} run setblock !BLOCKPOS!{}! minecraft:redstone_block destroy\n", idx, idx, label);
                    dyn_branch.push_str(&b);
                }
                BranchConv::Direct => {
                    dyn_branch.push_str(&read_cond());
                    dyn_branch.push('\n');

                    // FIXME: Reg0
                    let b = format!("execute if score %%tempcond reg matches {}..{} run function wasm:{}\n", idx, idx, label);
                    dyn_branch.push_str(&b);
                }
                bc => todo!("{:?}", bc)
            }
        }
    }

    if BRANCH_CONV == BranchConv::Direct {
        dyn_branch.push_str(&pop_cond());
        dyn_branch.push('\n');
    }

    // FIXME:
    //let e = format!("execute unless score %work%0%lo reg matches 0..{} run ", mc_functions.len() - 1);
    //dyn_branch.push_str(&e);
    //dyn_branch.push_str(r#"tellraw @a [{"text":"Attempt to branch to invalid function "},{"score":{"name":"%work%0%lo","objective":"reg"}}]"#);

    //mc_functions.push(("dyn_branch".to_string(), dyn_branch));

    dyn_branch
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

fn do_fixups(mc_functions: &mut Vec<(String, String)>) {
    // Apply fixups
    for func_idx in 0..mc_functions.len() {
        // FIXME: ugly hack
        while let Some(start) = mc_functions[func_idx].1.find("!B") {
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
                s => {
                    todo!("{:?}", s)
                }
            }
        }
    }
}

use datapack_vm::interpreter::InterpError;

fn run_and_compare2(mir: &mut State, cmd: &mut Interpreter, return_arity: usize) -> Vec<(i32, Option<i32>)> {
    match cmd.run_to_end() {
        Err(InterpError::BreakpointHit) => {
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
            Err(InterpError::BreakpointHit) => {
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

    compare_states(&mir, &cmd);

    let mut returns = Vec::new();
    for i in 0..return_arity {
        let lo = mir.registers.get_i32(Register::Return(i as u32));
        let hi = mir.registers.0.get(&Register::Return(i as u32).as_hi()).copied();
        returns.push((lo, hi));
    }

    returns

}

fn run_and_compare(basic_blocks: &[BasicBlock<Instr>], mc_functions: &[(String, String)], wasm_file: &WasmFile) -> Vec<(i32, Option<i32>)> {
    let mut mir = setup_state(&basic_blocks, wasm_file);

    let mut cmd = setup_commands(&mc_functions, &wasm_file.globals, &wasm_file.memory, &wasm_file.exports);

    let func_idx = wasm_file.exports.get_func("_start");
    let func_ty = wasm_file.functions.get_function_type(func_idx, &wasm_file.types);

    run_and_compare2(&mut mir, &mut cmd, func_ty.returns.len())
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
        let holder = datapack_vm::cir::ScoreHolder::new(reg.to_string()).unwrap();
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

        match &func_ty.returns[..] {
            [Type::I32] => {
                assert!(expected.1.is_none());
                assert_eq!(result[0].0, expected.0);
            }
            [Type::I64] => {
                assert_eq!(result[0].0, expected.0);
                assert_eq!(result[0].1.unwrap(), expected.1.unwrap());
            }
            r => todo!("{:?}", r)
        }

        //assert_eq!(run_wasm_file(path), expected);
    }

    fn test_mir<I>(program: Vec<Instr>, expected: I)
        where I: IntoIterator<Item=i32>,
    {
        let mut bb = BasicBlock::new(CodeFuncIdx(0), 0);
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

        loop {
            //println!("Stepping MIR, pc is {:?}", mir.pc);

            let mir_halted = mir.step();

            match cmd.run_to_end() {
                Err(InterpError::BreakpointHit) => {
                    let top_func = cmd.get_top_func();
                    compare_states(&mir, &cmd);
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

            println!();

        }

        compare_states(&mir, &cmd);

        for (idx, val) in expected.into_iter().enumerate() {
            assert_eq!(mir.registers.get_i32(Register::Return(idx as u32)), val);
        }

        //assert_eq!(ir_result, cmd_result);

        //assert_eq!(ir_result, expected);
    }


    #[test]
    #[ignore]
    fn chip8test() {
        test_whole_program(Path::new("../CHIP-8-Emulator/chip8.wasm"), 0);
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
        let val2_bts = val2.to_le_bytes();

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
        test_mir(load_word_unalign, Some(0x34_56_78_01));
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

        test_mir(instrs, Some(42));
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

        test_mir(instrs, Some(1));
    }

    // TODO: Add a test for division signs

    // TODO: Test I64 loads and stores


    fn load_state(data: &[u8]) -> TestState {
        let mut wasm_file = parse_wasm_file(data);
        link_intrinsics(&mut wasm_file);

        let basic_blocks = compile(&wasm_file);

        let mc_functions = assemble(&basic_blocks, &wasm_file, true);

        let state = prepare_state(&basic_blocks, &wasm_file);
        let interp = prepare_interp(&mc_functions, &wasm_file.globals, &wasm_file.memory);

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

    #[derive(Debug, Clone, PartialEq, Eq)]
    enum SExprToken {
        StrLit(String),
        IntLit(i64),
        Ident(String),
        OpenParen,
        CloseParen,
    }

    struct SExprLexer<'a> {
        s: &'a str
    }

    impl Iterator for SExprLexer<'_> {
        type Item = SExprToken;

        fn next(&mut self) -> Option<Self::Item> {
            while self.s.chars().next().unwrap().is_whitespace() {
                let mut indices = self.s.char_indices();
                indices.next();
                self.s = &self.s[indices.next().unwrap().0..];
            }

            if self.s.is_empty() {
                None
            } else if self.s.starts_with('(') {
                self.s = &self.s[1..];
                Some(SExprToken::OpenParen)
            } else if self.s.starts_with(')') {
                self.s = &self.s[1..];
                Some(SExprToken::CloseParen)
            } else if self.s.starts_with('"') {
                self.s = &self.s[1..];
                let mut end_idx = None;
                for (idx, c) in self.s.char_indices() {
                    if c == '\\' {
                        todo!()
                    } else if c == '"' {
                        end_idx = Some(idx);
                        break
                    }
                }
                let end_idx = end_idx.unwrap();
                let name = self.s[..end_idx].to_string();
                self.s = &self.s[end_idx+1..];
                Some(SExprToken::StrLit(name))
            } else if self.s.starts_with('-') || self.s.chars().next().unwrap().is_digit(10) {
                let is_neg = if self.s.starts_with('-') {
                    self.s = &self.s[1..];
                    true
                } else {
                    false
                };

                let radix = if self.s.starts_with("0x") || self.s.starts_with("0X") {
                    self.s = &self.s[2..];
                    16
                } else if self.s.starts_with("0b") || self.s.starts_with("0B") {
                    self.s = &self.s[2..];
                    2
                // TODO: ???
                /*
                } else if self.s.starts_with('0')  {
                    todo!("octal?")
                */
                } else {
                    10
                };

                let end_idx = self.s.find(|c: char| !c.is_digit(radix)).unwrap();
                let mut value = u64::from_str_radix(&self.s[..end_idx], radix).unwrap_or_else(|e| panic!("Failed to parse {} because {}", &self.s[..end_idx], e)) as i64;
                if is_neg {
                    value *= -1;
                }

                self.s = &self.s[end_idx..];
                Some(SExprToken::IntLit(value))
            } else {
                let end_idx = self.s.find(|c: char| c.is_whitespace() || c == '(' || c == ')').unwrap();
                let name = self.s[..end_idx].to_string();
                self.s = &self.s[end_idx..];
                Some(SExprToken::Ident(name))
            }
        }
    }

    struct SExprParser<'a> {
        s: std::iter::Peekable<SExprLexer<'a>>
    }

    impl<'a> SExprParser<'a> {
        pub fn new(s: &'a str) -> Self {
            SExprParser { s: SExprLexer { s }.peekable() }
        }

        pub fn parse(&mut self) -> Result<SExpr, String> {
            if self.s.peek() == Some(&SExprToken::OpenParen) {
                let (name, params) = self.parse_parenthesized()?;
                if name == "assert_return" {
                    let mut params = params.into_iter();
                    let lhs = params.next().unwrap();
                    let rhs = params.collect();
                    Ok(SExpr::AssertReturn(Box::new(lhs), rhs))
                } else if name == "assert_trap" {
                    let mut params = params.into_iter();
                    let lhs = params.next().unwrap();
                    let rhs = params.next().unwrap();
                    assert!(params.next().is_none());
                    let rhs = if let SExpr::String(s) = rhs {
                        s
                    } else {
                        panic!()
                    };
                    Ok(SExpr::AssertTrap(Box::new(lhs), rhs))
                } else {

                    Ok(SExpr::Node { name, params })
                }
            } else {
                let tok = self.s.next().unwrap();
                match tok {
                    SExprToken::IntLit(i) => Ok(SExpr::Int(i)),
                    SExprToken::StrLit(s) => Ok(SExpr::String(s)),
                    SExprToken::Ident(i) => Ok(SExpr::Ident(i)),
                    _ => todo!("{:?}", tok)
                }
            }
        }

        pub fn parse_parenthesized(&mut self) -> Result<(String, Vec<SExpr>), String> {
            if self.s.next() != Some(SExprToken::OpenParen) {
                return Err("expected opening parenthesis".to_string())
            }

            let name = if let Some(SExprToken::Ident(i)) = self.s.next() {
                i
            } else {
                return Err("expected ident".to_string())
            };

            let mut params = Vec::new();
            while self.s.peek() != Some(&SExprToken::CloseParen) {
                params.push(self.parse()?);
            }

            if self.s.next() != Some(SExprToken::CloseParen) {
                return Err("expecting closing paren".to_string());
            }

            Ok((name, params))
        }
    }

    #[derive(Debug)]
    enum SExpr {
        Node {
            name: String,
            params: Vec<SExpr>,
        },
        AssertReturn(Box<SExpr>, Vec<SExpr>),
        AssertTrap(Box<SExpr>, String),
        String(String),
        Ident(String),
        Int(i64),
    }

    impl std::str::FromStr for SExpr {
        type Err = String;

        fn from_str(mut s: &str) -> Result<Self, Self::Err> {
            SExprParser::new(s).parse()
        }
    }

    struct TestState<'a> {
        interp: Interpreter,
        state: State,
        basic_blocks: Vec<BasicBlock<Instr>>,
        mc_functions: Vec<(String, String)>,
        wasm_file: WasmFile<'a>,
    }

    use std::convert::TryFrom;

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
                                let holder = datapack_vm::cir::ScoreHolder::new(Register::Param(idx as u32).get_lo()).unwrap();
                                let obj = "reg".to_string();
                                self.interp.scoreboard.set(&holder, &obj, *arg);
                            }
                            [WasmValue::I64(arg)] => {
                                self.state.registers.set_i64(Register::Param(idx as u32), *arg);
                                let obj = "reg".to_string();
                                let holder_lo = datapack_vm::cir::ScoreHolder::new(Register::Param(idx as u32).get_lo()).unwrap();
                                let arg_lo = *arg as i32;
                                self.interp.scoreboard.set(&holder_lo, &obj, arg_lo);
                                let holder_hi = datapack_vm::cir::ScoreHolder::new(Register::Param(idx as u32).get_hi()).unwrap();
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

                    let result = run_and_compare2(&mut self.state, &mut self.interp, func_ty.returns.len());

                    let mut returns = Vec::new();
                    for (val, ty) in result.into_iter().zip(func_ty.returns.iter()) {
                        match ty {
                            Type::I32 => returns.push(WasmValue::I32(val.0)),
                            Type::I64 => returns.push(WasmValue::I64((val.0 as i64) | ((val.1.unwrap() as i64) << 32))),
                            t => todo!("{:?}", t)
                        }
                    }

                    returns
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
                    println!("TODO: ASSERT TRAP");
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