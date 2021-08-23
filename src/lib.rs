mod datapack;
mod state;
mod sexpr;
mod code_emitter;
mod mir;
mod wasm;
mod cmd_info;

use datapack_common::functions::{Command, Function};
use datapack_common::functions::command_components::{ScoreHolder, Objective};
use datapack_common::functions::command_components::FunctionIdent as FunctionId;

use code_emitter::{CodeEmitter, ConstantPool, assemble};
use mir::{Axis, BranchTarget, Instr, MirBasicBlock, Relation, Terminator, compile, get_next_state};

use datapack_vm::interpreter::Interpreter;
use wasm::{ExportList, GlobalList, MemoryList, WasmFile, parse_wasm_file};
use std::convert::TryInto;
use std::path::{Path, PathBuf};
use std::str;
use once_cell::sync::OnceCell;
use std::collections::{BTreeMap, HashMap, HashSet};

use state::State;

use wasmparser::{DataKind, ElementItem, ElementKind, InitExpr, MemoryType, Operator, Type};

/// Represents wasm's "Function Index Space"
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CodeFuncIdx(u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Label {
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

impl MirBasicBlock {
    fn lower(&self, bb_idx: usize, insert_sync: bool, state_info: Option<&StateInfo>, pool: &mut ConstantPool, call_graph: &CallGraph) -> BasicBlock<String> {
        // TODO: Should I use a virtual stack always?
        let instrs = CodeEmitter::emit_all(self, Some(bb_idx), true, insert_sync, state_info, pool, self.label.func_idx, Some(call_graph));

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
pub enum Half {
    Hi,
    Lo,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HalfRegister(pub Register, pub Half);

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
        let mut parts = s.split('%');

        if parts.next() != Some("") {
            return Err(s.to_string())
        }

        let name = parts.next().ok_or_else(|| s.to_string())?;

        let idx = parts.next().ok_or_else(|| s.to_string())?;
        let idx = idx.parse::<u32>().map_err(|_| s.to_string())?;

        let half = parts.next().ok_or_else(|| s.to_string())?;
        let half = match half {
            "hi" => Half::Hi,
            "lo" => Half::Lo,
            _ => return Err(s.to_string()),
        };

        match name {
            "work" => {
                let ns = parts.next().ok_or_else(|| s.to_string())?;
                let ns = ns.parse::<u32>().map_err(|_| s.to_string())?;

                Ok(Self(Register::Work(idx, ns), half))
            }
            "stack" => {
                let ns = parts.next().ok_or_else(|| s.to_string())?;
                let ns = ns.parse::<u32>().map_err(|_| s.to_string())?;

                Ok(Self(Register::Stack(idx, ns), half))
            }
            "param" => {
                if parts.next().is_some() { return Err(s.to_string()); }

                Ok(Self(Register::Param(idx), half))
            }
            "return" => {
                if parts.next().is_some() { return Err(s.to_string()); }

                Ok(Self(Register::Return(idx), half))
            }
            "temp" => {
                if parts.next().is_some() { return Err(s.to_string()); }

                Ok(Self(Register::Temp(idx), half))
            }
            _ => Err(s.to_string()),
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RealOpStack {
    stack: OpStack,
    /// The number of operands that are stored on the actual stack.
    /// Always less than or equal to the size of the full stack.
    real_size: usize,

    /// If false, this will behave like a normal [`OpStack`]
    use_virtual: bool,
}

impl RealOpStack {
    pub fn new(use_virtual: bool) -> Self {
        RealOpStack {
            stack: OpStack::new(),
            real_size: 0,
            use_virtual,
        }
    }

    pub fn reify_all(&mut self) {
        self.real_size = self.stack.0.len();
    }

    pub fn push_ty(&mut self, ty: Type) {
        if !self.use_virtual {
            self.real_size += 1;
        }

        self.stack.push_ty(ty);
    }

    pub fn push_i32(&mut self) {
        self.stack.push_i32();
    }

    pub fn push_i64(&mut self) {
        self.stack.push_i64();
    }

    /// Returns true if the value was on the real stack
    pub fn pop_value(&mut self) -> (Type, bool) {
        let is_real = self.real_size == self.stack.0.len();

        if is_real {
            self.real_size -= 1;
        }

        (self.stack.pop_value(), is_real)
    }

    pub fn pop_ty(&mut self, ty: Type) -> bool {
        let (t, real) = self.pop_value();
        assert_eq!(t, ty);
        real
    }

    pub fn pop_i32(&mut self) -> bool {
        self.pop_ty(Type::I32)
    }

    pub fn pop_i64(&mut self) -> bool {
        self.pop_ty(Type::I64)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpStack(Vec<Type>);

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Register {
    Work(u32, u32),
    Param(u32),
    Return(u32),
    Stack(u32, u32),
    Global(u32),
    Temp(u32),
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
            Register::Work(i, ns) => {
                format!("%work%{}%lo%{}", i, ns)
            }
            Register::Param(i) => {
                format!("%param%{}%lo", i)
            }
            Register::Return(i) => {
                format!("%return%{}%lo", i)
            }
            Register::Stack(i, ns) => {
                format!("%stack%{}%lo%{}", i, ns)
            }
            Register::Global(i) => {
                format!("%global%{}%lo", i)
            }
            Register::Temp(i) => {
                format!("%temp%{}%lo", i)
            }
        }
    }

    pub fn get_hi(&self) -> String {
        match self {
            Register::Work(i, ns) => {
                format!("%work%{}%hi%{}", i, ns)
            }
            Register::Param(i) => {
                format!("%param%{}%hi", i)
            }
            Register::Return(i) => {
                format!("%return%{}%hi", i)
            }
            Register::Stack(i, ns) => {
                format!("%stack%{}%hi%{}", i, ns)
            }
            Register::Global(i) => {
                format!("%global%{}%hi", i)
            }
            Register::Temp(i) => {
                format!("%temp%{}%hi", i)
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

#[derive(Debug, PartialEq, Eq)]
pub struct StateInfo {
    entry: RealOpStack,
    exit: Vec<(usize, RealOpStack)>,
}

impl StateInfo {
    fn new(basic_block: &MirBasicBlock, entry: RealOpStack, call_graph: &CallGraph) -> Self {
        let exit = get_next_state(basic_block, entry.clone(), call_graph);
        StateInfo { entry, exit }
    }
}

fn find_real_stack_sizes(basic_blocks: &[MirBasicBlock], call_graph: &CallGraph) -> Vec<StateInfo> {
    let mut states = BTreeMap::new();

    let entry_state = if BRANCH_CONV == BranchConv::Direct {
        RealOpStack {
            stack: OpStack(Vec::new()),
            real_size: 0,
            use_virtual: true,
        }
    } else {
        RealOpStack {
            stack: OpStack(vec![Type::I32]),
            real_size: 1,
            use_virtual: true,
        }
    };

    let start_state = StateInfo::new(&basic_blocks[0], entry_state, call_graph);

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
                let next_info = StateInfo::new(next_block, next_state.clone(), call_graph);
                states.insert(*next_idx, next_info);
                to_visit.push(*next_idx);
            }
        }
    }

    assert_eq!(states.len(), basic_blocks.len());

    basic_blocks.iter().map(|bb| states.remove(&bb.label.idx).unwrap()).collect()
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


fn get_entry_point(function: CodeFuncIdx) -> String {
    get_block_name(&Label::new(function, 0))
}

fn get_block_name(label: &Label) -> String {
    format!("__wasm{}_{}", label.func_idx.0, label.idx)
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BlockPos {
    x: i32,
    y: i32,
    z: i32,
}

impl std::fmt::Display for BlockPos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.x, self.y, self.z)
    }
}

pub struct CallGraph(HashMap<CodeFuncIdx, HashSet<CodeFuncIdx>>);

impl CallGraph {
    pub fn new() -> Self {
        CallGraph(HashMap::new())
    }

    pub fn build(blocks: &[MirBasicBlock]) -> Self {
        let mut result = CallGraph::new();

        for block in blocks.iter() {
            let caller = block.label.func_idx;

            for instr in block.instrs.iter() {
                match instr {
                    Instr::Call(callee) => result.add_edge(caller, *callee),
                    Instr::DynCall(_, _) => todo!(),
                    _ => {}
                }
            }

            match &block.terminator {
                Terminator::Call { func_idx, .. } => result.add_edge(caller, *func_idx),
                Terminator::DynCall { .. } => todo!(),
                _ => {}
            }
        }

        result
    }

    pub fn add_edge(&mut self, caller: CodeFuncIdx, callee: CodeFuncIdx) {
        self.0.entry(caller).or_insert_with(HashSet::new).insert(callee);
    }

    pub fn can_reach(&self, caller: CodeFuncIdx, callee: CodeFuncIdx) -> bool {
        let mut visited = HashSet::new();

        let mut queue = vec![caller];

        while let Some(next) = queue.pop() {
            if visited.contains(&next) {
                continue
            }

            if next == callee {
                return true;
            }

            visited.insert(next);

            if let Some(dests) = self.0.get(&next) {
                for dest in dests.iter() {
                    queue.push(*dest);
                }
            }
        }

        false
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
                    datapack.write_function(folder_path, "intrinsic", n, &contents).unwrap();
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

    let mut basic_blocks = compile(&wasm_file);

    optimize_mir(&mut basic_blocks);

    let mc_functions = assemble(&basic_blocks, &wasm_file, VERIFY_OUTPUT);

    println!("Done assembling");

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


fn link_intrinsics(file: &mut WasmFile) {
    /*
    let math = std::fs::read("../math2.wasm").unwrap();
    let math_file = parse_wasm_file(&math);

    link(file, math_file);
    */
}

fn run_ir(basic_blocks: &[MirBasicBlock], file: &WasmFile) -> State {
    let mut state = setup_state(basic_blocks, file);

    loop {
        if state.step().unwrap() { break }
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

fn get_tables(file: &WasmFile, basic_blocks: &[MirBasicBlock]) -> Vec<state::Table> {
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

fn prepare_state(basic_blocks: &[MirBasicBlock], file: &WasmFile) -> State {
    let tables = get_tables(file, basic_blocks);

    let mut state = State::new(basic_blocks.to_owned(), &file.globals, &file.memory, tables);

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

fn setup_state(basic_blocks: &[MirBasicBlock], file: &WasmFile) -> State {
    let mut state = prepare_state(basic_blocks, file);

    set_state_pos(&mut state, &file.exports, "_start");


    /*state.call(state.get_pc(&Label::new(exports.get_func("__wasm_call_ctors"), 0)));

    println!("=============");

    state.call(state.get_pc(&Label::new(exports.get_func("main"), 0)));*/

    state

}

struct BBGroupBy<'a> {
    list: &'a [MirBasicBlock]
}

impl<'a> Iterator for BBGroupBy<'a> {
    type Item = &'a [MirBasicBlock];

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

fn get_stack_states(basic_blocks: &[MirBasicBlock], call_graph: &CallGraph) -> Vec<StateInfo> {
    let mut stack_states = Vec::new();
    
    let iter = BBGroupBy { list: basic_blocks };

    for group in iter {
        stack_states.extend(find_real_stack_sizes(group, call_graph));
    }

    stack_states
}

fn optimize_mir(basic_blocks: &mut [MirBasicBlock]) {
    let call_graph = CallGraph::build(basic_blocks);
    
    let stack_states = get_stack_states(basic_blocks, &call_graph);

    for (bb, state) in basic_blocks.iter_mut().zip(stack_states.iter()) {
        if bb.instrs.is_empty() {
            continue;
        }

        let prev_size = bb.instrs.len();

        let actions = state::const_prop::get_actions(bb, &state.entry.stack);

        state::apply_actions(bb, actions);

        let actions = state::stack_drops::get_actions(bb, &state.entry.stack);

        state::apply_actions(bb, actions);

        let actions = state::dead_writes::get_actions(bb);
        
        state::apply_actions(bb, actions);

        if prev_size != bb.instrs.len() {
            println!("{} -> {}", prev_size, bb.instrs.len());
        }
    }

    println!("Done optimizing");
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
    setup.push_str("fill 0 0 0 50 0 0 minecraft:air replace\n");
    setup.push_str("fill 0 0 0 50 0 0 minecraft:jukebox{RecordItem:{id:\"minecraft:stone\",Count:1b,tag:{Memory:0}}} replace\n");
    
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

use datapack_vm::interpreter::InterpError;

fn run_and_compare2(mir: &mut State, cmd: &mut Interpreter, return_types: &[Type]) -> Vec<WasmValue> {
    loop {
        let pc = mir.pc.0.last().copied();

        match cmd.run_to_end() {
            Err(InterpError::SyncHit(f, i)) => {
                let top_func = cmd.get_top_func();

                let pc = pc.expect("sync hit but MIR halted");

                if pc == (f, i) {
                    compare_states(mir, cmd);

                    println!();
                    mir.step().unwrap();
                }

                cmd.finish_unwind(top_func);
            }
            Ok(()) => {
                assert!(pc.is_none());

                compare_states(mir, cmd);

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
            mir.registers.get_typed(Register::Return(idx as u32), *ty).unwrap()
        })
        .collect()
}

fn run_and_compare(basic_blocks: &[MirBasicBlock], mc_functions: &[(String, String)], wasm_file: &WasmFile) -> Vec<WasmValue> {
    let mut mir = setup_state(basic_blocks, wasm_file);

    let mut cmd = setup_commands(mc_functions, &wasm_file.globals, &wasm_file.memory, &wasm_file.exports);

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
    use wasmparser::{Export, ExternalKind, FuncType, ResizableLimits, TypeDef};
    use std::path::Path;

    /*fn run_wasm_file(path: &Path) -> i32 {
        use wasmtime::{Engine, Module, Instance, Store};

        let engine = Engine::default();
        let wasm = std::fs::read(path).unwrap();
        let module = Module::new(&engine, wasm).unwrap();
        
        let mut store = Store::new(&engine, 0);
        let instance = Instance::new(&mut store, &module, &[]).unwrap();
        let start_func = instance.get_typed_func::<(), i32, _>(&mut store, "_start").unwrap();
        start_func.call(&mut store, ()).unwrap()
    }*/

    fn test_whole_program(path: &Path, expected: i32) {
        test_whole_program2(path, (expected, None))
    }

    fn test_whole_program2(path: &Path, expected: (i32, Option<i32>)) {

        let file = std::fs::read(path).unwrap();
        let mut wasm_file = parse_wasm_file(&file);
        link_intrinsics(&mut wasm_file);

        let mut basic_blocks = compile(&wasm_file);

        optimize_mir(&mut basic_blocks);

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
        let mut bb = MirBasicBlock::new(CodeFuncIdx(0), 0, OpStack::new());
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
        instrs.extend(Instr::set_i64_const(Register::Temp(0), lhs));
        instrs.extend(Instr::set_i64_const(Register::Temp(1), rhs));
        instrs.push(Instr::I64UComp { dst: Register::Return(0), lhs: Register::Temp(0), op: Relation::LessThan, rhs: Register::Temp(1) });

        test_mir(instrs, Some(expected as i32));
    }

    fn i64_scomp_single(lhs: i64, rhs: i64) {
        let expected = lhs < rhs;

        let mut instrs = Vec::new();
        instrs.extend(Instr::set_i64_const(Register::Work(0, 0), lhs));
        instrs.extend(Instr::set_i64_const(Register::Work(1, 0), rhs));
        instrs.push(Instr::I64SComp { dst: Register::Return(0), lhs: Register::Work(0, 0), op: Relation::LessThan, rhs: Register::Work(1, 0) });

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
        instrs.extend(Instr::i64_neg(Register::Return(0), 0));

        test_mir(instrs, Some(expected));
    }

    fn i64_mul_single(lhs: i64, rhs: i64) {
        let expected = lhs.wrapping_mul(rhs);

        let mut instrs = Vec::new();
        instrs.extend(Instr::set_i64_const(Register::Work(0, 0), lhs));
        instrs.extend(Instr::set_i64_const(Register::Work(1, 0), rhs));
        instrs.extend(Instr::i64_mul(Register::Return(0), Register::Work(0, 0), Register::Work(1, 0), 0));

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
            Instr::PopI32Into(Register::Work(0, 0)),
            Instr::SetMemPtr(Register::Work(0, 0).as_lo().into()),
            
            Instr::PushI32Const(val1),
            Instr::PopI32Into(Register::Work(1, 0)),
            Instr::StoreI32(Register::Work(1, 0), 2),

            Instr::PushI32Const(8),
            Instr::PopI32Into(Register::Work(0, 0)),
            Instr::SetMemPtr(Register::Work(0, 0).as_lo().into()),
            
            Instr::PushI32Const(val2),
            Instr::PopI32Into(Register::Work(1, 0)),
            Instr::StoreI32(Register::Work(1, 0), 2),
        ];

        let mut load_byte = prelude.clone();
        load_byte.extend([
            Instr::PushI32Const(5),
            Instr::PopI32Into(Register::Work(0, 0)),
            Instr::SetMemPtr(Register::Work(0, 0).as_lo().into()),

            Instr::LoadI32_8U(Register::Return(0), 0),
        ]);
        test_mir(load_byte, Some(val1_bts[1] as u32 as i32));

        let mut load_halfword_unalign = prelude.clone();
        load_halfword_unalign.extend([
            Instr::PushI32Const(5),
            Instr::PopI32Into(Register::Work(0, 0)),
            Instr::SetMemPtr(Register::Work(0, 0).as_lo().into()),

            Instr::LoadI32_16U(Register::Return(0), 0),
        ]);
        test_mir(load_halfword_unalign, Some(u16::from_le_bytes([val1_bts[1], val1_bts[2]]) as u32 as i32));

        let mut load_halfword_align = prelude.clone();
        load_halfword_align.extend([
            Instr::PushI32Const(6),
            Instr::PopI32Into(Register::Work(0, 0)),
            Instr::SetMemPtr(Register::Work(0, 0).as_lo().into()),

            Instr::LoadI32_16U(Register::Return(0), 0),
        ]);
        test_mir(load_halfword_align, Some(u16::from_le_bytes([val1_bts[2], val1_bts[3]]) as u32 as i32));

        let mut load_word_unalign = prelude;
        load_word_unalign.extend([
            Instr::PushI32Const(7),
            Instr::PopI32Into(Register::Work(0, 0)),
            Instr::SetMemPtr(Register::Work(0, 0).as_lo().into()),

            Instr::LoadI32(Register::Return(0), 0),
           
        ]);
        test_mir(load_word_unalign, Some(0x34_56_78_01_i32));
    }

    #[test]
    fn load_store() {
        let instrs = vec![
            Instr::PushI32Const(4),
            Instr::PopI32Into(Register::Work(0, 0)),
            Instr::SetMemPtr(Register::Work(0, 0).as_lo().into()),

            Instr::PushI32Const(42),
            Instr::PopI32Into(Register::Work(1, 0)),
            Instr::StoreI32(Register::Work(1, 0), 2),

            Instr::PushI32Const(4),
            Instr::PopI32Into(Register::Work(0, 0)),
            Instr::SetMemPtr(Register::Work(0, 0).as_lo().into()),

            Instr::LoadI32(Register::Return(0), 2),
        ];

        test_mir(instrs, Some(42_i32));
    }

    #[test]
    fn locals() {
        let instrs = vec![
            Instr::PushFrame(2),

            Instr::PushI32Const(42),
            Instr::PopI32Into(Register::Work(0, 0)),
            Instr::SetLocalPtr(0),
            Instr::StoreLocalI32(Register::Work(0, 0)),

            Instr::PushI32Const(27),
            Instr::PopI32Into(Register::Work(0, 0)),
            Instr::SetLocalPtr(1),
            Instr::StoreLocalI32(Register::Work(0, 0)),

            Instr::SetLocalPtr(0),
            Instr::LoadLocalI32(Register::Work(0, 0)),
            Instr::SetLocalPtr(1),
            Instr::LoadLocalI32(Register::Work(1, 0)),

            Instr::I32Op { dst: Register::Work(2, 0).as_lo(), lhs: Register::Work(0, 0).as_lo(), op: "/=", rhs: Register::Work(1, 0).as_lo().into() },

            Instr::PushI32From(Register::Work(2, 0)),
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

        let mut basic_blocks = compile(&wasm_file);

        optimize_mir(&mut basic_blocks);

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
        basic_blocks: Vec<MirBasicBlock>,
        mc_functions: Vec<(String, String)>,
        wasm_file: WasmFile<'a>,
    }

    use std::convert::TryFrom;
    use crate::sexpr::SExpr;
    use crate::wasm::{DataList, ElementList, FunctionList, ImportList, TableList, TypeList};

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
                SExpr::Node { ref name, .. } if name == "invoke" => {
                    self.eval_expr(&s);
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