use wasmparser::{Data, DataKind, Element, Export, FuncType, Global, GlobalType, Import, ImportSectionEntryType, MemoryType, Operator, Parser, Payload, TableType, Type, TypeDef, TypeOrFuncType};
use crate::{CodeFuncIdx, Register, eval_init_expr};
use std::convert::TryInto;

pub struct DataList<'a> {
    pub data: Vec<Data<'a>>,
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

pub struct TableList {
    pub tables: Vec<TableType>,
}

impl TableList {
    pub fn new() -> Self {
        TableList { tables: Vec::new() }
    }

    pub fn add_table(&mut self, ty: TableType) {
        self.tables.push(ty);
    }
}

pub struct ElementList<'a> {
    pub elements: Vec<Element<'a>>,
}

impl<'a> ElementList<'a> {
    pub fn new() -> Self {
        ElementList { elements: Vec::new() }
    }

    pub fn add_element(&mut self, elem: Element<'a>) {
        self.elements.push(elem);
    }
}

pub struct GlobalList<'a> {
    pub globals: Vec<Global<'a>>,
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

    pub fn init(&self) -> Vec<String> {
        let mut cmds = Vec::new();

        for (i, global) in self.globals.iter().enumerate() {
            let reg = Register::Global(i as u32).as_lo();
            let value = eval_init_expr(global.init_expr);

            if !matches!(global.ty, GlobalType { content_type: Type::I32, mutable: _ }) {
                todo!()
            }

            cmds.push(format!("scoreboard players set {} reg {}", reg, value));
        }

        cmds
    }
}

pub struct ImportList<'a> {
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

pub struct ExportList<'a> {
    pub exports: Vec<Export<'a>>,
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
        for e in self.exports.iter() {
            if e.field == s {
                return CodeFuncIdx(e.index);
            }
        }

        panic!("couldn't find {:?}", s);
    }
}

pub struct MemoryList {
    pub memory: Vec<MemoryType>,
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

    /*pub fn get_offset(&self, memarg: MemoryImmediate) -> String {
        // FIXME:
        format!("{} 0 8", memarg.offset)
    }*/

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



pub struct FunctionBody<'a> {
    pub operators: Vec<Operator<'a>>,
    pub locals: Vec<(u32, Type)>,
}

pub struct TypeList<'a> {
    pub types: Vec<TypeDef<'a>>,
}

impl<'a> TypeList<'a> {
    pub fn new() -> Self {
        TypeList {
            types: Vec::new()
        }
    }

    pub fn add_type(&mut self, ty: TypeDef<'a>) {
        self.types.push(ty);
    }

    pub fn get_input_tys(&self, ty: TypeOrFuncType) -> Box<[Type]> {
        match ty {
            TypeOrFuncType::Type(_) => Vec::new().into_boxed_slice(),
            TypeOrFuncType::FuncType(i) => {
                let ty = &self.types[i as usize];
                if let TypeDef::Func(ty) = ty {
                    ty.params.clone()
                } else {
                    panic!()
                }
            }
        }
    }

    pub fn get_output_tys(&self, ty: TypeOrFuncType) -> Box<[Type]> {
        match ty {
            TypeOrFuncType::Type(Type::EmptyBlockType) => Vec::new().into_boxed_slice(),
            TypeOrFuncType::Type(t) => vec![t].into_boxed_slice(),
            TypeOrFuncType::FuncType(i) => {
                let ty = &self.types[i as usize];
                if let TypeDef::Func(ty) = ty {
                    ty.returns.clone()
                } else {
                    panic!()
                }
            }
        }
    }
}


#[derive(Debug, Clone)]
pub struct FunctionList {
    pub functions: Vec<u32>,
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


pub struct WasmFile<'a> {
    pub types: TypeList<'a>,
    pub globals: GlobalList<'a>,
    pub memory: MemoryList,
    pub exports: ExportList<'a>,
    pub imports: ImportList<'a>,
    pub data: DataList<'a>,
    pub tables: TableList,
    pub elements: ElementList<'a>,
    pub functions: FunctionList,
    pub bodies: Vec<FunctionBody<'a>>,
}

pub fn parse_wasm_file(file: &[u8]) -> WasmFile {
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

    for payload in Parser::new(0).parse_all(file) {
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

    for f in imports.imports.iter() {
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

