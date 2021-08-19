use wasmparser::Type;

use crate::mir::{Instr, MirBasicBlock};

use super::OptAction;


/// A cleanup pass for after constant propogation runs
struct StackDrop<'a> {
    block: &'a MirBasicBlock,

    stack: Vec<(Option<usize>, Type)>,

    actions: Vec<OptAction>,

    pc: usize
}

impl<'a> StackDrop<'a> {
    pub fn new(block: &'a MirBasicBlock, op_stack: &crate::OpStack) -> Self {
        let stack = op_stack.0.iter().map(|ty| (None, *ty)).collect();

        StackDrop { block, stack, actions: Vec::new(), pc: 0 }
    }

    pub fn run(&mut self) {
        while !self.step() {}
    }

    pub fn step(&mut self) -> bool {
        let instr = &self.block.instrs[self.pc];

        match instr {
            Instr::PushI32Const(_) | 
            Instr::PushI32From(_) => self.stack.push((Some(self.pc), Type::I32)),
            Instr::PushI64Const(_) |
            Instr::PushI64From(_) => self.stack.push((Some(self.pc), Type::I64)),

            Instr::PopI32Into(_) => {
                let (_, ty) = self.stack.pop().unwrap();
                assert_eq!(ty, Type::I32);
            }
            Instr::PopI64Into(_) => {
                let (_, ty) = self.stack.pop().unwrap();
                assert_eq!(ty, Type::I64);
            }

            Instr::Drop => {
                let (idx, _) = self.stack.pop().unwrap();
                if let Some(idx) = idx {
                    self.actions.push(OptAction::Replace {
                        id: idx,
                        instr: Vec::new(),
                    });
                    self.actions.push(OptAction::Replace {
                        id: self.pc,
                        instr: Vec::new(),
                    });
                }
            }

            _ => {},
        }

        self.pc += 1;
        
        self.pc == self.block.instrs.len()
    }
}

pub(crate) fn get_actions(basic_block: &MirBasicBlock, op_stack: &crate::OpStack) -> Vec<OptAction> {
    let mut drop = StackDrop::new(basic_block, op_stack);
    drop.run();
    drop.actions
}