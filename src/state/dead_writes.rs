use crate::mir::MirBasicBlock;

use super::OptAction;

pub(crate) struct DeadWrites<'a> {
	block: &'a MirBasicBlock,
	actions: Vec<OptAction>,
}

impl<'a> DeadWrites<'a> {
    pub fn new(block: &'a MirBasicBlock) -> Self {
        DeadWrites { block, actions: Vec::new() }
    }

    pub fn run(&mut self) {
        while !self.step() {}
    }

    pub fn step(&mut self) -> bool {
        todo!()
    }
}

pub(crate) fn get_actions(basic_block: &MirBasicBlock) -> Vec<OptAction> {
    let mut writes = DeadWrites::new(basic_block);
    writes.run();
    writes.actions
}