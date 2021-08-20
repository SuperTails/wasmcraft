use crate::{HalfRegister, mir::{Instr, InstrUses, MirBasicBlock, Usage}};

use super::OptAction;

pub(crate) fn get_actions(basic_block: &MirBasicBlock) -> Vec<OptAction> {
    let mut actions = Vec::new();

    'next: for (start_idx, reg) in candidate_writes(basic_block) {
        if let Some(end_idx) = find_finishing_write(basic_block, start_idx + 1, reg) {
            let mut temp_actions = vec![OptAction::Replace {
                id: start_idx,
                instr: Vec::new(),
            }];

            for idx in start_idx + 1..end_idx {
                let uses = basic_block.instrs[idx].get_uses();
                if uses.get(reg).is_none() {
                    // This instruction is irrelevant
                    continue;
                }

                if is_candidate_write(&basic_block.instrs[idx], true) == Some(reg) {
                    temp_actions.push(OptAction::Replace {
                        id: idx,
                        instr: Vec::new(),
                    })
                } else {
                    continue 'next;
                }
            }

            actions.extend(temp_actions);
        }
    }

    actions
}

pub fn is_candidate_write(instr: &Instr, readwrite: bool) -> Option<HalfRegister> {
    let intended_use = if readwrite { Usage::ReadWrite } else { Usage::Write };

    if instr.has_side_effect() {
        None
    } else {
        let uses = instr.get_uses();
        match uses {
            InstrUses::All(_) => None,
            InstrUses::Some(map) => {
                let mut reg = None;
            
                for (&r, &u) in map.iter() {
                    if u == intended_use {
                        if reg.is_none() {
                            reg = Some(r);
                        } else {
                            return None;
                        }
                    } else if u != Usage::Read {
                        return None;
                    }
                }

                reg
            }
        }
    }
}

// False positive lint
#[allow(clippy::needless_lifetimes)]
/// Candidate writes are ones that:
///  - Have no side effects
///  - Strictly write to exactly one register
///  - Strictly read from all other registers
pub fn candidate_writes<'a>(basic_block: &'a MirBasicBlock) -> impl Iterator<Item=(usize, HalfRegister)> + 'a {
    basic_block.instrs.iter().enumerate().filter_map(|(idx, instr)| {
        is_candidate_write(instr, false).map(|reg| (idx, reg))
    })
}

/// Finishing writes are ones that strictly write to the given register
pub fn find_finishing_write(basic_block: &MirBasicBlock, search_start: usize, reg: HalfRegister) -> Option<usize> {
    basic_block.instrs.iter().enumerate().skip(search_start).find_map(|(idx, instr)| {
        if instr.get_uses().get(reg) == Some(Usage::Write) {
            Some(idx)
        } else {
            None
        }
    })
}

#[cfg(test)]
mod test {
    use crate::{CodeFuncIdx, Label, OpStack, Register, mir::Terminator};

    use super::*;

    #[test]
    fn dead_write_test() {
        let block = MirBasicBlock { 
            label: Label::new(CodeFuncIdx(0), 0),
            op_stack: OpStack::new(),
            instrs: vec![
                Instr::SetConst(Register::Work(0).as_lo(), 42),
                Instr::SetConst(Register::Work(1).as_lo(), 11),
                Instr::AddI32Const(Register::Work(0).as_lo(), 24),
                Instr::SetConst(Register::Work(0).as_lo(), 0),
            ],
            terminator: Terminator::Halt,
        };

        let actions = get_actions(&block);

        assert_eq!(actions.len(), 2);
        assert!(matches!(actions[0], OptAction::Replace { id: 0, .. }));
        assert!(matches!(actions[1], OptAction::Replace { id: 2, .. }));
    }
}