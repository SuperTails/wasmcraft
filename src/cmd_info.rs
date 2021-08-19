use std::{collections::HashMap, rc::Rc};
use datapack_common::functions::{Command};
use datapack_common::functions::command_components::{Objective, ScoreOpKind, ScoreboardPlayer, ScoreboardTarget, Target};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Usage {
    Read,
    Write,
    ReadWrite,
}

#[derive(Debug, Clone)]
pub enum UsageSet {
    All,
    Some(HashMap<ScoreboardPlayer, Usage>)
} 

impl UsageSet {
    pub fn new() -> Self {
        UsageSet::Some(HashMap::new())
    }

    pub fn merge(&mut self, other: UsageSet) {
        match other {
            UsageSet::All => *self = UsageSet::All,
            UsageSet::Some(m) => {
                for (player, usage) in m {
                    self.add_usage(player, usage);
                }
            }
        }
    }

    pub fn add_usage(&mut self, player: ScoreboardPlayer, usage: Usage) {
        match self {
            UsageSet::All => {},
            UsageSet::Some(map) => {
                let prev = map.get(&player);
                match (prev, usage) {
                    // There was no previous usage so just insert this one
                    (None, _) => { map.insert(player, usage); }

                    // No change at all
                    (Some(Usage::Read), Usage::Read) |
                    (Some(Usage::Write), Usage::Write) |
                    (Some(Usage::ReadWrite), _) => {}

                    // Two differing usages, so just promote the whole thing to read-write
                    (_, _) => { map.insert(player, Usage::ReadWrite); },
                }
            }
        }
    }

    pub fn get_usage(&self, player: &ScoreboardPlayer) -> Option<Usage> {
        match self {
            UsageSet::All => Some(Usage::ReadWrite),
            UsageSet::Some(m) => m.get(player).copied(),
        }
    }

    pub fn get_single_write(&self) -> Option<&ScoreboardPlayer> {
        match self {
            UsageSet::All => None,
            UsageSet::Some(m) => {
                let mut result = None;

                for (player, usage) in m.iter() {
                    if *usage == Usage::Write {
                        if result.is_some() {
                            return None;
                        } else {
                            result = Some(player);
                        }
                    }
                }

                result
            }
        }
    }
}

pub struct CommandData {
    usages: UsageSet,
    has_side_effect: bool,
}

impl CommandData {
    pub fn new(cmd: &Command) -> Self {
        CommandData { usages: get_usages(cmd), has_side_effect: has_side_effect(cmd) }
    }
}

pub fn has_side_effect(cmd: &Command) -> bool {
    match cmd {
        Command::Comment(_) |
        Command::ScoreOp(_) |
        Command::ScoreAdd(_) |
        Command::ScoreGet(_) |
        Command::ScoreSet(_) => false,

        _ => true,
    }
}

fn get_scoreboard_player(target: ScoreboardTarget, target_obj: Objective) -> ScoreboardPlayer {
    let target = if let ScoreboardTarget::Target(Target::Name(target)) = target {
        target
    } else {
        todo!()
    };

    ScoreboardPlayer {
        player: Rc::new(target),
        scoreboard: Rc::new(target_obj),
    }
}

fn get_scoreboard_player2(target: Target, target_obj: Objective) -> ScoreboardPlayer {
    let target = if let Target::Name(target) = target {
        target
    } else {
        todo!()
    };

    ScoreboardPlayer {
        player: Rc::new(target),
        scoreboard: Rc::new(target_obj),
    }
}

// Writes are dead if:
//  - The sequence starts with a Write to the dead register
//  - The sequence ends with a Write to the dead register
//  - None of the commands involving the register have side effects
//  - None of the commands write to any register except the dead one
pub fn find_dead_writes(cmds: &[Command]) -> Option<Vec<usize>> {
    'range: for (start_index, dead_reg) in find_dead_write_start(cmds) {
        if let Some(end_index) = find_dead_write_end(cmds, start_index + 1, &dead_reg) {
            println!("end index: {}", end_index);
            let mut to_remove = Vec::new();
            for (idx, cmd) in cmds[start_index..end_index].iter().enumerate() {
                match check_dead_write(cmd, &dead_reg) {
                    DeadCmdResult::Invalid => continue 'range,
                    DeadCmdResult::NotUsed => continue,
                    DeadCmdResult::Removable => to_remove.push(idx + start_index),
                }
            }

            return Some(to_remove);
        }
    }

    None
}

enum DeadCmdResult {
    Removable,
    Invalid,
    NotUsed,
}

fn check_dead_write(cmd: &Command, dead_reg: &ScoreboardPlayer) -> DeadCmdResult {
    let data = CommandData::new(cmd);
    if let Some(usage) = data.usages.get_usage(dead_reg) {
        if data.has_side_effect {
            return DeadCmdResult::Invalid
        }

        match &data.usages {
            UsageSet::All => return DeadCmdResult::Invalid,
            UsageSet::Some(s) => {
                let mut has_other_writes = false;
                for (reg, usage) in s.iter() {
                    if reg != dead_reg && matches!(usage, Usage::Write | Usage::ReadWrite) {
                        has_other_writes = true;
                        break;
                    }
                }

                if has_other_writes {
                    return DeadCmdResult::Invalid;
                } else {
                    return DeadCmdResult::Removable;
                }
            }
        }
    }

    DeadCmdResult::NotUsed
}

fn find_dead_write_end(cmds: &[Command], start_index: usize, dead_reg: &ScoreboardPlayer) -> Option<usize> {
    cmds.iter().enumerate().skip(start_index).find_map(|(index, cmd)| {
        let data = CommandData::new(cmd);

        if data.has_side_effect {
            return None
        }

        data.usages.get_single_write().filter(|r| *r == dead_reg).map(|_| index)
    })
}

fn find_dead_write_start<'a>(cmds: &'a [Command]) -> impl Iterator<Item=(usize, ScoreboardPlayer)> + 'a {
    cmds.iter().enumerate().filter_map(|(index, cmd)| {
        let data = CommandData::new(cmd);

        if data.has_side_effect {
            return None
        }

        data.usages.get_single_write().map(|reg| (index, reg.clone()))
    })
}

pub fn get_usages(cmd: &Command) -> UsageSet {
    use datapack_common::functions::command::commands::*;

    match cmd {
        Command::Comment(_) => UsageSet::new(),
        Command::ScoreOp(ScoreOp { target, target_obj, op, source, source_obj }) => {
            let target = get_scoreboard_player(target.clone(), target_obj.clone());

            let source = get_scoreboard_player(source.clone(), source_obj.clone());

            let result = match op {
                ScoreOpKind::Add |
                ScoreOpKind::Sub |
                ScoreOpKind::Mul |
                ScoreOpKind::Div |
                ScoreOpKind::Mod |
                ScoreOpKind::Max |
                ScoreOpKind::Min => vec![(target, Usage::ReadWrite), (source, Usage::Read)],

                ScoreOpKind::Swap => vec![(target, Usage::ReadWrite), (source, Usage::ReadWrite)],

                ScoreOpKind::Assign => vec![(target, Usage::Write), (source, Usage::Read)],
            };

            UsageSet::Some(result.into_iter().collect())
        },
        Command::ScoreAdd(ScoreAdd { target, target_obj, score: _, remove: _ }) => {
            let target = get_scoreboard_player(target.clone(), target_obj.clone());
            let result = Some((target, Usage::ReadWrite));

            UsageSet::Some(result.into_iter().collect())
        }
        Command::ScoreSet(ScoreSet { target, target_obj, score: _ }) => {
            let target = get_scoreboard_player(target.clone(), target_obj.clone());
            let result = Some((target, Usage::Write));

            UsageSet::Some(result.into_iter().collect())
        }
        Command::ScoreGet(ScoreGet { target, target_obj }) => {
            let target = get_scoreboard_player(target.clone(), target_obj.clone());
            let result = Some((target, Usage::Read));

            UsageSet::Some(result.into_iter().collect())
        }
        Command::Execute(Execute { subcommands, run }) => {
            let mut usages = UsageSet::Some(HashMap::new());

            use datapack_common::functions::command::ExecuteSubCommand as Cmd;
            use datapack_common::functions::command::execute_sub_commands::*;

            for subcmd in subcommands.0.iter() {
                match subcmd {
                    Cmd::IfScoreMatches(IfScoreMatches { target, target_obj, .. }) => {
                        let target = get_scoreboard_player2(target.clone(), target_obj.clone());
                        usages.add_usage(target, Usage::Read);
                    }
                    Cmd::IfScoreRelation(IfScoreRelation { target, target_obj, source, source_obj, .. }) => {
                        let target = get_scoreboard_player2(target.clone(), target_obj.clone());
                        let source = get_scoreboard_player2(source.clone(), source_obj.clone());
                        usages.add_usage(target, Usage::Read);
                        usages.add_usage(source, Usage::Read);
                    }
                    Cmd::StoreScore(StoreScore { target, target_obj, .. }) => {
                        let target = get_scoreboard_player2(target.clone(), target_obj.clone());
                        usages.add_usage(target, Usage::Write);
                    }
                    Cmd::StoreStorage { .. } |
                    Cmd::IfBlock { .. } |
                    Cmd::As { .. } |
                    Cmd::At { .. } |
                    Cmd::Positioned { .. } => {},
                }
            }

            if let Some(run) = &run.0 {
                usages.merge(get_usages(run));
            }

            usages
        }
        Command::Teleport(..) => UsageSet::Some(HashMap::new()),
        // TODO:
        _ => UsageSet::All,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn find_dead_write_elim(code: &[&str]) -> Option<Vec<usize>> {
        let code = code
            .iter()
            .map(|s| s.to_owned().parse::<Command>().unwrap())
            .collect::<Vec<_>>();

        find_dead_writes(&code)
    }

    fn check_dead_write_elim(code: &[&str], expected: Option<Vec<usize>>) {
        assert_eq!(find_dead_write_elim(code), expected);
    }

    #[test]
    fn dead_write_elim1() {
        check_dead_write_elim(&[
            "scoreboard players set foo test 1",
            "scoreboard players set foo test 2",
        ], Some(vec![0]));
        
        check_dead_write_elim(&[
            "scoreboard players set foo test 1",
            "function test:do_stuff",
            "scoreboard players set foo test 2",
        ], None);

        check_dead_write_elim(&[
            "scoreboard players set foo test 1",
            "scoreboard players set bar test 1",
            "scoreboard players operation foo test += bar test",
            "scoreboard players set foo test 2",
        ], Some(vec![0, 2]));
    }

    #[test]
    fn dead_write_elim2() {
        let s = 
r##"# PushFrame(7)
# Push frame with 7 locals
execute at @e[tag=frameptr] run fill ~ ~ ~ ~6 ~ ~1 minecraft:jukebox{RecordItem:{id:"minecraft:stone",Count:1b,tag:{Memory:0}}}
execute as @e[tag=frameptr] at @e[tag=frameptr] run tp @s ~7 ~ ~
# Comment("#   Parameter 0")
# #   Parameter 0
# SetLocalPtr(0)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-1 0 1
# StoreLocalI64(Param(0))
execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %param%0%lo reg
execute at @e[tag=localptr] store result block ~ ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %param%0%hi reg
# Comment("#   Parameter 1")
# #   Parameter 1
# SetLocalPtr(1)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-2 0 1
# StoreLocalI64(Param(1))
execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %param%1%lo reg
execute at @e[tag=localptr] store result block ~ ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %param%1%hi reg
# Comment("I64Const { value: 63 }")
# I64Const { value: 63 }
# PushI64Const(63)
scoreboard players set %stack%0%lo reg 63
scoreboard players set %stack%0%hi reg 0
# Comment("LocalSet { local_index: 2 }")
# LocalSet { local_index: 2 }
# SetLocalPtr(2)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-3 0 1
# Drop
# SetConst(HalfRegister(Work(0), Lo), 63)
scoreboard players set %work%0%lo reg 63
# SetConst(HalfRegister(Work(0), Hi), 0)
scoreboard players set %work%0%hi reg 0
# StoreLocalI64(Work(0))
execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=localptr] store result block ~ ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
# Comment("I64Const { value: 0 }")
# I64Const { value: 0 }
# PushI64Const(0)
scoreboard players set %stack%0%lo reg 0
scoreboard players set %stack%0%hi reg 0
# Comment("LocalSet { local_index: 3 }")
# LocalSet { local_index: 3 }
# SetLocalPtr(3)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-4 0 1
# Drop
# SetConst(HalfRegister(Work(0), Lo), 0)
scoreboard players set %work%0%lo reg 0
# SetConst(HalfRegister(Work(0), Hi), 0)
scoreboard players set %work%0%hi reg 0
# StoreLocalI64(Work(0))
execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=localptr] store result block ~ ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
# Comment("I64Const { value: 0 }")
# I64Const { value: 0 }
# PushI64Const(0)
scoreboard players set %stack%0%lo reg 0
scoreboard players set %stack%0%hi reg 0
# Comment("LocalSet { local_index: 4 }")
# LocalSet { local_index: 4 }
# SetLocalPtr(4)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-5 0 1
# Drop
# SetConst(HalfRegister(Work(0), Lo), 0)
scoreboard players set %work%0%lo reg 0
# SetConst(HalfRegister(Work(0), Hi), 0)
scoreboard players set %work%0%hi reg 0
# StoreLocalI64(Work(0))
execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=localptr] store result block ~ ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
# Comment("Loop { ty: Type(EmptyBlockType) }")
# Loop { ty: Type(EmptyBlockType) }
#   Branch to __wasm0_3
#   Jump to __wasm0_3
function wasm:__wasm0_3
scoreboard players set %%taken wasm 1"##;

        let code = s.lines().map(|l| l.trim()).filter(|l| !l.is_empty()).collect::<Vec<&str>>();

        panic!("{:?}", find_dead_write_elim(&code));
    }
}