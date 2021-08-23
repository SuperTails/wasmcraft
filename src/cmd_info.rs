use std::rc::Rc;
use datapack_common::functions::{Command};
use datapack_common::functions::command_components::{Objective, ScoreOpKind, ScoreboardPlayer, ScoreboardTarget, Target};

use crate::{CmdUses, Usage};

pub struct CommandData {
    usages: CmdUses,
    has_side_effect: bool,
}

impl CommandData {
    pub fn new(cmd: &Command) -> Self {
        CommandData { usages: get_usages(cmd), has_side_effect: has_side_effect(cmd) }
    }
}

pub fn has_side_effect(cmd: &Command) -> bool {
    use datapack_common::functions::command::commands::{Gamerule, ExecuteRun, Execute};
    use datapack_common::functions::command::Optional;

    match cmd {
        Command::Comment(_) |
        Command::ScoreOp(_) |
        Command::ScoreAdd(_) |
        Command::ScoreGet(_) |
        Command::ScoreSet(_) |
        Command::DataGet(_) |
        Command::Gamerule(Gamerule { rule: _, value: Optional(None) }) => false,

        Command::ExecuteRun(ExecuteRun { command }) => {
            if let Some(command) = command.0.as_deref() {
                has_side_effect(command)
            } else {
                false
            }
        }
        Command::Execute(Execute { subcommands, run }) => {
            use datapack_common::functions::command::ExecuteSubCommand as Cmd;

            for subcmd in subcommands.0.iter() {
                match subcmd {
                    Cmd::StoreScore(_) |
                    Cmd::StoreStorage(_) => return true,

                    Cmd::IfScoreMatches(_) |
                    Cmd::IfScoreRelation(_) |
                    Cmd::IfBlock(_) |
                    Cmd::As(_) |
                    Cmd::At(_) |
                    Cmd::Positioned(_) => {},
                }
            }

            if let Some(run) = run.0.as_deref() {
                return has_side_effect(run);
            } else {
                false
            }
        }

        // TODO: Can I get this more fine grained for, e.g., instrinsics?
        Command::FuncCall(_) => true,

        Command::Gamerule(_) |
        Command::Tellraw(_) |
        Command::Teleport(_) |
        Command::Summon(_) |
        Command::Kill(_) |
        Command::ObjAdd(_) |
        Command::ObjRemove(_) |
        Command::DataModify(_) |
        Command::SetBlock(_) |
        Command::Fill(_) |
        Command::Clone(_) => true,
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

pub fn get_usages(cmd: &Command) -> CmdUses {
    use datapack_common::functions::command::commands::*;

    match cmd {
        Command::ScoreOp(ScoreOp { target, target_obj, op, source, source_obj }) => {
            let target = get_scoreboard_player(target.clone(), target_obj.clone());

            let source = get_scoreboard_player(source.clone(), source_obj.clone());

            match op {
                ScoreOpKind::Add |
                ScoreOpKind::Sub |
                ScoreOpKind::Mul |
                ScoreOpKind::Div |
                ScoreOpKind::Mod |
                ScoreOpKind::Max |
                ScoreOpKind::Min => CmdUses::some(vec![(target, Usage::ReadWrite), (source, Usage::Read)]),

                ScoreOpKind::Swap => CmdUses::some(vec![(target, Usage::ReadWrite), (source, Usage::ReadWrite)]),

                ScoreOpKind::Assign => CmdUses::some(vec![(target, Usage::Write), (source, Usage::Read)]),
            }
        },
        Command::ScoreAdd(ScoreAdd { target, target_obj, score: _, remove: _ }) => {
            let target = get_scoreboard_player(target.clone(), target_obj.clone());

            CmdUses::one(target, Usage::ReadWrite)
        }
        Command::ScoreSet(ScoreSet { target, target_obj, score: _ }) => {
            let target = get_scoreboard_player(target.clone(), target_obj.clone());

            CmdUses::one(target, Usage::Write)
        }
        Command::ScoreGet(ScoreGet { target, target_obj }) => {
            let target = get_scoreboard_player(target.clone(), target_obj.clone());

            CmdUses::one(target, Usage::Read)
        }
        Command::Execute(Execute { subcommands, run }) => {
            let mut usages = CmdUses::none();

            use datapack_common::functions::command::ExecuteSubCommand as Cmd;
            use datapack_common::functions::command::execute_sub_commands::*;

            for subcmd in subcommands.0.iter() {
                match subcmd {
                    Cmd::IfScoreMatches(IfScoreMatches { target, target_obj, .. }) => {
                        let target = get_scoreboard_player2(target.clone(), target_obj.clone());
                        usages.add(target, Usage::Read);
                    }
                    Cmd::IfScoreRelation(IfScoreRelation { target, target_obj, source, source_obj, .. }) => {
                        let target = get_scoreboard_player2(target.clone(), target_obj.clone());
                        let source = get_scoreboard_player2(source.clone(), source_obj.clone());
                        usages.add(target, Usage::Read);
                        usages.add(source, Usage::Read);
                    }
                    Cmd::StoreScore(StoreScore { target, target_obj, .. }) => {
                        let target = get_scoreboard_player2(target.clone(), target_obj.clone());
                        usages.add(target, Usage::Write);
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
        Command::ExecuteRun(ExecuteRun { command }) => {
            if let Some(command) = command.0.as_deref() {
                get_usages(command)
            } else {
                CmdUses::none()
            }
        }

        Command::ObjAdd(_) => CmdUses::All(Usage::Write),
        Command::ObjRemove(_) => CmdUses::All(Usage::Write),

        // TODO: Is it possible to be more specific with this?
        Command::FuncCall(_) => CmdUses::All(Usage::ReadWrite),

        // TODO:
        Command::Tellraw(_) => CmdUses::All(Usage::Read),

        Command::Comment(_) => CmdUses::none(),
        Command::DataGet(_) => CmdUses::none(),
        Command::DataModify(_) => CmdUses::none(),
        Command::Teleport(_) => CmdUses::none(),
        Command::SetBlock(_) => CmdUses::none(),
        Command::Clone(_) => CmdUses::none(),
        Command::Fill(_) => CmdUses::none(),
        Command::Gamerule(_) => CmdUses::none(),
        Command::Summon(_) => CmdUses::none(),
        Command::Kill(_) => CmdUses::none(),
    }
}