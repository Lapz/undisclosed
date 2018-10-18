use std::collections::{HashMap, HashSet};
use tac::Program;
use tac::{BlockEnd, BlockID, Function, Instruction};
use temp::Label;
pub struct Optimizer {}

impl Optimizer {
    pub fn strength_reduction(ir: Program) -> Program {
        for f in ir.functions.iter() {
            // for inst in f.body.iter() {}
        }

        ir
    }

    pub fn unused_labels(targets: &mut Vec<Label>, ir: &mut Vec<Instruction>) {
        // for instruction in ir.iter() {
        //     // if let Instruction::Jump(ref label) = instruction {
        //     //     targets.push(*label)
        //     // } else if let Instruction::CJump(_, _, _, ref ltrue, ref lfalse) = instruction {
        //     //     targets.push(*ltrue);
        //     //     targets.push(*lfalse)
        //     // }
        // }

        // ir.retain(|&ref instruction| match instruction {
        //     &Instruction::Label(ref label) => {
        //         if targets.contains(&label) {
        //             true
        //         } else {
        //             false
        //         }
        //     }
        //     _ => true,
        // });
    }
}

pub fn unused_blocks(functions: &mut Vec<Function>) {
    let mut remove = Vec::new();

    for f in functions.iter_mut() {
        let mut exist = HashSet::new();

        for (id, block) in f.blocks.iter() {
            exist.insert(*id);
            match block.end {
                BlockEnd::Jump(jump_id) => {
                    if !exist.insert(jump_id) {
                        remove.push(*id);
                    }
                }
                BlockEnd::End | BlockEnd::Return(_) => (),
                BlockEnd::Branch(_, id1, id2) => {
                    if !exist.insert(id1) && !block.instructions.is_empty() {
                        remove.push(*id);
                    } else if !exist.insert(id2) && !block.instructions.is_empty() {
                        remove.push(*id)
                    }
                }
            }

             // removes empty blocks
        }

        for id in remove.iter() {
            f.blocks.remove(&id);
        }

        remove.clear();
    }
}
