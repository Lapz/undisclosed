use ir::Instruction;
use temp::Label;
pub struct Optimizer {}

impl Optimizer {
    // pub fn strength_reduction(ir: &mut Vec<Instruction>) {
    //     ir.retain(|&ref instruction| match instruction {
    //         &Instruction::Copy(ref lhs, ref rhs) => {
    //             if lhs == rhs {
    //                 false
    //             } else {
    //                 true
    //             }
    //         }

    //         _ => true,
    //     });
    // }

    // pub fn unused_labels(ir: &mut Vec<Instruction>) {
    //     let mut targets = vec![];
    //     {
    //         for instruction in ir.iter() {
    //             if let Instruction::Jump(ref label) = instruction {
    //                 targets.push(label)
    //             } else if let Instruction::CJump(_, _, _, ref ltrue, ref lfalse) = instruction {
    //                 targets.push(ltrue);
    //                 targets.push(lfalse)
    //             }
    //         }
    //     }
    //     ir.retain(|&ref instruction| match instruction {
    //         &Instruction::Label(ref label) => {
    //             if targets.contains(&label) {
    //                 true
    //             } else {
    //                 false
    //             }
    //         }

    //         _ => true,
    //     });
    // }
}
