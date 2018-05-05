use ir::Instruction;

pub struct Optimizer<'a> {
    code: &'a mut Vec<Instruction>,
}

impl<'a> Optimizer<'a> {
    pub fn strength_reduction(ir: &mut Vec<Instruction>) {
        ir.retain(|&ref instruction| match instruction {
            &Instruction::Copy(ref lhs, ref rhs) => {
                if lhs == rhs {
                    false
                } else {
                    true
                }
            }

            _ => true,
        });
    }
}
