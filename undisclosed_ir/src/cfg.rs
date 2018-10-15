use tac;

#[derive(Debug, Clone, Copy)]
pub struct BlockID(pub usize);

#[derive(Debug, Clone, Copy)]
pub enum BlockEnd {
    Jump(BlockID),
    End,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub instructions: Vec<tac::Instruction>,
    pub end: BlockEnd,
}

#[derive(Debug, Default)]
pub struct Builder {
    blocks: Vec<Block>,
    block_ids: usize,
    current_block: Option<Block>,
}
