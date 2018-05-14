use std::collections::HashMap;

use cfg;

// struct LoopDescr {
//     start: cfg::BlockId,
//     after: cfg::BlockId,
// }

// struct Builder<'a> {
//     parameters: Vec<cfg::Reg>,
//     registers: HashMap<cfg::Reg, cfg::Type>,
//     blocks: HashMap<cfg::BlockId, cfg::Block>,
//     current_loop: Option<LoopDescr>,
//     ctx: &'a mut CompileCtx,
//     next_block_id: u32,
//     next_reg: u32,
//     var_registers: HashMap<t::Symbol, cfg::Reg>,
//     register_vars: HashMap<cfg::Reg, t::Symbol>,
//     current_block: Option<(cfg::BlockId, Vec<Spanned<cfg::Instruction>>)>,
//     var_mutability: HashMap<t::Symbol, Mut>,
// }
