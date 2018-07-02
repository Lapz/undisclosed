use ir::Instruction;

#[derive(Default, Debug)]
pub struct CFG {
    nodes: Vec<NodeData>,
    edges: Vec<EdgeData>,
}

pub type NodeIndex = usize;

#[derive(Debug)]
pub struct NodeData {
    instruction: Instruction,
    first_outgoing_edge: Option<EdgeIndex>,
}

pub type EdgeIndex = usize;

#[derive(Debug)]
pub struct EdgeData {
    target: NodeIndex,
    next_outgoing_edge: Option<EdgeIndex>,
}

// impl CFG {
//     pub fn new() -> Self {
//         Self::default()
//     }
//     pub fn add_node(&mut self, data: Instruction) -> NodeIndex {
//         let index = self.nodes.len();
//         self.nodes.push(NodeData {
//             instruction: data,
//             first_outgoing_edge: None,
//         });
//         index
//     }

//     pub fn add_edge(&mut self, source: NodeIndex, target: NodeIndex) {
//         let edge_index = self.edges.len();
//         let node_data = &mut self.nodes[source];
//         self.edges.push(EdgeData {
//             target: target,
//             next_outgoing_edge: node_data.first_outgoing_edge,
//         });
//         node_data.first_outgoing_edge = Some(edge_index);
//     }
// }

// pub fn construct_cfg(mut ir: ir::Program) -> CFG {
//     let mut graph = CFG::new();

//     println!("{:?}",ir);

//     for func in ir.functions.iter_mut() {
//        graph.add_node(func.body.remove(0));

//     //    let mut indexs = vec![];

//        for (i,instruction) in func.body.iter_mut().enumerate() {

//            if has_label(&instruction) {

//               graph.add_node(func.body[i].clone());
//            }
//        }

//        println!("{:?}",func.body);

//     //    for index in indexs {
//     //        println!("index {}",func.body[index]);
//     //     //    graph.add_node(func.body.remove(index-1));
//     //    }

//     }

//     graph
// }

// fn has_label(instruction:&Instruction) -> bool {
//     use self::Instruction::*;
//     match instruction {
//         Store(_,_) | Copy(_,_) => false,
//         Cast(_,_,_) => false,
//         BinOp(_,_,_,_) => false,
//         UnOp(_,_,_) => false,
//         Value(_)| Return(_)| Load(_) => false,
//         Block(_,_) => false,
//         _ => true,
//     }
// }
