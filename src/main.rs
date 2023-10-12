mod ir;

fn main() {
    use ir::*;
    use Patt as Pa;
    use RuleAtom as Ra;
    let program = Program {
        patts: vec![],
        rules: vec![Rule {
            head: vec![Ra::Constant(Constant(1))],
            body_pos: vec![],
            body_neg: vec![],
        }],
    };
    let pg = program.patt_graph();
    println!("PATT GRAPH {:#?}", pg);
}
