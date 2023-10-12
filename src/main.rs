mod ir;

fn main() {
    use ir::*;
    use Patt as Pa;
    use RuleAtom as Ra;
    let program = Program {
        patts: vec![Pa::Wildcard],
        rules: vec![Rule {
            head: vec![Ra::Tuple(vec![Ra::Variable(Variable(0))])],
            body_pos: vec![],
            body_neg: vec![],
        }],
    };

    let pg = program.patt_graph();
    println!("PATT GRAPH {:?}", pg);

    if let Ok(pg) = pg {
        let r = pg.reachability();
        println!("REACHABILITY {:?}", r);

        let cycle = r.cycle();
        println!("CYCLE {:?}", cycle);
    }
}
