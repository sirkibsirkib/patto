mod ir;
mod parse;

fn main() {
    let (_, program) = parse::program(b"").expect("OK!");
    // use ir::*;
    // use Patt as Pa;
    // use RuleAtom as Ra;
    // let patt = Pa::Tuple(vec![Pa::Constant(Constant(2)), Pa::Wildcard]);
    // let ra = Ra::Tuple(vec![Ra::Constant(Constant(2)), Ra::Variable(Variable(0))]);
    // println!("{:?}", patt.matching(&ra));
    // let program = Program {
    //     patts: vec![patt],
    //     rules: vec![Rule { head: vec![ra.clone()], body_pos: vec![ra], body_neg: vec![] }],
    // };

    println!("program {:#?}", &program);

    let pg = program.patt_graph();
    println!("PATT GRAPH {:?}", pg);

    if let Ok(pg) = pg {
        let r = pg.reachability();
        println!("REACHABILITY {:?}", r);

        let cycle = r.cycle();
        println!("CYCLE {:?}", cycle);
    }
}
