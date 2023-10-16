mod ir;
mod parse;

fn get_source() -> Vec<u8> {
    let mut stdin = std::io::stdin().lock();
    let mut buffer = vec![];
    std::io::Read::read_to_end(&mut stdin, &mut buffer).expect("buffer overflow");
    buffer
}

fn main() -> Result<(), ()> {
    let source = get_source();
    let res = parse::program(&source);

    let (rest, program) = res.map_err(drop)?;

    println!("REMAINING: {:?}", rest);
    println!("PROGRAM {:#?}", &program);

    let pg = program.patt_graph();
    println!("PATT GRAPH {:?}", pg);

    if let Ok(pg) = pg {
        let r = pg.reachability();
        println!("REACHABILITY {:?}", r);

        let cycle = r.cycle();
        println!("CYCLE {:#?}", cycle);
    }
    Ok(())
}
