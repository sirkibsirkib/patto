use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Variable(pub Vec<u8>);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Constant(pub Vec<u8>);

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum RuleAtom {
    Constant(Constant),
    Variable(Variable),
    Tuple(Vec<RuleAtom>),
}
#[derive(Clone, Eq, PartialEq, Hash)]
pub enum Patt {
    Wildcard,
    Constant(Constant),
    Tuple(Vec<Patt>),
}

#[derive(Debug)]
pub struct Cycle<'a> {
    cyclic_patt: &'a Patt,
    cyclic_step: Vec<(&'a Patt, How<'a>)>,
}

pub struct Rule {
    pub head: Vec<RuleAtom>,
    pub body_pos: Vec<RuleAtom>,
    pub body_neg: Vec<RuleAtom>,
}

#[derive(Default, Debug)]
pub struct Program {
    pub rules: Vec<Rule>,
    pub patts: Vec<Patt>,
}

#[derive(Debug, Clone, Copy)]
pub struct How<'a> {
    pub rule: &'a Rule,
    pub consequent: &'a RuleAtom,
    pub argument: &'a RuleAtom,
}

#[derive(Debug)]
pub struct PattGraph<'a> {
    patts: &'a [Patt],
    inside: HashMap<[&'a Patt; 2], How<'a>>,
}

#[derive(Debug)]
pub struct Reachability<'a> {
    patts: &'a [Patt],
    toward: HashMap<[&'a Patt; 2], (&'a Patt, How<'a>)>,
}

#[derive(Debug)]
pub enum PattErr<'a> {
    NoMatchingPatt { rule: &'a Rule, ra: &'a RuleAtom },
}

impl PattGraph<'_> {
    pub fn reachability(&self) -> Reachability {
        let mut r = Reachability { patts: self.patts, toward: Default::default() };
        for ([a, b], &how) in self.inside.iter() {
            r.toward.insert([a, b], (b, how));
        }
        for src in self.patts.iter() {
            for between in self.patts.iter() {
                if let Some((toward_between, how)) = r.toward.get(&[src, between]).copied() {
                    for dest in self.patts.iter() {
                        if r.toward.get(&[between, dest]).is_some()
                            && r.toward.get(&[src, dest]).is_none()
                        {
                            r.toward.insert([src, dest], (toward_between, how));
                        }
                    }
                }
            }
        }
        r
    }
}

impl Reachability<'_> {
    pub fn cycle(&self) -> Option<Cycle> {
        for patt in self.patts.iter() {
            if let Some((first, how)) = self.toward.get(&[patt, patt]).copied() {
                let mut cyclic_step = vec![(first, how)];
                let mut next = first;
                while next != patt {
                    let (n, how) = self.toward.get(&[patt, patt]).copied().unwrap();
                    cyclic_step.push((n, how));
                    next = n;
                }
                return Some(Cycle { cyclic_patt: patt, cyclic_step });
            }
        }
        None
    }
}

impl fmt::Debug for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, ra) in self.head.iter().enumerate() {
            if i > 0 {
                write!(f, " +")?;
            }
            ra.fmt(f)?;
        }
        if self.body().next().is_some() {
            write!(f, " :- ")?;
            for (i, (pos, ra)) in self.body().enumerate() {
                match (i, pos) {
                    (0, true) => write!(f, ""),
                    (_, true) => write!(f, " +"),
                    (_, false) => write!(f, " -"),
                }?;
                ra.fmt(f)?;
            }
        }
        Ok(())
    }
}

impl fmt::Debug for Patt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Constant(c) => {
                for x in &c.0 {
                    write!(f, "{}", *x as char)?;
                }
                Ok(())
            }
            Self::Wildcard => write!(f, "_"),
            Self::Tuple(args) => {
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    if arg.is_tuple() { write!(f, "({:?})", arg) } else { arg.fmt(f) }?;
                }
                Ok(())
            }
        }
    }
}

impl fmt::Debug for RuleAtom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Constant(c) => {
                for x in &c.0 {
                    write!(f, "{}", *x as char)?;
                }
            }
            Self::Variable(v) => {
                for x in &v.0 {
                    write!(f, "{}", *x as char)?;
                }
            }
            Self::Tuple(args) => {
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    if arg.is_tuple() { write!(f, "({:?})", arg) } else { arg.fmt(f) }?;
                }
            }
        }
        Ok(())
    }
}

impl Patt {
    fn is_tuple(&self) -> bool {
        match self {
            Self::Tuple(_) => true,
            _ => false,
        }
    }
}

impl RuleAtom {
    fn contains_ra(&self, other: &Self) -> bool {
        if self == other {
            return true;
        }
        if let Self::Tuple(args) = self {
            return args.iter().any(|x| x.contains_ra(other));
        } else {
            false
        }
    }
    fn is_tuple(&self) -> bool {
        match self {
            Self::Tuple(_) => true,
            _ => false,
        }
    }

    fn is_variable(&self) -> bool {
        match self {
            Self::Variable(_) => true,
            _ => false,
        }
    }

    fn ground(&self) -> bool {
        match self {
            Self::Constant(_) => true,
            Self::Variable(_) => false,
            Self::Tuple(args) => args.iter().all(Self::ground),
        }
    }
}

impl Patt {
    pub fn matching(&self, ra: &RuleAtom) -> bool {
        match (self, ra) {
            (Self::Wildcard, _) | (_, RuleAtom::Variable(_)) => true,
            (Self::Constant(x), RuleAtom::Constant(y)) => x == y,
            (Self::Tuple(x), RuleAtom::Tuple(y)) => {
                x.len() == y.len() && x.iter().zip(y.iter()).all(|(a, b)| a.matching(b))
            }
            _ => false,
        }
    }
}

impl Program {
    pub fn patts_matching<'a>(&'a self, ra: &'a RuleAtom) -> impl Iterator<Item = &'a Patt> {
        self.patts.iter().filter(|patt| patt.matching(ra))
    }
    pub fn patt_graph(&self) -> Result<PattGraph, PattErr> {
        let mut pg = PattGraph { patts: self.patts.as_slice(), inside: Default::default() };
        for rule in &self.rules {
            println!("RULE {:?}", rule);
            for c in rule.head.iter() {
                println!("CONSEQUENT {:?}", c);
                if c.ground() || rule.body_pos.iter().any(|ra| ra == c) {
                    continue;
                }
                if let RuleAtom::Tuple(args) = c {
                    if self.patts_matching(c).next().is_none() {
                        return Err(PattErr::NoMatchingPatt { rule, ra: c });
                    }
                    for arg in args {
                        println!("ARG {:?}", arg);
                        if arg.ground() {
                            continue;
                        }
                        if self.patts_matching(arg).next().is_none() {
                            return Err(PattErr::NoMatchingPatt { rule, ra: arg });
                        }
                        for ap in self.patts_matching(arg) {
                            for cp in self.patts_matching(c) {
                                pg.inside
                                    .insert([cp, ap], How { consequent: c, argument: arg, rule });
                            }
                        }
                    }
                }
            }
        }
        Ok(pg)
    }
}

impl Rule {
    fn body(&self) -> impl Iterator<Item = (bool, &RuleAtom)> {
        let p = std::iter::repeat(true).zip(self.body_pos.iter());
        let n = std::iter::repeat(false).zip(self.body_neg.iter());
        p.chain(n)
    }
}
