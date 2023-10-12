use std::collections::HashMap;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Variable(pub u16);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Constant(pub u16);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum RuleAtom {
    Constant(Constant),
    Variable(Variable),
    Tuple(Vec<RuleAtom>),
}
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Patt {
    Wildcard,
    Constant(Constant),
    Tuple(Vec<Patt>),
}

#[derive(Debug)]
pub struct Rule {
    pub head: Vec<RuleAtom>,
    pub body_pos: Vec<RuleAtom>,
    pub body_neg: Vec<RuleAtom>,
}

pub struct Program {
    pub rules: Vec<Rule>,
    pub patts: Vec<Patt>,
}

#[derive(Debug, Default)]
pub struct PattGraph<'a> {
    inside: HashMap<[&'a Patt; 2], (&'a RuleAtom, &'a Rule)>,
}

#[derive(Debug)]
pub enum PattErr<'a> {
    NoMatchingPatt { rule: &'a Rule, ra: &'a RuleAtom },
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
    fn matching(&self, ra: &RuleAtom) -> bool {
        match (self, ra) {
            (Self::Wildcard, _) => true,
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
        let mut pg = PattGraph::default();
        let mut consequents: Vec<&RuleAtom> = vec![];
        let mut construct_patts: Vec<&Patt> = vec![];
        let mut argument_patts: Vec<&Patt> = vec![];
        for rule in &self.rules {
            let novel = |ra: &RuleAtom| {
                ra.is_variable() || rule.body_pos.iter().any(|body_ra| body_ra.contains_ra(ra))
            };
            consequents.extend(rule.head.as_slice());
            while let Some(c) = consequents.pop() {
                construct_patts.clear();
                argument_patts.clear();
                if let RuleAtom::Tuple(args) = c {
                    if !c.ground() && novel(c) {
                        // needs a type
                        construct_patts.extend(self.patts_matching(c));
                    }
                    if construct_patts.is_empty() {
                        return Err(PattErr::NoMatchingPatt { rule, ra: c });
                    }
                    for arg in args {
                        if !arg.ground() {
                            argument_patts.extend(self.patts_matching(c));
                            for &cp in &construct_patts {
                                for &ap in &argument_patts {
                                    pg.inside.insert([cp, ap], (c, rule));
                                }
                            }
                        }
                    }
                    consequents.extend(args);
                }
            }
        }
        Ok(pg)
    }
}
