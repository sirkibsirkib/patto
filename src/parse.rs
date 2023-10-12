use crate::ir::*;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char as nomchar, multispace0, satisfy},
    combinator::{map as nommap, opt, recognize, verify},
    error::ParseError,
    multi::{many0, many_m_n, separated_list0},
    sequence::{delimited, pair, preceded, terminated},
    IResult,
};

pub fn wsl<'a, F, O, E>(inner: F) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], O, E>
where
    E: ParseError<&'a [u8]>,
    F: FnMut(&'a [u8]) -> IResult<&'a [u8], O, E> + 'a,
{
    preceded(multispace0, inner)
}

pub fn wsr<'a, F, O, E>(inner: F) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], O, E>
where
    E: ParseError<&'a [u8]>,
    F: FnMut(&'a [u8]) -> IResult<&'a [u8], O, E> + 'a,
{
    terminated(inner, multispace0)
}

pub fn parenthesized<'a, F, O, E>(inner: F) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], O, E>
where
    E: ParseError<&'a [u8]> + 'a,
    F: FnMut(&'a [u8]) -> IResult<&'a [u8], O, E> + 'a,
{
    delimited(wsl(tag("(")), inner, wsl(tag(")")))
}

pub fn constant(s: &[u8]) -> IResult<&[u8], Constant> {
    nommap(wsl(nom::character::complete::u16), Constant)(s)
}
pub fn variable(s: &[u8]) -> IResult<&[u8], Variable> {
    nommap(preceded(wsl(tag("V")), nom::character::complete::u16), Variable)(s)
}

pub fn patt(s: &[u8]) -> IResult<&[u8], Patt> {
    alt((
        nommap(many_m_n(2, usize::MAX, patt), Patt::Tuple),
        nommap(wsl(tag("_")), |_| Patt::Wildcard),
        nommap(constant, Patt::Constant),
        parenthesized(patt),
    ))(s)
}

pub fn patt_stmt(s: &[u8]) -> IResult<&[u8], Patt> {
    preceded(wsl(tag("::")), patt)(s)
}

pub fn rule_atom(s: &[u8]) -> IResult<&[u8], RuleAtom> {
    alt((
        nommap(many_m_n(2, usize::MAX, rule_atom), RuleAtom::Tuple),
        nommap(variable, RuleAtom::Variable),
        nommap(constant, RuleAtom::Constant),
        parenthesized(rule_atom),
    ))(s)
}

pub fn rule(s: &[u8]) -> IResult<&[u8], Rule> {
    struct Literal {
        sign: bool,
        ra: RuleAtom,
    }
    let head = separated_list0(wsl(tag("+"), rule_atom));
    let body = 
    let (head, body) = tuple((head, body))(s)?;
    let [mut body_pos, mut body_neg] = [vec![], vec![]];
    Rule {
        head, body_pos, body_neg
    }
}

pub fn program(s: &[u8]) -> IResult<&[u8], Program> {
    enum PattOrRule {
        Rule(Rule),
        Patt(Patt),
    }
    let p = nommap(patt_stmt, PattOrRule::Patt);
    let r = nommap(rule, PattOrRule::Rule);
    let (i, rules_or_patts) = many0(alt((p, r)))(s)?;
    let mut program = Program::default();
    for por in rules_or_patts {
        match por {
            PattOrRule::Patt(p) => program.patts.push(p),
            PattOrRule::Rule(r) => program.rules.push(r),
        }
    }
    Ok((i, program))
}

/*

pub fn ident_suffix(s: &[u8]) -> IResult<&[u8], &[u8]> {
    recognize(many0(satisfy(|c| {
        c.is_alphanumeric() || c == '_' || c == '-'
    })))(s)
}
pub fn constant(s: &[u8]) -> IResult<&[u8], Constant> {
    nommap(
        verify(
            recognize(pair(satisfy(char::is_lowercase), ident_suffix)),
            |x: &[u8]| x != b"if" && x != b"and" && x != b"not",
        ),
        |s| Constant(s.to_vec()),
    )(s)
}
pub fn variable(s: &[u8]) -> IResult<&[u8], Variable> {
    nommap(
        recognize(pair(satisfy(char::is_uppercase), ident_suffix)),
        |s| Variable(s.to_vec()),
    )(s)
}
pub fn atom(s: &[u8]) -> IResult<&[u8], Atom> {
    let tuple = nommap(many_m_n(2, usize::MAX, inner_atom), Atom::Tuple);
    wsl(alt((tuple, inner_atom)))(s)
}

pub fn inner_atom(s: &[u8]) -> IResult<&[u8], Atom> {
    let parenthesized = delimited(nomchar('('), atom, nomchar(')'));
    let wil = nommap(wsl(nomchar('_')), |_| Atom::Wildcard);
    let var = nommap(variable, Atom::Variable);
    let con = nommap(constant, Atom::Constant);
    wsl(alt((parenthesized, wil, var, con)))(s)
}

pub fn neg(s: &[u8]) -> IResult<&[u8], &[u8]> {
    wsl(alt((tag("!"), tag("not"))))(s)
}

pub fn literal(s: &[u8]) -> IResult<&[u8], Literal> {
    let sign = nommap(opt(neg), |x| match x {
        Some(_) => Sign::Neg,
        None => Sign::Pos,
    });
    nommap(pair(sign, atom), |(sign, atom)| Literal { sign, atom })(s)
}

pub fn rules(s: &[u8]) -> IResult<&[u8], Vec<Rule>> {
    wsr(many0(rule))(s)
}

pub fn sep(s: &[u8]) -> IResult<&[u8], &[u8]> {
    wsl(alt((tag(","), tag("and"))))(s)
}

pub fn rule(s: &[u8]) -> IResult<&[u8], Rule> {
    let c = separated_list0(sep, atom);
    let a = nommap(
        opt(preceded(
            wsl(alt((tag(":-"), tag("if")))),
            separated_list0(sep, literal),
        )),
        Option::unwrap_or_default,
    );
    nommap(
        terminated(pair(c, a), wsl(nomchar('.'))),
        |(consequents, antecedents)| Rule {
            consequents,
            antecedents,
        },
    )(s)
}
*/
