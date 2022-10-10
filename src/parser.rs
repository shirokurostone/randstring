use std::cmp::Ordering;
use std::ops::RangeInclusive;

#[derive(Debug, PartialEq)]
pub enum Token {
    Literal(String),
    AndGroup(Vec<Token>),
    OrGroup(Vec<Token>),
    Quantifier(RangeInclusive<u32>, Option<Box<Token>>),
    CharacterClass(CharacterClassMode, Vec<CharacterRangeInclusive>),
    Or,
}

#[derive(Debug, PartialEq)]
pub enum CharacterClassMode {
    Include,
    Exclude,
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    Skip,
    RangeError,
    SyntaxError,
    UnicodeRangeError,
    QuantifierOutOfLimit,
    InvalidRange,
    CharacterClassRangeError,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord)]
pub struct CharacterRangeInclusive {
    start: u32,
    end: u32,
}

impl std::cmp::PartialOrd for CharacterRangeInclusive {
    fn partial_cmp(&self, other: &CharacterRangeInclusive) -> Option<Ordering> {
        match self.start.partial_cmp(&other.start) {
            Some(Ordering::Equal) => self.end.partial_cmp(&other.end),
            result => result,
        }
    }
}

impl CharacterRangeInclusive {
    pub fn new(start: u32, end: u32) -> CharacterRangeInclusive {
        CharacterRangeInclusive { start, end }
    }

    pub fn contains(&self, item: u32) -> bool {
        self.start <= item && item <= self.end
    }

    pub fn less_than(&self, other: &CharacterRangeInclusive) -> bool {
        self.end < other.start
    }

    pub fn greater_than(&self, other: &CharacterRangeInclusive) -> bool {
        other.end < self.start
    }

    pub fn count(&self) -> u32 {
        self.end - self.start + 1
    }
}

fn quantifier_max_value() -> u32 {
    100u32
}

#[test]
fn test_root() {
    assert_eq!(Ok((Token::OrGroup(vec![]), "")), root(""));

    assert_eq!(
        Ok((
            Token::OrGroup(vec![
                Token::AndGroup(vec![Token::Literal("A".to_string())]),
                Token::AndGroup(vec![Token::Literal("B".to_string())])
            ]),
            ""
        )),
        root("A|B")
    );

    assert_eq!(
        Ok((
            Token::AndGroup(vec![Token::CharacterClass(
                CharacterClassMode::Include,
                vec![CharacterRangeInclusive::new('0' as u32, '9' as u32)]
            )]),
            ""
        )),
        root("[0-9]")
    );
}

fn quantifier<'a>(input: &'a str) -> Result<(Token, &'a str), ParseError> {
    let mut target = input;

    if !target.starts_with("{") {
        return Err(ParseError::Skip);
    } else if target.starts_with("{}") {
        return Err(ParseError::SyntaxError);
    }

    target = target
        .get("{".len()..target.len())
        .ok_or(ParseError::RangeError)?;

    let start = if let Some(i) = target.find(|c: char| !c.is_digit(10)) {
        if i == 0 {
            0
        } else {
            let num = target
                .get(0..i)
                .ok_or(ParseError::RangeError)?
                .to_string()
                .parse::<u32>()
                .map_err(|_| ParseError::SyntaxError)?;
            target = target.get(i..).ok_or(ParseError::RangeError)?;
            num
        }
    } else {
        0u32
    };

    let end = if target.starts_with("}") {
        start
    } else if target.starts_with(",") {
        target = target
            .get(",".len()..target.len())
            .ok_or(ParseError::RangeError)?;

        if let Some(i) = target.find(|c: char| !c.is_digit(10)) {
            if i == 0 {
                quantifier_max_value()
            } else {
                let num = target
                    .get(0..i)
                    .ok_or(ParseError::RangeError)?
                    .to_string()
                    .parse::<u32>()
                    .map_err(|_| ParseError::SyntaxError)?;
                target = target.get(i..).ok_or(ParseError::RangeError)?;
                num
            }
        } else {
            quantifier_max_value()
        }
    } else {
        return Err(ParseError::SyntaxError);
    };

    if quantifier_max_value() < start || quantifier_max_value() < end {
        return Err(ParseError::QuantifierOutOfLimit);
    }
    if end < start {
        return Err(ParseError::InvalidRange);
    }

    if !target.starts_with("}") {
        return Err(ParseError::SyntaxError);
    }

    target = target
        .get("}".len()..target.len())
        .ok_or(ParseError::RangeError)?;

    return Ok((Token::Quantifier(start..=end, None), target));
}

#[test]
fn test_quantifier() {
    assert_eq!(
        Ok((Token::Quantifier(10..=20, None), "")),
        quantifier("{10,20}")
    );
    assert_eq!(
        Ok((Token::Quantifier(10..=quantifier_max_value(), None), "")),
        quantifier("{10,}")
    );
    assert_eq!(
        Ok((Token::Quantifier(0..=20, None), "")),
        quantifier("{,20}")
    );
    assert_eq!(
        Ok((Token::Quantifier(10..=10, None), "")),
        quantifier("{10}")
    );

    assert_eq!(Err(ParseError::SyntaxError), quantifier("{}"));
    assert_eq!(Err(ParseError::SyntaxError), quantifier("{10,20]"));
    assert_eq!(Err(ParseError::SyntaxError), quantifier("{abc,20}"));
    assert_eq!(Err(ParseError::SyntaxError), quantifier("{10,abc}"));
    assert_eq!(Err(ParseError::InvalidRange), quantifier("{20,10}"));
    assert_eq!(
        Err(ParseError::QuantifierOutOfLimit),
        quantifier("{10,1000}")
    );
}

#[test]
fn test_normalize_character_range_inclusive() {
    assert_eq!(
        normalize_character_range_inclusive(vec![
            CharacterRangeInclusive::new(0, 10),
            CharacterRangeInclusive::new(0, 10),
        ]),
        vec![CharacterRangeInclusive::new(0, 10)]
    );

    assert_eq!(
        normalize_character_range_inclusive(vec![
            CharacterRangeInclusive::new(0, 10),
            CharacterRangeInclusive::new(20, 30),
        ]),
        vec![
            CharacterRangeInclusive::new(0, 10),
            CharacterRangeInclusive::new(20, 30),
        ]
    );

    assert_eq!(
        normalize_character_range_inclusive(vec![
            CharacterRangeInclusive::new(0, 10),
            CharacterRangeInclusive::new(5, 20),
        ]),
        vec![CharacterRangeInclusive::new(0, 20),]
    );

    assert_eq!(
        normalize_character_range_inclusive(vec![
            CharacterRangeInclusive::new(0, 10),
            CharacterRangeInclusive::new(1, 9),
        ]),
        vec![CharacterRangeInclusive::new(0, 10),]
    );
}

fn normalize_character_range_inclusive(
    mut ranges: Vec<CharacterRangeInclusive>,
) -> Vec<CharacterRangeInclusive> {
    ranges.sort();
    ranges.into_iter().fold(vec![], |v, e| {
        let mut new_ranges: Vec<CharacterRangeInclusive> = vec![];
        let mut target: Option<CharacterRangeInclusive> = Some(e);
        for r in v {
            match target {
                Some(t) => {
                    if t.less_than(&r) {
                        new_ranges.push(t);
                        new_ranges.push(r);
                        target = None;
                    } else if !r.contains(t.start) && r.contains(t.end) {
                        target = Some(CharacterRangeInclusive::new(t.start, r.end));
                    } else if r.contains(t.start) && r.contains(t.end) {
                        target = Some(r);
                    } else if t.contains(r.start) && t.contains(r.end) {
                        target = Some(t);
                    } else if r.contains(t.start) && !r.contains(t.end) {
                        target = Some(CharacterRangeInclusive::new(r.start, t.end));
                    } else {
                        new_ranges.push(r);
                    }
                }
                None => {
                    new_ranges.push(r);
                }
            };
        }
        if let Some(t) = target {
            new_ranges.push(t);
        }
        return new_ranges;
    })
}

fn character_class<'a>(input: &'a str) -> Result<(Token, &'a str), ParseError> {
    let mut target = input;

    let bracket_not = "[^";
    let bracket = "[";
    let mode = if input.starts_with(bracket_not) {
        target = target
            .get(bracket_not.len()..target.len())
            .ok_or(ParseError::RangeError)?;
        CharacterClassMode::Exclude
    } else if target.starts_with(bracket) {
        target = target
            .get(bracket.len()..target.len())
            .ok_or(ParseError::RangeError)?;
        CharacterClassMode::Include
    } else {
        return Err(ParseError::Skip);
    };

    let it = &mut target.char_indices();
    let mut prev = '\0';
    let mut is_range = false;
    let mut is_escape = false;

    let mut ranges: Vec<CharacterRangeInclusive> = vec![];
    loop {
        match &it.next() {
            Some((_, '\\')) => {
                if is_range {
                    is_range = false;
                    continue;
                }

                if is_escape {
                    ranges.push(CharacterRangeInclusive::new('\\' as u32, '\\' as u32));
                    is_escape = false;
                } else {
                    is_escape = true;
                }
            }
            Some((_, '-')) => {
                if is_escape {
                    ranges.push(CharacterRangeInclusive::new('-' as u32, '-' as u32));
                    is_escape = false;
                } else {
                    is_range = true;
                }
            }
            Some((_, ']')) => {
                if is_escape {
                    ranges.push(CharacterRangeInclusive::new(']' as u32, ']' as u32));
                    is_escape = false;
                    continue;
                }

                if is_range {
                    return Err(ParseError::SyntaxError);
                }
                break;
            }
            Some((_, c)) => {
                if is_range {
                    is_range = false;
                    ranges.push(CharacterRangeInclusive::new(prev as u32, *c as u32));
                    prev = *c;
                    continue;
                } else if is_escape {
                    is_escape = false;
                    ranges.push(CharacterRangeInclusive::new('\\' as u32, '\\' as u32));
                }

                ranges.push(CharacterRangeInclusive::new(*c as u32, *c as u32));
                prev = *c;
            }
            _ => {
                return Err(ParseError::SyntaxError);
            }
        }
    }

    let token = Token::CharacterClass(mode, normalize_character_range_inclusive(ranges));
    return match &it.next() {
        Some((i, _)) => Ok((
            token,
            target.get(*i..target.len()).ok_or(ParseError::RangeError)?,
        )),
        None => Ok((token, target.get(0..0).ok_or(ParseError::RangeError)?)),
    };
}

#[test]
fn test_character_class() {
    assert_eq!(
        Ok((
            Token::CharacterClass(
                CharacterClassMode::Include,
                vec![CharacterRangeInclusive::new('a' as u32, 'c' as u32)]
            ),
            ""
        )),
        character_class("[a-c]")
    );

    assert_eq!(
        Ok((
            Token::CharacterClass(
                CharacterClassMode::Exclude,
                vec![CharacterRangeInclusive::new('a' as u32, 'c' as u32)]
            ),
            ""
        )),
        character_class("[^a-c]")
    );

    assert_eq!(
        Ok((
            Token::CharacterClass(
                CharacterClassMode::Include,
                vec![CharacterRangeInclusive::new(']' as u32, ']' as u32)]
            ),
            ""
        )),
        character_class("[\\]]")
    );

    assert_eq!(
        Ok((
            Token::CharacterClass(
                CharacterClassMode::Include,
                vec![
                    CharacterRangeInclusive::new(' ' as u32, ' ' as u32),
                    CharacterRangeInclusive::new('\\' as u32, '\\' as u32),
                ]
            ),
            ""
        )),
        character_class("[\\ ]")
    );
}

fn alt<'a>(
    input: &'a str,
    parsers: &[for<'b> fn(&'b str) -> Result<(Token, &'b str), ParseError>],
) -> Result<(Token, &'a str), ParseError> {
    for parser in parsers {
        match parser(input) {
            Ok((token, next)) => return Ok((token, next)),
            Err(ParseError::Skip) => continue,
            Err(err) => return Err(err),
        }
    }

    Err(ParseError::Skip)
}

fn asterisk<'a>(input: &'a str) -> Result<(Token, &'a str), ParseError> {
    let word = "*";
    if !input.starts_with(word) {
        return Err(ParseError::Skip);
    }

    Ok((
        Token::Quantifier(0..=quantifier_max_value(), None),
        input
            .get(word.len()..input.len())
            .ok_or(ParseError::RangeError)?,
    ))
}

#[test]
fn test_asterisk() {
    assert_eq!(
        Ok((Token::Quantifier(0..=quantifier_max_value(), None), "")),
        asterisk("*")
    );
    assert_eq!(Err(ParseError::Skip), asterisk("_"));
}

fn plus<'a>(input: &'a str) -> Result<(Token, &'a str), ParseError> {
    let word = "+";
    if !input.starts_with(word) {
        return Err(ParseError::Skip);
    }

    Ok((
        Token::Quantifier(1..=quantifier_max_value(), None),
        input
            .get(word.len()..input.len())
            .ok_or(ParseError::RangeError)?,
    ))
}

#[test]
fn test_plus() {
    assert_eq!(
        Ok((Token::Quantifier(1..=quantifier_max_value(), None), "")),
        plus("+")
    );
    assert_eq!(Err(ParseError::Skip), plus("_"));
}

fn question<'a>(input: &'a str) -> Result<(Token, &'a str), ParseError> {
    let word = "?";
    if !input.starts_with(word) {
        return Err(ParseError::Skip);
    }

    Ok((
        Token::Quantifier(0..=1, None),
        input
            .get(word.len()..input.len())
            .ok_or(ParseError::RangeError)?,
    ))
}

#[test]
fn test_question() {
    assert_eq!(Ok((Token::Quantifier(0..=1, None), "")), question("?"));
    assert_eq!(Err(ParseError::Skip), question("_"));
}

fn vertical_line<'a>(input: &'a str) -> Result<(Token, &'a str), ParseError> {
    let word = "|";
    if !input.starts_with(word) {
        return Err(ParseError::Skip);
    }

    Ok((
        Token::Or,
        input
            .get(word.len()..input.len())
            .ok_or(ParseError::RangeError)?,
    ))
}

#[test]
fn test_vertical_line() {
    assert_eq!(Ok((Token::Or, "")), vertical_line("|"));
    assert_eq!(Err(ParseError::Skip), vertical_line("_"));
}

fn metacharacter<'a>(input: &'a str) -> Result<(Token, &'a str), ParseError> {
    let characters = [
        (
            "\\d",
            Token::CharacterClass(
                CharacterClassMode::Include,
                vec![CharacterRangeInclusive::new('0' as u32, '9' as u32)],
            ),
        ),
        (
            "\\D",
            Token::CharacterClass(
                CharacterClassMode::Exclude,
                vec![CharacterRangeInclusive::new('0' as u32, '9' as u32)],
            ),
        ),
        (
            "\\s",
            Token::CharacterClass(
                CharacterClassMode::Include,
                vec![
                    CharacterRangeInclusive::new(' ' as u32, ' ' as u32),
                    CharacterRangeInclusive::new('\t' as u32, '\t' as u32),
                    CharacterRangeInclusive::new('\n' as u32, '\n' as u32),
                    CharacterRangeInclusive::new('\r' as u32, '\r' as u32),
                ],
            ),
        ),
        (
            "\\S",
            Token::CharacterClass(
                CharacterClassMode::Exclude,
                vec![
                    CharacterRangeInclusive::new(' ' as u32, ' ' as u32),
                    CharacterRangeInclusive::new('\t' as u32, '\t' as u32),
                    CharacterRangeInclusive::new('\n' as u32, '\n' as u32),
                    CharacterRangeInclusive::new('\r' as u32, '\r' as u32),
                ],
            ),
        ),
        (
            "\\w",
            Token::CharacterClass(
                CharacterClassMode::Include,
                vec![
                    CharacterRangeInclusive::new('0' as u32, '9' as u32),
                    CharacterRangeInclusive::new('A' as u32, 'Z' as u32),
                    CharacterRangeInclusive::new('a' as u32, 'z' as u32),
                    CharacterRangeInclusive::new('_' as u32, '_' as u32),
                ],
            ),
        ),
        (
            "\\W",
            Token::CharacterClass(
                CharacterClassMode::Exclude,
                vec![
                    CharacterRangeInclusive::new('0' as u32, '9' as u32),
                    CharacterRangeInclusive::new('A' as u32, 'Z' as u32),
                    CharacterRangeInclusive::new('a' as u32, 'z' as u32),
                    CharacterRangeInclusive::new('_' as u32, '_' as u32),
                ],
            ),
        ),
    ];

    for c in characters {
        let word = c.0;
        if !input.starts_with(word) {
            continue;
        }

        return Ok((
            c.1,
            input
                .get(word.len()..input.len())
                .ok_or(ParseError::RangeError)?,
        ));
    }
    Err(ParseError::Skip)
}

fn literal<'a>(input: &'a str) -> Result<(Token, &'a str), ParseError> {
    let it = &mut input.char_indices();

    return match &it.next() {
        Some((0, c)) => match &it.next() {
            Some((i, _)) => Ok((
                Token::Literal(c.to_string()),
                input.get(*i..input.len()).ok_or(ParseError::RangeError)?,
            )),
            None => Ok((
                Token::Literal(c.to_string()),
                input.get(0..0).ok_or(ParseError::RangeError)?,
            )),
        },
        _ => Err(ParseError::Skip),
    };
}

#[test]
fn test_literal() {
    assert_eq!(Ok((Token::Literal("a".to_string()), "")), literal("a"));
    assert_eq!(Ok((Token::Literal("a".to_string()), "bc")), literal("abc"));
}

pub fn root<'a>(input: &'a str) -> Result<(Token, &'a str), ParseError> {
    let mut tokens: Vec<Token> = vec![];
    let mut target = input;

    loop {
        if target.starts_with(")") {
            return Err(ParseError::SyntaxError);
        }

        if target.len() == 0 {
            return create_group_token(target, tokens);
        }

        match alt(
            target,
            &[
                asterisk,
                plus,
                question,
                quantifier,
                group,
                character_class,
                vertical_line,
                metacharacter,
                literal,
            ],
        ) {
            Ok((token, next)) => {
                tokens.push(token);
                target = next;
                continue;
            }
            Err(ParseError::Skip) => {}
            Err(err) => {
                return Err(err);
            }
        }

        return Err(ParseError::SyntaxError);
    }
}

fn group<'a>(input: &'a str) -> Result<(Token, &'a str), ParseError> {
    if !input.starts_with("(") {
        return Err(ParseError::Skip);
    }

    let mut tokens: Vec<Token> = vec![];
    let mut target = input
        .get("(".len()..input.len())
        .ok_or(ParseError::RangeError)?;

    loop {
        if target.starts_with(")") {
            target = target
                .get(")".len()..target.len())
                .ok_or(ParseError::RangeError)?;

            return create_group_token(target, tokens);
        }

        if target.len() == 0 {
            return Err(ParseError::SyntaxError);
        }

        match alt(
            target,
            &[
                asterisk,
                plus,
                question,
                quantifier,
                group,
                character_class,
                vertical_line,
                metacharacter,
                literal,
            ],
        ) {
            Ok((token, next)) => {
                tokens.push(token);
                target = next;
                continue;
            }
            Err(ParseError::Skip) => {}
            Err(err) => {
                return Err(err);
            }
        }

        return Err(ParseError::SyntaxError);
    }
}

fn create_group_token<'a>(
    input: &'a str,
    mut tokens: Vec<Token>,
) -> Result<(Token, &'a str), ParseError> {
    let mut section: Vec<Token> = vec![];
    let mut ts: Vec<Token> = vec![];

    tokens.reverse();
    let mut it = tokens.into_iter();
    loop {
        match it.next() {
            Some(Token::Quantifier(range, None)) => match it.next() {
                Some(Token::Quantifier(_, _)) => {
                    return Err(ParseError::SyntaxError);
                }
                Some(t) => {
                    ts.push(Token::Quantifier(range, Some(Box::new(t))));
                }
                None => {
                    return Err(ParseError::SyntaxError);
                }
            },
            Some(Token::Quantifier(_, Some(_))) => {
                return Err(ParseError::SyntaxError);
            }
            Some(token) => {
                ts.push(token);
            }
            None => break,
        }
    }

    ts.reverse();
    let mut ts2: Vec<Token> = vec![];

    for t in ts {
        if t == Token::Or {
            ts2.push(Token::AndGroup(section));
            section = vec![];
        } else {
            section.push(t);
        }
    }
    if section.len() != 0 {
        if ts2.len() == 0 {
            return Ok((Token::AndGroup(section), input));
        }
        ts2.push(Token::AndGroup(section));
    }
    return Ok((Token::OrGroup(ts2), input));
}

#[derive(Debug, PartialEq)]
pub enum GenerateRandStringErrorType {
    NotImplemented,
    TokenError,
    StatusError,
}

pub fn generate_rand_string(token: &Token) -> Result<String, GenerateRandStringErrorType> {
    let mut v = Vec::<char>::new();
    let mut rnd = rand::thread_rng();
    generate_rand_string_private(token, &mut v, &mut rnd)?;
    return Ok(String::from_iter(v));
}

fn generate_rand_string_private(
    token: &Token,
    v: &mut Vec<char>,
    rng: &mut impl rand::Rng,
) -> Result<(), GenerateRandStringErrorType> {
    match token {
        Token::Literal(string) => {
            for c in string.chars() {
                v.push(c)
            }
        }
        Token::AndGroup(tokens) => {
            for t in tokens {
                generate_rand_string_private(t, v, rng)?;
            }
        }
        Token::OrGroup(tokens) => {
            let i = rng.gen_range(0..tokens.len());
            generate_rand_string_private(&tokens[i], v, rng)?;
        }
        Token::Quantifier(range, token) => {
            let s = *range.start();
            let e = *range.end();
            let i = rng.gen_range(s..=e);
            for _ in 0..i {
                match token {
                    Some(t) => generate_rand_string_private(t, v, rng)?,
                    _ => return Err(GenerateRandStringErrorType::TokenError),
                };
            }
        }
        Token::CharacterClass(mode, ranges) => match mode {
            CharacterClassMode::Include => {
                let count = ranges.iter().fold(0, |sum, range| sum + range.count());
                let i = rng.gen_range(0..count);

                let result = ranges
                    .iter()
                    .fold((i, None), |(c, result), range| match result {
                        None => {
                            if range.count() < c {
                                return (0, Some(range.start + c - range.count()));
                            }
                            return (c - range.count(), None);
                        }
                        Some(code) => {
                            return (c, Some(code));
                        }
                    });
                match result {
                    (_, Some(code)) => match char::from_u32(code) {
                        Some(c) => v.push(c),
                        _ => {
                            return Err(GenerateRandStringErrorType::StatusError);
                        }
                    },
                    _ => {
                        return Err(GenerateRandStringErrorType::StatusError);
                    }
                }
            }
            CharacterClassMode::Exclude => return Err(GenerateRandStringErrorType::NotImplemented),
        },
        _ => return Err(GenerateRandStringErrorType::TokenError),
    }

    return Ok(());
}

#[test]
fn test_generate_rand_string() {
    assert_eq!(
        Ok("ABC".to_string()),
        generate_rand_string(&root("ABC").unwrap().0)
    );

    assert_eq!(
        Ok("AAAAA".to_string()),
        generate_rand_string(&root("A{5}").unwrap().0)
    );
}
