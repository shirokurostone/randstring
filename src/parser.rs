use std::ops::Range;

#[derive(Debug, PartialEq)]
pub enum Token {
    Literal(Vec<char>),
    AndGroup(Vec<Token>),
    OrGroup(Vec<Token>),
    Quantifier(Range<u32>),
    CharacterClass(CharacterClassMode, String),
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
    FormatError,
    UnicodeRangeError,
}

#[test]
fn test_parse() {
    assert_eq!(Ok((Token::OrGroup(vec![]), "")), root(""));

    assert_eq!(
        Ok((
            Token::AndGroup(vec![
                Token::Literal(vec!['h']),
                Token::Literal(vec!['o']),
                Token::Literal(vec!['g']),
                Token::Literal(vec!['e']),
            ]),
            ""
        )),
        root("hoge")
    );

    assert_eq!(
        Ok((
            Token::OrGroup(vec![
                Token::AndGroup(vec![Token::Literal(vec!['A']),]),
                Token::AndGroup(vec![Token::Literal(vec!['B']),])
            ]),
            ""
        )),
        root("A|B")
    );

    assert_eq!(
        Ok((
            Token::AndGroup(vec![Token::Literal(vec!['A']), generate_quantifier(10, 20)]),
            ""
        )),
        root("A{10,20}")
    );

    assert_eq!(
        Ok((
            Token::AndGroup(vec![Token::Literal(vec!['A']), generate_quantifier(0, 20)]),
            ""
        )),
        root("A{,20}")
    );

    assert_eq!(
        Ok((
            Token::AndGroup(vec![
                Token::Literal(vec!['A']),
                generate_quantifier(10, u32::MAX)
            ]),
            ""
        )),
        root("A{10,}")
    );

    assert_eq!(
        Ok((
            Token::AndGroup(vec![Token::Literal(vec!['A']), generate_quantifier(10, 10)]),
            ""
        )),
        root("A{10}")
    );

    assert_eq!(
        Ok((
            Token::AndGroup(vec![Token::CharacterClass(
                CharacterClassMode::Include,
                "0123456789".to_string()
            )]),
            ""
        )),
        root("[0-9]")
    );
}

fn generate_quantifier(start: u32, end: u32) -> Token {
    Token::Quantifier(Range { start, end })
}

fn quantifier<'a>(input: &'a str) -> Result<(Token, &'a str), ParseError> {
    let mut target = input;

    if !target.starts_with("{") {
        return Err(ParseError::Skip);
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
                .map_err(|_| ParseError::FormatError)?;
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
                u32::MAX
            } else {
                let num = target
                    .get(0..i)
                    .ok_or(ParseError::RangeError)?
                    .to_string()
                    .parse::<u32>()
                    .map_err(|_| ParseError::FormatError)?;
                target = target.get(i..).ok_or(ParseError::RangeError)?;
                num
            }
        } else {
            u32::MAX
        }
    } else {
        return Err(ParseError::FormatError);
    };

    if !target.starts_with("}") {
        return Err(ParseError::FormatError);
    }

    target = target
        .get("}".len()..target.len())
        .ok_or(ParseError::RangeError)?;

    return Ok((generate_quantifier(start, end), target));
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

    let mut chars = vec![];
    loop {
        match &it.next() {
            Some((_, '\\')) => {
                if is_range {
                    is_range = false;
                    continue;
                }

                if is_escape {
                    chars.push('\\');
                    is_escape = false;
                } else {
                    is_escape = true;
                }
            }
            Some((_, '-')) => {
                if is_escape {
                    chars.push('-');
                    is_escape = false;
                } else {
                    is_range = true;
                }
            }
            Some((_, ']')) => {
                if is_escape {
                    chars.push(']');
                    is_escape = false;
                    continue;
                }

                if is_range {
                    return Err(ParseError::FormatError);
                }
                break;
            }
            Some((_, c)) => {
                if is_range {
                    is_range = false;
                    for p in ((prev as u32) + 1)..((*c as u32) + 1) {
                        chars.push(char::from_u32(p).ok_or(ParseError::UnicodeRangeError)?);
                    }
                    prev = *c;
                    continue;
                }
                chars.push(*c);
                prev = *c;
            }
            _ => {
                return Err(ParseError::FormatError);
            }
        }
    }

    let token = Token::CharacterClass(mode, String::from_iter(chars));
    return match &it.next() {
        Some((i, _)) => Ok((
            token,
            target.get(*i..target.len()).ok_or(ParseError::RangeError)?,
        )),
        None => Ok((token, target.get(0..0).ok_or(ParseError::RangeError)?)),
    };
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
        generate_quantifier(0, u32::MAX),
        input
            .get(word.len()..input.len())
            .ok_or(ParseError::RangeError)?,
    ))
}

fn plus<'a>(input: &'a str) -> Result<(Token, &'a str), ParseError> {
    let word = "+";
    if !input.starts_with(word) {
        return Err(ParseError::Skip);
    }

    Ok((
        generate_quantifier(1, u32::MAX),
        input
            .get(word.len()..input.len())
            .ok_or(ParseError::RangeError)?,
    ))
}

fn question<'a>(input: &'a str) -> Result<(Token, &'a str), ParseError> {
    let word = "?";
    if !input.starts_with(word) {
        return Err(ParseError::Skip);
    }

    Ok((
        generate_quantifier(0, 1),
        input
            .get(word.len()..input.len())
            .ok_or(ParseError::RangeError)?,
    ))
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

fn metacharacter<'a>(input: &'a str) -> Result<(Token, &'a str), ParseError> {
    let word = "\\d";
    if !input.starts_with(word) {
        return Err(ParseError::Skip);
    }

    Ok((
        Token::CharacterClass(CharacterClassMode::Include, "0123456789".to_string()),
        input
            .get(word.len()..input.len())
            .ok_or(ParseError::RangeError)?,
    ))
}

fn literal<'a>(input: &'a str) -> Result<(Token, &'a str), ParseError> {
    let it = &mut input.char_indices();

    return match &it.next() {
        Some((0, c)) => match &it.next() {
            Some((i, _)) => Ok((
                Token::Literal(vec![*c]),
                input.get(*i..input.len()).ok_or(ParseError::RangeError)?,
            )),
            None => Ok((
                Token::Literal(vec![*c]),
                input.get(0..0).ok_or(ParseError::RangeError)?,
            )),
        },
        _ => Err(ParseError::Skip),
    };
}

pub fn root<'a>(input: &'a str) -> Result<(Token, &'a str), ParseError> {
    let mut tokens: Vec<Token> = vec![];
    let mut target = input;

    loop {
        if target.starts_with(")") {
            return Err(ParseError::FormatError);
        }

        if target.len() == 0 {
            let mut section: Vec<Token> = vec![];
            let mut ts: Vec<Token> = vec![];
            for t in tokens {
                if t == Token::Or {
                    ts.push(Token::AndGroup(section));
                    section = vec![];
                } else {
                    section.push(t);
                }
            }
            if section.len() != 0 {
                if ts.len() == 0 {
                    return Ok((
                        Token::AndGroup(section),
                        target.get(0..0).ok_or(ParseError::RangeError)?,
                    ));
                }
                ts.push(Token::AndGroup(section));
            }
            return Ok((
                Token::OrGroup(ts),
                target.get(0..0).ok_or(ParseError::RangeError)?,
            ));
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

        return Err(ParseError::FormatError);
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

            let mut section: Vec<Token> = vec![];
            let mut ts: Vec<Token> = vec![];
            for t in tokens {
                if t == Token::Or {
                    ts.push(Token::AndGroup(section));
                    section = vec![];
                } else {
                    section.push(t);
                }
            }
            if section.len() != 0 {
                if ts.len() == 0 {
                    return Ok((Token::AndGroup(section), target));
                }
                ts.push(Token::AndGroup(section));
            }
            return Ok((Token::OrGroup(ts), target));
        }

        if target.len() == 0 {
            return Err(ParseError::FormatError);
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

        return Err(ParseError::FormatError);
    }
}
