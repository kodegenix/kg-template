use super::*;

mod config;
mod fragment;

use self::config::*;
use self::fragment::*;

pub use self::config::Config;

use std::collections::VecDeque;


#[derive(Debug, Display, Detail)]
#[diag(code_offset = 400)]
pub enum ParseErr {
    #[display(fmt = "unexpected token")]
    UnexpectedToken,
}

pub type Error = ParseDiag;


#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum LexContext {
    Text,
    Stmt(bool),
}


#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ParseContext {
    Text,
    Directive(bool),
    Expr,
    Interpolation,
}


#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Terminal {
    End,
    Unknown,
    Text,
    StmtBlockOpen,
    StmtBlockClose,
    StmtLineOpen,
    StmtLineClose,
    BlockOpen,
    BlockClose,
    DirectiveOpen,
    DirectiveFormalOpen,
    DirectiveFormalClose,
    InterpolationOpen,
    InterpolationClose,
    KwDef,
    KwDiff,
    KwElseif,
    KwElse,
    KwEnd,
    KwFor,
    KwIf,
    KwIn,
    KwSet,
    KwPrint,
    KwInclude,
    Id,
    Var,
    Label,
    Assign,
    Comma,
    ParenLeft,
    ParenRight,
}


//FIXME (jc) make common type Token<T: Terminal>
#[derive(Debug, PartialEq, Eq, Clone)]
struct Token {
    term: Terminal,
    from: Position,
    to: Position,
}

impl Token {
    fn new(term: Terminal, p1: Position, p2: Position) -> Token {
        Token {
            term: term,
            from: p1,
            to: p2,
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}-{}) {:?}", self.from, self.to, self.term)
    }
}


#[derive(Debug)]
struct DirectiveBody {
    arm1: Option<Fragment>,
    arm2: Option<Fragment>,
    term: Terminal,
}

#[derive(Debug)]
pub struct Parser {
    config: Config,
    lex_context_stack: Vec<LexContext>,
    parse_context_stack: Vec<ParseContext>,
    token_queue: VecDeque<Token>,
    expr_parser: opath::Parser,
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            config: Default::default(),
            lex_context_stack: Vec::with_capacity(16),
            parse_context_stack: Vec::with_capacity(16),
            token_queue: VecDeque::with_capacity(16),
            expr_parser: opath::Parser::new().with_partial(true),
        }
    }

    pub fn with_config(config: Config) -> Parser {
        Parser {
            config,
            .. Parser::new()
        }
    }

    fn lex_context(&self) -> LexContext {
        *self.lex_context_stack.last().unwrap()
    }

    fn push_lex_context(&mut self, ctx: LexContext) {
        self.lex_context_stack.push(ctx);
    }

    fn pop_lex_context(&mut self) -> LexContext {
        self.lex_context_stack.pop().unwrap()
    }

    fn lex(&mut self, r: &mut dyn CharReader) -> Result<Token, Error> {
        #[inline]
        fn consume(r: &mut dyn CharReader, count: usize, term: Terminal) -> Result<Token, Error> {
            let p1 = r.position();
            r.skip_chars(count)?;
            let p2 = r.position();
            Ok(Token::new(term, p1, p2))
        }

        #[inline]
        fn is_var_first(c: char) -> bool {
            c == '$'
        }

        #[inline]
        fn is_var_rest(c: char) -> bool {
            c.is_alphanumeric() || c == '_' || c == '$'
        }

        #[inline]
        fn is_id_first(c: char) -> bool {
            c.is_alphabetic() || c == '_'
        }

        #[inline]
        fn is_id_rest(c: char) -> bool {
            c.is_alphanumeric() || c == '_' || c == '$'
        }

        #[inline]
        fn is_label_first(c: char) -> bool {
            is_id_first(c)
        }

        #[inline]
        fn is_label_rest(c: char) -> bool {
            is_id_rest(c)
        }

        #[inline]
        fn consume_rest(r: &mut dyn CharReader, f: &dyn Fn(char) -> bool, t: Terminal) -> Result<Token, Error> {
            let p1 = r.position();
            r.next_char()?;
            r.skip_while(f)?;
            let p2 = r.position();
            Ok(Token::new(t, p1, p2))
        }

        match self.lex_context() {
            //FIXME (jc) handle escapes
            LexContext::Text => {
                if r.eof() {
                    let p1 = r.position();
                    Ok(Token::new(Terminal::End, p1, p1))
                } else if r.match_str(&self.config.statement_block_open)? {
                    self.push_lex_context(LexContext::Stmt(true));
                    consume(r, self.config.statement_block_open.len(), Terminal::StmtBlockOpen)
                } else if r.match_str(&self.config.statement_line_open)? {
                    self.push_lex_context(LexContext::Stmt(false));
                    consume(r, self.config.statement_line_open.len(), Terminal::StmtLineOpen)
                } else {
                    let p1 = r.position();
                    loop {
                        if r.eof() {
                            break;
                        } else if r.match_char('\n')? {
                            r.next_char()?;
                            break;
                        } else if r.match_str(&self.config.statement_block_open)? || r.match_str(&self.config.statement_line_open)? {
                            break;
                        }
                        r.next_char()?;
                    }
                    Ok(Token::new(Terminal::Text, p1, r.position()))
                }
            }
            LexContext::Stmt(block) => {
                if self.config.mode == Mode::Protected {
                    if block {
                        if r.match_str(&self.config.statement_block_close)? {
                            self.pop_lex_context();
                            return consume(r, self.config.statement_block_close.len(), Terminal::StmtBlockClose);
                        }
                    } else {
                        if r.eof() {
                            let p = r.position();
                            self.pop_lex_context();
                            return Ok(Token::new(Terminal::StmtLineClose, p, p));
                        } else if let Some('\n') = r.peek_char(0)? {
                            self.pop_lex_context();
                            return consume(r, 1, Terminal::StmtLineClose);
                        }
                    }
                }

                match self.parse_context() {
                    ParseContext::Text => {
                        if r.match_str(&self.config.directive_formal_open)? {
                            let count = self.config.directive_formal_open.len();
                            return consume(r, count, Terminal::DirectiveFormalOpen);
                        } else if r.match_str(&self.config.directive_open)? {
                            let count = self.config.directive_open.len();
                            return consume(r, count, Terminal::DirectiveOpen);
                        } else if r.match_str(&self.config.block_open)? {
                            let count = self.config.block_open.len();
                            return consume(r, count, Terminal::BlockOpen);
                        } else if r.match_str(&self.config.interpolation_open)? {
                            let count = self.config.interpolation_open.len();
                            return consume(r, count, Terminal::InterpolationOpen);
                        }

                        let p1 = r.position();

                        loop {
                            if r.eof() {
                                let p2 = r.position();
                                return Ok(Token::new(if p1 == p2 {
                                    Terminal::End
                                } else {
                                    Terminal::Text
                                }, p1, p2));
                            } else if r.match_char('\n')? {
                                r.next_char()?;
                                let p2 = r.position();
                                return Ok(Token::new(Terminal::Text, p1, p2));
                            } else {
                                if r.match_str(&self.config.directive_open)?
                                    || r.match_str(&self.config.directive_formal_open)?
                                    || r.match_str(&self.config.block_open)?
                                    || r.match_str(&self.config.interpolation_open)? {
                                    let p2 = r.position();
                                    return Ok(Token::new(Terminal::Text, p1, p2));
                                } else if block
                                    && self.config.mode == Mode::Protected
                                    && r.match_str(&self.config.statement_block_close)? {
                                    let p2 = r.position();
                                    return Ok(Token::new(Terminal::Text, p1, p2));
                                }
                                r.next_char()?;
                            }
                        }
                    }
                    ParseContext::Directive(formal) => {
                        if formal {
                            if block {
                                r.skip_whitespace()?;
                            } else {
                                r.skip_whitespace_nonl()?;
                            }
                            if r.match_str(&self.config.directive_formal_close)? {
                                return consume(r, self.config.directive_formal_close.len(), Terminal::DirectiveFormalClose);
                            }
                        }
                        match r.peek_char(0)? {
                            None => Ok(Token::new(Terminal::End, r.position(), r.position())),
                            Some('d') => {
                                if r.match_str("def")? {
                                    consume(r, 3, Terminal::KwDef)
                                } else {
                                    consume_rest(r, &is_id_rest, Terminal::Id)
                                }
                            }
                            Some('e') => {
                                if r.match_str("elseif")? {
                                    consume(r, 4, Terminal::KwElseif)
                                } else if r.match_str("else")? {
                                    consume(r, 4, Terminal::KwElse)
                                } else if r.match_str("end")? {
                                    consume(r, 3, Terminal::KwEnd)
                                } else {
                                    consume_rest(r, &is_id_rest, Terminal::Id)
                                }
                            }
                            Some('f') => {
                                if r.match_str("for")? {
                                    consume(r, 3, Terminal::KwFor)
                                } else {
                                    consume_rest(r, &is_id_rest, Terminal::Id)
                                }
                            }
                            Some('i') => {
                                if r.match_str("if")? {
                                    consume(r, 2, Terminal::KwIf)
                                } else if r.match_str("include")? {
                                    consume(r, 7, Terminal::KwInclude)
                                } else {
                                    consume_rest(r, &is_id_rest, Terminal::Id)
                                }
                            }
                            Some('s') => {
                                if r.match_str("set")? {
                                    consume(r, 3, Terminal::KwSet)
                                } else {
                                    consume_rest(r, &is_id_rest, Terminal::Id)
                                }
                            }
                            Some('p') => {
                                if r.match_str("print")? {
                                    consume(r, 5, Terminal::KwPrint)
                                } else {
                                    consume_rest(r, &is_id_rest, Terminal::Id)
                                }
                            }
                            Some(c) if is_id_first(c) => {
                                consume_rest(r, &is_id_rest, Terminal::Id)
                            }
                            Some(_) => {
                                consume(r, 1, Terminal::Unknown)
                            }
                        }
                    }
                    ParseContext::Expr => {
                        r.skip_whitespace_nonl()?;

                        if r.match_str(&self.config.directive_open)? {
                            let count = self.config.directive_open.len();
                            return consume(r, count, Terminal::DirectiveOpen);
                        } else if r.match_str(&self.config.directive_formal_open)? {
                            let count = self.config.directive_formal_open.len();
                            return consume(r, count, Terminal::DirectiveFormalOpen);
                        } else if r.match_str(&self.config.block_open)? {
                            let count = self.config.block_open.len();
                            return consume(r, count, Terminal::BlockOpen);
                        } else if r.match_str(&self.config.interpolation_open)? {
                            let count = self.config.interpolation_open.len();
                            return consume(r, count, Terminal::InterpolationOpen);
                        }

                        match r.peek_char(0)? {
                            None => Ok(Token::new(Terminal::End, r.position(), r.position())),
                            Some('(') => consume(r, 1, Terminal::ParenLeft),
                            Some(')') => consume(r, 1, Terminal::ParenRight),
                            Some(',') => consume(r, 1, Terminal::Comma),
                            Some('=') => consume(r, 1, Terminal::Assign),
                            Some('i') => {
                                if r.match_str("in")? {
                                    consume(r, 2, Terminal::KwIn)
                                } else {
                                    consume_rest(r, &is_id_rest, Terminal::Id)
                                }
                            }
                            Some(c) if is_id_first(c) => {
                                consume_rest(r, &is_id_rest, Terminal::Id)
                            }
                            Some(c) if is_var_first(c) => {
                                consume_rest(r, &is_var_rest, Terminal::Var)
                            }
                            Some(_) => {
                                consume(r, 1, Terminal::Unknown)
                            }
                        }
                    }
                    ParseContext::Interpolation => {
                        r.skip_whitespace()?;

                        if r.match_str(&self.config.interpolation_open)? {
                            let count = self.config.interpolation_open.len();
                            consume(r, count, Terminal::InterpolationOpen)
                        } else if r.match_str(&self.config.interpolation_close)? {
                            let count = self.config.interpolation_close.len();
                            consume(r, count, Terminal::InterpolationClose)
                        } else {
                            let p = r.position();
                            Err(parse_diag!(ParseErr::UnexpectedToken)) //FIXME (jc) add error info
                        }
                    }
                }
            }
        }
    }

    fn next_token(&mut self, r: &mut dyn CharReader) -> Result<Token, Error> {
        let t = if self.token_queue.is_empty() {
            self.lex(r)?
        } else {
            self.token_queue.pop_front().unwrap()
        };
        Ok(t)
    }

    fn peek_token(&mut self, r: &mut dyn CharReader) -> Result<Token, Error> {
        let t = if self.token_queue.is_empty() {
            let t = self.lex(r)?;
            self.token_queue.push_front(t.clone());
            t
        } else {
            self.token_queue.front().unwrap().clone()
        };
        Ok(t)
    }

    fn push_token(&mut self, t: Token) {
        self.token_queue.push_back(t);
    }

    fn expect_token(&mut self, r: &mut dyn CharReader, term: Terminal) -> Result<Token, Error> {
        let t = self.next_token(r)?;
        if t.term == term {
            Ok(t)
        } else {
            Err(parse_diag!(ParseErr::UnexpectedToken)) //FIXME (jc) add error info
        }
    }


    fn parse_context(&self) -> ParseContext {
        *self.parse_context_stack.last().unwrap()
    }

    fn push_parse_context(&mut self, ctx: ParseContext) {
        self.parse_context_stack.push(ctx);
    }

    fn pop_parse_context(&mut self) -> ParseContext {
        self.parse_context_stack.pop().unwrap()
    }

    pub fn parse(&mut self, r: &mut dyn CharReader) -> Result<Template, Error> {
        self.token_queue.clear();
        self.lex_context_stack.clear();
        self.parse_context_stack.clear();

        self.push_lex_context(match self.config.mode {
            Mode::Replace => LexContext::Stmt(true),
            Mode::Protected => LexContext::Text,
        });
        self.push_parse_context(ParseContext::Text);

        let mut fe = self.parse_sequence(r)?;

        //let rr = RefCell::new(r);
        //println!("{}", FragmentDisp::new(&fe, &rr));
        fe.prune_ws();
        //println!("--- pruned:\n{}", FragmentDisp::new(&fe, &rr));

        //let mut r = rr.into_inner();

        let t = Template(build(fe, r)?);

        Ok(t)
    }

    fn parse_element(&mut self, r: &mut dyn CharReader) -> Result<Option<Fragment>, Error> {
        let t = self.peek_token(r)?;
        match t.term {
            Terminal::End => Ok(None),
            Terminal::Text |
            Terminal::StmtBlockOpen |
            Terminal::StmtBlockClose |
            Terminal::StmtLineOpen |
            Terminal::StmtLineClose => Ok(Some(self.parse_text(r)?)),
            Terminal::DirectiveOpen | Terminal::DirectiveFormalOpen => Ok(Some(self.parse_directive(r)?)),
            Terminal::InterpolationOpen => Ok(Some(self.parse_interpolation(r)?)),
            _ => {
                Err(parse_diag!(ParseErr::UnexpectedToken)) //FIXME (jc) add error info
            }
        }
    }

    fn parse_sequence(&mut self, r: &mut dyn CharReader) -> Result<Fragment, Error> {
        let from = r.position();
        let mut elems = Vec::new();
        //let mut ws_idx = std::usize::MAX;
        while let Some(e) = self.parse_element(r)? {
            elems.push(e);
        }
        Ok(Fragment::new(from, r.position(), Element::Sequence { elems, }))
    }

    fn parse_text(&mut self, r: &mut dyn CharReader) -> Result<Fragment, Error> {
        self.push_parse_context(ParseContext::Text);
        let t = self.next_token(r)?;
        self.pop_parse_context();
        match t.term {
            Terminal::Text |
            Terminal::StmtBlockOpen |
            Terminal::StmtBlockClose |
            Terminal::StmtLineOpen |
            Terminal::StmtLineClose => {
                let s = r.slice_pos(t.from, t.to)?;
                let ws = s.trim().is_empty();
                let nl = s.contains('\n');
                Ok(Fragment::new(t.from, t.to, Element::Text { ws, nl }))
            }
            _ => Err(parse_diag!(ParseErr::UnexpectedToken)), //FIXME(jc) add error info
        }
    }

    fn parse_directive(&mut self, r: &mut dyn CharReader) -> Result<Fragment, Error> {
        let t = self.next_token(r)?;
        match t.term {
            Terminal::DirectiveFormalOpen => {
                self.push_parse_context(ParseContext::Directive(true));
                self.expr_parser.set_multiline(true);
            }
            Terminal::DirectiveOpen => {
                self.push_parse_context(ParseContext::Directive(false));
                self.expr_parser.set_multiline(false);
            }
            _ => return Err(parse_diag!(ParseErr::UnexpectedToken)), //FIXME(jc) add error info
        }
        let tn = self.parse_directive_name(r)?;
        let mut f = match tn.term {
            Terminal::KwSet => self.parse_directive_set(r)?,
            Terminal::KwIf => self.parse_directive_if(r)?,
            Terminal::KwFor => self.parse_directive_for(r)?,
            Terminal::KwDef => self.parse_directive_def(r)?,
            Terminal::KwPrint => self.parse_directive_print(r)?,
            Terminal::KwInclude => self.parse_directive_include(r)?,
            _ => return Err(parse_diag!(ParseErr::UnexpectedToken)), //FIXME(jc) add error info
        };
        self.pop_parse_context();
        f.set_from(t.from);
        f.set_to(r.position());
        Ok(f)
    }

    fn parse_directive_name(&mut self, r: &mut dyn CharReader) -> Result<Token, Error> {
        let t = self.next_token(r)?;
        match t.term {
            Terminal::KwDef |
            Terminal::KwEnd |
            Terminal::KwElse |
            Terminal::KwElseif |
            Terminal::KwFor |
            Terminal::KwIf |
            Terminal::KwPrint |
            Terminal::KwInclude |
            Terminal::KwSet => Ok(t),
            _ => Err(parse_diag!(ParseErr::UnexpectedToken)), //FIXME(jc) add error info
        }
    }

    fn parse_directive_body(&mut self, r: &mut dyn CharReader, allow_else: bool, allow_elseif: bool) -> Result<DirectiveBody, Error> {
        fn pack_elems(elems: Vec<Fragment>) -> Option<Fragment> {
            if elems.is_empty() {
                None
            } else {
                let from = elems.first().unwrap().from();
                let to = elems.last().unwrap().to();
                Some(Fragment::new(from, to, Element::Sequence { elems }))
            }
        }

        if let ParseContext::Directive(true) = self.parse_context() {
            self.expect_token(r, Terminal::DirectiveFormalClose)?;
        }

        self.push_parse_context(ParseContext::Text);

        let mut body = DirectiveBody {
            arm1: None,
            arm2: None,
            term: Terminal::KwEnd,
        };

        let mut elems = Vec::new();
        loop {
            let t = self.next_token(r)?;
            let mut done = false;
            if t.term == Terminal::DirectiveOpen || t.term == Terminal::DirectiveFormalOpen {
                let formal = t.term == Terminal::DirectiveFormalOpen;
                self.push_parse_context(ParseContext::Directive(formal));
                let tn = self.parse_directive_name(r)?;
                match tn.term {
                    Terminal::KwEnd => {
                        body.arm1 = pack_elems(elems);
                        if formal {
                            self.expect_token(r, Terminal::DirectiveFormalClose)?;
                        }
                        done = true;
                        elems = Vec::new();
                    }
                    Terminal::KwElse => {
                        if allow_else {
                            body.arm1 = pack_elems(elems);
                            body.arm2 = self.parse_directive_body(r, false, false)?.arm1;
                            body.term = Terminal::KwElse;
                            done = true;
                            elems = Vec::new();
                        } else {
                            return Err(parse_diag!(ParseErr::UnexpectedToken)); //FIXME(jc) add error info (unexpected #else)
                        }
                    }
                    Terminal::KwElseif => {
                        if allow_elseif {
                            body.arm1 = pack_elems(elems);
                            body.arm2 = Some(self.parse_directive_if(r)?);
                            body.term = Terminal::KwElseif;
                            done = true;
                            elems = Vec::new();
                        } else {
                            return Err(parse_diag!(ParseErr::UnexpectedToken)); //FIXME(jc) add error info (unexpected #elseif)
                        }
                    }
                    _ => {
                        self.push_token(t);
                        self.push_token(tn);
                    }
                }
                self.pop_parse_context();
                if done {
                    break;
                }
            } else {
                self.push_token(t);
            }

            match self.parse_element(r)? {
                Some(e) => elems.push(e),
                None => return Err(parse_diag!(ParseErr::UnexpectedToken)), //FIXME(jc) add error info, (unexpected eof - unterminated directive body),
            }
        }

        self.pop_parse_context();

        Ok(body)
    }

    fn parse_directive_set(&mut self, r: &mut dyn CharReader) -> Result<Fragment, Error> {
        self.push_parse_context(ParseContext::Expr);
        let var = self.parse_var(r)?;
        self.expect_token(r, Terminal::Assign)?;
        self.pop_parse_context();
        let expr = self.parse_expr(r)?;
        Ok(Fragment::new(Position::new(), r.position(), Element::Set {
            var,
            expr,
        }))
    }

    fn parse_directive_if(&mut self, r: &mut dyn CharReader) -> Result<Fragment, Error> {
        let expr = self.parse_expr(r)?;
        let body = self.parse_directive_body(r, true, true)?;
        Ok(Fragment::new(Position::new(), r.position(), Element::If {
            expr,
            body_if: body.arm1.map(|b| Box::new(b)),
            body_else: body.arm2.map(|b| Box::new(b)),
        }))
    }

    fn parse_directive_for(&mut self, r: &mut dyn CharReader) -> Result<Fragment, Error> {
        self.push_parse_context(ParseContext::Expr);
        let value_var = self.parse_var(r)?;
        self.expect_token(r, Terminal::KwIn)?;
        self.pop_parse_context();
        let expr = self.parse_expr(r)?;
        let body = self.parse_directive_body(r, true, false)?;
        Ok(Fragment::new(Position::new(), r.position(), Element::For {
            key_var: Var::new("".into()),
            value_var,
            expr,
            body_some: body.arm1.map(|b| Box::new(b)),
            body_none: body.arm2.map(|b| Box::new(b)),
        }))
    }

    fn parse_directive_def(&mut self, r: &mut dyn CharReader) -> Result<Fragment, Error> {
        self.push_parse_context(ParseContext::Expr);
        let tname = self.expect_token(r, Terminal::Id)?;
        let name = r.slice_pos(tname.from, tname.to)?.to_string();
        let args = self.parse_var_list(r)?;
        self.pop_parse_context();
        let body = self.parse_directive_body(r, false, false)?.arm1;
        Ok(Fragment::new(Position::new(), r.position(), Element::Def {
            name,
            args,
            body: body.map(|b| Box::new(b)),
        }))
    }

    fn parse_directive_include(&mut self, r: &mut dyn CharReader) -> Result<Fragment, Error> {
        self.push_parse_context(ParseContext::Expr);
        let expr = self.parse_expr(r)?;
        self.pop_parse_context();
        
        let fr = Fragment::new(Position::new(), r.position(), Element::Include(expr));

        Ok(fr)
    }



    fn parse_directive_print(&mut self, r: &mut dyn CharReader) -> Result<Fragment, Error> {
        self.push_parse_context(ParseContext::Expr);
        let tname = self.expect_token(r, Terminal::Id)?;
        let name = r.slice_pos(tname.from, tname.to)?.to_string();
        self.expect_token(r, Terminal::ParenLeft)?;
        let mut args = Vec::new();
        let mut has_arg = false;
        loop {
            let t = self.next_token(r)?;
            match t.term {
                Terminal::ParenRight => {
                    break;
                }
                Terminal::Comma => {
                    if has_arg {
                        has_arg = false
                    } else {
                        return Err(parse_diag!(ParseErr::UnexpectedToken)); //FIXME(jc) add error info (unexpected comma)
                    }
                }
                _ => {
                    r.seek(t.from)?;
                    let e = self.parse_expr(r)?;
                    args.push(e);
                    has_arg = true;
                }
            }
        }
        self.pop_parse_context();
        let body = self.parse_directive_body(r, false, false)?;
        Ok(Fragment::new(Position::new(), r.position(), Element::Print {
            name,
            args,
            body: body.arm1.map(|b| Box::new(b)),
        }))
    }

    fn parse_var(&mut self, r: &mut dyn CharReader) -> Result<Var, Error> {
        let t = self.expect_token(r, Terminal::Var)?;
        Ok(Var::new(r.slice(t.from.offset + 1, t.to.offset)?.into()))
    }

    fn parse_var_list(&mut self, r: &mut dyn CharReader) -> Result<Vec<Var>, Error> {
        self.expect_token(r, Terminal::ParenLeft)?;
        let mut vars = Vec::new();
        let mut has_var = false;
        loop {
            let t = self.next_token(r)?;
            match t.term {
                Terminal::Var if !has_var => {
                    vars.push(Var::new(r.slice(t.from.offset + 1, t.to.offset)?.into()));
                    has_var = true;
                }
                Terminal::Comma if has_var => {
                    has_var = false;
                }
                Terminal::ParenRight => break,
                _ => return Err(parse_diag!(ParseErr::UnexpectedToken)), //FIXME(jc) add error info
            }
        }
        Ok(vars)
    }

    fn parse_expr(&mut self, r: &mut dyn CharReader) -> Result<Expr, Error> {
        self.push_parse_context(ParseContext::Expr);
        let e = self.expr_parser.parse(r);
        self.pop_parse_context();
        match e {
            Ok(e) => Ok(Expr::new(e)),
            Err(err) => Err(err),
        }
    }

    fn parse_interpolation(&mut self, r: &mut dyn CharReader) -> Result<Fragment, Error> {
        self.push_parse_context(ParseContext::Interpolation);
        self.expect_token(r, Terminal::InterpolationOpen)?;
        self.expr_parser.set_multiline(true);
        let e = self.parse_expr(r);
        self.expect_token(r, Terminal::InterpolationClose)?;
        self.pop_parse_context();
        match e {
            Ok(expr) => Ok(Fragment::new(Position::new(), r.position(), Element::Expr { expr, })),
            Err(err) => Err(err),
        }
    }
}

