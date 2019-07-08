use super::*;

use std::cell::{RefCell, RefMut};
use std::borrow::Cow;

use kg_tree::opath::Opath;
use kg_display::ListDisplay;


#[derive(Debug, Clone)]
pub struct Var {
    name: String,
}

impl Var {
    pub fn new(name: Cow<str>) -> Var {
        Var {
            name: name.into_owned(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

impl std::fmt::Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "`${}`", self.name)
    }
}


#[derive(Debug, Clone)]
pub struct Expr {
    expr: Opath,
}

impl Expr {
    pub fn new(expr: Opath) -> Expr {
        Expr {
            expr,
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "/{}/", self.expr)
    }
}


#[derive(Debug, Clone)]
pub enum Element {
    Text {
        ws: bool,
        nl: bool,
    },
    Label {
        id: String,
        body: Option<Box<Fragment>>,
    },
    Expr {
        expr: Expr,
    },
    Sequence {
        elems: Vec<Fragment>,
    },
    Set {
        var: Var,
        expr: Expr,
    },
    If {
        expr: Expr,
        body_if: Option<Box<Fragment>>,
        body_else: Option<Box<Fragment>>,
    },
    For {
        key_var: Var,
        value_var: Var,
        expr: Expr,
        body_some: Option<Box<Fragment>>,
        body_none: Option<Box<Fragment>>,
    },
    Def {
        name: String,
        args: Vec<Var>,
        body: Option<Box<Fragment>>,
    },
    Print {
        name: String,
        args: Vec<Expr>,
        body: Option<Box<Fragment>>,
    },
    Include(Expr),
}

impl Element {
    pub fn is_directive(&self) -> bool {
        match *self {
            Element::Set { .. } |
            Element::If { .. } |
            Element::For { .. } |
            Element::Def { .. } |
            Element::Print { .. } => true,
            _ => false,
        }
    }
}


#[derive(Debug, Clone)]
pub struct Fragment {
    from: Position,
    to: Position,
    elem: Element,
}

impl Fragment {
    pub fn new(from: Position, to: Position, elem: Element) -> Fragment {
        Fragment {
            from,
            to,
            elem,
        }
    }

    pub fn from(&self) -> Position {
        self.from
    }

    pub fn set_from(&mut self, from: Position) {
        self.from = from;
    }

    pub fn to(&self) -> Position {
        self.to
    }

    pub fn set_to(&mut self, to: Position) {
        self.to = to;
    }

    pub fn elem(&self) -> &Element {
        &self.elem
    }

    pub fn prune_ws(&mut self) {
        fn prune(f: &mut Fragment, nested: bool) {
            match f.elem {
                Element::Sequence { ref mut elems } => {
                    let mut nl_idx = elems.len();
                    let mut ws_idx = elems.len();
                    let mut dir_idx = elems.len();
                    let mut removed = Vec::new();
                    for (i, f) in elems.iter().enumerate() {
                        if let Element::Text { nl, ws } = f.elem {
                            if nl {
                                nl_idx = i;
                            } else if ws {
                                ws_idx = i;
                                if nested && i == elems.len() - 1 {
                                    removed.push(i);
                                }
                            }
                            if ws && nl {
                                if (nested && i == 0) || (i > 0 && dir_idx == i - 1) {
                                    removed.push(i);
                                }
                            }
                        } else if f.elem.is_directive() {
                            dir_idx = i;
                            if nl_idx == i - 2 && ws_idx == i - 1 {
                                removed.push(ws_idx);
                            }
                        }
                    }
                    while let Some(i) = removed.pop() {
                        elems.remove(i);
                    }
                    for e in elems.iter_mut() {
                        prune(e, false);
                    }
                }
                Element::If { ref mut body_if, ref mut body_else, .. } => {
                    if let Some(ref mut b) = *body_if {
                        prune(b, true);
                    }
                    if let Some(ref mut b) = *body_else {
                        prune(b, true);
                    }
                }
                Element::For { ref mut body_some, ref mut body_none, .. } => {
                    if let Some(ref mut b) = *body_some {
                        prune(b, true);
                    }
                    if let Some(ref mut b) = *body_none {
                        prune(b, true);
                    }
                }
                Element::Def { ref mut body, .. } => {
                    if let Some(ref mut b) = *body {
                        prune(b, true);
                    }
                }
                Element::Print { ref mut body, .. } => {
                    if let Some(ref mut b) = *body {
                        prune(b, true);
                    }
                }
                _ => {}
            }
        }

        prune(self, false);
    }
}


pub (super) struct FragmentDisp<'a, 'b: 'a> {
    level: usize,
    f: &'a Fragment,
    r: &'a RefCell<&'b mut dyn CharReader>,
}

impl<'a, 'b> FragmentDisp<'a, 'b> {
    pub fn new(f: &'a Fragment, r: &'a RefCell<&'b mut dyn CharReader>) -> FragmentDisp<'a, 'b> {
        FragmentDisp {
            level: 0,
            f,
            r,
        }
    }

    fn nested(f: &'a Fragment, r: &'a RefCell<&'b mut dyn CharReader>, level: usize) -> FragmentDisp<'a, 'b> {
        FragmentDisp {
            level,
            f,
            r,
        }
    }

    fn reader(&self) -> RefMut<&'b mut dyn CharReader> {
        self.r.borrow_mut()
    }
}

impl<'a, 'b> std::fmt::Display for FragmentDisp<'a, 'b> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if f.alternate() {
            write!(f, "{}", self.reader().slice_pos(self.f.from, self.f.to).unwrap())?;
        } else {
            fn print_level(f: &mut std::fmt::Formatter, level: usize) -> std::fmt::Result {
                if level > 0 {
                    write!(f, "\n")?;
                    for _ in 0..level {
                        write!(f, "  ")?;
                    }
                }
                Ok(())
            };

            match self.f.elem {
                Element::Text { ws, nl } => {
                    fn flag(f: bool) -> char {
                        if f { '+' } else { '-' }
                    }
                    print_level(f, self.level)?;
                    write!(f, "text: (ws:{}, nl:{}) {:?}", flag(ws), flag(nl), self.reader().slice_pos(self.f.from, self.f.to).unwrap())?;
                }
                Element::Label { ref id, ref body } => {
                    print_level(f, self.level)?;
                    write!(f, "{{{{:{}\n", id)?;
                    if let Some(ref b) = *body {
                        write!(f, "{}", FragmentDisp::nested(b, self.r, self.level + 1))?;
                    }
                    write!(f, "}}}}\n")?;
                }
                Element::Expr { ref expr } => {
                    print_level(f, self.level)?;
                    write!(f, "expr: {}", expr)?;
                }
                Element::Set { ref var, ref expr } => {
                    print_level(f, self.level)?;
                    write!(f, "set: {} = {}", var, expr)?;
                }
                Element::Def { ref name, ref args, ref body } => {
                    print_level(f, self.level)?;
                    write!(f, "def: {:?} ({})", name, ListDisplay(args))?;
                    if let Some(ref b) = *body {
                        write!(f, "{}", FragmentDisp::nested(b, self.r, self.level + 1))?;
                    }
                }
                Element::If { ref expr, ref body_if, ref body_else } => {
                    print_level(f, self.level)?;
                    write!(f, "if: {}", expr)?;
                    if let Some(ref b) = *body_if {
                        write!(f, "{}", FragmentDisp::nested(b, self.r, self.level + 1))?;
                    }
                    if let Some(ref b) = *body_else {
                        print_level(f, self.level)?;
                        write!(f, "else:")?;
                        write!(f, "{}", FragmentDisp::nested(b, self.r, self.level + 1))?;
                    }
                }
                Element::For { ref key_var, ref value_var, ref expr, ref body_some, ref body_none } => {
                    print_level(f, self.level)?;
                    write!(f, "for: {} in {}", value_var, expr)?;
                    if let Some(ref b) = *body_some {
                        write!(f, "{}", FragmentDisp::nested(b, self.r, self.level + 1))?;
                    }
                    if let Some(ref b) = *body_none {
                        print_level(f, self.level)?;
                        write!(f, "else:")?;
                        write!(f, "{}", FragmentDisp::nested(b, self.r, self.level + 1))?;
                    }
                }
                Element::Print { ref name, ref args, ref body } => {
                    print_level(f, self.level)?;
                    write!(f, "print: {:?} ({})", name, ListDisplay(args))?;
                    if let Some(ref b) = *body {
                        write!(f, "{}", FragmentDisp::nested(b, self.r, self.level + 1))?;
                    }
                }
                Element::Sequence { ref elems } => {
                    if self.level > 0 {
                        write!(f, " {{")?;
                    } else {
                        write!(f, "{{")?;
                    }
                    for e in elems.iter() {
                        write!(f, "{}", FragmentDisp::nested(e, self.r, self.level + 1))?;
                    }
                    if self.level > 0 {
                        print_level(f, self.level - 1)?;
                    } else {
                        write!(f, "\n")?;
                    }
                    write!(f, "}}")?;
                }
                Element::Include(ref segment)=>{
                    write!(f, "{:?}", segment)?;
                }
            }
        }
        Ok(())
    }
}


pub fn build(f: Fragment, r: &mut dyn CharReader) -> Result<Segment, Error> {
    fn build_opt(b: Option<Box<Fragment>>, r: &mut dyn CharReader) -> Result<Option<Box<Segment>>, Error> {
        Ok(match b {
            Some(b) => Some(Box::new(build(*b, r)?)),
            None => None,
        })
    }

    Ok(match f.elem {
        Element::Text { .. } => {
            Segment::Text(r.slice_pos(f.from, f.to)?.to_string())
        }
        Element::Label { id, body } => {
            Segment::Label {
                id,
                body: build_opt(body, r)?,
            }
        }
        Element::Expr { expr } => {
            Segment::Expr(expr.expr)
        }
        Element::Sequence { elems } => {
            let mut nelems = Vec::with_capacity(elems.len());
            let mut iter = elems.into_iter().peekable();
            while let Some(e) = iter.next() {
                match *e.elem() {
                    Element::Text { .. } => {
                        let p1 = e.from;
                        let mut p2 = e.to;
                        loop {
                            if let Some(ne) = iter.peek() {
                                if let Element::Text { .. } = *ne.elem() {
                                    p2 = ne.to;
                                } else {
                                    break;
                                }
                            } else {
                                break;
                            }
                            iter.next();
                        }
                        nelems.push(Segment::Text(r.slice_pos(p1, p2)?.to_string()));
                    }
                    _ => nelems.push(build(e, r)?),
                }
            }
            if nelems.len() == 1 {
                nelems.pop().unwrap()
            } else {
                Segment::Sequence {
                    elems: nelems
                }
            }
        }
        Element::Set { var, expr } => {
            Segment::Set {
                var: var.name,
                expr: expr.expr,
            }
        }
        Element::If { expr, body_if, body_else } => {
            Segment::If {
                expr: expr.expr,
                body_if: build_opt(body_if, r)?,
                body_else: build_opt(body_else, r)?,
            }
        }
        Element::For { key_var, value_var, expr, body_some, body_none } => {
            Segment::For {
                key_var: key_var.name,
                value_var: value_var.name,
                expr: expr.expr,
                body_some: build_opt(body_some, r)?,
                body_none: build_opt(body_none, r)?,
            }
        }
        Element::Def { name, args, body } => {
            Segment::Def {
                name,
                args: args.into_iter().map(|a| a.name).collect(),
                body: build_opt(body, r)?,
            }
        }
        Element::Print { name, args, body } => {
            Segment::Print {
                name,
                args: args.into_iter().map(|a| a.expr).collect(),
                body: build_opt(body, r)?,
            }
        }
        Element::Include(path) => Segment::Include {path: path.expr}
    })
}

