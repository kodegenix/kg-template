use super::*;

#[derive(Debug, Clone)]
pub enum Segment {
    Text(String),
    Expr(Opath),
    Label {
        id: String,
        body: Option<Box<Segment>>,
    },
    Sequence {
        elems: Vec<Segment>,
    },
    Set {
        var: String,
        expr: Opath,
    },
    If {
        expr: Opath,
        body_if: Option<Box<Segment>>,
        body_else: Option<Box<Segment>>,
    },
    For {
        key_var: String,
        value_var: String,
        expr: Opath,
        body_some: Option<Box<Segment>>,
        body_none: Option<Box<Segment>>,
    },
    Def {
        name: String,
        args: Vec<String>,
        body: Option<Box<Segment>>,
    },
    Print {
        name: String,
        args: Vec<Opath>,
        body: Option<Box<Segment>>,
    },
    Include {
        path: Opath,
    },
}

impl std::fmt::Display for Segment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use kg_display::{ListDisplay, PrettyPrinter};
        use std::fmt::Write;
        const PADDING: &str = "    ";

        fn write_body(f: &mut std::fmt::Formatter, body: &Segment) -> std::fmt::Result {
            match *body {
                Segment::Sequence { .. } => {
                    write!(f, " {}", body)?;
                }
                _ => {
                    {
                        let mut p = PrettyPrinter::new(f, PADDING);
                        write!(p, " {{\n{}\n", body)?;
                    }
                    write!(f, "}}")?;
                }
            }
            Ok(())
        }

        match *self {
            Segment::Text(ref s) => {
                write!(f, "text: {:?}", s)?;
            }
            Segment::Expr(ref expr) => {
                write!(f, "expr: {}", expr)?;
            }
            Segment::Label { ref id, ref body } => {
                write!(f, "{{{{:{}\n", id)?;
                if let Some(ref b) = *body {
                    write_body(f, b)?;
                }
                write!(f, "}}}}\n")?;
            }
            Segment::Set { ref var, ref expr } => {
                write!(f, "set: ${} = {}", var, expr)?;
            }
            Segment::Def {
                ref name,
                ref args,
                ref body,
            } => {
                write!(f, "def: {:?} ({})", name, ListDisplay(args))?;
                if let Some(ref b) = *body {
                    write_body(f, b)?;
                }
            }
            Segment::If {
                ref expr,
                ref body_if,
                ref body_else,
            } => {
                write!(f, "if: {}", expr)?;
                if let Some(ref b) = *body_if {
                    write_body(f, b)?;
                }
                if let Some(ref b) = *body_else {
                    write!(f, "else:")?;
                    write_body(f, b)?;
                }
            }
            Segment::For {
                key_var: _,
                ref value_var,
                ref expr,
                ref body_some,
                ref body_none,
            } => {
                write!(f, "for: ${} in {}", value_var, expr)?;
                if let Some(ref b) = *body_some {
                    write_body(f, b)?;
                }
                if let Some(ref b) = *body_none {
                    write!(f, "else:")?;
                    write_body(f, b)?;
                }
            }
            Segment::Print {
                ref name,
                ref args,
                ref body,
            } => {
                write!(f, "print: {:?} ({})", name, ListDisplay(args))?;
                if let Some(ref b) = *body {
                    write_body(f, b)?;
                }
            }
            Segment::Sequence { ref elems } => {
                {
                    let mut p = PrettyPrinter::new(f, PADDING);
                    write!(p, "[\n")?;
                    for e in elems.iter() {
                        write!(p, "{}\n", e)?;
                    }
                }
                write!(f, "]")?;
            }
            Segment::Include { ref path } => {
                write!(f, "include: {}", path)?;
            }
        }
        Ok(())
    }
}
