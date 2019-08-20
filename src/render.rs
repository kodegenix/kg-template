use std::collections::HashMap;
use std::ops::Deref;

use super::segment::*;
use super::*;
use crate::render::TemplateErrorDetail::{
    ExprErr, IncludeSingleValueExpected, IncludeStringExpected, IoErr, ParseErr,
};

pub type TemplateError = BasicDiag;

pub type TemplateResult<T> = Result<T, TemplateError>;

#[derive(Debug, Display, Detail)]
#[diag(code_offset = 800)]
pub enum TemplateErrorDetail {
    #[display(fmt = "string expected, got '{kind}'")]
    IncludeStringExpected { kind: Kind },

    #[display(fmt = "single value expected")]
    IncludeSingleValueExpected,

    #[display( fmt = "cannot evaluate expression")]
    ExprErr,

    #[display(fmt = "cannot read template file")]
    IoErr,

    #[display(fmt = "cannot parse template file")]
    ParseErr,
}

#[derive(Debug)]
struct RenderScope<'a> {
    expr_scope: ScopeMut,
    defs: HashMap<&'a str, &'a Segment>,
    parent: Option<&'a RenderScope<'a>>,
}

impl<'a> RenderScope<'a> {
    fn root(expr_scope: Option<Scope>) -> RenderScope<'a> {
        RenderScope {
            expr_scope: match expr_scope {
                Some(s) => ScopeMut::child(s),
                None => ScopeMut::new(),
            },
            defs: HashMap::new(),
            parent: None,
        }
    }

    fn child(render_scope: &'a RenderScope<'a>) -> RenderScope<'a> {
        RenderScope {
            expr_scope: ScopeMut::child(render_scope.expr_scope.clone().into()),
            defs: HashMap::new(),
            parent: Some(render_scope),
        }
    }

    fn set_def(&mut self, name: &'a str, def: &'a Segment) {
        self.defs.insert(name, def);
    }

    #[allow(dead_code)]
    fn get_def(&self, name: &str) -> Option<&Segment> {
        match self.defs.get(name) {
            Some(def) => Some(def),
            None => match self.parent {
                Some(p) => p.get_def(name),
                None => None,
            },
        }
    }
}

impl<'a> Deref for RenderScope<'a> {
    type Target = ScopeMut;

    fn deref(&self) -> &Self::Target {
        &self.expr_scope
    }
}

pub fn render<'a>(
    s: &Segment,
    root: &NodeRef,
    current: &NodeRef,
    scope: Option<Scope>,
    out: &mut String,
) -> TemplateResult<()> {
    let mut scope = RenderScope::root(scope);
    render_recursive(s, root, current, &mut scope, out)
}

fn render_recursive<'a>(
    s: &'a Segment,
    root: &NodeRef,
    current: &NodeRef,
    scope: &mut RenderScope<'a>,
    out: &mut String,
) -> TemplateResult<()> {
    match *s {
        Segment::Text(ref s) => {
            out.push_str(s);
        }
        Segment::Expr(ref expr) => {
            let res = expr
                .apply_ext(root, current, scope.as_ref())
                .map_err_as_cause(|| ExprErr)?;
            for n in res {
                out.push_str(&n.data().as_string());
            }
        }
        Segment::Label { .. } => {}
        Segment::Set { ref var, ref expr } => {
            let res = expr
                .apply_ext(root, current, scope.as_ref())
                .map_err_as_cause(|| ExprErr)?;
            scope.set_var(var.into(), res);
        }
        Segment::If {
            ref expr,
            ref body_if,
            ref body_else,
        } => {
            let res = expr
                .apply_ext(root, current, scope.as_ref())
                .map_err_as_cause(|| ExprErr)?;
            let e = match res {
                NodeSet::Empty => false,
                NodeSet::One(a) => a.as_boolean(),
                NodeSet::Many(_) => true,
            };
            let body = if e { body_if } else { body_else };
            if let Some(ref b) = *body {
                let mut scope = RenderScope::child(scope);
                render_recursive(b, root, current, &mut scope, out)?;
            }
        }
        Segment::For {
            ref key_var,
            ref value_var,
            ref expr,
            ref body_some,
            ref body_none,
        } => {
            let res = expr
                .apply_ext(root, current, scope.as_ref())
                .map_err_as_cause(|| ExprErr)?;
            match res {
                NodeSet::Empty => {
                    if let Some(ref b) = *body_none {
                        let mut scope = RenderScope::child(scope);
                        render_recursive(b, root, current, &mut scope, out)?;
                    }
                }
                NodeSet::One(n) => {
                    if let Some(ref b) = *body_some {
                        let mut scope = RenderScope::child(scope);
                        let first = NodeRef::boolean(false);
                        let last = NodeRef::boolean(false);
                        scope.set_var("first".into(), NodeSet::One(first.clone()));
                        scope.set_var("last".into(), NodeSet::One(last.clone()));

                        match *n.data().value() {
                            Value::Array(ref elems) => {
                                for (i, n) in elems.iter().enumerate() {
                                    *first.data_mut().value_mut() = Value::Boolean(i == 0);
                                    *last.data_mut().value_mut() =
                                        Value::Boolean(i == elems.len() - 1);
                                    if !key_var.is_empty() {
                                        let index = NodeRef::integer(i as i64);
                                        scope.set_var(key_var.into(), NodeSet::One(index));
                                    }
                                    if !value_var.is_empty() {
                                        scope.set_var(value_var.into(), NodeSet::One(n.clone()));
                                    }
                                    render_recursive(b, root, n, &mut scope, out)?;
                                }
                            }
                            Value::Object(ref props) => {
                                for (i, (k, n)) in props.iter().enumerate() {
                                    *first.data_mut().value_mut() = Value::Boolean(i == 0);
                                    *last.data_mut().value_mut() =
                                        Value::Boolean(i == props.len() - 1);
                                    if !key_var.is_empty() {
                                        scope.set_var(
                                            key_var.into(),
                                            NodeSet::One(NodeRef::string(k.as_ref())),
                                        );
                                    }
                                    if !value_var.is_empty() {
                                        scope.set_var(value_var.into(), NodeSet::One(n.clone()));
                                    }
                                    render_recursive(b, root, n, &mut scope, out)?;
                                }
                            }
                            _ => {}
                        }
                    }
                }
                NodeSet::Many(elems) => {
                    if let Some(ref b) = *body_some {
                        let mut scope = RenderScope::child(scope);
                        let first = NodeRef::boolean(false);
                        let last = NodeRef::boolean(false);
                        scope.set_var("first".into(), NodeSet::One(first.clone()));
                        scope.set_var("last".into(), NodeSet::One(last.clone()));

                        for (i, n) in elems.iter().enumerate() {
                            *first.data_mut().value_mut() = Value::Boolean(i == 0);
                            *last.data_mut().value_mut() = Value::Boolean(i == elems.len() - 1);
                            if !key_var.is_empty() {
                                let index = NodeRef::integer(i as i64);
                                scope.set_var(key_var.into(), NodeSet::One(index));
                            }
                            if !value_var.is_empty() {
                                scope.set_var(value_var.into(), NodeSet::One(n.clone()));
                            }
                            render_recursive(b, root, n, &mut scope, out)?;
                        }
                    }
                }
            }
        }
        Segment::Sequence { ref elems } => {
            for e in elems.iter() {
                render_recursive(e, root, current, scope, out)?;
            }
        }
        Segment::Def { ref name, .. } => {
            scope.set_def(name, s);
        }
        Segment::Print { .. } => {}
        Segment::Include { ref path } => {
            let res = path
                .apply_ext(root, current, scope.as_ref())
                .map_err_as_cause(|| ExprErr)?;

            let path = if let NodeSet::One(path) = res {
                if path.is_string() {
                    path.into_string()
                } else {
                    return Err(IncludeStringExpected {
                        kind: path.data().kind(),
                    }
                    .into());
                }
            } else {
                // FIXME add error info
                return Err(IncludeSingleValueExpected.into());
            };

            let template_str = match kg_diag::io::fs::read_string(path).into_diag_res() {
                Ok(t) => t,
                Err(err) => {
                    return Err(IoErr.with_cause(err));
                }
            };

            let mut r = MemCharReader::new(template_str.as_ref());

            let template = match Parser::new().parse(&mut r) {
                Ok(t) => t,
                Err(err) => {
                    return Err(ParseErr.with_cause(err));
                }
            };

            let mut include_out = String::new();
            let mut scope = RenderScope::root(None);

            render_recursive(&template.0, root, current, &mut scope, &mut include_out)?;

            // TODO add padding
            out.push_str(&include_out);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn include() {
        let ctx = r#"{
            "arr": [1,2,3,4]
        }"#;

        let input = r#"
        before include
        #include "test-data/test_template"
        after include
        "#;

        let mut t = Template::parse(input.as_bytes()).unwrap();
        println!("{}", t);

        let n = NodeRef::from_json(ctx).unwrap();
        let scope = ScopeMut::new();
        scope.set_var("type2".into(), NodeSet::One(n.clone()));

        let mut out = String::new();
        t.render_ext(&n, &n, scope.as_ref(), &mut out).unwrap();

        println!("out:\n---\n{}\n---", out.replace(' ', "\u{b7}"));

        let expected = r#"
        before include
        line1
line2
    -
    value is 1
    value is 2
    value is 3
    value is 4

        after include
        "#;

        assert_eq!(expected, &out)
    }

    #[test]
    fn render() {
        let ctx = r#"{
           "package": "org.example.geom",
           "name": "Point3",
           "fields": [ {
             "name": "x"
           }, {
             "name": "y"
           }, {
             "name": "z"
           } ]
         }"#;

        let input = r#"aaa
         text
         more text
     #set $type = $type2.fields
     #for $i in $type
         #if $first
         -
         #end
         value is <% $i.name %> <% 1+2 %>
     #end
         sss
         "#;

        let mut t = Template::parse(input.as_bytes()).unwrap();
        println!("{}", t);

        let n = NodeRef::from_json(ctx).unwrap();
        let scope = ScopeMut::new();
        scope.set_var("type2".into(), NodeSet::One(n.clone()));

        let mut out = String::new();
        t.render_ext(&n, &n, scope.as_ref(), &mut out);

        println!("out:\n---\n{}\n---", out.replace(' ', "\u{b7}"));
    }
}
