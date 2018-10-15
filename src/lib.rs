#![feature(box_syntax, specialization)]

#[macro_use]
extern crate serde_derive;
extern crate serde;
extern crate toml;

#[macro_use]
extern crate kg_diag;
#[macro_use]
extern crate kg_diag_derive;
extern crate kg_display;
#[macro_use]
extern crate kg_display_derive;
extern crate kg_io;
extern crate kg_tree;
extern crate kg_utils;


use kg_diag::*;
use kg_io::*;
use kg_tree::*;
use kg_tree::opath::*;

pub mod parse;
mod segment;
mod render;

use std::cell::RefCell;
use std::borrow::Cow;

pub use parse::{Parser, Error as ParseError};
pub use render::Error as RenderError;

use segment::Segment;

pub struct Template(Segment);

impl Template {
    pub fn parse(input: &[u8]) -> Result<Template, ParseError> {
        let mut r = MemCharReader::new(input);
        Parser::new().parse(&mut r)
    }

    pub fn render(&self, root: &NodeRef, current: &NodeRef, out: &mut String) -> Result<(), RenderError> {
        self::render::render(&self.0, root, current, None, out)
    }

    pub fn render_ext(&self, root: &NodeRef, current: &NodeRef, scope: &Scope, out: &mut String) -> Result<(), RenderError> {
        self::render::render(&self.0, root, current, Some(scope.clone()), out)
    }
}

impl std::fmt::Display for Template {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
