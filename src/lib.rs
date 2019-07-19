#![feature(specialization)]

extern crate kg_diag;
#[macro_use]
extern crate kg_diag_derive;
#[macro_use]
extern crate kg_display_derive;
#[macro_use]
extern crate serde_derive;

use crate::render::TemplateResult;
use kg_diag::*;
use kg_io::*;
use kg_tree::opath::*;
use kg_tree::*;
pub use parse::{Error as ParseError, Parser};
use segment::Segment;

pub mod parse;
mod render;
mod segment;

pub struct Template(Segment);

impl Template {
    pub fn parse(input: &[u8]) -> Result<Template, ParseError> {
        let mut r = MemCharReader::new(input);
        Parser::new().parse(&mut r)
    }

    pub fn render(
        &self,
        root: &NodeRef,
        current: &NodeRef,
        out: &mut String,
    ) -> TemplateResult<()> {
        self::render::render(&self.0, root, current, None, out)
    }

    pub fn render_ext(
        &self,
        root: &NodeRef,
        current: &NodeRef,
        scope: &Scope,
        out: &mut String,
    ) -> TemplateResult<()> {
        self::render::render(&self.0, root, current, Some(scope.clone()), out)
    }
}

impl std::fmt::Display for Template {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
