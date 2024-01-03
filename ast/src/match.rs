use dbg_pls::DebugPls;

use context::source::{SourceSegment, SourceSegmentHolder};
use src_macros::segment_holder;

use crate::value::{Literal, TemplateString};
use crate::variable::{Identifier, VarReference};
use crate::Expr;

/// structure of a `match` expression.
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Match<'a> {
    pub operand: Box<Expr<'a>>,
    pub arms: Vec<MatchArm<'a>>,
}

///the arm (a@ b | c if d => ..) of a match expression
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct MatchArm<'a> {
    //the extracted value name (x@ ..)
    pub val_name: Option<Identifier<'a>>,
    //the pattern (.. x | $y | "z" ..)
    pub patterns: Vec<MatchPattern<'a>>,
    //the arm's guard (.. if .. => ..)
    pub guard: Option<Expr<'a>>,
    //the body (.. => <body>)
    pub body: Expr<'a>,
}

///all different kinds of patterns available for a pattern expression
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum MatchPattern<'a> {
    //*, any
    Wildcard(SourceSegment),

    //refer to wrapped structures documentation
    VarRef(VarReference<'a>),
    Literal(Literal),
    Template(TemplateString<'a>),
}

impl SourceSegmentHolder for MatchPattern<'_> {
    fn segment(&self) -> SourceSegment {
        match self {
            Self::Wildcard(segment) => segment.clone(),
            Self::VarRef(var_ref) => var_ref.segment(),
            Self::Literal(literal) => literal.segment(),
            Self::Template(template) => template.segment(),
        }
    }
}
