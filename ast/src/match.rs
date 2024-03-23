use context::source::{SourceSegment, SourceSegmentHolder};
use src_macros::segment_holder;

use crate::value::{Literal, TemplateString};
use crate::variable::{Identifier, VarReference};
use crate::Expr;

/// structure of a `match` expression.
#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct Match {
    pub operand: Box<Expr>,
    pub arms: Vec<MatchArm>,
}

///the arm (a@ b | c if d => ..) of a match expression
#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    //the extracted value name (x@ ..)
    pub val_name: Option<Identifier>,
    //the pattern (.. x | $y | "z" ..)
    pub patterns: Vec<MatchPattern>,
    //the arm's guard (.. if .. => ..)
    pub guard: Option<Expr>,
    //the body (.. => <body>)
    pub body: Expr,
}

///all different kinds of patterns available for a pattern expression
#[derive(Debug, Clone, PartialEq)]
pub enum MatchPattern {
    //*, any
    Wildcard(SourceSegment),

    //refer to wrapped structures documentation
    VarRef(VarReference),
    Literal(Literal),
    Template(TemplateString),
}

impl SourceSegmentHolder for MatchPattern {
    fn segment(&self) -> SourceSegment {
        match self {
            Self::Wildcard(segment) => segment.clone(),
            Self::VarRef(var_ref) => var_ref.segment(),
            Self::Literal(literal) => literal.segment(),
            Self::Template(template) => template.segment(),
        }
    }
}
