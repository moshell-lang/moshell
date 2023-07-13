//! contains diagnostics that can be emitted by any step

use context::source::SourceSegment;

use crate::diagnostic::{Diagnostic, DiagnosticID, Observation, SourceLocation};
use crate::environment::variables::TypeInfo;
use crate::name::Name;
use crate::relations::SourceId;

pub fn diagnose_invalid_symbol(
    base_type: TypeInfo,
    env_id: SourceId,
    name: &Name,
    segments: &[SourceSegment],
) -> Diagnostic {
    let name_root = name.root();
    let (_, tail) = name.parts().split_first().unwrap();
    let base_type_name = base_type.to_string();
    let msg = format!("`{name_root}` is a {base_type_name} which cannot export any inner symbols");

    let mut observations: Vec<_> = segments
        .iter()
        .map(|seg| Observation::new(SourceLocation::new(env_id, seg.clone())))
        .collect();
    observations.sort_by_key(|s| s.location.segment.start);

    Diagnostic::new(DiagnosticID::InvalidSymbol, msg)
        .with_observations(observations)
        .with_help(format!(
            "`{}` is an invalid symbol in {base_type_name} `{name_root}`",
            Name::from(tail)
        ))
}
