use crate::runner::Runner;
use pretty_assertions::assert_eq;
use vm::value::VmValue;
use vm::VmError;

#[test]
fn test_option_parse() {
    let mut runner = Runner::default();
    runner.eval("use std::convert::{parse_int, parse_int_radix}");
    assert_eq!(runner.eval("parse_int('+8')"), Some(VmValue::Int(8)));
    assert_eq!(
        runner.eval("parse_int_radix('1011', 2)"),
        Some(VmValue::Int(11))
    );
    assert_eq!(runner.eval("parse_int('15t')"), None);
    assert_eq!(
        runner.eval("parse_int_radix('aA', 16)"),
        Some(VmValue::Int(170))
    );
    assert_eq!(
        runner.try_eval("parse_int('0x').unwrap()"),
        Err(VmError::Panic)
    )
}
