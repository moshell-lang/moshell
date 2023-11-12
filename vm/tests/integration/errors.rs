use crate::runner::Runner;
use pretty_assertions::assert_eq;
use vm::VmError;

#[test]
fn divide_by_zero() {
    let mut runner = Runner::default();
    assert_eq!(runner.try_eval("1 / 0"), Err(VmError::Panic));
}
