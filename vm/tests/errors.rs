use crate::runner::Runner;
use vm::VmError;

mod runner;

#[test]
fn divide_by_zero() {
    let mut runner = Runner::default();
    assert_eq!(runner.try_eval("1 / 0"), Err(VmError::Panic));
}
