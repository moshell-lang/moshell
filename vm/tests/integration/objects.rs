use crate::runner::Runner;
use pretty_assertions::assert_eq;

use vm::value::VmValue;

#[test]
fn test_structure_declaration() {
    let mut runner = Runner::default();
    let res = runner.try_eval(
        "\
        struct Test {
            a: Int,
            b: String,
        }
    ",
    );

    assert_eq!(res, Ok(Some(VmValue::Void)))
}

#[test]
fn test_structure_instantiation() {
    let mut runner = Runner::default();
    let res = runner.try_eval(
        "\
        struct Test {
            a: Int,
            b: String,
        }

        Test(1, 'two')
    ",
    );

    assert_eq!(
        res,
        Ok(Some(VmValue::Struct(vec![
            Some(VmValue::Int(1)),
            Some("two".into()),
        ])))
    )
}

#[test]
fn test_structure_assign() {
    let mut runner = Runner::default();
    let res = runner.try_eval(
        "\
        struct Test {
            a: Int,
            b: String
        }

        val t = Test(1, 'two')
        $t.a = 70
        $t
    ",
    );

    assert_eq!(
        res,
        Ok(Some(VmValue::Struct(vec![
            Some(VmValue::Int(70)),
            Some("two".into()),
        ])))
    )
}

#[test]
#[ignore] //FIXME see #167
fn test_structure_field_subscript() {
    let mut runner = Runner::default();
    let res = runner.try_eval(
        "\
        struct Test {
            a: Int,
            b: Vec[Int],
        }

        val t = Test(1, std::new_vec())
        $t.b.push(10)
        $t.b[0]
    ",
    );

    assert_eq!(res, Ok(Some(VmValue::Int(10))))
}

#[test]
#[ignore] //FIXME see #167
fn test_structure_field_subscript_assign() {
    let mut runner = Runner::default();
    let res = runner.try_eval(
        "\
        struct Test {
            a: Int,
            b: Vec[Int],
        }

        val t = Test(1, std::new_vec())
        $t.b.push(10)
        $t.b[0] = 78
        $t
    ",
    );

    assert_eq!(
        res,
        Ok(Some(VmValue::Struct(vec![
            Some(VmValue::Int(1)),
            Some(VmValue::Vec(vec![Some(VmValue::Int(78))])),
        ])))
    )
}

#[test]
fn test_structure_gc() {
    let mut runner = Runner::default();
    runner.eval(
        "\
        struct Test {
            a: Int,
            b: Vec[Int],
        }

        val t = Test(1, std::new_vec())
        $t.b.push(10)
        $t.b = std::new_vec()
    ",
    );

    assert_eq!(
        runner.gc(),
        vec![VmValue::Int(10), VmValue::Vec(vec![Some(VmValue::Int(10))])].into()
    );
}
