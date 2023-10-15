use crate::runner::{GarbageCollection, Runner};
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
            c: Test
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
            c: Option[Test]
        }

        Test(1, 'two', std::some(Test(3, 'four', std::none())))
    ",
    );

    assert_eq!(
        res,
        Ok(Some(VmValue::Struct(vec![
            Some(VmValue::Int(1)),
            Some("two".into()),
            Some(VmValue::Struct(vec![
                Some(VmValue::Int(3)),
                Some("four".into()),
                None,
            ])),
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
            b: String,
            c: Option[Test]
        }

        val t = Test(1, 'two', std::some(Test(3, 'four', std::none())))
        $t.c.unwrap().a = 70
        $t
    ",
    );

    assert_eq!(
        res,
        Ok(Some(VmValue::Struct(vec![
            Some(VmValue::Int(1)),
            Some("two".into()),
            Some(VmValue::Struct(vec![
                Some(VmValue::Int(70)),
                Some("four".into()),
                None,
            ])),
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

#[test]
fn test_self_referenced_structure_gc() {
    let mut runner = Runner::default();
    runner.eval(
        "\
        struct Foo {
            n: Option[Foo],
        }

        var t = std::some(Foo(std::none()))
        $t.unwrap().n = $t
    ",
    );

    assert_eq!(runner.gc(), GarbageCollection::from(Vec::<VmValue>::new()));

    // release root link
    runner.eval("$t = std::none()");

    assert_eq!(runner.gc(), vec![VmValue::Struct(vec![]),].into())
}
