fn main() {
    println!("parse ouput: {:?}", rcas::test("21e21"));
    println!("parse ouput: {:?}", rcas::test("1.21/3"));
    println!("parse ouput: {:?}", rcas::test("-21/0"));
    println!("parse ouput: {:?}", rcas::test("21*2"));
    println!("parse ouput: {:?}", rcas::test("21/0"));
}
