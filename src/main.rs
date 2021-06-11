fn main() {
    println!("parse ouput: {:?}", rcas::test("21e21"));
    println!("parse ouput: {:?}", rcas::test("21.21"));
    println!("parse ouput: {:?}", rcas::test("21"));
    println!("parse ouput: {:?}", rcas::test("21+2"));
    println!("parse ouput: {:?}", rcas::test("21*2"));
    println!("Hello, world!");
}
