fn main() {
    println!("parse ouput: {:?}", rcas::parse_eval("21e21"));
    println!("parse ouput: {:?}", rcas::parse_eval("1.21/3"));
    println!("parse ouput: {:?}", rcas::parse_eval("-21/0"));
    println!("parse ouput: {:?}", rcas::parse_eval("21*2"));
    println!("parse ouput: {:?}", rcas::parse_eval("21/0"));
}
