use std::env;
use std::fmt;

fn main() {
    let args: Vec<String> = env::args().collect();

    let name:String = args[1].parse().unwrap();
    let pwrd:String = args[2].parse().unwrap();

    let mut ctx = zmq::Context::new();
    
    let req = ctx.socket(zmq::REQ).unwrap();
    assert!(req.connect("tcp://127.0.0.1:5555").is_ok());

    let mut msg = zmq::Message::new();

    let action = format!("AUTHORIZE {} {}", name, pwrd);
    req.send(&action, 0).unwrap();
    req.recv(&mut msg, 0).unwrap();

    println!("Authorizing user .... {}", msg.as_str().unwrap());
}
