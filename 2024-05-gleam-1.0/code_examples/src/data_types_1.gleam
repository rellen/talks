import gleam/io
import gleam/string

pub type Person {
  Person(name: String, age: Int)
}

pub fn main() {
  let ashley = Person("Ashley", 42)
  let message =
    ashley.name <> " is " <> string.inspect(ashley.age) <> " years old"
  io.println(message)
  // "Ashley is 42 years old"
}
