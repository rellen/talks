import gleam/io
import gleam/list

// prints "one" then errors at two()
pub fn main() {
  one()
  two()
  three()
}

pub fn one() {
  io.println("one")
}

pub fn two() {
  todo as "two() is not implemented yet!"
}

pub fn three() {
  todo as "three() is not implemented yet!"
}
