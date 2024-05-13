import gleam/io

pub type SpecialString = String

pub fn main() {
  let normal: String = "I'm a string"
  let special: SpecialString = "I'm a special string"

  io.debug(normal == special) // False
}
