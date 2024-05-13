import gleam/io

pub opaque type OpaqueString {
	OpaqueString(inner: String)
}

pub fn make(str: String) -> OpaqueString {
  OpaqueString(str <> " (totally a string)")
}

pub fn main() {
  let normal: String = "I'm a string"
  let special: OpaqueString = make("I'm not a string")

  // io.debug(normal == special) // Compile error
}
