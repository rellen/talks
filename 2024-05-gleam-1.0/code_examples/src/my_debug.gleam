import gleam/io

pub type MyDebug(a) {
  MyDebug(debug: fn(a) -> String)
}

pub fn main() {
  let my_dbg = MyDebug(debug: fn(str) { io.debug("The string is: " <> str) })
  my_dbg.debug("Hello, World!!!")
}
