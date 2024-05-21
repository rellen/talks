import gleam/io
import gleam/list
import gleam/string

fn upcase_duplicate(strings) {
  use str <- list.map(strings)
  let dup = string.append(to: str, suffix: str)
  string.uppercase(dup)
}

pub fn main() {
  ["hello", "world!!"]
  |> upcase_duplicate()
  |> io.debug()
}
