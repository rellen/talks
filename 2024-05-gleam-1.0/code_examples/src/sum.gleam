import gleam/io
import gleam/list

fn sum(xs: List(Int)) -> Int {
  list.fold(xs, 0, fn(acc, x) { acc + x })
}

pub fn main() {
  io.debug(sum([1, 2, 3])) // 6
}
