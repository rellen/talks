import gleam/io
import gleam/list

fn sum(xs) {
  list.fold(xs, 0, fn(acc, x) { acc + x })
}

pub fn main() {
todo
  // sum(["a", "b", "c"])
}

//   │
// 8 │   sum(["a", "b", "c"])
//   │       ^^^^^^^^^^^^^^^

// Expected type:

//     List(Int)

// Found type:

//     List(String)
