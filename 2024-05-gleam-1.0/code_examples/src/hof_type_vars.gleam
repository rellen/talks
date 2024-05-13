import gleam/io
import gleam/string

fn my_fold(collection: List(a), accumulator: b, reducer: fn(a, b) -> b) -> b {
  case collection {
    [] -> accumulator
    [x, ..xs] -> {
      let new_acc = reducer(x, accumulator)
      my_fold(xs, new_acc, reducer)
    }
  }
}

pub fn main() {
  io.debug(my_fold([1, 2, 3], "", fn(a, b) { string.inspect(a) <> b }))
  // "321"
}
