pub type Shape {
  Square(side: Float)
  Rectangle(length: Float, width: Float)
  Circle(radius: Float)
}

pub fn area(shape: Shape) -> Float {
  case shape {
    // note the *. operator
    Square(side: s) -> s *. s
    Rectangle(length: l, width: w) -> l *. w
    Circle(radius: r) -> 3.14 *. r *. r
  }
}
// something like this from Erlang/Elixir won't compile
// pub fn area2(Square(side: s)) {
//   s *. s
// }
