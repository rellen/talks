import gleam/io
import gleam/result
import gleam/string

pub fn main() {
  let res = {
    use data <- result.try(read_data())
    use record <- result.map(find_record(data))
    format(record)
  }

  case res {
    Ok(formatted) -> io.println(formatted)
    Error(error) -> io.println("ERROR: " <> error)
  }
}

fn read_data() {
  Ok(#("Ashley", 42))
  Error("couldn't read data")
}

fn find_record(record) {
  Ok(record)
  Error("not found")
}

fn format(record) {
  string.inspect(record)
}
