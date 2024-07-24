# Elixir 1.17 Typechecker

## Setup

defmodule ModelledTypes do
  def match1() do
    name = "Ashley"
    age = 42
    ^name = age
  end

  def match2() do
    ashley = "Ashley"
    brooklyn = "Brooklyn"
    ^ashley = brooklyn
  end

  def match3({dislike, value}) do
    likes = ["programming language theory", "cat memes"]
    dislikes = %{dislike => value, dynamic_typing: true, pineapple_on_pizza: 200 / 2}
    ^likes = dislikes
  end

  def match4() do
    ashley_dob = %Date{} = ~D[1982-07-24]
    brooklyn_dob = %DateTime{} = ~U[1982-07-24 10:00:00Z]
    ^ashley_dob = brooklyn_dob
  end

  # match5 is the only function that would not warn in v1.16
  def match5() do
    brooklyn = {"Brooklyn", 42.0}
    increase_age = fn {name, age} -> {name, age + 0.1} end
    ^brooklyn = increase_age
  end
end

defmodule MapKeys do
  def non_existent_key1() do
    ashley = %{first_name: "Ashley", age: 42}
    # would be a runtime error
    ashley.last_name
    :ok
  end

  def non_existent_key2() do
    ashley = %{first_name: "Ashley", age: 42}
    # nil
    ashley[:last_name]
    :ok
  end

  def non_existent_key3() do
    ashley = %{first_name: "Ashley", age: 42}
    # open vs closed
    %{last_name: _last_name} = ashley
    :ok
  end

  def non_existent_key4() do
    ashley = %{first_name: "Ashley", age: 42}
    ashley1 = Map.put(ashley, :occupation, "software developer")
    ashley1.last_name
    :ok
  end

  def non_existent_key5() do
    foo = :bar
    ashley = %{first_name: "Ashley", age: 42}
    # ?? runtime error
    ashley = %{ashley | hobby: "cooking"}
    ^foo = ashley
    ashley.last_name
    :ok
  end

  def maybe_non_existent_key(%{first_name: _first_name, age: _age} = params) do
    foo = :bar
    # just here to show that params is an "open" map
    ^foo = params
    params.last_name
    :ok
  end
end

defmodule Person do
  defstruct [:first_name, :age]

  @type t :: %__MODULE__{first_name: String.t(), age: integer()}

  def build_person(first_name, age) do
    %Person{first_name: first_name, age: age}
  end
end

defmodule StructKeys1 do
  def non_existent_key() do
    ashley = %Person{first_name: "Ashley", age: 42}
    ashley.last_name
    :ok
  end

  def non_existent_key2() do
    ashley = %Person{}
    ashley.last_name
    :ok
  end

  def value_type() do
    ashley = %Person{first_name: "Ashley", age: 42}
    ashley.age <> "1"
    :ok
  end
end

defmodule StructKeys2 do
  @spec struct_arg1(Person.t()) :: :ok
  def struct_arg1(person) do
    person.last_name
    :ok
  end

  def struct_arg2(%Person{} = person) do
    person.last_name
    :ok
  end

  def no_inference() do
    # build_person => %Person{first_name: first_name, age: age}
    person = Person.build_person("Ashley", 42)
    person.last_name
    :ok
  end

  def no_inference2() do
    person = %Person{} = Person.build_person("Ashley", 42)
    person.last_name
    :ok
  end

  def not_a_person(), do: :not_a_person

  def no_inference3() do
    person = %Person{} = not_a_person()
    person.first_name
    :ok
  end
end

defmodule NoInference do
  defmodule Foo do
    defstruct [:foo]
  end

  defmodule Bar do
    defstruct [:bar]
  end

  defmodule Baz do
    defstruct [:baz]
  end

  @spec do_something(%Foo{} | %Bar{}) :: :ok
  def do_something(%Foo{} = f), do: IO.inspect(f.foo)
  :ok
  def do_something(%Bar{} = b), do: IO.inspect(b.bar)
  :ok

  def call_do_something() do
    do_something(%Baz{baz: "baz"})
  end
end

defmodule Functions do
  def call_on_unknown_value(arg) do
    arg.my_fun()
    arg.()
  end

  def call_on_non_module() do
    value = 2
    # warns in v1.16
    value.my_fun()
  end

  def call_on_non_function() do
    value = 2
    # warns in v1.16
    value.()
  end

  def bad_call_on_module() do
    mod = Person
    mod.bad("Ashley", 42)
  end

  def bad_call_on_module2() do
    # type-checks - no return-type inference, remember?
    mod = String.to_atom("Person")
    mod.bad("Ashley", 42)
  end
end

defmodule ValueComparisons do
  def compare_ages() do
    ashley_age = 42
    brooklyn_age = "42"
    brooklyn_name = "Brooklyn"
    IO.inspect(ashley_age > brooklyn_age, label: "greater than?")
    # false
    IO.inspect(ashley_age == brooklyn_age, label: "equal?")
    # false
    IO.inspect(ashley_age === brooklyn_age, label: "triple equal?")
    # false
    IO.inspect(ashley_age == brooklyn_name, label: "equal?")
    :ok
  end

  def compare_ages2() do
    ashley_age = 42
    brooklyn_age = 42.0
    # false
    IO.inspect(ashley_age > brooklyn_age, label: "greater than?")
    # true
    IO.inspect(ashley_age == brooklyn_age, label: "WAT?")
    # false
    IO.inspect(ashley_age === brooklyn_age, label: "triple WAT?")
    :ok
  end
end

defmodule StructComparisons do
  def compare_structs() do
    ashley = %Person{first_name: "Ashley", age: 42}
    ashley_too = %{first_name: "Ashley", age: 42}
    IO.inspect(ashley > ashley_too, label: "greater than?")
  end

  def compare_structs2() do
    ashley = %Person{first_name: "Ashley", age: 42}
    ashley_older = %Person{first_name: "Ashley", age: 43}
    IO.inspect(ashley_older > ashley, label: "greater than?")
  end

  def compare_structs3() do
    ashley = %Person{first_name: "Ashley", age: 42}
    ashley_too = %Person{first_name: "Ashley", age: 42}
    IO.inspect(ashley > ashley_too, label: "head like a hole-driven development")
  end
end

## Fin
