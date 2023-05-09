defmodule DialyzerExample.TypeExample do
  def add(a, b), do: a + b

  def concat(str1, str2), do: str1 <> str2

  @spec wat(number(), String.t()) :: number()
  def wat(a, b), do: a + b

  def run_add, do: add(1, :two)
  def run_concat, do: concat(:not_a_string, "suffix")
end
