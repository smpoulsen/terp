defmodule Terp.IOTest do
  use ExUnit.Case
  doctest Terp.IO
  alias Terp.IO

  test "vars.tp" do
    assert IO.run_terp("examples/vars.tp") == 5
  end

  test "identity.tp" do
    assert IO.run_terp("examples/identity.tp") == 7
  end

  test "recursive.tp" do
    assert IO.run_terp("examples/recursive.tp") == 120
  end
end
