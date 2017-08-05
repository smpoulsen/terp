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

  test "factorial.tp" do
    assert IO.run_terp("examples/factorial.tp") == 840
  end

  test "conditional.tp" do
    assert IO.run_terp("examples/conditional.tp") == "bark"
  end

  test "partial_application.tp" do
    assert IO.run_terp("examples/partial_application.tp") == 8
  end

  test "modules.tp" do
    assert IO.run_terp("examples/modules.tp") == 3628800
  end
end
