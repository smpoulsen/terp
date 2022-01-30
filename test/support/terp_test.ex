defmodule Support.TerpTest do
  @moduledoc """
  Helper to parse the test and run it in the context of the prelude.
  """
  alias Terp.TypeSystem


  defmacro __using__(_opts) do
    quote do
      import Support.TerpTest
    end
  end

  def eval_terp(src) do
    TypeSystem.start_environment()
    "(require prelude/typeclass/classes)\n" <> src
    |> Terp.eval()
  end

  def type_check_src(src) do
    TypeSystem.start_environment()
    "(require prelude/typeclass/classes)\n"
    |> Terp.eval()
    TypeSystem.check_src(src)
  end
end
