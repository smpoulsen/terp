defmodule Terp.Types.Types do
  @moduledoc """
  Types

  Constructors:
    - Tconst -> Constants
    - Tvar   -> type variables
    - Tarrow -> arrow type; function
    - Tlist  -> list

  """
  alias Terp.Types.Types
  alias Terp.Types.TypeEvaluator

  defstruct [:constructor, :t, :str]

  @type t :: %Types{constructor: atom(), t: atom(), str: String.t}

  @spec bool() :: Types.t
  def bool() do
    %Types{constructor: :Tconst, t: :BOOLEAN, str: "Bool"}
  end

  @spec int() :: Types.t
  def int() do
    %Types{constructor: :Tconst, t: :INTEGER, str: "Int"}
  end

  @spec string() :: Types.t
  def string() do
    %Types{constructor: :Tconst, t: :STRING, str: "String"}
  end

  @spec function(Types.t, Types.t) :: Types.t
  def function(%Types{} = t1, %Types{} = t2) do
    %Types{
      constructor: :Tarrow,
      t: {t1, t2},
      str: "(-> #{t1.str} #{t2.str})"
    }
  end

  @spec list(Types.t) :: Types.t
  def list(%Types{} = x) do
    %Types{constructor: :Tlist, t: x, str: "[#{x.str}]"}
  end

  @spec tuple(Types.t, Types.t) :: Types.t
  def tuple(%Types{} = x, %Types{} = y) do
    %Types{constructor: :Ttuple, t: {x, y}, str: "{#{x.str}, #{y.str}}"}
  end

  @spec var(String.t | atom()) :: Types.t
  def var(x) do
    %Types{constructor: :Tvar, t: x, str: to_string(x)}
  end

  @doc """
  Run the type evaluator for a given piece of source code.
  """
  @spec type_check(String.t) :: [Types.t]
  def type_check(src) do
    src
    |> Terp.to_ast()
    |> Enum.map(fn x ->
      TypeEvaluator.run_infer(x)
    end)
  end
end
