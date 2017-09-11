defmodule Terp.Types.Types do
  @moduledoc """
  Types

  Constructors:
    - Tconst -> Constants
    - Tvar   -> type variables
    - Tarrow -> arrow type; function
    - Tlist  -> list

  """
  alias Terp.Error
  alias Terp.Types.Types
  alias Terp.Types.TypeEvaluator
  alias Terp.Types.TypeEnvironment

  defstruct [:constructor, :t, :vars, :type_constructor]

  @type t :: %Types{constructor: atom(), t: atom()}

  @spec bool() :: Types.t
  def bool() do
    %Types{constructor: :Tconst, t: :Bool}
  end

  @spec int() :: Types.t
  def int() do
    %Types{constructor: :Tconst, t: :Int}
  end

  @spec string() :: Types.t
  def string() do
    %Types{constructor: :Tconst, t: :String}
  end

  @spec function(Types.t, Types.t) :: Types.t
  def function(%Types{} = t1, %Types{} = t2) do
    %Types{
      constructor: :Tarrow,
      t: {t1, t2},
    }
  end

  @spec list(Types.t) :: Types.t
  def list(%Types{} = x) do
    %Types{constructor: :Tlist, t: x}
  end

  @spec tuple(Types.t, Types.t) :: Types.t
  def tuple(%Types{} = x, %Types{} = y) do
    %Types{constructor: :Ttuple, t: {x, y}}
  end

  @spec var(String.t | atom()) :: Types.t
  def var(x) do
    %Types{constructor: :Tvar, t: x}
  end

  @doc """
  Run the type evaluator for a given piece of source code.
  """
  @spec type_check(String.t) :: [Types.t]
  def type_check(src) do
    # TODO Should reduce this instead of map to just the :ok/type
    TypeEnvironment.start_if_unstarted()
    res = src
    |> Terp.to_ast()
    |> Enum.reduce({:ok, []},
    fn tree, {:ok, types} ->
      case TypeEvaluator.run_infer(tree) do
        {:error, _} = e ->
          e
        %Error{} = e ->
          e
        {:ok, type} ->
          {:ok, [type | types]}
      end
      (_tree, {:error, e}) -> {:error, e}
      (_tree, %Error{} = e) -> e
    end)
    case res do
      {:ok, types} -> {:ok, Enum.reverse(types)}
      error -> error
    end
  end

  @doc """
  Define a sum type.
  Expects the type name, a list of type variables (as atoms),
  and a list of tuples containing {type, type vars}.
  """
  def sum_type(name, type_vars, constructors) do
    ts = for {name, args} <- constructors do
      if Enum.all?(args, fn arg -> Enum.member?(type_vars, arg) end) do
        constructor = to_string(name)
        # This to_type won't work for HKTs
        %Types{constructor: constructor,
               t: Enum.map(args, &to_type/1),
               vars: args
        }
      else
        {:error, :invalid_type_var}
      end
    end
    %Types{type_constructor: name, t: ts, vars: type_vars}
  end

  def constructor_for_type(name) do
    type = TypeEnvironment.contents.type_defs
    |> Enum.find(fn {_k, v} ->
      Enum.member?(Enum.map(v.t, &(&1.constructor)), name)
    end)
    case type do
      nil ->
        {:error, {:type, :not_a_constructor}}
      {_n, t} ->
        {:ok, t}
    end
  end

  def value_constructor(name) do
    case (constructor_for_type(name)) do
      {:error, _e} = error->
        error
      {:ok, t} ->
        c = t.t
        |> Enum.find(&(to_string(&1.constructor) == name))
        case c do
          [] -> {:error, {:type, :no_matching_constructor}}
          type -> {:ok, type}
        end
    end
  end

  def to_type(%Types{} = x), do: x
  def to_type("Int"), do: int()
  def to_type("Bool"), do: bool()
  def to_type("String"), do: string()
  def to_type([constructor | vars]) do
    case TypeEnvironment.lookup_def(constructor) do
      {:error, e} ->
        {:error, e}
      {:ok, t} ->
        replace_type_vars({t, vars})
    end
  end
  def to_type(x), do: var(x)
  def to_type("List", x), do: list(to_type(x))
  def to_type("Tuple", x, y), do: tuple(to_type(x), to_type(y))
  def to_type("Arrow", x, y), do: function(to_type(x), to_type(y))
  def to_type(:__arrow, x, y), do: function(to_type(x), to_type(y))

  def replace_type_vars({type, new_vars}) do
    zipped = Enum.zip(type.vars, List.wrap(new_vars))
    updated_type = Enum.reduce(zipped, type, fn (var, type) -> replace_type_var(type, var) end)
    updated_type
  end
  def replace_type_var(%Types{constructor: :Tvar, t: t} = type, {old_var, new_var}) do
    if t == old_var, do: to_type(new_var), else: type
  end
  def replace_type_var(%Types{constructor: :Tlist, t: t} = type, vars) do
    subbed_t = replace_type_var(t, vars)
    %{type | t: subbed_t}
  end
  def replace_type_var(%Types{constructor: :Ttuple, t: {t1, t2}} = type, vars) do
    subbed_t1 = replace_type_var(t1, vars)
    subbed_t2 = replace_type_var(t2, vars)
    %{type | t: {subbed_t1, subbed_t2}}
  end
  def replace_type_var(%Types{constructor: :Tarrow, t: {t1, t2}} = type, vars) do
    subbed_t1 = replace_type_var(t1, vars)
    subbed_t2 = replace_type_var(t2, vars)
    %{type | t: {subbed_t1, subbed_t2}}
  end
  def replace_type_var(%Types{type_constructor: nil, t: ts} = type, vars) when is_list(ts) do
    updated = Enum.map(ts, &replace_type_var(&1, vars))
    updated_vars = Enum.map(type.vars, &(if &1 == elem(vars, 0), do: elem(vars, 1), else: &1))
    %{type | t: updated, vars: updated_vars}
  end
  def replace_type_var(%Types{type_constructor: _t, t: ts} = type, vars) when is_list(ts) do
    updated_data_constructors = Enum.map(ts, &replace_type_var(&1, vars))
    # TODO can only handle 1 type variable currently
    updated_vars = Enum.map(type.vars, &(if &1 == elem(vars, 0), do: elem(vars, 1), else: &1))
    %{type | t: updated_data_constructors, vars: updated_vars}
  end
  def replace_type_var(type, _vars), do: type

  defimpl String.Chars do
    def to_string(%Types{constructor: :Tconst, t: t}), do: Kernel.to_string(t)
    def to_string(%Types{constructor: :Tvar, t: t}), do: Kernel.to_string(t)
    def to_string(%Types{constructor: :Tlist, t: x}), do: "[#{Kernel.to_string(x)}]"
    def to_string(%Types{constructor: :Ttuple, t: {x, y}}), do: "{#{Kernel.to_string(x)}, #{Kernel.to_string(y)}}"
    def to_string(%Types{constructor: :Tarrow, t: {x, y}}), do: "(-> #{Kernel.to_string(x)} #{Kernel.to_string(y)})"
    def to_string(%Types{constructor: nil, type_constructor: t, vars: vars}) do
      var_string = vars
      |> Enum.map(&Kernel.to_string/1) |> Enum.join(" ")
      "[#{Kernel.to_string(t)} #{var_string}]"
    end
    def to_string(%Types{constructor: c, t: ts}) do
      var_string = ts
      |> Enum.map(&Kernel.to_string/1)
      |> Enum.join(" ")
      if var_string == "" do
        "[#{Kernel.to_string(c)}]"
      else
        "[#{Kernel.to_string(c)} #{var_string}]"
      end
    end
  end
end
