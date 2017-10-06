defmodule Terp.TypeSystem.Type do
  @moduledoc """
  Type

  Constructors:
    - Tconst -> Constants
    - Tvar   -> type variables
    - Tarrow -> arrow type; function
    - Tlist  -> list

  """
  alias __MODULE__
  alias Terp.TypeSystem.Environment

  defstruct [:constructor, :t, :vars, :type_constructor, :classes]

  @type t :: %__MODULE__{}

  @spec bool() :: Type.t
  def bool() do
    %Type{constructor: :Tconst, t: :Bool}
  end

  @spec int() :: Type.t
  def int() do
    %Type{constructor: :Tconst, t: :Int}
  end

  @spec float() :: Type.t
  def float() do
    %Type{constructor: :Tconst, t: :Float}
  end

  @spec string() :: Type.t
  def string() do
    %Type{constructor: :Tconst, t: :String}
  end

  @spec function(Type.t, Type.t) :: Type.t
  def function(%Type{} = t1, %Type{} = t2) do
    %Type{
      constructor: :Tarrow,
      t: {t1, t2},
    }
  end

  @spec list(Type.t) :: Type.t
  def list(%Type{} = x) do
    %Type{constructor: :Tlist, t: x}
  end

  @spec tuple(Type.t, Type.t) :: Type.t
  def tuple(%Type{} = x, %Type{} = y) do
    %Type{constructor: :Ttuple, t: {x, y}}
  end

  @spec var(String.t | atom()) :: Type.t
  def var(x) do
    %Type{constructor: :Tvar, t: x}
  end

  @doc """
  Define a sum type.
  Expects the type name, a list of type variables (as atoms),
  and a list of tuples containing {type, type vars}.
  """
  @spec higher_kinded(String.t, [String.t], [{String.t, [String.t]}]) :: Type.t
  def higher_kinded(name, type_vars, constructors) do
    ts = for {name, args} <- constructors do
      constructor = to_string(name)
      # This to_type won't work for HKTs
      types = Enum.map(args, &to_type/1)
      all_args_in_type_vars = types
      |> Enum.all?(fn type ->
        Enum.member?(type_vars, type.t) || type.constructor !== :Tvar
      end)

      if all_args_in_type_vars do
        %Type{constructor: constructor,
               t: types,
               vars: args}
      else
        {:error, :invalid_type_var}
      end
    end
    %Type{type_constructor: name, t: ts, vars: type_vars}
  end

  def constructor_for_type(name) do
    type = Environment.contents.type_defs
    |> Enum.find(fn {_k, v} ->
      Enum.member?(Enum.map(Map.get(v, :t, []), &(&1.constructor)), name)
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

  # to_type/1
  def to_type(%Type{} = x), do: x
  def to_type("Int"), do: int()
  def to_type("Float"), do: float()
  def to_type("Bool"), do: bool()
  def to_type("String"), do: string()
  def to_type(x), do: var(x)

  # to_type/2
  def to_type("List", x), do: list(to_type(x))
  def to_type(constructor, vars) do
    case Environment.lookup_def(constructor) do
      {:ok, t} ->
        replace_type_vars({t, vars})
      _ ->
        vars_list = List.wrap(vars)
        higher_kinded(constructor, vars_list, [{var(constructor), vars_list}])
    end
  end

  # to_type/3
  def to_type("Tuple", x, y), do: tuple(to_type(x), to_type(y))
  def to_type("Arrow", x, y), do: function(to_type(x), to_type(y))
  def to_type(:__arrow, x, y) do
    left = if is_list(x) do
      apply(Type, :to_type, x)
    else
      to_type(x)
    end
    right = if is_list(y) do
      apply(Type, :to_type, y)
    else
      to_type(y)
    end
    function(left, right)
  end

  def replace_type_vars({type, new_vars}) do
    zipped = Enum.zip(type.vars, List.wrap(new_vars))
    updated_type = Enum.reduce(zipped, type, fn (var, type) -> replace_type_var(type, var) end)
    updated_type
  end
  def replace_type_var(%Type{constructor: :Tvar, t: t} = type, {old_var, new_var}) do
    if t == old_var, do: to_type(new_var), else: type
  end
  def replace_type_var(%Type{constructor: :Tlist, t: t} = type, vars) do
    subbed_t = replace_type_var(t, vars)
    %{type | t: subbed_t}
  end
  def replace_type_var(%Type{constructor: :Ttuple, t: {t1, t2}} = type, vars) do
    subbed_t1 = replace_type_var(t1, vars)
    subbed_t2 = replace_type_var(t2, vars)
    %{type | t: {subbed_t1, subbed_t2}}
  end
  def replace_type_var(%Type{constructor: :Tarrow, t: {t1, t2}} = type, vars) do
    subbed_t1 = replace_type_var(t1, vars)
    subbed_t2 = replace_type_var(t2, vars)
    %{type | t: {subbed_t1, subbed_t2}}
  end
  def replace_type_var(%Type{type_constructor: nil, t: ts} = type, vars) when is_list(ts) do
    updated = Enum.map(ts, &replace_type_var(&1, vars))
    updated_vars = Enum.map(type.vars, &(if &1 == elem(vars, 0), do: elem(vars, 1), else: &1))
    %{type | t: updated, vars: updated_vars}
  end
  def replace_type_var(%Type{type_constructor: _t, t: ts} = type, vars) when is_list(ts) do
    updated_data_constructors = Enum.map(ts, &replace_type_var(&1, vars))
    # TODO can only handle 1 type variable currently
    updated_vars = Enum.map(type.vars, &(if &1 == elem(vars, 0), do: elem(vars, 1), else: &1))
    %{type | t: updated_data_constructors, vars: updated_vars}
  end
  def replace_type_var(type, _vars), do: type

  defimpl String.Chars do
    def to_string(%Type{constructor: :Tconst, t: t}), do: Kernel.to_string(t)
    def to_string(%Type{constructor: :Tvar, t: t}), do: Kernel.to_string(t)
    def to_string(%Type{constructor: :Tlist, t: x}), do: "[#{Kernel.to_string(x)}]"
    def to_string(%Type{constructor: :Ttuple, t: {x, y}}), do: "{#{Kernel.to_string(x)}, #{Kernel.to_string(y)}}"
    def to_string(%Type{constructor: :Tarrow, t: {x, y}}) do
      if x.constructor == :Tarrow do
          "(#{Kernel.to_string(x)}) -> #{Kernel.to_string(y)}"
      else
          "#{Kernel.to_string(x)} -> #{Kernel.to_string(y)}"
      end
    end
    def to_string(%Type{constructor: nil, type_constructor: t, vars: vars}) do
      var_string = vars
      |> Enum.map(&Kernel.to_string/1)
      |> Enum.join(" ")
      "[#{Enum.join([Kernel.to_string(t), var_string], " ")}]"
    end
    def to_string(%Type{constructor: c, t: ts}) do
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
