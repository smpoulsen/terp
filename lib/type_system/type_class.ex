defmodule Terp.TypeSystem.TypeClass do
  @moduledoc """
  Type classes provide a mechanism for parametric polymorphism.

  (class [Num a]
    [(type + (-> a (-> a a)))
     (type - (-> a (-> a a)))
     (type * (-> a (-> a a)))
     (type negate (-> a a))
     (type abs (-> a a))
     (type signum (-> a a))
     (type fromInteger (-> Int a))])
  """
  alias Terp.AST
  alias Terp.Error
  alias Terp.TypeSystem.Annotation
  alias Terp.TypeSystem.Type
  alias Terp.TypeSystem.Environment

  @doc """
  Defines a new type class.

  TODO validate that vars in type definitions are present in the class definition
  """
  def define_class(name, types) do
    [class_name, vars] = name
    type_dict = for type <- types, into: %{} do
      [%{node: fn_name}, %{node: type_info}] = type.children
      t = apply(Type, :to_type, List.wrap(Annotation.extract_type_nodes(type_info)))
      {fn_name, %{t | classes: List.wrap({class_name, vars})}}
    end
    Environment.define_class(type_dict)
  end

  def define_instance(type_class, definitions) do
    [class_name, vars] = extract_instance_vars(type_class)
    instance_dict = definitions
    |> Enum.reduce(%{}, fn (defn, dict) ->
      case AST.fn_name(defn) do
        {:ok, fn_name} ->
          {:ok, _type} = Environment.lookup_class_defn(class_name, fn_name)
          # Updates the type with it's variables so that the concrete
          # vars can be subbed in.
          var_type = apply(Type, :to_type, List.wrap(vars))
          case var_type.constructor do
            nil ->
              Map.put(dict, fn_name, %{var_type.type_constructor => defn})
            constructor ->
              key = if constructor == :Tconst, do: var_type.t, else: constructor
              Map.put(dict, fn_name, %{key => defn})
          end
        _error ->
          dict
      end
    end)
    Environment.define_instance(class_name, instance_dict)
  end

  def extract_instance_vars(%{node: node}), do: extract_instance_vars(node)
  def extract_instance_vars(vars) when is_list(vars) do
    vars
    |> Enum.map(&extract_instance_vars/1)
  end
  def extract_instance_vars(var), do: var

  def implements_class?(class, type) do
    case _implements_class?(class, type) do
      true ->
        :ok
      false ->
        classes = class
        |> Enum.map(&elem(&1, 0))
        |> Enum.join(", ")
        %Error{kind: :type,
               type: :type_class,
               evaluating: type,
               message: "Type #{to_string(type)} does not implement #{classes}"}
    end
  end
  def _implements_class?(nil, _type), do: true
  def _implements_class?(class, %Type{constructor: :Tarrow, t: {x, y}}) do
    _implements_class?(class, x) || _implements_class?(class, y)
  end
  def _implements_class?(class, %Type{constructor: :Tconst, t: t}), do: check_all_classes(class, t)
  def _implements_class?(class, %Type{constructor: c, t: %Type{}}), do: check_all_classes(class, c)
  def _implements_class?(class, %Type{} = t), do: check_all_classes(class, t)
  def _implements_class?(_nil, _type), do: true
  defp check_all_classes(classes, t) do
    classes
    |> Enum.map(&elem(&1, 0))
    |> Enum.all?(&Environment.implements_class?(&1, t))
  end
end
