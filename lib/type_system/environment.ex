defmodule Terp.TypeSystem.Environment do
  @moduledoc """
  """
  use GenServer
  alias __MODULE__
  alias RoseTree.Zipper
  alias Terp.Error
  alias Terp.TypeSystem.Type
  alias Terp.TypeSystem.TypeVars
  alias Terp.TypeSystem.Evaluator

  defstruct [inferred_types: %{},
             type_classes: %{},
             type_instances: %{},
             type_defs: %{}]

  # Client
  @doc """
  Starts the type vars server; it is named :type_env.
  """
  def start_link() do
    GenServer.start_link(__MODULE__, %Environment{}, name: :type_env)
  end

  @doc """
  If the type environment hasn't been started, it starts.
  If it has, nothing happens.
  """
  def start_if_unstarted() do
    case start_link() do
      {:ok, _} -> :ok
      {:error, _} = _error -> :ok
    end
  end

  @doc """
  """
  def extend(name, scheme) do
    GenServer.cast(:type_env, {:extend, name, scheme})
  end

  def lookup(name) do
    GenServer.call(:type_env, {:lookup, name})
  end

  def restrict(name) do
    GenServer.cast(:type_env, {:restrict, name})
  end

  @doc """
  """
  def contents() do
    GenServer.call(:type_env, :contents)
  end

  @doc """
  """
  def reset() do
    GenServer.cast(:type_env, :reset)
  end

  # User defined types
  def define_type(name, type) do
    GenServer.cast(:type_env, {:define, name, type})
  end

  def lookup_def(name) do
    GenServer.call(:type_env, {:lookup_def, name})
  end

  ## Type Annotations
  def annotate(fn_name, type) do
    GenServer.cast(:type_env, {:annotate, fn_name, type})
  end

  def lookup_annotation(fn_name) do
    GenServer.call(:type_env, {:lookup_annotation, fn_name})
  end

  def extend_environment(expr, type_scheme) do
    z = Zipper.from_tree(expr)
    with {:ok, {%RoseTree{node: t}, _history}} = expr_type <- Zipper.first_child(z),
         true <- Enum.member?([:__let, :__letrec], t),
         {:ok, {%RoseTree{node: name}, _history}} <- Zipper.lift(expr_type, &Zipper.next_sibling/1) do
      extend(name, type_scheme)
      :ok
    else
      # Swallow errors and don't extend the environment.
      {:error, _e} ->
        :ok
      false ->
        :ok
    end
  end

  ## Type Classes
  def define_class(type_dict) do
    GenServer.cast(:type_env, {:define_class, type_dict})
  end

  def lookup_class_defn(class_name, fn_name) do
    GenServer.call(:type_env, {:lookup_class_defn, class_name, fn_name})
  end

  def define_instance(name, type_dict) do
    GenServer.cast(:type_env, {:define_instance, name, type_dict})
  end

  def lookup_instance_defn(fn_name) do
    GenServer.call(:type_env, {:lookup_instance_defn, fn_name})
  end

  def implements_class?(class, type) do
    GenServer.call(:type_env, {:implements_class, class, type})
  end

  # server

  @doc false
  def handle_call({:lookup, name}, _from, %Environment{inferred_types: types} = state) do
    res = case Map.get(types, name) do
            nil ->
              {:ok, {%{}, TypeVars.fresh()}}
            x ->
              {:ok, {%{}, Evaluator.instantiate(Evaluator.generalize(%{}, x))}}
          end
    {:reply, res, state}
  end

  @doc false
  def handle_call(:contents, _from, %Environment{} = state) do
    {:reply, state, state}
  end

  @doc false
  def handle_call({:lookup_def, name}, _from, %Environment{type_defs: types} = state) do
    res = case Map.get(types, name) do
            nil ->
              {:error, {:type_def, "Data constructor not in scope: #{name}"}}
            x ->
              {:ok, x}
          end
    {:reply, res, state}
  end

  @doc false
  def handle_call({:lookup_annotation, fn_name}, _from, %Environment{inferred_types: types} = state) do
    res = case Map.get(types, fn_name) do
            nil ->
              {:error, {:type_annotation, "No annotation for: #{fn_name}"}}
            x ->
              {:ok, x}
          end
    {:reply, res, state}
  end

  @doc false
  def handle_call({:lookup_class_defn, class_name, fn_name}, _from, %Environment{inferred_types: inferred_types} = state) do
    res = case Map.get(inferred_types, fn_name, :no_fn) do
            :no_fn ->
              %Error{kind: :type,
                     type: :class_definition,
                     message: "Type class #{class_name} does not define #{fn_name}"}
            %Type{classes: classes} = type ->
              belongs_to_class? = classes
              |> Enum.map(&elem(&1, 0))
              |> Enum.member?(class_name)
              if belongs_to_class? do
                {:ok, type}
              else
                  %Error{kind: :type,
                         type: :class_definition,
                         message: "Type class #{class_name} does not define #{fn_name}"}
              end
          end
    {:reply, res, state}
  end

  @doc false
  def handle_call({:lookup_instance_defn, fn_name}, _from, %Environment{type_instances: type_instances} = state) do
    res = case Map.get(type_instances, fn_name, :no_fn) do
      :no_fn ->
        %Error{kind: :type,
               type: :instance_definition,
               message: "#{fn_name} is not defined"}
      type ->
        {:ok, type}
    end
    {:reply, res, state}
  end

  @doc false
  def handle_call({:implements_class, nil, _type}, _from, state), do: {:reply, true, state}
  def handle_call({:implements_class, class, type}, _from, %Environment{type_classes: type_classes} = state) do
    res = case Map.get(type_classes, class) do
            nil ->
              %Error{kind: :type,
                     type: :instance_definition,
                     message: "Type class #{class} is not defined"}
            classes ->
              with false <- MapSet.member?(classes, type),
                   false <- is_map(type) && Map.get(type, :constructor) === :Tvar,
                   false <- _hkt_implements_class(classes, type) do
                false
              else
                true ->
                  true
              end
          end
    {:reply, res, state}
  end

  def _hkt_implements_class(classSet, %Type{constructor: c, t: %Type{}}) do
    classSet
    |> Enum.member?(c)
  end
  def _hkt_implements_class(classSet, %Type{type_constructor: c}) do
    classSet
    |> Enum.member?(c)
  end
  def _hkt_implements_class(_classSet, _type), do: false

  @doc false
  def handle_cast({:extend, name, scheme}, %Environment{inferred_types: types} = state) do
    updated_types = Map.put_new(types, name, scheme)
    {:noreply, %{state | inferred_types: updated_types}}
  end

  def handle_cast({:restrict, name}, %Environment{inferred_types: types} = state) do
    updated_types = Map.drop(types, name)
    {:noreply, %{state | inferred_types: updated_types}}
  end

  @doc false
  def handle_cast(:reset, %Environment{} = _state) do
    {:noreply, %Environment{}}
  end

  # User defined types
  @doc false
  def handle_cast({:define, name, type}, %Environment{type_defs: types} = state) do
    updated_types = Map.put_new(types, name, type)
    {:noreply, %{state | type_defs: updated_types}}
  end

  # Type annotations
  @doc false
  def handle_cast({:annotate, fn_name, type}, %Environment{inferred_types: types} = state) do
    updated_types = Map.put_new(types, fn_name, type)
    {:noreply, %{state | inferred_types: updated_types}}
  end

  # Type Classes
  @doc false
  def handle_cast({:define_class, type_dict}, %Environment{inferred_types: inferred_types} = state) do
    updated_inferred_types = Map.merge(inferred_types, type_dict)
    {:noreply, %{state | inferred_types: updated_inferred_types}}
  end

  @doc false
  def handle_cast({:define_instance, name, type_dict}, %Environment{type_classes: types, type_instances: type_instances} = state) do
    updated_type_instances = Map.merge(type_instances, type_dict, fn (_k, v1, v2) ->
      Map.merge(v1, v2)
    end)
    updated_classes = if Map.has_key?(types, name) do
      new_instances = type_dict
      |> Enum.map(fn {_k, v} -> Map.keys(v) end)
      |> List.flatten()
      |> MapSet.new
      MapSet.union(new_instances, Map.get(types, name))
    else
      type_dict
      |> Enum.map(fn {_k, v} -> Map.keys(v) end)
      |> List.flatten()
      |> MapSet.new
    end
    {:noreply, %{state | type_instances: updated_type_instances, type_classes: Map.put(types, name, updated_classes)}}
  end
end
