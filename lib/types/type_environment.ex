defmodule Terp.Types.TypeEnvironment do
  @moduledoc """
  """
  use GenServer
  alias __MODULE__
  alias RoseTree.Zipper
  alias Terp.Types.TypeVars

  defstruct [inferred_types: %{}, annotated_types: %{}, type_defs: %{}]

  alias Terp.Types.TypeEvaluator

  # Client
  @doc """
  Starts the type vars server; it is named :type_env.
  """
  def start_link() do
    GenServer.start_link(__MODULE__, %TypeEnvironment{}, name: :type_env)
  end

  @doc """
  If the type environment hasn't been started, it starts.
  If it has, nothing happens.
  """
  def start_if_unstarted() do
    case start_link() do
      {:ok, _} -> :ok
      {:error, _} = error -> :ok
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

  # server

  @doc false
  def handle_call({:lookup, name}, _from, %TypeEnvironment{inferred_types: types} = state) do
    res = case Map.get(types, name) do
            nil ->
              {:ok, {%{}, TypeVars.fresh()}}
            x ->
              {:ok, {%{}, TypeEvaluator.instantiate(TypeEvaluator.generalize(%{}, x))}}
          end
    {:reply, res, state}
  end

  @doc false
  def handle_call(:contents, _from, %TypeEnvironment{} = state) do
    {:reply, state, state}
  end

  @doc false
  def handle_call({:lookup_def, name}, _from, %TypeEnvironment{type_defs: types} = state) do
    res = case Map.get(types, name) do
            nil ->
              {:error, {:type_def, "Data constructor not in scope: #{name}"}}
            x ->
              {:ok, x}
          end
    {:reply, res, state}
  end

  @doc false
  def handle_call({:lookup_annotation, fn_name}, _from, %TypeEnvironment{inferred_types: types} = state) do
    res = case Map.get(types, fn_name) do
            nil ->
              {:error, {:type_annotation, "No annotation for: #{fn_name}"}}
            x ->
              {:ok, x}
          end
    {:reply, res, state}
  end

  @doc false
  def handle_cast({:extend, name, scheme}, %TypeEnvironment{inferred_types: types} = state) do
    updated_types = Map.put_new(types, name, scheme)
    {:noreply, %{state | inferred_types: updated_types}}
  end

  def handle_cast({:restrict, name}, %TypeEnvironment{inferred_types: types} = state) do
    updated_types = Map.drop(types, name)
    {:noreply, %{state | inferred_types: updated_types}}
  end

  @doc false
  def handle_cast(:reset, %TypeEnvironment{} = _state) do
    {:noreply, %TypeEnvironment{}}
  end

  # User defined types
  @doc false
  def handle_cast({:define, name, type}, %TypeEnvironment{type_defs: types} = state) do
    updated_types = Map.put_new(types, name, type)
    {:noreply, %{state | type_defs: updated_types}}
  end

  # Type annotations
  @doc false
  def handle_cast({:annotate, fn_name, type}, %TypeEnvironment{inferred_types: types} = state) do
    updated_types = Map.put_new(types, fn_name, type)
    {:noreply, %{state | inferred_types: updated_types}}
  end
end
