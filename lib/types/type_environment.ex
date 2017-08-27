defmodule Terp.Types.TypeEnvironment do
  @moduledoc """
  """
  use GenServer
  alias __MODULE__
  alias RoseTree.Zipper

  defstruct [types: %{}]

  alias Terp.Types.TypeEvaluator

  # Client
  @doc """
  Starts the type vars server; it is named :type_env.
  """
  def start_link() do
    GenServer.start_link(__MODULE__, %TypeEnvironment{}, name: :type_env)
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
  def handle_call({:lookup, name}, _from, %TypeEnvironment{types: types} = state) do
    res = case Map.get(types, name) do
            nil ->
              {:error, {:unbound, name}}
            x ->
              {:ok, {%{}, TypeEvaluator.instantiate(x)}}
          end
    {:reply, res, state}
  end

  @doc false
  def handle_call(:contents, _from, %TypeEnvironment{types: types} = state) do
    {:reply, types, state}
  end

  @doc false
  def handle_cast({:extend, name, scheme}, %TypeEnvironment{types: types} = state) do
    updated_types = Map.put_new(types, name, scheme)
    {:noreply, %{state | types: updated_types}}
  end

  def handle_cast({:restrict, name}, %TypeEnvironment{types: types} = state) do
    updated_types = Map.drop(types, name)
    {:noreply, %{state | types: updated_types}}
  end

  @doc false
  def handle_cast(:reset, %TypeEnvironment{} = _state) do
    {:noreply, %TypeEnvironment{}}
  end
end
