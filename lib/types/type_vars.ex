defmodule Terp.Types.TypeVars do
  @moduledoc """
  A generator for fresh type variables. Fresh variables are generated frequently
  as a part of type inference. Running the generator as its own genserver
  removes the need to pass local state through every function used in type
  inference.
  """
  use GenServer
  alias __MODULE__

  defstruct [var_index: 0]

  alias Terp.Types.Types

  # Client
  @doc """
  Starts the type vars server; it is named :gen_type_vars.
  """
  def start_link() do
    GenServer.start_link(__MODULE__, %TypeVars{}, name: :gen_type_vars)
  end

  @doc """
  Generates a fresh type variable.
  The current approach cycles the alphabet, but something more
  sophisticated could be used freely.
  """
  def fresh() do
    GenServer.call(:gen_type_vars, :fresh)
  end

  @doc """
  Resets the internal state of the type variable generator;
  fresh type vars will restart from the beginning.
  """
  def reset() do
    GenServer.cast(:gen_type_vars, :reset)
  end

  # server

  @doc false
  def handle_call(:fresh, _from, %TypeVars{var_index: index} = state) do
    vars = Stream.cycle([
      "a", "b", "c", "d", "e", "f", "g", "h", "i",
      "j", "k", "l", "m", "n", "o", "p", "q", "r",
      "s", "t", "u", "v", "w", "x", "y", "z"
    ])
    fresh_var = vars
    |> Enum.at(index)
    |> Types.var()
    {:reply, fresh_var, %{state | var_index: index + 1}}
  end

  @doc false
  def handle_cast(:reset, %TypeVars{} = state) do
    {:noreply, %{state | var_index: 0}}
  end
end
