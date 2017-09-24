defmodule Terp.TypeSystem.TypeVars do
  @moduledoc """
  A generator for fresh type variables. Fresh variables are generated frequently
  as a part of type inference. Running the generator as its own genserver
  removes the need to pass local state through every function used in type
  inference.
  """
  use GenServer
  alias __MODULE__

  defstruct [var_index: 0]

  alias Terp.TypeSystem.Types

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
  Generates a fresh type variable.
  This is used when back substituting type variables after completing inference.
  """
  def finalize() do
    GenServer.call(:gen_type_vars, :finalize)
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
      "aa", "bb", "cc", "dd", "ee", "ff", "gg", "hh", "ii",
      "jj", "kk", "ll", "mm", "nn", "oo", "pp", "qq", "rr",
      "ss", "tt", "uu", "vv", "ww", "xx", "yy", "zz",
      "aaa", "bbb", "ccc", "ddd", "eee", "fff", "ggg", "hhh", "iii",
      "jjj", "kkk", "lll", "mmm", "nnn", "ooo", "ppp", "qqq", "rrr",
      "sss", "ttt", "uuu", "vvv", "www", "xxx", "yyy", "zzz",
      "aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "gggg", "hhhh", "iiii",
      "jjjj", "kkkk", "llll", "mmmm", "nnnn", "oooo", "pppp", "qqqq", "rrrr",
      "ssss", "tttt", "uuuu", "vvvv", "wwww", "xxxx", "yyyy", "zzzz",
    ])
    fresh_var = vars
    |> Enum.at(index)
    |> Types.var()
    {:reply, fresh_var, %{state | var_index: index + 1}}
  end

  @doc false
  def handle_call(:finalize, _from, %TypeVars{var_index: index} = state) do
    vars = Stream.cycle([
      "a", "b", "c", "d", "e", "f", "g", "h", "i",
      "j", "k", "l", "m", "n", "o", "p", "q", "r",
      "s", "t", "u", "v", "w", "x", "y", "z",
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
