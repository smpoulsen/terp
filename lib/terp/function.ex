defmodule Terp.Function do
  @moduledoc """
  Function/anonymous function definition
  """

  def define([name | [args | [body | []]]], env) do
    Map.put(env, name, define_helper(args, body))
  end
  def define_helper([], body, bound_args) do
    apply(body, Enum.reverse(bound_args))
  end
  def define_helper([_arg | args], body, bound_args \\ []) do
    fn x -> define_helper(args, body, [x | bound_args] ) end
  end

  @doc """
  Defines an anonymous function.
  """
  def lambda([%RoseTree{node: x} | [body | []]], env) do
    fn arg ->
      Terp.eval_tree(body, fn y -> if x == y, do: arg, else: env.(y) end)
    end
  end

  def apply_fn(func, args) do
    apply(func, args)
  end
end
