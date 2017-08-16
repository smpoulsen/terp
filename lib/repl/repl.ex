defmodule Terp.Repl do
  @moduledoc """
  A REPL (read-eval-print-loop) for terp.
  """
  alias RoseTree.Zipper
  alias Terp.Types.Types

  def init() do
    loop("", fn (z) -> {:error, {:unbound_variable, z}} end, init_history(RoseTree.new(:start)))
  end

  def loop(init_expr, environment, history) do
    if init_expr == "" do
      :io.put_chars(init_expr)
    end
    expr = IO.gets("terp> ")
    case expr do
      <<2>> ->
        # Ctl-B
        {previous, new_history} = scroll_back(history)
        loop(previous, environment, new_history)
      <<6>> ->
        # Ctl-F
        {next, new_history} = scroll_forward(history)
        loop(next, environment, new_history)
      :eof ->
        # Ctl-D
        IO.write("\nBye!")
      _ ->
        if String.starts_with?(expr, ":t ") do
          type_check(expr)
          loop("", environment, history)
        else
          updated_history = add_history(expr, history)
          env =  eval(expr, environment)
          loop("", env, updated_history)
        end
    end
  end

  defp type_check(expr) do
    trimmed = expr
    |> String.trim()
    |> String.trim_leading(":t ")

    type = trimmed
    |> Types.type_check()
    |> List.first() #TODO

    Bunt.puts([:blue, trimmed, :green, " : ", :yellow, type.str])
  end

  # Evaluate the expression and return the updated environment.
  defp eval(expr, environment) do
    {res, env} = Terp.evaluate_source(expr, environment)
    case res do
      {:error, {type, msg}} ->
        Bunt.puts([:red, "There was an error:"])
        IO.puts("\tmsg: #{Atom.to_string(type)}")
        IO.puts("\targ: #{msg}")
        environment
      {:ok, {type, msg}} ->
        Bunt.puts([:green, "Success!"])
        Bunt.puts([:blue, "#{msg}"])
        env
      nil ->
        Bunt.puts([:green, "ok"])
        env
      _ ->
        IO.inspect(res, charlists: :as_lists)
        env
    end
    env
  end

  ### History ###
  # History is implemented as a 1-ary rose tree.

  # Initialize with an empty tree
  defp init_history(item) do
    item
    |> RoseTree.new()
    |> Zipper.from_tree()
  end

  # Insert new item into history.
  defp add_history(new_item, history) do
    # use a zipper to track history.
    {:ok, updated} = history
    |> Zipper.insert_first_child(RoseTree.new(String.trim(new_item)))
    updated
  end

  # Scroll backwards through history. Wraps around if at the top.
  defp scroll_back(history) do
    with {:ok, previous} <- Zipper.ascend(history),
         tree <- Zipper.to_tree(previous) do
    {tree.node, history}
    else
      {:error, {:rose_tree, :no_parent}} ->
        last = history
        |> Zipper.to_leaf()
        |> Zipper.to_tree()
        {last.node, history}
    {:error, e} -> e
    end
  end

  # Scroll forwards through history. Wraps around if at the bottom.
  defp scroll_forward(history) do
    with {:ok, next} <- Zipper.first_child(history),
         tree <- Zipper.to_tree(next) do
      {tree.node, history}
    else
      {:error, {:rose_tree, :bad_path}} ->
        first = history
        |> Zipper.to_root()
        |> Zipper.to_tree()
      {first.node, history}
    {:error, e} -> e
    end
  end
end
