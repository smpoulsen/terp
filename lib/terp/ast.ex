defmodule Terp.AST do
  @moduledoc """
  Interface for working with the Terp.AST.
  """
  alias RoseTree.Zipper
  alias Terp.Parser

  @doc """
  Parse source code and convert it to an ast.
  """
  @spec from_src(String.t) :: [RoseTree.t]
  def from_src(str) do
    str
    |> Parser.parse()
    |> Enum.flat_map(&to_tree/1)
    |> filter_nodes(:__comment)
  end

  @doc """
  `to_tree/1` takes a tokenized expression and builds a parse tree.

  ## Examples

      iex> [:__apply, [:+, 1, 2, 3]]
      ...> |> Terp.AST.to_tree()
      [
      %RoseTree{node: :__apply, children: []},
        [
          %RoseTree{node: :+, children: []},
          %RoseTree{node: 1, children: []},
          %RoseTree{node: 2, children: []},
          %RoseTree{node: 3, children: []},
        ]
      ]

      iex> [:+, 1, 2, [:*, 2, 3]]
      ...> |> Terp.AST.to_tree()
      [
        %RoseTree{node: :+, children: []},
        %RoseTree{node: 1, children: []},
        %RoseTree{node: 2, children: []},
        [
          %RoseTree{node: :*, children: []},
          %RoseTree{node: 2, children: []},
          %RoseTree{node: 3, children: []},
        ]
      ]
  """
  def to_tree([]), do: []
  def to_tree(expr) when is_list(expr) do
    for v <- expr do
      case v do
        {s, x} when is_atom(s) ->
          RoseTree.new(s, to_tree(x))
        val when is_list(val) ->
          to_tree(val)
        _ -> RoseTree.new(v)
      end
    end
  end
  def to_tree(x), do: RoseTree.new(x)

  @doc """
  Converts an AST into a string that (should) represent the function
  encapsulated by the AST.

  NOTE: This does not currently re-sugar desugared expressions (e.g. defn and defrec),
  so the resulting string is that of the desugared expression.

  ## Examples

      iex> "(+ 1 2 3 (* 2 3))"
      ...> |> Terp.Parser.parse()
      ...> |> Terp.AST.to_tree()
      ...> |> Terp.AST.stringify()
      "(+ 1 2 3 (* 2 3))"

      iex> "(data (Maybe a) [Just a] [Nothing])"
      ...> |> Terp.Parser.parse()
      ...> |> Terp.AST.to_tree()
      ...> |> Terp.AST.stringify()
      "(data (Maybe a) [Just a] [Nothing])"

      iex> "(defn plusFive (x) (+ x 5))"
      ...> |> Terp.Parser.parse()
      ...> |> Terp.AST.to_tree()
      ...> |> Terp.AST.stringify()
      "(let plusFive (lambda (x) (+ x 5)))"
  """
  @spec stringify(RoseTree.t | [RoseTree.t]) :: String.t
  def stringify(%RoseTree{node: node, children: children}) do
    case node do
      :__apply ->
        "(#{stringify(children)})"
      :__data ->
        [type_constructor | [value_constructors | []]] = children
        type_string = stringify(type_constructor)

        value_strings = value_constructors.node
        |> Enum.reduce("", fn (constructor, acc) ->
          s = constructor
          |> Enum.map(&stringify/1)
          |> Enum.join(" ")
          if acc == "", do: "[#{s}]", else: "#{acc} [#{s}]"
          end)
        "(data #{type_string} #{value_strings})"
      :__match ->
        "(match #{stringify(children)})"
      :__string ->
        raw_string = children
        |> List.wrap()
        |> Enum.map(&stringify/1)
        |> Enum.join()
        "\"#{raw_string}\""
      :__quote ->
        raw_string = children
        |> Enum.map(&stringify/1)
        |> Enum.join(", ")
        "'(#{raw_string})"
      x when is_list(x) ->
        res = x
        |> Enum.map(&stringify/1)
        |> Enum.join(" ")
        "(#{res})"
      x ->
        with true <- is_atom(x),
             s <- Atom.to_string(x),
             true <- String.starts_with?(s, "__") do
          String.trim(s, "__")
        else
          %RoseTree{node: n, children: cs} ->
            res = cs
            |> Enum.map(&stringify/1)
            |> Enum.join(" ")
            "(#{stringify(n)} #{res})"
          _ ->
            Kernel.to_string(x)
        end
    end
  end
  def stringify(trees) when is_list(trees) do
    trees
    |> Enum.map(&stringify/1)
    |> Enum.join(" ")
  end

  @doc """
  Filters nodes out of the AST.
  """
  def filter_nodes(trees, node_name) do
    Enum.reject(trees, fn %RoseTree{node: node} -> node == node_name end)
  end

  @spec fn_name(RoseTree.t) :: {:ok, String.t} | {:error, :no_fn_name}
  def fn_name(expr) do
    z = Zipper.from_tree(expr)
    with {:ok, {%RoseTree{node: t}, _history}} = expr_type <- Zipper.first_child(z),
         true <- Enum.member?([:__let, :__letrec], t),
         {:ok, {%RoseTree{node: name}, _history}} <- Zipper.lift(expr_type, &Zipper.next_sibling/1) do
      {:ok, name}
    else
      _ ->
        {:error, :no_fn_name}
    end
  end
end
