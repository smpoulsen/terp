defmodule Terp.IO do
  @moduledoc """
  Read and evaluate a file containing terp code.
  """
  alias Terp.Error
  alias Terp.Types.Types

  @doc """
  Given a filepath, reads the file and evaluates its contents.

  Valid terp files end in `.tp`.
  """
  def run_terp(file) do
    with true <- is_terp_file(file),
         {:ok, src} <- File.read(file),
         {:ok, _type} <- Types.type_check(src) do
      Terp.eval(src)
    else
      false ->
        "#{file} is not a valid terp file"
      %Error{} = error ->
        Error.pretty_print_error(error)
      {:error, _} = error ->
        Error.pretty_print_error(error)
    end
  end

  defp is_terp_file(filename) do
    ".tp" == Path.extname(filename)
  end
end
