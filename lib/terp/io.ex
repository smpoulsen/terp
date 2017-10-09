defmodule Terp.IO do
  @moduledoc """
  Read and evaluate a file containing terp code.
  """
  alias Terp.Error
  alias Terp.TypeSystem

  @doc """
  Given a filepath, reads the file and evaluates its contents.

  Valid terp files end in `.tp`.
  """
  def run_terp(file) do
    TypeSystem.start_environment()
    with true <- is_terp_file(file),
         {:ok, src} <- File.read(file),
         {:ok, prelude} <- File.read("prelude/prelude.tp"),
         {_, _environment} <- Terp.eval_source(prelude),
         {:ok, _type} <- TypeSystem.check_src(src) do
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

  @doc """
  Checks that the file extension is valid for terp, e.g. '.tp'.
  """
  def is_terp_file(filename) do
    ".tp" == Path.extname(filename)
  end
end
