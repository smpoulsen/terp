defmodule Terp.IO do
  @moduledoc """
  Read and evaluate a file containing terp code.
  """

  @doc """
  Given a filepath, reads the file and evaluates its contents.

  Valid terp files end in `.tp`.
  """
  def run_terp(file) do
    if is_terp_file(file) do
      {:ok, src} = File.read(file)
      src
      |> Terp.eval()
    else
      "#{file} is not a valid terp file"
    end
  end

  defp is_terp_file(filename) do
    ".tp" == Path.extname(filename)
  end
end
