defmodule Terp.IO do
  @doc """
  Read and evaluate a file containing terp code.
  """
  def run_terp(file) do
    with ".tp" = Path.extname(file),
         {:ok, src} <- File.read(file) do
      Terp.eval(src)
    end
  end
end
