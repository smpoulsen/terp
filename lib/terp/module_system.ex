defmodule Terp.ModuleSystem do

  def provide(args, env) do
  end

  def require_modules([], env), do: env
  def require_modules([filename | filenames], env) do
    case File.read(filename) do
      {:ok, module} ->
        {_res, environment} = Terp.load_code(module, env)
        require_modules(filenames, environment)
      {:error, :enoent} ->
        {:error, {:module_doesnt_exist, filename}}
    end
  end
end
