defmodule Terp.Mixfile do
  use Mix.Project

  def project do
    [
      app: :terp,
      version: "0.1.0",
      elixir: ">= 1.4.0",
      start_permanent: Mix.env == :prod,
      description: description(),
      package: package(),
      deps: deps(),
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  def description() do
    """
    An interpreter for a toy lisp implementation.
    """
  end

  defp deps do
    [
      {:bunt, "~> 0.2.0"},
      {:combine, "~> 0.9.6"},
      {:rose_tree, "~> 0.2.0"},
      {:credo, "~> 0.8.2", only: [:dev, :test]},
      {:dialyxir, "~> 0.5.0", only: [:dev, :test]},
      {:excheck, "~> 0.5.3", only: :test},
      {:triq, github: "triqng/triq", only: :test},
    ]
  end

  defp package do
    [
      name: :terp,
      licenses: ["BSD2"],
      maintainers: ["Travis Poulsen"],
      links: %{
        "GitHub" => "https://github.com/tpoulsen/terp",
      }
    ]
  end
end
