defmodule Terp.Prelude.FunctorTest do
  use ExUnit.Case
  setup do
    {:ok, src} = File.read("prelude/functor.tp")
    {:ok, %{src: src}}
  end

  test "Prelude Functor - Sum", %{src: src} do
    assert src <> "(sum '(1 2 3))"
    |> Terp.eval == 6

    assert src <> "(sum '())"
    |> Terp.eval == 0
  end


  test "Prelude Functor - Length", %{src: src} do
    assert src <> "(length '(1 2 3 5))"
    |> Terp.eval == 4

    assert src <> "(length '())"
    |> Terp.eval == 0
  end
end
