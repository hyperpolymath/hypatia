defmodule HypatiaTest do
  use ExUnit.Case
  doctest Hypatia

  test "greets the world" do
    assert Hypatia.hello() == :world
  end
end
