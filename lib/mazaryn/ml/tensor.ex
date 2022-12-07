defmodule ML.Tensor do

  def tens(list) do
    Nx.tensor(list)
  end

  def f16(list) do
    Nx.tensor(list, type: :f16)
  end

  def f32(list) do
    Nx.tensor(list, type: :f32)
  end

  def f64(list) do
    Nx.tensor(list, type: :f64)
  end

  def add(a, b) do
    x = Nx.tensor(a)
    y = Nx.tensor(b)
    z = Nx.add(x, y)
  end

  def multiply(list, b) do
    x = Nx.tensor(list)
    y = Nx.multiply(x, b)
  end

  def asin(list) do
    x = Nx.tensor(list)
    y = Nx.asin(x)
  end

  def asinh(list) do
    x = Nx.tensor(list)
    y = Nx.asinh(x)
  end

  def atan(list) do
    x = Nx.tensor(list)
    y = Nx.atan(x)
  end
end
