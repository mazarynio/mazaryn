defmodule Sensor do

  def start do
    Axon.input({nil, 1})
    |> Axon.attach_hook(&IO.inspect/1)
    |> Axon.relu()
    |> Axon.attach_hook(&IO.inspect/1)
  end

  def start2 do
    input = Axon.input({nil, 784})
    model =
      input
      |> Axon.dense(128, activation: :relu)
      |> Axon.batch_norm()
      |> Axon.dropout(rate: 0.8)
      |> Axon.dense(64)
      |> Axon.tanh()
      |> Axon.dense(10)
      |> Axon.activation(:softmax)

      IO.inspect(model)
  end
end
