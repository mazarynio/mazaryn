defmodule Mazaryn.DataAbstractor do
  def get_item_by_id(items, id) do
    Enum.find(items, fn item -> item.id == id end)
  end

  def filter_item_by_id(items, id) do
    Enum.filter(items, fn item -> item.id !== id end)
  end

  def update_item(items, new_value) do
    Enum.map(items, fn item ->
      if item.id == new_value.id do
        result =
          Map.merge(item, new_value, fn _k, _v1, v2 ->
            v2
          end)

        result
      else
        item
      end
    end)
  end

  def update_uuid(items) do
    Enum.map(items, fn item ->
      id = convert_uuid(item.id)

      %{
        item
        | id: id
      }
    end)
  end

  defp convert_uuid(id) do
    {:ok, id} = Ecto.UUID.load(id)
    String.replace_suffix(id, ">", "")
  end
end
