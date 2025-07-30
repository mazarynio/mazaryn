defmodule Mazaryn.Login.FormTest do
  use ExUnit.Case, async: true
  alias Mazaryn.Login.Form
  alias Account.User

  describe "changeset/2 validations" do
    test "invalid with missing fields" do
      changeset = Form.changeset(%Form{}, %{})
      refute changeset.valid?
      assert %{email: ["Email address is required"], password: ["Password is required"]} = errors_on(changeset)
    end

    test "invalid email format" do
      changeset = Form.changeset(%Form{}, %{email: "invalid_email", password: "password123"})
      refute changeset.valid?
      assert %{email: ["has invalid format"]} = errors_on(changeset)
    end

    test "password too short" do
      changeset = Form.changeset(%Form{}, %{email: "test@example.com", password: "short"})
      refute changeset.valid?
      assert %{password: ["Password must be between 8 and 20 characters"]} = errors_on(changeset)
    end

    test "valid input" do
      changeset = Form.changeset(%Form{}, %{email: "user@example.com", password: "validPass123"})
      assert changeset.valid?
    end
  end

  defp errors_on(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {msg, opts} ->
      Regex.replace(~r"%{(\w+)}", msg, fn _, key ->
        opts |> Keyword.get(String.to_existing_atom(key), key) |> to_string()
      end)
    end)
  end
end
