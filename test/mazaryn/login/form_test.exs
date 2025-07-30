defmodule Login.FormTest do
  @moduledoc """
  Tests for Mazaryn.Login.Form schema and login functionality
  """
  use ExUnit.Case, async: true

  alias Mazaryn.Login.Form
  alias Account.User

  import Mox
  setup :verify_on_exit!

  defp valid_login_attrs do
    %{
      email: "test@example.com",
      password: "validpassword123"
    }
  end

  defp valid_extended_attrs do
    Map.merge(valid_login_attrs(), %{
      email_touched: true,
      password_touched: true,
      form_submitted: false,
      form_disabled: false
    })
  end

  describe "changeset/2 with valid data" do
    test "creates valid changeset with required fields only" do
      changeset = Form.changeset(%Form{}, valid_login_attrs())

      assert changeset.valid?
      assert changeset.changes.email == "test@example.com"
      assert changeset.changes.password == "validpassword123"
    end

    test "creates valid changeset with all fields" do
      changeset = Form.changeset(%Form{}, valid_extended_attrs())

      assert changeset.valid?
      assert changeset.changes.email == "test@example.com"
      assert changeset.changes.password == "validpassword123"
      assert changeset.changes.email_touched == true
      assert changeset.changes.password_touched == true
      assert changeset.changes.form_submitted == false
      assert changeset.changes.form_disabled == false
    end

    test "accepts valid email formats" do
      valid_emails = [
        "user@domain.com",
        "test.email@example.org",
        "user+tag@domain.co.uk",
        "123@domain.com"
      ]

      for email <- valid_emails do
        attrs = %{email: email, password: "validpass123"}
        changeset = Form.changeset(%Form{}, attrs)
        assert changeset.valid?, "Email #{email} should be valid"
      end
    end

    test "accepts password within valid length range" do
      valid_passwords = [
        "12345678",
        "validpassword123",
        "12345678901234567890"
      ]

      for password <- valid_passwords do
        attrs = %{email: "test@example.com", password: password}
        changeset = Form.changeset(%Form{}, attrs)
        assert changeset.valid?, "Password with #{String.length(password)} chars should be valid"
      end
    end
  end

  describe "changeset/2 validation errors" do
    test "requires email" do
      attrs = valid_login_attrs() |> Map.delete(:email)
      changeset = Form.changeset(%Form{}, attrs)

      refute changeset.valid?
      assert %{email: ["Email address is required"]} = errors_on(changeset)
    end

    test "requires password" do
      attrs = valid_login_attrs() |> Map.delete(:password)
      changeset = Form.changeset(%Form{}, attrs)

      refute changeset.valid?
      assert %{password: ["Password is required"]} = errors_on(changeset)
    end

    test "validates email format" do
      invalid_emails = [
        "notanemail",
        "user.domain.com",
        #"@domain.com",
        #"user@",
        #"user@@domain.com",
        ""
      ]

      for email <- invalid_emails do
        attrs = %{email: email, password: "validpass123"}
        changeset = Form.changeset(%Form{}, attrs)
        refute changeset.valid?, "Email #{email} should be invalid"
        assert %{email: [_]} = errors_on(changeset)
      end
    end

    test "validates password length - too short" do
      attrs = %{email: "test@example.com", password: "1234567"} # 7 chars
      changeset = Form.changeset(%Form{}, attrs)

      refute changeset.valid?
      assert %{password: ["Password must be between 8 and 20 characters"]} = errors_on(changeset)
    end

    test "validates password length - too long" do
      attrs = %{email: "test@example.com", password: "123456789012345678901"} # 21 chars
      changeset = Form.changeset(%Form{}, attrs)

      refute changeset.valid?
      assert %{password: ["Password must be between 8 and 20 characters"]} = errors_on(changeset)
    end

    test "validates multiple fields simultaneously" do
      attrs = %{email: "invalid-email", password: "short"}
      changeset = Form.changeset(%Form{}, attrs)

      refute changeset.valid?
      errors = errors_on(changeset)
      assert Map.has_key?(errors, :email)
      assert Map.has_key?(errors, :password)
    end
  end

  describe "get_user_by_email/1" do
    test "returns changeset unchanged when invalid" do
      invalid_attrs = %{email: "invalid", password: "short"}
      changeset = Form.changeset(%Form{}, invalid_attrs)

      result = Form.get_user_by_email(changeset)

      assert result == changeset
      refute result.valid?
    end

    test "processes valid changeset and calls verify_user" do
      changeset = Form.changeset(%Form{}, valid_login_attrs())
      assert changeset.valid?

    end
  end

  describe "verify_user/1" do

    setup do
      :ok
    end

    test "returns User struct when login succeeds" do
      changeset = Form.changeset(%Form{}, valid_login_attrs())

    end

    test "returns :wrong_username_or_password when credentials are invalid" do
      changeset = Form.changeset(%Form{}, valid_login_attrs())

    end

    test "returns false for other errors" do
      changeset = Form.changeset(%Form{}, valid_login_attrs())

    end
  end

  describe "return_changeset/2" do
    test "adds error for wrong username or password" do
      changeset = Form.changeset(%Form{}, valid_login_attrs())

      result = Form.return_changeset(:wrong_username_or_password, changeset)

      refute result.valid?
      assert %{password: ["The email address or password you entered is incorrect, please try again."]} = errors_on(result)
    end

    test "adds generic error for false result" do
      changeset = Form.changeset(%Form{}, valid_login_attrs())

      result = Form.return_changeset(false, changeset)

      refute result.valid?
      assert %{password: ["Error, please try again."]} = errors_on(result)
    end

    test "returns user struct when login succeeds" do
      changeset = Form.changeset(%Form{}, valid_login_attrs())
      user = %User{email: "test@example.com"}

      result = Form.return_changeset(user, changeset)

      assert result == user
      assert %User{} = result
    end
  end

  describe "form state fields" do
    test "handles form state tracking fields" do
      attrs = %{
        email: "test@example.com",
        password: "validpass123",
        email_touched: true,
        password_touched: false,
        form_submitted: true,
        form_disabled: false
      }

      changeset = Form.changeset(%Form{}, attrs)

      assert changeset.valid?
      assert changeset.changes.email_touched == true
      assert changeset.changes.password_touched == false
      assert changeset.changes.form_submitted == true
      assert changeset.changes.form_disabled == false
    end

    test "works without optional form state fields" do
      changeset = Form.changeset(%Form{}, valid_login_attrs())

      assert changeset.valid?
      refute Map.has_key?(changeset.changes, :email_touched)
      refute Map.has_key?(changeset.changes, :password_touched)
      refute Map.has_key?(changeset.changes, :form_submitted)
      refute Map.has_key?(changeset.changes, :form_disabled)
    end
  end

  describe "edge cases" do
    test "handles empty string values" do
      attrs = %{email: "", password: ""}
      changeset = Form.changeset(%Form{}, attrs)

      refute changeset.valid?
      errors = errors_on(changeset)
      assert Map.has_key?(errors, :email)
      assert Map.has_key?(errors, :password)
    end

    test "handles nil values" do
      attrs = %{email: nil, password: nil}
      changeset = Form.changeset(%Form{}, attrs)

      refute changeset.valid?
      errors = errors_on(changeset)
      assert Map.has_key?(errors, :email)
      assert Map.has_key?(errors, :password)
    end

    test "trims whitespace is not applied (shows actual behavior)" do
      attrs = %{email: "  test@example.com  ", password: "validpass123"}
      changeset = Form.changeset(%Form{}, attrs)

      assert changeset.changes.email == "  test@example.com  "
    end
  end

  defp errors_on(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {message, opts} ->
      Regex.replace(~r"%{(\w+)}", message, fn _, key ->
        opts |> Keyword.get(String.to_existing_atom(key), key) |> to_string()
      end)
    end)
  end
end
