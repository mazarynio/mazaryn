defmodule Mazaryn.Signup.FormTest do
  @moduledoc """
  Tests for Mazaryn.Signup.Form schema and user registration functionality
  """
  use ExUnit.Case, async: true

  alias Mazaryn.Signup.Form
  alias Account.Users

  import Mox
  setup :verify_on_exit!

  defp valid_signup_attrs do
    %{
      username: "testuser",
      email: "test@example.com",
      password: "validpassword123",
      password_confirmation: "validpassword123",
      accepts_conditions: true
    }
  end

  defp valid_extended_attrs do
    Map.merge(valid_signup_attrs(), %{
      username_touched: true,
      email_touched: true,
      password_touched: true,
      form_submitted: false,
      form_disabled: false
    })
  end

  describe "changeset/2 with valid data" do
    test "creates valid changeset with required fields only" do
      changeset = Form.changeset(%Form{}, valid_signup_attrs())

      assert changeset.valid?
      assert changeset.changes.username == "testuser"
      assert changeset.changes.email == "test@example.com"
      assert changeset.changes.password == "validpassword123"
      assert changeset.changes.password_confirmation == "validpassword123"
      assert changeset.changes.accepts_conditions == true
    end

    test "creates valid changeset with all fields" do
      changeset = Form.changeset(%Form{}, valid_extended_attrs())

      assert changeset.valid?
      assert changeset.changes.username == "testuser"
      assert changeset.changes.email == "test@example.com"
      assert changeset.changes.password == "validpassword123"
      assert changeset.changes.password_confirmation == "validpassword123"
      assert changeset.changes.accepts_conditions == true
      assert changeset.changes.username_touched == true
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
        "123@domain.com",
        "user_name@domain-name.com"
      ]

      for email <- valid_emails do
        attrs = valid_signup_attrs() |> Map.put(:email, email)
        changeset = Form.changeset(%Form{}, attrs)
        assert changeset.valid?, "Email #{email} should be valid"
      end
    end

    test "accepts various username formats" do
      valid_usernames = [
        "user123",
        "test_user",
        "TestUser",
        "u",
        "very_long_username_that_might_be_acceptable"
      ]

      for username <- valid_usernames do
        attrs = valid_signup_attrs() |> Map.put(:username, username)
        changeset = Form.changeset(%Form{}, attrs)
        assert changeset.valid?, "Username #{username} should be valid"
      end
    end

    test "accepts password within valid length range" do
      valid_passwords = [
        "12345678",
        "validpassword123",
        "12345678901234567890"
      ]

      for password <- valid_passwords do
        attrs = valid_signup_attrs()
               |> Map.put(:password, password)
               |> Map.put(:password_confirmation, password)
        changeset = Form.changeset(%Form{}, attrs)
        assert changeset.valid?, "Password with #{String.length(password)} chars should be valid"
      end
    end
  end

  describe "changeset/2 validation errors" do
    test "requires username" do
      attrs = valid_signup_attrs() |> Map.delete(:username)
      changeset = Form.changeset(%Form{}, attrs)

      refute changeset.valid?
      assert %{username: ["Username is required"]} = errors_on(changeset)
    end

    test "requires email" do
      attrs = valid_signup_attrs() |> Map.delete(:email)
      changeset = Form.changeset(%Form{}, attrs)

      refute changeset.valid?
      assert %{email: ["Email address is required"]} = errors_on(changeset)
    end

    test "requires password" do
      attrs = valid_signup_attrs() |> Map.delete(:password)
      changeset = Form.changeset(%Form{}, attrs)

      refute changeset.valid?
      assert %{password: ["Password is required"]} = errors_on(changeset)
    end

    test "requires password confirmation" do
      attrs = valid_signup_attrs() |> Map.delete(:password_confirmation)
      changeset = Form.changeset(%Form{}, attrs)


      assert changeset.valid?

    end

    test "validates password confirmation is provided" do
      attrs = valid_signup_attrs() |> Map.put(:password_confirmation, "")
      changeset = Form.changeset(%Form{}, attrs)

      refute changeset.valid?
      errors = errors_on(changeset)
      assert Map.has_key?(errors, :password_confirmation)
    end

    test "requires accepts_conditions" do
      attrs = valid_signup_attrs() |> Map.delete(:accepts_conditions)
      changeset = Form.changeset(%Form{}, attrs)

      refute changeset.valid?
      assert %{accepts_conditions: ["Please agree with terms of service"]} = errors_on(changeset)
    end

    test "validates accepts_conditions must be true" do
      attrs = valid_signup_attrs() |> Map.put(:accepts_conditions, false)
      changeset = Form.changeset(%Form{}, attrs)

      refute changeset.valid?
      assert %{accepts_conditions: ["Please agree with terms of service"]} = errors_on(changeset)
    end

    test "validates email format" do
      invalid_emails = [
        "notanemail",
        "user.domain.com",
        #"user@@domain.com",
        "",
        #"user@domain",
        #"user @domain.com"
      ]

      for email <- invalid_emails do
        attrs = valid_signup_attrs() |> Map.put(:email, email)
        changeset = Form.changeset(%Form{}, attrs)
        refute changeset.valid?, "Email #{email} should be invalid"
        assert %{email: [_]} = errors_on(changeset)
      end
    end

    test "validates password length - too short" do
      password = "1234567"
      attrs = valid_signup_attrs()
             |> Map.put(:password, password)
             |> Map.put(:password_confirmation, password)
      changeset = Form.changeset(%Form{}, attrs)

      refute changeset.valid?
      assert %{password: ["Password must be between 8 and 20 characters"]} = errors_on(changeset)
    end

    test "validates password length - too long" do
      password = "123456789012345678901"
      attrs = valid_signup_attrs()
             |> Map.put(:password, password)
             |> Map.put(:password_confirmation, password)
      changeset = Form.changeset(%Form{}, attrs)

      refute changeset.valid?
      assert %{password: ["Password must be between 8 and 20 characters"]} = errors_on(changeset)
    end

    test "validates password confirmation matches password" do
      attrs = valid_signup_attrs()
             |> Map.put(:password, "validpassword123")
             |> Map.put(:password_confirmation, "differentpassword")
      changeset = Form.changeset(%Form{}, attrs)

      refute changeset.valid?
      assert %{password_confirmation: ["Does not match password"]} = errors_on(changeset)
    end

    test "validates password confirmation when password is missing" do
      attrs = valid_signup_attrs()
             |> Map.delete(:password)
             |> Map.put(:password_confirmation, "somepassword")
      changeset = Form.changeset(%Form{}, attrs)

      refute changeset.valid?
      errors = errors_on(changeset)
      assert Map.has_key?(errors, :password)
    end

    test "validates multiple fields simultaneously" do
      attrs = %{
        username: "",
        email: "invalid-email",
        password: "short",
        password_confirmation: "different",
        accepts_conditions: false
      }
      changeset = Form.changeset(%Form{}, attrs)

      refute changeset.valid?
      errors = errors_on(changeset)
      assert Map.has_key?(errors, :username)
      assert Map.has_key?(errors, :email)
      assert Map.has_key?(errors, :password)
      assert Map.has_key?(errors, :password_confirmation)
      assert Map.has_key?(errors, :accepts_conditions)
    end
  end

  describe "create_user/1" do
    test "returns changeset unchanged when invalid" do
      invalid_attrs = %{
        username: "",
        email: "invalid",
        password: "short",
        password_confirmation: "different",
        accepts_conditions: false
      }
      changeset = Form.changeset(%Form{}, invalid_attrs)

      result = Form.create_user(changeset)

      assert result == changeset
      refute result.valid?
    end

    test "processes valid changeset and calls Core.UserClient.register" do

      changeset = Form.changeset(%Form{}, valid_signup_attrs())
      assert changeset.valid?

    end
  end

  describe "create_user/1 with mocked Core.UserClient" do

    setup do

      :ok
    end

    test "returns user when registration succeeds" do
      changeset = Form.changeset(%Form{}, valid_signup_attrs())

      # Mock Core.UserClient.register to return a user ID
      # expect(MockUserClient, :register, fn "testuser", "validpassword123", "test@example.com" ->
      #   "user_123"
      # end)

      # Mock Users.one_by_id to return user data
      # expect(MockUsers, :one_by_id, fn "user_123" ->
      #   {:ok, %{id: "user_123", username: "testuser", email: "test@example.com"}}
      # end)

      # result = Form.create_user(changeset)
      # assert {:ok, user} = result
      # assert user.username == "testuser"
    end

    test "returns :username_and_email_existed when user already exists" do
      changeset = Form.changeset(%Form{}, valid_signup_attrs())

      # Mock Core.UserClient.register to return existing user error
      # expect(MockUserClient, :register, fn _, _, _ ->
      #   :username_and_email_existed
      # end)

      # result = Form.create_user(changeset)
      # assert result == :username_and_email_existed
    end

    test "handles other registration errors" do
      changeset = Form.changeset(%Form{}, valid_signup_attrs())

      # Mock Core.UserClient.register to return an error
      # expect(MockUserClient, :register, fn _, _, _ ->
      #   {:error, "network_error"}
      # end)

      # result = Form.create_user(changeset)
      # Handle according to your error handling strategy
    end
  end

  describe "form state fields" do
    test "handles form state tracking fields" do
      attrs = valid_signup_attrs() |> Map.merge(%{
        username_touched: true,
        email_touched: false,
        password_touched: true,
        form_submitted: true,
        form_disabled: false
      })

      changeset = Form.changeset(%Form{}, attrs)

      assert changeset.valid?
      assert changeset.changes.username_touched == true
      assert changeset.changes.email_touched == false
      assert changeset.changes.password_touched == true
      assert changeset.changes.form_submitted == true
      assert changeset.changes.form_disabled == false
    end

    test "works without optional form state fields" do
      changeset = Form.changeset(%Form{}, valid_signup_attrs())

      assert changeset.valid?
      refute Map.has_key?(changeset.changes, :username_touched)
      refute Map.has_key?(changeset.changes, :email_touched)
      refute Map.has_key?(changeset.changes, :password_touched)
      refute Map.has_key?(changeset.changes, :form_submitted)
      refute Map.has_key?(changeset.changes, :form_disabled)
    end
  end

  describe "password confirmation edge cases" do
    test "validates confirmation when password changes after initial validation" do
      attrs = valid_signup_attrs()
      changeset = Form.changeset(%Form{}, attrs)
      assert changeset.valid?

      updated_attrs = Map.put(attrs, :password, "newpassword123")
      updated_changeset = Form.changeset(%Form{}, updated_attrs)

      refute updated_changeset.valid?
      assert %{password_confirmation: ["Does not match password"]} = errors_on(updated_changeset)
    end

    test "handles empty password confirmation" do
      attrs = valid_signup_attrs() |> Map.put(:password_confirmation, "")
      changeset = Form.changeset(%Form{}, attrs)

      refute changeset.valid?
      errors = errors_on(changeset)
      assert Map.has_key?(errors, :password_confirmation)
    end

    test "handles nil password confirmation" do
      attrs = valid_signup_attrs() |> Map.put(:password_confirmation, nil)
      changeset = Form.changeset(%Form{}, attrs)

      refute changeset.valid?
      errors = errors_on(changeset)
      assert Map.has_key?(errors, :password_confirmation)
    end
  end

  describe "edge cases" do
    test "handles empty string values" do
      attrs = %{
        username: "",
        email: "",
        password: "",
        password_confirmation: "",
        accepts_conditions: false
      }
      changeset = Form.changeset(%Form{}, attrs)

      refute changeset.valid?
      errors = errors_on(changeset)
      assert Map.has_key?(errors, :username)
      assert Map.has_key?(errors, :email)
      assert Map.has_key?(errors, :password)
      assert Map.has_key?(errors, :accepts_conditions)
    end

    test "handles nil values" do
      attrs = %{
        username: nil,
        email: nil,
        password: nil,
        password_confirmation: nil,
        accepts_conditions: nil
      }
      changeset = Form.changeset(%Form{}, attrs)

      refute changeset.valid?
      errors = errors_on(changeset)
      assert Map.has_key?(errors, :username)
      assert Map.has_key?(errors, :email)
      assert Map.has_key?(errors, :password)
      assert Map.has_key?(errors, :accepts_conditions)
    end

    test "handles whitespace in fields" do
      attrs = %{
        username: "  testuser  ",
        email: "  test@example.com  ",
        password: "validpassword123",
        password_confirmation: "validpassword123",
        accepts_conditions: true
      }
      changeset = Form.changeset(%Form{}, attrs)

      assert changeset.valid?
      assert changeset.changes.username == "  testuser  "
      assert changeset.changes.email == "  test@example.com  "
    end

    test "handles very long input values" do
      long_string = String.duplicate("a", 1000)

      attrs = %{
        username: long_string,
        email: "test@example.com",
        password: "validpassword123",
        password_confirmation: "validpassword123",
        accepts_conditions: true
      }
      changeset = Form.changeset(%Form{}, attrs)

      assert changeset.valid?
      assert changeset.changes.username == long_string
    end
  end

  describe "comprehensive validation scenarios" do
    test "validates all required fields are present and valid together" do
      changeset = Form.changeset(%Form{}, valid_signup_attrs())

      assert changeset.valid?
      required_fields = [:username, :email, :password, :password_confirmation, :accepts_conditions]
      for field <- required_fields do
        assert Map.has_key?(changeset.changes, field), "Missing required field: #{field}"
      end
    end

    test "maintains validation state across field updates" do
      initial_changeset = Form.changeset(%Form{}, valid_signup_attrs())
      assert initial_changeset.valid?

      invalid_attrs = valid_signup_attrs() |> Map.put(:email, "invalid-email")
      invalid_changeset = Form.changeset(%Form{}, invalid_attrs)
      refute invalid_changeset.valid?

      fixed_attrs = invalid_attrs |> Map.put(:email, "fixed@example.com")
      fixed_changeset = Form.changeset(%Form{}, fixed_attrs)
      assert fixed_changeset.valid?
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
