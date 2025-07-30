defmodule Account.UsersTest do
  @moduledoc """
  Tests for Account.Users module
  """
  use ExUnit.Case, async: true

  import Mock
  alias Account.Users
  alias Account.User
  alias Core.UserClient
  alias Mazaryn.Mailer

  defp sample_erlang_tuple do
    {:user,
     "user_123",                                    # id
     "192.168.1.100",                              # p2p_node_address
     "QmXoYPHfhjsdfkjhsadkfj",                     # ipfs_key
     "ai_123456",                                  # ai_user_id
     ["business_1", "business_2"],                 # business_id
     ["ad_1", "ad_2"],                            # ads_id
     "quantum_789",                               # quantum_id
     "testuser",                                  # username
     "hashed_password",                           # password
     "test@example.com",                          # email
     "123 Main St",                               # address
     ["node1", "node2"],                          # knode
     ["media1.jpg"],                              # media
     ["post_1", "post_2"],                        # posts
     ["blog_1"],                                  # blog_post
     ["notif_1"],                                 # notif
     ["user_2", "user_3"],                        # following
     ["user_4", "user_5"],                        # follower
     ["spam_user"],                               # blocked
     ["saved_post_1"],                            # saved_posts
     [{"bio", "Test bio"}],                       # other_info
     false,                                       # private
     ~U[2024-01-01 10:00:00Z],                   # date_created
     ~U[2024-01-02 10:00:00Z],                   # date_updated
     "/images/custom-avatar.jpg",                 # avatar_url
     "/images/banner.jpg",                        # banner_url
     "token_xyz789",                              # token_id
     ["chat_1", "chat_2"],                        # chat
     true,                                        # verified
     [],                                          # report
     5,                                           # level
     ~U[2024-01-03 10:00:00Z],                   # last_activity
     [],                                          # suspend
     ["dataset_1"],                               # datasets
     ["comp_1"],                                  # competitions
     %{"preferences" => %{"theme" => "dark"}}     # data
    }
  end

  setup_all do
    unless :ets.info(:mazaryn_auth_table) != :undefined do
      :ets.new(:mazaryn_auth_table, [:set, :public, :named_table])
    end
    on_exit(fn ->
      if :ets.info(:mazaryn_auth_table) != :undefined do
        :ets.delete(:mazaryn_auth_table)
      end
    end)
    :ok
  end

  setup do
    if :ets.info(:mazaryn_auth_table) != :undefined do
      :ets.delete_all_objects(:mazaryn_auth_table)
    end
    :ok
  end

  describe "signing_salt/0" do
    test "returns signing salt from config" do
      with_mock MazarynWeb.Endpoint, config: fn :live_view -> [signing_salt: "test_salt"] end do
        assert Users.signing_salt() == "test_salt"
      end
    end

    test "raises AuthenticationError when signing salt is missing" do
      with_mock MazarynWeb.Endpoint, config: fn :live_view -> [] end do
        assert_raise MazarynWeb.AuthenticationError, "missing signing_salt", fn ->
          Users.signing_salt()
        end
      end
    end
  end

  describe "get_by_session_uuid/1" do
    test "returns user when session uuid is valid" do
      email = "test@example.com"
      token = "valid_token"
      :ets.insert(:mazaryn_auth_table, {:session_123, token})

      with_mocks([
        {Phoenix.Token, [], [verify: fn _endpoint, _salt, ^token, _opts -> {:ok, email} end]},
        {UserClient, [], [get_user_by_email: fn ^email -> sample_erlang_tuple() end]}
      ]) do
        {:ok, user} = Users.get_by_session_uuid("session_123")
        assert %User{} = user
        assert user.email == email
        assert user.username == "testuser"
      end
    end

    test "returns nil when session uuid is not found" do
      assert Users.get_by_session_uuid("non_existent_session") == nil
    end

  end

  describe "verify_token/1" do
    test "returns email when token is valid" do
      with_mock Phoenix.Token, verify: fn _endpoint, _salt, _token, _opts -> {:ok, "test@example.com"} end do
        assert Users.verify_token("valid_token") == "test@example.com"
      end
    end

    test "returns nil when token is invalid" do
      with_mock Phoenix.Token, verify: fn _endpoint, _salt, _token, _opts -> {:error, :invalid} end do
        assert Users.verify_token("invalid_token") == nil
      end
    end
  end

  describe "insert_avatar/2" do
    test "successfully updates avatar for user" do
      user_id = "user_123"
      avatar_url = "/images/new-avatar.jpg"

      with_mock UserClient, insert_avatar: fn ^user_id, ^avatar_url -> sample_erlang_tuple() end do
        {:ok, user} = Users.insert_avatar(user_id, avatar_url)
        assert %User{} = user
        assert user.id == user_id
        assert user.avatar_url == "/images/custom-avatar.jpg"
      end
    end
  end

  describe "insert_banner/2" do
    test "successfully updates banner for user" do
      user_id = "user_123"
      banner_url = "/images/new-banner.jpg"

      with_mock UserClient, insert_banner: fn ^user_id, ^banner_url -> sample_erlang_tuple() end do
        {:ok, user} = Users.insert_banner(user_id, banner_url)
        assert %User{} = user
        assert user.id == user_id
        assert user.banner_url == "/images/banner.jpg"
      end
    end
  end

  describe "one_by_username/1" do
    test "returns user when username exists" do
      username = "testuser"
      with_mock UserClient, get_user: fn ^username -> sample_erlang_tuple() end do
        {:ok, user} = Users.one_by_username(username)
        assert %User{} = user
        assert user.username == username
      end
    end

    test "returns error when username does not exist" do
      with_mock UserClient, get_user: fn _ -> :not_exist end do
        assert Users.one_by_username("nonexistent") == {:error, :not_found}
      end
    end
  end

  describe "one_by_email/1" do
    test "returns user when email exists" do
      email = "test@example.com"
      with_mock UserClient, get_user_by_email: fn ^email -> sample_erlang_tuple() end do
        {:ok, user} = Users.one_by_email(email)
        assert %User{} = user
        assert user.email == email
      end
    end

    test "returns error when email does not exist" do
      with_mock UserClient, get_user_by_email: fn _ -> :user_not_exist end do
        assert Users.one_by_email("nonexistent@example.com") == {:error, :user_not_exist}
      end
    end
  end

  describe "one_by_id/1" do
    test "returns user when id exists" do
      user_id = "user_123"
      with_mock UserClient, get_user_by_id: fn ^user_id -> sample_erlang_tuple() end do
        {:ok, user} = Users.one_by_id(user_id)
        assert %User{} = user
        assert user.id == user_id
      end
    end

    test "returns nil when id does not exist" do
      with_mock UserClient, get_user_by_id: fn _ -> :user_not_exist end do
        assert Users.one_by_id("nonexistent") == nil
      end
    end
  end

  describe "list/0" do
    test "returns list of user IDs" do
      with_mock UserClient, get_all: fn -> ["user_123", "user_456"] end do
        assert Users.list() == ["user_123", "user_456"]
      end
    end
  end

  describe "register/3" do
    test "successfully registers user and sends email" do
      username = "testuser"
      password = "password123"
      email = "test@example.com"

      with_mocks([
        {UserClient, [], [register: fn ^username, ^password, ^email -> ["user_123"] end]},
        {Mail.UserEmail, [], [register_email: fn ^username, ^email -> %{} end]},
        {Mailer, [], [deliver: fn _ -> {:ok, :sent} end]}
      ]) do
        assert Users.register(username, password, email) == {:ok, ["user_123"]}
      end
    end

    test "returns error when registration fails" do
      with_mocks([
        {UserClient, [], [register: fn _, _, _ -> :registration_failed end]},
        {Mail.UserEmail, [], [register_email: fn _, _ -> %{} end]},
        {Mailer, [], [deliver: fn _ -> {:ok, :sent} end]}
      ]) do
        assert Users.register("testuser", "password123", "test@example.com") ==
                 {:error, :registration_failed}
      end
    end

    test "returns error when email delivery fails" do
      with_mocks([
        {UserClient, [], [register: fn _, _, _ -> ["user_123"] end]},
        {Mail.UserEmail, [], [register_email: fn _, _ -> %{} end]},
        {Mailer, [], [deliver: fn _ -> {:error, :email_failed} end]}
      ]) do
        assert Users.register("testuser", "password123", "test@example.com") ==
                 {:error, :email_delivery_failed}
      end
    end
  end

  describe "login/2" do
    test "successfully logs in user" do
      email = "test@example.com"
      password = "password123"

      with_mock UserClient, login: fn ^email, ^password -> :logged_in end do
        assert Users.login(email, password) == {:ok, :logged_in}
      end
    end

    test "returns error on login failure" do
      with_mock UserClient, login: fn _, _ -> :invalid_credentials end do
        assert Users.login("test@example.com", "wrongpass") == {:error, :invalid_credentials}
      end
    end
  end

  describe "follow/2" do
    test "successfully follows user" do
      follower = "user_123"
      following = "user_456"

      with_mock UserClient, follow: fn ^follower, ^following -> {:atomic, :ok} end do
        assert Users.follow(follower, following) == :ok
      end
    end

    test "returns error when follow fails" do
      with_mock UserClient, follow: fn _, _ -> :follow_failed end do
        assert Users.follow("user_123", "user_456") == {:error, :follow_failed}
      end
    end
  end

  describe "get_following/1" do
    test "returns following list when user exists" do
      user_id = "user_123"
      following = ["user_456", "user_789"]

      with_mock UserClient, get_following: fn ^user_id -> following end do
        assert Users.get_following(user_id) == following
      end
    end

    test "returns :ok when user does not exist" do
      with_mock UserClient, get_following: fn _ -> :not_exist end do
        assert Users.get_following("nonexistent") == :ok
      end
    end
  end

  describe "reset_password/1" do
    test "successfully resets password" do
      user = %User{email: "test@example.com"}
      assert Users.reset_password(user) == {:ok, :reseted}
    end
  end

  describe "list_users/0" do
    test "returns list of user structs" do
      user_ids = ["user_123", "user_456"]
      with_mock UserClient,
        get_all: fn -> user_ids end,
        get_user_by_id: fn id -> if id in user_ids, do: sample_erlang_tuple(), else: :user_not_exist end do
        users = Users.list_users()
        assert length(users) == 2
        assert Enum.all?(users, fn user -> %User{} = user end)
      end
    end

    test "returns empty list when no users exist" do
      with_mock UserClient, get_all: fn -> [] end do
        assert Users.list_users() == []
      end
    end
  end

  describe "create_user/3" do
    test "creates user and returns user struct" do
      username = "testuser"
      password = "password123"
      email = "test@example.com"
      user_id = "user_123"

      with_mocks([
        {UserClient, [], [register: fn ^username, ^password, ^email -> user_id end]},
        {UserClient, [], [get_user_by_id: fn ^user_id -> sample_erlang_tuple() end]}
      ]) do
        {:ok, user} = Users.create_user(username, password, email)
        assert %User{} = user
        assert user.id == user_id
        assert user.username == username
      end
    end
  end

  describe "get_user_by_id/1" do
    test "returns user struct for valid id" do
      user_id = ~c"user_123"
      with_mock UserClient, get_user_by_id: fn ^user_id -> sample_erlang_tuple() end do
        user = Users.get_user_by_id(user_id)
        assert %User{} = user
        assert user.id == to_string(user_id)
      end
    end
  end
end
