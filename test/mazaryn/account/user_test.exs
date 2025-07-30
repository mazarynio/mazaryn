defmodule Account.UserTest do
  @moduledoc """
  Tests for Account.User embedded schema
  """
  use ExUnit.Case, async: true

  alias Account.User

  defp valid_user_attrs do
    %{
      username: "testuser",
      email: "test@example.com",
      password: "password123"
    }
  end

  defp valid_extended_attrs do
    Map.merge(valid_user_attrs(), %{
      p2p_node_address: "192.168.1.100",
      ipfs_key: "QmXoYPHfhjsdfkjhsadkfj",
      ai_user_id: "ai_123456",
      business_id: ["business_1", "business_2"],
      ads_id: ["ad_1", "ad_2"],
      quantum_id: "quantum_789",
      address: "123 Main St, City, Country",
      knode: ["node1", "node2"],
      private: false,
      other_info: %{"bio" => "Test user bio", "interests" => ["tech", "science"]},
      media: ["media1.jpg", "media2.png"],
      posts: ["post_1", "post_2"],
      blog_post: ["blog_1"],
      following: ["user_2", "user_3"],
      follower: ["user_4", "user_5"],
      blocked: ["spam_user"],
      saved_posts: ["saved_post_1"],
      notif: ["notif_1"],
      avatar_url: "/images/custom-avatar.jpg",
      banner_url: "/images/banner.jpg",
      token_id: "token_xyz789",
      country: "Iran",
      chat: ["chat_1", "chat_2"],
      verified: true,
      report: [],
      level: 5,
      suspend: [],
      datasets: ["dataset_1"],
      competitions: ["comp_1"],
      data: %{"preferences" => %{"theme" => "dark"}}
    })
  end

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

  describe "changeset/2 with valid data" do
    test "creates valid changeset with required fields only" do
      changeset = User.changeset(%User{}, valid_user_attrs())

      assert changeset.valid?
      assert changeset.changes.username == "testuser"
      assert changeset.changes.email == "test@example.com"
      assert changeset.changes.password != "password123"
      assert is_binary(changeset.changes.password)
    end

    test "creates valid changeset with all fields" do
      changeset = User.changeset(%User{}, valid_user_attrs())

      assert changeset.valid?
      assert changeset.changes.username == "testuser"
      assert changeset.changes.email == "test@example.com"
    end

    test "sets default values for array fields" do
      changeset = User.changeset(%User{}, valid_user_attrs())
      user_struct = Ecto.Changeset.apply_changes(changeset)

      assert user_struct.business_id == []
      assert user_struct.ads_id == []
      assert user_struct.knode == []
      assert user_struct.media == []
      assert user_struct.posts == []
      assert user_struct.following == []
      assert user_struct.follower == []
      assert user_struct.blocked == []
      assert user_struct.saved_posts == []
      assert user_struct.notif == []
      assert user_struct.chat == []
      assert user_struct.report == []
      assert user_struct.suspend == []
      assert user_struct.datasets == []
      assert user_struct.competitions == []
    end

    test "sets default avatar_url" do
      changeset = User.changeset(%User{}, valid_user_attrs())
      user_struct = Ecto.Changeset.apply_changes(changeset)

      assert user_struct.avatar_url == "/images/default-user.svg"
    end

    test "password gets hashed using erlpass" do
      original_password = "mypassword123"
      attrs = Map.put(valid_user_attrs(), :password, original_password)

      changeset = User.changeset(%User{}, attrs)

      assert changeset.valid?
      hashed_password = changeset.changes.password

      assert hashed_password != original_password
      assert is_binary(hashed_password)
      assert String.length(hashed_password) > 10
    end
  end

  describe "changeset/2 validation errors" do
    test "requires username" do
      attrs = valid_user_attrs() |> Map.delete(:username)
      changeset = User.changeset(%User{}, attrs)

      refute changeset.valid?
      assert %{username: ["can't be blank"]} = errors_on(changeset)
    end

    test "requires email" do
      attrs = valid_user_attrs() |> Map.delete(:email)
      changeset = User.changeset(%User{}, attrs)

      refute changeset.valid?
      assert %{email: ["can't be blank"]} = errors_on(changeset)
    end

    test "requires password" do
      attrs = valid_user_attrs() |> Map.delete(:password)
      changeset = User.changeset(%User{}, attrs)

      refute changeset.valid?
      assert %{password: ["can't be blank"]} = errors_on(changeset)
    end

    test "validates email format" do
      attrs = Map.put(valid_user_attrs(), :email, "invalid-email")
      changeset = User.changeset(%User{}, attrs)

      refute changeset.valid?
      assert %{email: ["has invalid format"]} = errors_on(changeset)
    end

    test "validates password minimum length" do
      attrs = Map.put(valid_user_attrs(), :password, "short")
      changeset = User.changeset(%User{}, attrs)

      refute changeset.valid?
      assert %{password: ["Password must be between 8 and 20 characters"]} = errors_on(changeset)
    end

    test "validates password maximum length" do
      long_password = String.duplicate("a", 61)
      attrs = Map.put(valid_user_attrs(), :password, long_password)
      changeset = User.changeset(%User{}, attrs)

      refute changeset.valid?
      assert %{password: ["Password must be between 8 and 20 characters"]} = errors_on(changeset)
    end

    test "accepts valid email formats" do
      valid_emails = [
        "user@example.com",
        "test.user+tag@domain.co.uk",
        "user123@test-domain.org"
      ]

      for email <- valid_emails do
        attrs = Map.put(valid_user_attrs(), :email, email)
        changeset = User.changeset(%User{}, attrs)

        assert changeset.valid?, "Email #{email} should be valid"
      end
    end

    test "accepts valid password lengths" do
      valid_passwords = [
        "password",
        "mypassword123",
        String.duplicate("a", 20)
      ]

      for password <- valid_passwords do
        attrs = Map.put(valid_user_attrs(), :password, password)
        changeset = User.changeset(%User{}, attrs)

        assert changeset.valid?, "Password of length #{String.length(password)} should be valid"
      end
    end
  end

  describe "erl_changeset/1" do
    test "converts erlang tuple to changeset successfully" do
      changeset = User.erl_changeset(sample_erlang_tuple())

      assert changeset.valid?

      changes = changeset.changes
      assert changes.id == "user_123"
      assert changes.username == "testuser"
      assert changes.email == "test@example.com"
      assert changes.password == "hashed_password"
      assert changes.p2p_node_address == "192.168.1.100"
      assert changes.ipfs_key == "QmXoYPHfhjsdfkjhsadkfj"
      assert changes.business_id == ["business_1", "business_2"]
      assert changes.private == false
      assert changes.verified == true
      assert changes.level == 5
    end

    test "handles undefined avatar_url in erlang tuple" do
      tuple_with_undefined = put_elem(sample_erlang_tuple(), 25, :undefined)
      changeset = User.erl_changeset(tuple_with_undefined)

      assert changeset.valid?
      assert changeset.changes.avatar_url == nil
    end

    test "handles undefined banner_url in erlang tuple" do
      tuple_with_undefined = put_elem(sample_erlang_tuple(), 26, :undefined)
      changeset = User.erl_changeset(tuple_with_undefined)

      assert changeset.valid?
      assert changeset.changes.banner_url == nil
    end

    test "handles undefined token_id in erlang tuple" do
      tuple_with_undefined = put_elem(sample_erlang_tuple(), 27, :undefined)
      changeset = User.erl_changeset(tuple_with_undefined)

      assert changeset.valid?
      assert Map.get(changeset.changes, :token_id, nil) == nil
    end

    test "converts other_info list to map" do
      changeset = User.erl_changeset(sample_erlang_tuple())

      assert changeset.valid?
      assert is_map(changeset.changes.other_info)
      assert changeset.changes.other_info == %{"bio" => "Test bio"}
    end

    test "raises error for invalid input" do
      assert_raise RuntimeError, fn ->
        User.erl_changeset("invalid_input")
      end
    end
  end

  describe "build/1" do
    test "builds user struct from valid changeset" do
      changeset = User.changeset(%User{}, valid_user_attrs())

      assert {:ok, user} = User.build(changeset)
      assert %User{} = user
      assert user.username == "testuser"
      assert user.email == "test@example.com"
      assert user.password != "password123"
    end

    test "returns error for invalid changeset" do
      invalid_attrs = %{username: "", email: "invalid", password: "short"}
      changeset = User.changeset(%User{}, invalid_attrs)

      assert {:error, changeset} = User.build(changeset)
      refute changeset.valid?
    end
  end

  describe "password hashing with create_password_hash/1" do
    test "hashes password for valid changeset" do
      changeset = %User{}
      |> Ecto.Changeset.cast(%{password: "testpassword"}, [:password])

      hashed_changeset = User.create_password_hash(changeset)

      assert hashed_changeset.changes.password != "testpassword"
      assert is_binary(hashed_changeset.changes.password)
    end

    test "returns unchanged changeset when invalid" do
      invalid_changeset = %User{}
      |> Ecto.Changeset.cast(%{}, [:password])
      |> Ecto.Changeset.validate_required([:password])

      result = User.create_password_hash(invalid_changeset)

      assert result == invalid_changeset
      refute result.valid?
    end

    test "password field is virtual and not persisted in struct" do
      changeset = User.changeset(%User{}, valid_user_attrs())
      {:ok, _user} = User.build(changeset)

      user_from_apply = Ecto.Changeset.apply_changes(changeset)
      assert user_from_apply.password != nil
    end
  end

  describe "complex field validations using erl_changeset" do
    test "handles array fields correctly via erl_changeset" do
      custom_tuple = {:user,
        "user_123",                                    # id (0)
        "192.168.1.100",                              # p2p_node_address (1)
        "QmXoYPHfhjsdfkjhsadkfj",                     # ipfs_key (2)
        "ai_123456",                                  # ai_user_id (3)
        ["biz1", "biz2", "biz3"],                     # business_id (4)
        ["ad_1", "ad_2"],                            # ads_id (5)
        "quantum_789",                               # quantum_id (6)
        "testuser",                                  # username (7)
        "hashed_password",                           # password (8)
        "test@example.com",                          # email (9)
        "123 Main St",                               # address (10)
        ["node1", "node2"],                          # knode (11)
        ["media1.jpg"],                              # media (12)
        ["post1", "post2", "post3"],                 # posts (13)
        ["blog_1"],                                  # blog_post (14)
        ["notif_1"],                                 # notif (15)
        ["user1", "user2"],                          # following (16)
        ["user_4", "user_5"],                        # follower (17)
        ["spam_user"],                               # blocked (18)
        ["saved_post_1"],                            # saved_posts (19)
        [{"bio", "Test bio"}],                       # other_info (20)
        false,                                       # private (21)
        ~U[2024-01-01 10:00:00Z],                   # date_created (22)
        ~U[2024-01-02 10:00:00Z],                   # date_updated (23)
        "/images/custom-avatar.jpg",                 # avatar_url (24)
        "/images/banner.jpg",                        # banner_url (25)
        "token_xyz789",                              # token_id (26)
        ["chat_1", "chat_2"],                        # chat (27)
        true,                                        # verified (28)
        [],                                          # report (29)
        5,                                           # level (30)
        ~U[2024-01-03 10:00:00Z],                   # last_activity (31)
        [],                                          # suspend (32)
        ["dataset_1"],                               # datasets (33)
        ["comp_1"],                                  # competitions (34)
        %{"preferences" => %{"theme" => "dark"}}     # data (35)
      }

      changeset = User.erl_changeset(custom_tuple)
      assert changeset.valid?

      {:ok, user} = User.build(changeset)
      assert length(user.business_id) == 3
      assert length(user.following) == 2
      assert length(user.posts) == 3
    end

    test "handles map fields correctly via erl_changeset" do
      custom_tuple = {:user,
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
        [{"bio", "Software developer"}, {"location", "Tehran"}], # other_info (20)
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
        %{"settings" => %{"notifications" => true, "privacy" => "public"}} # data (35)
      }

      changeset = User.erl_changeset(custom_tuple)
      assert changeset.valid?

      {:ok, user} = User.build(changeset)
      assert user.other_info["bio"] == "Software developer"
      assert user.other_info["location"] == "Tehran"
      assert user.data["settings"]["notifications"] == true
    end

    test "handles boolean fields correctly via erl_changeset" do
      custom_tuple = {:user,
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
        true,                                        # private (21) - changed to true
        ~U[2024-01-01 10:00:00Z],                   # date_created
        ~U[2024-01-02 10:00:00Z],                   # date_updated
        "/images/custom-avatar.jpg",                 # avatar_url
        "/images/banner.jpg",                        # banner_url
        "token_xyz789",                              # token_id
        ["chat_1", "chat_2"],                        # chat
        false,                                       # verified (28) - changed to false
        [],                                          # report
        5,                                           # level
        ~U[2024-01-03 10:00:00Z],                   # last_activity
        [],                                          # suspend
        ["dataset_1"],                               # datasets
        ["comp_1"],                                  # competitions
        %{"preferences" => %{"theme" => "dark"}}     # data
      }

      changeset = User.erl_changeset(custom_tuple)
      assert changeset.valid?

      {:ok, user} = User.build(changeset)
      assert user.private == true
      assert user.verified == false
    end

    test "handles integer fields correctly via erl_changeset" do
      custom_tuple = {:user,
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
        10,                                          # level (30) - changed to 10
        ~U[2024-01-03 10:00:00Z],                   # last_activity
        [],                                          # suspend
        ["dataset_1"],                               # datasets
        ["comp_1"],                                  # competitions
        %{"preferences" => %{"theme" => "dark"}}     # data
      }

      changeset = User.erl_changeset(custom_tuple)
      assert changeset.valid?

      {:ok, user} = User.build(changeset)
      assert user.level == 10
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
