defmodule Mazaryn.PostsTest do
  @moduledoc """
  Tests for Mazaryn.Posts module
  """
  use ExUnit.Case, async: true

  import Mock
  alias Mazaryn.Posts
  alias Mazaryn.Schema.Post
  alias Mazaryn.Schema.Comment
  alias Core.PostClient

  defp sample_post_tuple do
    {:post,
     "post_123",                                   # id
     "user_123",                                   # author
     "This is a test post content",                # content
     ["image1.jpg", "image2.jpg"],                # media
     ["#elixir", "#test"],                        # hashtag
     ["@user456"],                                # mention
     "https://example.com",                       # link_url
     100,                                         # likes
     25,                                          # reposts
     ["comment_1", "comment_2"],                  # comments
     ~U[2024-01-01 10:00:00Z],                   # date_created
     ~U[2024-01-02 10:00:00Z]                    # date_updated
    }
  end

  defp sample_comment_tuple do
    {:comment,
     "comment_123",                               # id
     "user_456",                                  # author
     "post_123",                                  # post_id
     "This is a test comment",                    # content
     10,                                          # likes
     ~U[2024-01-01 12:00:00Z],                   # date_created
     ~U[2024-01-01 12:00:00Z]                    # date_updated
    }
  end

  describe "create_post/1" do
    test "returns changeset when changeset is invalid" do
      invalid_changeset = %Ecto.Changeset{valid?: false}
      assert Posts.create_post(invalid_changeset) == invalid_changeset
    end

    test "creates post successfully with valid changeset" do
      post_data = %Post{media: [], hashtag: "#mazaryn", mention: "@user_123", link_url: "https://github.com/mazarynio/mazaryn"}
      changeset = Ecto.Changeset.cast(post_data, %{author: "user_123", content: "Test post content"}, [:author, :content])
      changeset = %{changeset | valid?: true}

      with_mocks([
        {PostClient, [], [create: fn _, _, _, _, _, _ -> "post_123" end]},
        {PostClient, [], [get_by_id: fn "post_123" -> sample_post_tuple() end]},
        {Post, [], [erl_changeset: fn _ -> changeset end]},
        {Post, [], [build: fn _ -> {:ok, %Post{id: "post_123", content: "Test post content"}} end]}
      ]) do
        {:ok, post} = Posts.create_post(changeset)
        assert %Post{} = post
        assert post.id == "post_123"
      end
    end

  end

  describe "create_comment/1" do
    test "returns changeset when changeset is invalid" do
      invalid_changeset = %Ecto.Changeset{valid?: false}
      assert Posts.create_comment(invalid_changeset) == invalid_changeset
    end

    test "creates comment successfully with valid changeset" do
      changeset = %Ecto.Changeset{
        valid?: true,
        changes: %{
          author: "user_456",
          content: "Test comment",
          post_id: "post_123"
        }
      }

      with_mocks([
        {PostClient, [], [add_comment: fn _, _, _ -> "comment_123" end]},
        {PostClient, [], [get_single_comment: fn "comment_123" -> sample_comment_tuple() end]},
        {Comment, [], [erl_changeset: fn _ -> changeset end]}
      ]) do
        result = Posts.create_comment(changeset)
        assert result == changeset
      end
    end

    test "returns error when comment creation fails" do
      changeset = %Ecto.Changeset{
        valid?: true,
        changes: %{
          author: "user_456",
          content: "Test comment",
          post_id: "post_123"
        }
      }

      with_mock PostClient, add_comment: fn _, _, _ -> {:error, :comment_failed} end do
        result = Posts.create_comment(changeset)
        assert {:error, :comment_failed} = result
      end
    end
  end

  describe "update_comment/1" do
    test "returns changeset when changeset is invalid" do
      invalid_changeset = %Ecto.Changeset{valid?: false}
      assert Posts.update_comment(invalid_changeset) == invalid_changeset
    end

    test "updates comment successfully" do
      changeset = %{
        changes: %{
          id: "comment_123",
          content: "Updated comment content"
        }
      }

      with_mocks([
        {PostClient, [], [update_comment: fn _, _ -> :ok end]},
        {PostClient, [], [get_single_comment: fn _ -> sample_comment_tuple() end]},
        {Comment, [], [erl_changeset: fn _ -> changeset end]}
      ]) do
        result = Posts.update_comment(changeset)
        assert result == changeset
      end
    end

    test "returns error when comment update fails" do
      changeset = %{
        changes: %{
          id: "comment_123",
          content: "Updated comment content"
        }
      }

      with_mock PostClient, update_comment: fn _, _ -> {:error, :update_failed} end do
        result = Posts.update_comment(changeset)
        assert {:error, :update_failed} = result
      end
    end
  end

  describe "create/6" do
    test "creates post and returns post_id" do
      with_mock PostClient, create: fn _, _, _, _, _, _ -> "post_123" end do
        {:ok, post_id} = Posts.create("user_123", "content", [], nil, nil, nil)
        assert post_id == "post_123"
      end
    end
  end

  describe "one_by_id/1" do
    test "returns post struct when post exists" do
      changeset = %Ecto.Changeset{valid?: true}

      with_mocks([
        {PostClient, [], [get_by_id: fn "post_123" -> sample_post_tuple() end]},
        {Post, [], [erl_changeset: fn _ -> changeset end]},
        {Post, [], [build: fn _ -> {:ok, %Post{id: "post_123"}} end]}
      ]) do
        {:ok, post} = Posts.one_by_id("post_123")
        assert %Post{} = post
        assert post.id == "post_123"
      end
    end
  end

  describe "build_comments_structure/1" do
    test "builds comment structure successfully" do
      changeset = %Ecto.Changeset{valid?: true}

      with_mocks([
        {PostClient, [], [get_single_comment: fn _ -> {:ok, sample_comment_tuple()} end]},
        {Comment, [], [erl_changeset: fn _ -> changeset end]},
        {Comment, [], [build: fn _ -> {:ok, %Comment{id: "comment_123"}} end]}
      ]) do
        comment = Posts.build_comments_structure("comment_123")
        assert %Comment{} = comment
        assert comment.id == "comment_123"
      end
    end

    test "returns empty map when comment fetch fails" do
      with_mock PostClient, get_single_comment: fn _ -> {:error, :not_found} end do
        result = Posts.build_comments_structure("non_existent")
        assert result == %{}
      end
    end

    test "returns empty map when comment build fails" do
      changeset = %Ecto.Changeset{valid?: true}

      with_mocks([
        {PostClient, [], [get_single_comment: fn _ -> {:ok, sample_comment_tuple()} end]},
        {Comment, [], [erl_changeset: fn _ -> changeset end]},
        {Comment, [], [build: fn _ -> {:error, :build_failed} end]}
      ]) do
        result = Posts.build_comments_structure("comment_123")
        assert result == %{}
      end
    end
  end

  describe "get_comment_by_post_id/1" do
    test "returns comments for binary post_id" do
      changeset = %Ecto.Changeset{valid?: true}

      with_mocks([
        {:postdb, [], [get_all_comment_ids: fn _ -> ["comment_1", "comment_2"] end]},
        {PostClient, [], [get_single_comment: fn _ -> {:ok, sample_comment_tuple()} end]},
        {Comment, [], [erl_changeset: fn _ -> changeset end]},
        {Comment, [], [build: fn _ -> {:ok, %Comment{id: "comment_123"}} end]}
      ]) do
        comments = Posts.get_comment_by_post_id("post_123")
        assert length(comments) == 2
        assert Enum.all?(comments, fn comment -> %Comment{} = comment end)
      end
    end

    test "returns comments for charlist post_id" do
      changeset = %Ecto.Changeset{valid?: true}

      with_mocks([
        {:postdb, [], [get_all_comment_ids: fn _ -> ["comment_1"] end]},
        {PostClient, [], [get_single_comment: fn _ -> {:ok, sample_comment_tuple()} end]},
        {Comment, [], [erl_changeset: fn _ -> changeset end]},
        {Comment, [], [build: fn _ -> {:ok, %Comment{id: "comment_123"}} end]}
      ]) do
        comments = Posts.get_comment_by_post_id('post_123')
        assert length(comments) == 1
      end
    end

    test "filters out empty comment structures" do
      with_mocks([
        {:postdb, [], [get_all_comment_ids: fn _ -> ["comment_1", "comment_2"] end]},
        {PostClient, [], [get_single_comment: fn _ -> {:error, :not_found} end]}
      ]) do
        comments = Posts.get_comment_by_post_id("post_123")
        assert comments == []
      end
    end
  end

  describe "get_posts_by_author/1" do
    test "returns sorted posts by author" do
      post1 = sample_post_tuple() |> put_elem(10, ~U[2024-01-01 10:00:00Z])
      post2 = sample_post_tuple() |> put_elem(10, ~U[2024-01-02 10:00:00Z])
      posts = [post1, post2]

      changeset = %Ecto.Changeset{valid?: true}

      with_mocks([
        {PostClient, [], [get_posts_by_author: fn _ -> posts end]},
        {Post, [], [erl_changeset: fn _ -> changeset end]},
        {Post, [], [build: fn _ -> {:ok, %Post{id: "post_123", date_created: ~U[2024-01-01 10:00:00Z]}} end]}
      ]) do
        result = Posts.get_posts_by_author("user_123")
        assert length(result) == 2
        assert Enum.all?(result, fn post -> %Post{} = post end)
      end
    end

    test "logs error when get_posts_by_author fails" do
      import ExUnit.CaptureLog

      with_mock PostClient, get_posts_by_author: fn _ -> :error end do
        log = capture_log(fn ->
          Posts.get_posts_by_author("user_123")
        end)

        assert log =~ "handle here"
      end
    end
  end

  describe "get_posts_by_user_id/1" do
    test "returns sorted posts by user_id" do
      posts = [sample_post_tuple()]
      changeset = %Ecto.Changeset{valid?: true}

      with_mocks([
        {PostClient, [], [get_posts_by_user_id: fn _ -> posts end]},
        {Post, [], [erl_changeset: fn _ -> changeset end]},
        {Post, [], [build: fn _ -> {:ok, %Post{id: "post_123", date_created: ~U[2024-01-01 10:00:00Z]}} end]}
      ]) do
        result = Posts.get_posts_by_user_id("user_123")
        assert length(result) == 1
        assert Enum.all?(result, fn post -> %Post{} = post end)
      end
    end

    test "logs error when get_posts_by_user_id fails" do
      import ExUnit.CaptureLog

      with_mock PostClient, get_posts_by_user_id: fn _ -> :error end do
        log = capture_log(fn ->
          Posts.get_posts_by_user_id("user_123")
        end)

        assert log =~ "handle here"
      end
    end
  end

  describe "get_posts_by_hashtag/1" do
    test "returns sorted posts by hashtag" do
      posts = [sample_post_tuple()]
      changeset = %Ecto.Changeset{valid?: true}

      with_mocks([
        {PostClient, [], [get_posts_by_hashtag: fn _ -> posts end]},
        {Post, [], [erl_changeset: fn _ -> changeset end]},
        {Post, [], [build: fn _ -> {:ok, %Post{id: "post_123", date_created: ~U[2024-01-01 10:00:00Z]}} end]}
      ]) do
        result = Posts.get_posts_by_hashtag("#elixir")
        assert length(result) == 1
        assert Enum.all?(result, fn post -> %Post{} = post end)
      end
    end

    test "logs error when get_posts_by_hashtag fails" do
      import ExUnit.CaptureLog

      with_mock PostClient, get_posts_by_hashtag: fn _ -> :error end do
        log = capture_log(fn ->
          Posts.get_posts_by_hashtag("#elixir")
        end)

        assert log =~ "handle here"
      end
    end
  end

  describe "get_home_posts/0" do
    test "returns sorted home posts" do
      post_ids = ["post_123", "post_456"]
      changeset = %Ecto.Changeset{valid?: true}

      with_mocks([
        {PostClient, [], [get_posts: fn -> post_ids end]},
        {PostClient, [], [get_by_id: fn _ -> sample_post_tuple() end]},
        {Post, [], [erl_changeset: fn _ -> changeset end]},
        {Post, [], [build: fn _ -> {:ok, %Post{id: "post_123", date_created: ~U[2024-01-01 10:00:00Z]}} end]}
      ]) do
        result = Posts.get_home_posts()
        assert length(result) == 2
        assert Enum.all?(result, fn post -> %Post{} = post end)
      end
    end

    test "logs error when get_posts fails" do
      import ExUnit.CaptureLog

      with_mock PostClient, get_posts: fn -> :error end do
        log = capture_log(fn ->
          Posts.get_home_posts()
        end)

        assert log =~ "handle here"
      end
    end
  end

  describe "posts_from_user_following/1" do
    test "returns empty list (commented out functionality)" do
      result = Posts.posts_from_user_following("test@example.com")
      assert result == []
    end
  end

  describe "get_posts/0" do
    test "returns sorted posts" do
      post_ids = ["post_123"]
      changeset = %Ecto.Changeset{valid?: true}

      with_mocks([
        {PostClient, [], [get_posts: fn -> post_ids end]},
        {PostClient, [], [get_by_id: fn _ -> sample_post_tuple() end]},
        {Post, [], [erl_changeset: fn _ -> changeset end]},
        {Post, [], [build: fn _ -> {:ok, %Post{id: "post_123", date_created: ~U[2024-01-01 10:00:00Z]}} end]}
      ]) do
        result = Posts.get_posts()
        assert length(result) == 1
        assert Enum.all?(result, fn post -> %Post{} = post end)
      end
    end

    test "logs error when get_posts fails" do
      import ExUnit.CaptureLog

      with_mock PostClient, get_posts: fn -> :error end do
        log = capture_log(fn ->
          Posts.get_posts()
        end)

        assert log =~ "error getting posts"
      end
    end
  end

  describe "create_a_post/6" do
    test "creates post and returns post struct" do
      changeset = %Ecto.Changeset{valid?: true}

      with_mocks([
        {PostClient, [], [create: fn _, _, _, _, _, _ -> "post_123" end]},
        {PostClient, [], [get_by_id: fn _ -> sample_post_tuple() end]},
        {Post, [], [erl_changeset: fn _ -> changeset end]},
        {Post, [], [build: fn _ -> {:ok, %Post{id: "post_123"}} end]}
      ]) do
        {:ok, post} = Posts.create_a_post("user_123", "content", [], nil, nil, nil)
        assert %Post{} = post
        assert post.id == "post_123"
      end
    end
  end

  describe "get_likes_by_post_id/1" do
    test "returns likes for post" do
      likes = [%{user_id: "user_123", timestamp: ~U[2024-01-01 10:00:00Z]}]

      with_mock PostClient, get_likes: fn _ -> likes end do
        result = Posts.get_likes_by_post_id(123)
        assert result == likes
      end
    end
  end
end
