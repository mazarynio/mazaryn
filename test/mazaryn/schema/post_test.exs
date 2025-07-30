defmodule Schema.PostTest do
  @moduledoc """
  Tests for Mazaryn.Schema.Post embedded schema
  """
  use ExUnit.Case, async: true

  alias Mazaryn.Schema.Post

  defp valid_post_attrs do
    %{
      content: "This is a test post"
    }
  end

  defp valid_extended_attrs do
    Map.merge(valid_post_attrs(), %{
      ai_post_id: "ai_post_123",
      user_id: "user_123",
      business_id: "biz_456",
      media: ["image1.jpg", "video1.mp4"],
      hashtag: "#test #social",
      mention: "@friend",
      ipns: "QmXoYPHfhjsdfkjhsadkfj",
      emoji: "😊👍",
      link_url: "https://example.com",
      author: "testuser",
      other: ["extra1", "extra2"],
      comments: ["comment_1", "comment_2"],
      likes: [1, 2, 3],
      profile_tags: ["tag1", "tag2"],
      photo_url: "/images/post.jpg",
      date_created: ~U[2024-01-01 10:00:00Z],
      date_updated: ~U[2024-01-02 10:00:00Z],
      report: ["report_1"],
      device_info: "iPhone 12",
      pin_info: %{"pinned" => true, "until" => "2024-12-31"},
      data: %{"metadata" => %{"source" => "mobile"}}
    })
  end

  defp sample_erlang_tuple do
    {:post,
     "post_123",
     "ai_post_123",
     "user_123",
     "biz_456",
     "This is a test post",
     ["comment_1", "comment_2"],
     [1, 2, 3],
     ["image1.jpg"],
     "#test",
     "@friend",
     "QmXoYPHfhjsdfkjhsadkfj",
     "😊",
     "https://example.com",
     "testuser",
     ["extra1"],
     ~U[2024-01-01 10:00:00Z],
     ~U[2024-01-02 10:00:00Z],
     ["report_1"],
     "iPhone 12",
     %{"pinned" => true},
     %{"metadata" => %{"source" => "mobile"}}
    }
  end

  describe "changeset/2 with valid data" do
    test "creates valid changeset with required fields only" do
      changeset = Post.changeset(%Post{}, valid_post_attrs())

      assert changeset.valid?
      assert changeset.changes.content == "This is a test post"
    end

    test "creates valid changeset with all fields" do
      changeset = Post.changeset(%Post{}, valid_extended_attrs())

      assert changeset.valid?
      assert changeset.changes.content == "This is a test post"
      assert changeset.changes.ai_post_id == "ai_post_123"
      assert changeset.changes.user_id == "user_123"
      assert changeset.changes.business_id == "biz_456"
      assert changeset.changes.media == ["image1.jpg", "video1.mp4"]
      assert changeset.changes.hashtag == "#test #social"
      assert changeset.changes.mention == "@friend"
      assert changeset.changes.emoji == "😊👍"
      assert changeset.changes.link_url == "https://example.com"
      assert changeset.changes.author == "testuser"
      assert changeset.changes.other == ["extra1", "extra2"]
      assert changeset.changes.comments == ["comment_1", "comment_2"]
      assert changeset.changes.likes == [1, 2, 3]
      assert changeset.changes.profile_tags == ["tag1", "tag2"]
      assert changeset.changes.photo_url == "/images/post.jpg"
      assert changeset.changes.device_info == "iPhone 12"
      assert changeset.changes.pin_info == %{"pinned" => true, "until" => "2024-12-31"}
      assert changeset.changes.data == %{"metadata" => %{"source" => "mobile"}}
    end

    test "sets default values for array fields" do
      changeset = Post.changeset(%Post{}, valid_post_attrs())
      post_struct = Ecto.Changeset.apply_changes(changeset)

      assert post_struct.media == []
      assert post_struct.comments == []
      assert post_struct.likes == []
      assert post_struct.other == []
      assert post_struct.profile_tags == []
      assert post_struct.report == []
    end
  end

  describe "changeset/2 validation errors" do
    test "requires content" do
      attrs = valid_post_attrs() |> Map.delete(:content)
      changeset = Post.changeset(%Post{}, attrs)

      refute changeset.valid?
      assert %{content: ["can't be blank"]} = errors_on(changeset)
    end
  end

  describe "erl_changeset/1" do
    test "converts erlang tuple to changeset successfully" do
      changeset = Post.erl_changeset(sample_erlang_tuple())

      assert changeset.valid?, inspect(errors_on(changeset), label: "Changeset errors")
      changes = changeset.changes
      assert changes.id == "post_123"
      assert changes.content == "This is a test post"
      assert changes.ai_post_id == "ai_post_123"
      assert changes.user_id == "user_123"
      assert changes.business_id == "biz_456"
      assert changes.media == ["image1.jpg"]
      assert changes.hashtag == "#test"
      assert changes.mention == "@friend"
      assert changes.ipns == "QmXoYPHfhjsdfkjhsadkfj"
      assert changes.emoji == "😊"
      assert changes.link_url == "https://example.com"
      assert changes.author == "testuser"
      assert changes.other == ["extra1"]
      assert changes.comments == ["comment_1", "comment_2"]
      assert changes.likes == [1, 2, 3]
      assert changes.device_info == "iPhone 12"
      assert changes.pin_info == %{"pinned" => true}
      assert changes.data == %{"metadata" => %{"source" => "mobile"}}
    end

    test "handles undefined datetime fields" do
      tuple_with_undefined = put_elem(sample_erlang_tuple(), 16, :undefined)
      IO.inspect(tuple_with_undefined, label: "Tuple with undefined date_created")
      changeset = Post.erl_changeset(tuple_with_undefined)

      assert changeset.valid?, inspect(errors_on(changeset), label: "Changeset errors")
      assert changeset.changes.date_created == nil
    end

    test "handles nil comments" do
      tuple_with_nil = put_elem(sample_erlang_tuple(), 6, nil)
      IO.inspect(tuple_with_nil, label: "Tuple with nil comments")
      changeset = Post.erl_changeset(tuple_with_nil)

      assert changeset.valid?, inspect(errors_on(changeset), label: "Changeset errors")
      assert changeset.changes.comments == []
    end

    test "handles non-list likes" do
      tuple_with_invalid_likes = put_elem(sample_erlang_tuple(), 7, :invalid)
      IO.inspect(tuple_with_invalid_likes, label: "Tuple with invalid likes")
      changeset = Post.erl_changeset(tuple_with_invalid_likes)

      assert changeset.valid?, inspect(errors_on(changeset), label: "Changeset errors")
      assert changeset.changes.likes == []
    end
  end

  describe "build/1" do
    test "builds post struct from valid changeset" do
      changeset = Post.changeset(%Post{}, valid_post_attrs())

      assert {:ok, post} = Post.build(changeset)
      assert %Post{} = post
      assert post.content == "This is a test post"
    end

    test "returns error for invalid changeset" do
      invalid_attrs = %{content: ""}
      changeset = Post.changeset(%Post{}, invalid_attrs)

      assert {:error, changeset} = Post.build(changeset)
      refute changeset.valid?
    end
  end

  describe "complex field validations" do
    test "handles array fields correctly" do
      attrs = %{
        content: "Test post",
        media: ["image1.jpg", "image2.jpg"],
        comments: ["comment_1", "comment_2"],
        likes: [1, 2, 3, 4],
        profile_tags: ["tag1", "tag2"],
        other: ["extra1", "extra2"],
        report: ["report_1"]
      }

      changeset = Post.changeset(%Post{}, attrs)
      assert changeset.valid?

      {:ok, post} = Post.build(changeset)
      assert length(post.media) == 2
      assert length(post.comments) == 2
      assert length(post.likes) == 4
      assert length(post.profile_tags) == 2
      assert length(post.other) == 2
      assert length(post.report) == 1
    end

    test "handles map fields correctly" do
      attrs = %{
        content: "Test post",
        pin_info: %{"pinned" => true, "until" => "2024-12-31"},
        data: %{"metadata" => %{"source" => "web", "version" => "1.0"}}
      }

      changeset = Post.changeset(%Post{}, attrs)
      assert changeset.valid?

      {:ok, post} = Post.build(changeset)
      assert post.pin_info["pinned"] == true
      assert post.data["metadata"]["source"] == "web"
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
