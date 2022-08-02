# defmodule Post do
#   @moduledoc """
#   Post Struct
#   """

#   defstruct id: nil,
#             media: [],
#             content: nil,
#             likes_count: 0,
#             gif_url: nil,
#             removed: nil,
#             pinned: nil,
#             profile_tags: nil,
#             user: nil,
#             likes: [],
#             comments: [],
#             author: nil,
#             date_created: nil

#   def new(
#         {:post, id, content, media, likes_count, gif_url, removed, pinned, profile_tags, user,
#          likes, comments, author, date_created}
#       ) do
#     struct(Post, %{
#       id: id,
#       content: content,
#       media: media,
#       likes_count: likes_count,
#       gif_url: gif_url,
#       removed: removed,
#       pinned: pinned,
#       profile_tags: profile_tags,
#       user: user,
#       likes: likes,
#       comments: comments,
#       author: author,
#       date_created: date_created
#     })
#   end
# end
