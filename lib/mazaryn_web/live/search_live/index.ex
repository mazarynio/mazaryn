defmodule MazarynWeb.SearchLive.Index do
  use MazarynWeb, :live_view

  alias Account.Users

  alias Account.User
  alias Mazaryn.Repo

  @impl true
  def mount(_params, %{"user_id" => email} = _session, socket) do
    {:ok, user} = user_info(email)

    found_users = [
      %Account.User{
      avatar_url: "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0f/Zoe_Saldana_by_Gage_Skidmore_2.jpg/440px-Zoe_Saldana_by_Gage_Skidmore_2.jpg",
      blocked: [],
      country: nil,
      date_created: nil,
      date_updated: nil,
      email: "codepawn@icloud.com",
      follower: [],
      following: [],
      id: 3,
      media: ["@manga"],
      notifications: [],
      other_info: %{},
      password: "$2a$12$UT8A6gmH/6Vs2JfD.Zn30e5rMFBj5kTTYuHZ2Jv5DXLVH5EmrjU3u",
      posts: ["e4a22767-368f-48b9-a4d2-ddeea8fc0a0f",
       "f0d08917-395f-416a-9e9f-d4b44023b34c"],
      private: false,
      saved_posts: [],
      username: "foobar"
    },
    %Account.User{
      avatar_url: "https://www.looper.com/img/gallery/20-epic-movies-like-avatar-you-need-to-watch-next/l-intro-1645555067.jpg",
      blocked: [],
      country: nil,
      date_created: nil,
      date_updated: nil,
      email: "codepawn@icloud.com",
      follower: [],
      following: [],
      id: "2dc564ed-29f8-429a-8d8e-c3db20ca7bdf",
      media: ["@anima"],
      notifications: [],
      other_info: %{},
      password: "$2a$12$UT8A6gmH/6Vs2JfD.Zn30e5rMFBj5kTTYuHZ2Jv5DXLVH5EmrjU3u",
      posts: ["e4a22767-368f-48b9-a4d2-ddeea8fc0a0f",
       "f0d08917-395f-416a-9e9f-d4b44023b34c"],
      private: false,
      saved_posts: [],
      username: "foobar"
    },
    %Account.User{
      avatar_url: "https://www.looper.com/img/gallery/20-epic-movies-like-avatar-you-need-to-watch-next/l-intro-1645555067.jpg",
      blocked: [],
      country: nil,
      date_created: nil,
      date_updated: nil,
      email: "codepawn@icloud.com",
      follower: [],
      following: [],
      id: 9,
      media: ["@anima"],
      notifications: [],
      other_info: %{},
      password: "$2a$12$UT8A6gmH/6Vs2JfD.Zn30e5rMFBj5kTTYuHZ2Jv5DXLVH5EmrjU3u",
      posts: ["e4a22767-368f-48b9-a4d2-ddeea8fc0a0f",
       "f0d08917-395f-416a-9e9f-d4b44023b34c"],
      private: false,
      saved_posts: [],
      username: "foobar"
    },
    %Account.User{
      avatar_url: "https://www.looper.com/img/gallery/20-epic-movies-like-avatar-you-need-to-watch-next/l-intro-1645555067.jpg",
      blocked: [],
      country: nil,
      date_created: nil,
      date_updated: nil,
      email: "codepawn@icloud.com",
      follower: [],
      following: [],
      id: 5,
      media: ["@anima"],
      notifications: [],
      other_info: %{},
      password: "$2a$12$UT8A6gmH/6Vs2JfD.Zn30e5rMFBj5kTTYuHZ2Jv5DXLVH5EmrjU3u",
      posts: ["e4a22767-368f-48b9-a4d2-ddeea8fc0a0f",
       "f0d08917-395f-416a-9e9f-d4b44023b34c"],
      private: false,
      saved_posts: [],
      username: "foobar"
    }
  ]

    socket =
      socket
      |> assign(:user, user)
      |> assign(:search, [])
      |> assign(:found_users, found_users)

    {:ok, socket}
  end

  defp user_info(email) do
    Users.one_by_email(email)
  end
end
