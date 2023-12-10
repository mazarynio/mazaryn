%%%------- Club Records -------%%%
-record(club, {
    id,
    name,
    owner,
    admins = [],
    members = [],
    type = []
}).

-record(book_club, {
    id,
    avatar,
    subjects = []
}).

-record(ai_club, {
    id,
    avatar,
    subjects = []
}).