%%%------- Club Records -------%%%
-record(club, {
    id,
    name,
    owner,
    admins = [],
    members = [],
    private = true,
    type = []
}).

-record(ai_club, {
    id,
    avatar,
    subjects = [],
    channels = []
}).

-record(art_club, {
    id,
    avatar,
    subjects = [],
    channels = []
}).

-record(book_club, {
    id,
    avatar,
    subjects = [],
    channels = []
}).

-record(medical_club, {
    id, 
    avatar,
    subjects = [],
    channels = []
}).

-record(space_club, {
    id,
    avatar,
    subjects = [],
    channels = []
}).

-record(tech_club, {
    id,
    avatar,
    subjects = [],
    channels = []
}).

