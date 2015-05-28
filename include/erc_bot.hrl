-record(connection, {host    :: string(),
                     port    :: integer(),
                     status  :: connected | connecting | disconnected,
                     nick    :: string()}).
-type connection() :: #connection{}.
