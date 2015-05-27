-record(connection, {server_name :: binary(),
                     port        :: integer(),
                     status      :: connected | connecting | disconnected,
                     nick        :: binary()}).
-type connection() :: #connection{}.
