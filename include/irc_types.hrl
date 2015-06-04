-record(irc_notice, {server,
                     type,
                     message}).

-record(irc_ping, {response}).

-record(irc_privmsg, {nick,
                      user,
                      channel,
                      message}).

-record(irc_unknown, {message}).
