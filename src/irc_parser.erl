-module(irc_parser).

-include("irc_types.hrl").

-export([parse_message/1]).

space(X) -> X =/= $ .

parse_message([$:|Msg]) ->
    {Server, [_|Msg1]} = lists:splitwith(fun space/1, Msg),
    {Type, [_|Msg2]} = lists:splitwith(fun space/1, Msg1),
    case Type of
        "NOTICE" -> handle_notice(Server, Msg2);
        _        -> #irc_unknown{message=Msg}
    end;
parse_message([$P,$I,$N,$G,_|Resp]) -> #irc_ping{response=Resp};
parse_message(Msg) -> #irc_unknown{message=Msg}.

handle_notice(Server, Msg) ->
    {Type, [_,$:|Msg1]} = lists:splitwith(fun space/1, Msg),
    #irc_notice{server=Server, type=Type, message=Msg1}.
