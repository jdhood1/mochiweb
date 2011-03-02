%% @author Bob Ippolito <bob@mochimedia.com>
%% @author Brad Anderson <brad@sankatygroup.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc RTSP server.

-module(mochiweb_rtsp).
-author('bob@mochimedia.com').
-author('brad@sankatygroup.com').
-export([start/0, start/1, stop/0, stop/1]).
-export([loop/2, default_body/1]).
-export([after_response/2, reentry/1]).
-export([parse_range_request/1, range_skip_length/2]).

-define(REQUEST_RECV_TIMEOUT, 300000).   % timeout waiting for request line
-define(HEADERS_RECV_TIMEOUT, 30000). % timeout waiting for headers

-define(MAX_HEADERS, 1000).
-define(DEFAULTS, [{name, ?MODULE},
                   {port, 8888}]).

-include_lib("eunit/include/eunit.hrl").

parse_options(Options) ->
  {loop, HttpLoop} = proplists:lookup(loop, Options),
  Loop = fun (S) ->
             ?MODULE:loop(S, HttpLoop)
         end,
  Options1 = [{loop, Loop} | proplists:delete(loop, Options)],
  mochilists:set_defaults(?DEFAULTS, Options1).

stop() ->
  mochiweb_socket_server:stop(?MODULE).

stop(Name) ->
  mochiweb_socket_server:stop(Name).

start() ->
  start([{ip, "127.0.0.1"},
         {loop, {?MODULE, default_body}}]).

%% @spec start(Options) -> ServerRet
%%     Options = [option()]
%%     Option = {name, atom()} | {ip, string() | tuple()} | {backlog, integer()}
%%              | {nodelay, boolean()} | {acceptor_pool_size, integer()}
%%              | {ssl, boolean()} | {profile_fun, undefined | (Props) -> ok}
%% @doc Start a mochiweb server.
%%      profile_fun is used to profile accept timing.
%%      After each accept, if defined, profile_fun is called with a proplist of
%%      a subset of the mochiweb_socket_server state and timing information.
%%      The proplist is as follows: [{name, Name}, {port, Port},
%%      {active_sockets, ActiveSockets}, {timing, Timing}].
%% @end
start(Options) ->
  mochiweb_socket_server:start(parse_options(Options)).

frm(Body) ->
    ["<html><head></head><body>"
     "<form method=\"POST\">"
     "<input type=\"hidden\" value=\"message\" name=\"hidden\"/>"
     "<input type=\"submit\" value=\"regular POST\">"
     "</form>"
     "<br />"
     "<form method=\"POST\" enctype=\"multipart/form-data\""
     " action=\"/multipart\">"
     "<input type=\"hidden\" value=\"multipart message\" name=\"hidden\"/>"
     "<input type=\"file\" name=\"file\"/>"
     "<input type=\"submit\" value=\"multipart POST\" />"
     "</form>"
     "<pre>", Body, "</pre>"
     "</body></html>"].

default_body(Req, _Method, _Path) ->
    Req:respond({501, [], []}).

default_body(Req) ->
    default_body(Req, Req:get(method), Req:get(path)).

loop(Socket, Body) ->
    %mochiweb_socket:setopts(Socket, []),
    request(Socket, Body).

request(Socket, Body) ->
  mochiweb_socket:setopts(Socket, [{active, once}]),
  receive
    {tcp, _Port, BinMessage} when is_binary(BinMessage) ->
      Resp0 = binary_to_list(BinMessage),
      Resp1 = rtsp_response:strip_cr(Resp0),
      case parse_msg(Resp1) of
        {rtsp, Method, Path} ->
          {HeadersPL, _BodyPL} = rtsp_response:process_response(Resp1),
          % TODO: use mochiweb_util:urlsplit and do more than abs_path
          Req = new_request(Socket,
                            {Method, {abs_path, Path}, {1,0}},
                            HeadersPL),
          call_body(Body, Req),
          ?MODULE:after_response(Body, Req);
        Other ->
          ?debugFmt("receive invalid request~n~p~n", [Other]),
          handle_invalid_request(Socket)
      end;
    {tcp_closed, _} ->
      mochiweb_socket:close(Socket),
      exit(normal);
    Other ->
      io:format("receive invalid request~n~p~n", [Other]),
      handle_invalid_request(Socket)
  after ?REQUEST_RECV_TIMEOUT ->
      mochiweb_socket:close(Socket),
      exit(normal)
  end.

reentry(Body) ->
    fun (Req) ->
            ?MODULE:after_response(Body, Req)
    end.

parse_msg(Msg) ->
  try
    [FirstLine, _Payload] = string:tokens(Msg, "\n"),
    [Method, Path, "RTSP/1.0"] = string:tokens(FirstLine, " "),
    {rtsp, list_to_atom(Method), Path}
  catch
    _:_ -> invalid_request
  end.

call_body({M, F, A}, Req) ->
    erlang:apply(M, F, [Req | A]);
call_body({M, F}, Req) ->
    M:F(Req);
call_body(Body, Req) ->
    Body(Req).

handle_invalid_request(Socket) ->
    handle_invalid_request(Socket, {'GET', {abs_path, "/"}, {0,9}}, []),
    exit(normal).

handle_invalid_request(Socket, Request, RevHeaders) ->
    Req = new_request(Socket, Request, RevHeaders),
    Req:respond({400, [], []}),
    mochiweb_socket:close(Socket),
    exit(normal).

%% TODO: use mochiweb_util:urlsplit and do more than just abs_path
new_request(Socket, {Method, {abs_path, Uri}, Version}, RevHeaders) ->
    mochiweb_socket:setopts(Socket, [{packet, raw}]),
    mochiweb_rtsp_request:new(Socket,
                              Method,
                              Uri,
                              Version,
                              mochiweb_headers:make(lists:reverse(RevHeaders))).

after_response(Body, Req) ->
    Socket = Req:get(socket),
    case Req:should_close() of
        true ->
            mochiweb_socket:close(Socket),
            exit(normal);
        false ->
            Req:cleanup(),
            ?MODULE:loop(Socket, Body)
    end.

parse_range_request("bytes=0-") ->
    undefined;
parse_range_request(RawRange) when is_list(RawRange) ->
    try
        "bytes=" ++ RangeString = RawRange,
        Ranges = string:tokens(RangeString, ","),
        lists:map(fun ("-" ++ V)  ->
                          {none, list_to_integer(V)};
                      (R) ->
                          case string:tokens(R, "-") of
                              [S1, S2] ->
                                  {list_to_integer(S1), list_to_integer(S2)};
                              [S] ->
                                  {list_to_integer(S), none}
                          end
                  end,
                  Ranges)
    catch
        _:_ ->
            fail
    end.

range_skip_length(Spec, Size) ->
    case Spec of
        {none, R} when R =< Size, R >= 0 ->
            {Size - R, R};
        {none, _OutOfRange} ->
            {0, Size};
        {R, none} when R >= 0, R < Size ->
            {R, Size - R};
        {_OutOfRange, none} ->
            invalid_range;
        {Start, End} when 0 =< Start, Start =< End, End < Size ->
            {Start, End - Start + 1};
        {_OutOfRange, _End} ->
            invalid_range
    end.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

range_test() ->
    %% valid, single ranges
    ?assertEqual([{20, 30}], parse_range_request("bytes=20-30")),
    ?assertEqual([{20, none}], parse_range_request("bytes=20-")),
    ?assertEqual([{none, 20}], parse_range_request("bytes=-20")),

    %% trivial single range
    ?assertEqual(undefined, parse_range_request("bytes=0-")),

    %% invalid, single ranges
    ?assertEqual(fail, parse_range_request("")),
    ?assertEqual(fail, parse_range_request("garbage")),
    ?assertEqual(fail, parse_range_request("bytes=-20-30")),

    %% valid, multiple range
    ?assertEqual(
       [{20, 30}, {50, 100}, {110, 200}],
       parse_range_request("bytes=20-30,50-100,110-200")),
    ?assertEqual(
       [{20, none}, {50, 100}, {none, 200}],
       parse_range_request("bytes=20-,50-100,-200")),

    %% no ranges
    ?assertEqual([], parse_range_request("bytes=")),
    ok.

range_skip_length_test() ->
    Body = <<"012345678901234567890123456789012345678901234567890123456789">>,
    BodySize = byte_size(Body), %% 60
    BodySize = 60,

    %% these values assume BodySize =:= 60
    ?assertEqual({1,9}, range_skip_length({1,9}, BodySize)), %% 1-9
    ?assertEqual({10,10}, range_skip_length({10,19}, BodySize)), %% 10-19
    ?assertEqual({40, 20}, range_skip_length({none, 20}, BodySize)), %% -20
    ?assertEqual({30, 30}, range_skip_length({30, none}, BodySize)), %% 30-

    %% valid edge cases for range_skip_length
    ?assertEqual({BodySize, 0}, range_skip_length({none, 0}, BodySize)),
    ?assertEqual({0, BodySize}, range_skip_length({none, BodySize}, BodySize)),
    ?assertEqual({0, BodySize}, range_skip_length({0, none}, BodySize)),
    BodySizeLess1 = BodySize - 1,
    ?assertEqual({BodySizeLess1, 1},
                 range_skip_length({BodySize - 1, none}, BodySize)),

    %% out of range, return whole thing
    ?assertEqual({0, BodySize},
                 range_skip_length({none, BodySize + 1}, BodySize)),
    ?assertEqual({0, BodySize},
                 range_skip_length({none, -1}, BodySize)),

    %% invalid ranges
    ?assertEqual(invalid_range,
                 range_skip_length({-1, 30}, BodySize)),
    ?assertEqual(invalid_range,
                 range_skip_length({0, BodySize + 1}, BodySize)),
    ?assertEqual(invalid_range,
                 range_skip_length({-1, BodySize + 1}, BodySize)),
    ?assertEqual(invalid_range,
                 range_skip_length({BodySize, 40}, BodySize)),
    ?assertEqual(invalid_range,
                 range_skip_length({-1, none}, BodySize)),
    ?assertEqual(invalid_range,
                 range_skip_length({BodySize, none}, BodySize)),
    ok.

-endif.
