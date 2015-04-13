-module(collection_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([hello_to_html/2]).

init(Req, Opts) ->
    io:format("Req: ~p\n", [Req]),
    %[LogLink] = Opts,
    %LogLink ! {"Named: ~s\n", ["named"]},
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, hello_to_html}
     ], Req, State}.

hello_to_html(Req, State) ->
    Body = "{\"Collection\": true}",
	{Body, Req, State}.
