:- use_module(library(http/websocket)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(lib/ws, [ on_connect/1 ]).

:- http_handler(root(.), http_reply_from_files('./public', []), [prefix]).

:- http_handler(
    root(ws),
    http_upgrade_to_websocket(reg, []),
    [spawn([])]
  ).

:- initialization
  http_server(http_dispatch, [port(4080)]).
