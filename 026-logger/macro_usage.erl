-module(macro_usage).
-include_lib("kernel/include/logger.hrl").
-export([log_error/1]).

log_error(Mensaje) ->
    ?LOG_ERROR(Mensaje),
   ok.
