%-define (DEBUGLOGGING, 1).

-ifdef (DEBUGLOGGING).
-define(DEBUGLOG(Fmt, List), io:format(Fmt, List)).
-else.
-define(DEBUGLOG(Fmt, List), ok).
-endif.
