-define(TAG_OK, ok).
-define(TAG_ERROR, error).
-define(TAG_DATA, data).
-define(TAG_CALL, call).
-define(TAG_NODE, node).

-define(EJMSGREF(Pid,Ref), {Pid,Ref}).
-define(EJMSG(Ref,Tag,Body), {self(), Ref, {Tag, Body}}).
-define(EJMSGPART(Key, Value), {Key, Value}).