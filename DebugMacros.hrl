%% nice simple way to stub a method and not get any warnings, and also get
%% it to shout if I call it
-define(TODO(X),
    ( begin
        io:format("*** TODO ~p ~p ~p~n",[?MODULE, ?LINE, X]),
        exit( todo )
    end ) 
).
