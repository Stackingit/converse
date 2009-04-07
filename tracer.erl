%%=====================================================
%% Abstract
%%
%% This bascially give a way to start tracking on a list
%% of modules as approprite
%%=====================================================

%%======================================================
%% Tag the file
%%======================================================
-module( tracer ).
-compile( export_all ).
-author("Stephen Bailey").
-email("Stephen.Bailey@stackingit.com").
-vsn( "0.0.0.1" ).
-include_lib("stdlib/include/ms_transform.hrl"). %% Here for the tracing

%%=====================================================
%% Starts and outputs tracer methods for modules
%%=====================================================
trace( Modules, Flags ) ->
    % starts the tracer
    dbg:tracer(),
    % module, function (or '_' for everything). We're not really sure about dbg:fun2ms - some sort of black magic to format the output. 
    [ dbg:tpl( Module , '_' , '_', dbg:fun2ms( fun(_)-> return_trace() end ) ) || Module <- Modules ], 	
     % this registers what we're interested in, c = "calls". s = "send", r = "receive", m = "send/receive"
    dbg:p(all, Flags ).	