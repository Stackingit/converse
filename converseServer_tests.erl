%%=====================================================
%% Abstract
%%
%%=====================================================

%%======================================================
%% Tag the file
%%======================================================
-module(converseServer_tests).
-include_lib("eunit/include/eunit.hrl").
-author("Stephen Bailey").
-email("Stephen.Bailey@stackingit.com").
-vsn( "0.0.0.1" ).


        
%have a tests that will break once I update it, so that it reminds me to update the test
%well we will see it if works ;-)
checkMail_test() ->
    todo = converseServer:checkMail( todo ).
    
optOut_test() ->
    todo = converseServer:optOut( todo, todo ).
    


