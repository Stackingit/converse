%%=====================================================
%% Abstract
%%
%% This does the basic tests on the converseDB module
%%=====================================================

%%======================================================
%% Tag the file
%%======================================================
-module( converseDB_tests ).
-include_lib("eunit/include/eunit.hrl").
-compile( export_all ).
-author("Stephen Bailey").
-email("Stephen.Bailey@stackingit.com").
-vsn( "0.0.0.1" ).


%%======================================================
%% setup state: I know there is a way to do this more
%% formally, but for now I will go the simple way since
%% I have not internet access.. plus should help me 
%% understand when I do get to it :-)
%%======================================================
setup_test()->
    mnesia:stop(),                      % make sure there is not one running already
    mnesia:delete_schema( node() ),      % make sure the DB does not exist
    converseDB:createDB(),              % create a DB for this node 
    converseDB:startDB().               % create the structure     

%%======================================================
%% Check the adding of a user
%%======================================================
addingAUser_test()->
    { user, doesNotExist } = converseDB:validateUser("User1"),
    converseDB:addUser("User1","Password1"),
    {user,exists} = converseDB:validateUser("User1").
    
%%======================================================
%% Clean up my scafolding DB
%%======================================================    
shutdown_test()->
    mnesia:stop(),
    mnesia:delete_schema( [ node() ] ).
    