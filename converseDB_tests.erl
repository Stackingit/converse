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
%% setup scaffolding Mnesia DB
%%
%% I know there is a way to do this more
%% formally, but for now I will go the simple way since
%% I have not internet access.. plus should help me 
%% understand when I do get to it :-)
%%======================================================
setup_test()->
    mnesia:stop(),                      % make sure there is not one running already
    mnesia:delete_schema( [ node() ] ), % make sure the DB does not exist
    converseDB:createDB(),              % create a DB for this node 
    converseDB:startDB().               % create the structure     

%%======================================================
%% Check the adding of a user
%%======================================================
addingAUser_test()->
    { user, does_not_exist } = converseDB:validateUser("User1"), % validate the user does not already exist
    { user, ok } = converseDB:addUser("User1","Password1"),      % add him sucessfully
    { user, exists } = converseDB:validateUser("User1").         % check that he is indeed there

addingAUserThatAlreadyExists_test()->
    { user, exists } = converseDB:validateUser("User1"),                % validate our user is still there
    { user, already_exists } = converseDB:addUser("User1","Password1"). % validate you cannot add an existing user

%%======================================================
%% Check the Password validation
%%======================================================
validatePassword_test() ->
    { validate, pass } = converseDB:validatePassword("User1","Password1"),
    { validate, fail } = converseDB:validatePassword("User1","Not the Password").
    
%%======================================================
%% Check the adding of a conversation
%%======================================================
%% testing when everything is perfect in the world
startingAConversation_test()->
    % create a user to add as a happy talker
    converseDB:addUser("User2","Password2"),                             
    % add a message
    { conversation, ok } = converseDB:addConversation("User1","Password1", "Subject1","Message1",["User2"]), 
    %% check the message
    ActiveConversations = converseDB:getActiveConversations("User2","Password2"),
    %% should be only 1 talking message and no listening
    {{talking,[ConversationId]},{listening,[]}} = ActiveConversations,
    %%Check the conversation
    {_,"User1","Subject1","Message1",Talkers,[],_} = converseDB:getConversation( ConversationId ).
    
%%======================================================
%% Clean up my scafolding Mnesia DB
%%======================================================    
shutdown_test()->
    mnesia:stop(),
    mnesia:delete_schema( [ node() ] ).
    