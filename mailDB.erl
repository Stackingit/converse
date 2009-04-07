%%=====================================================
%% Abstract
%%
%% This is the module that abstracts away the DB layer
%% and make the inforamtion available from the ether
%%
%% TODO: Use Mnesia now to learn it, but next look to
%%       use CouchDB... not sure if it adds any real 
%%       value, but intersting none the less to try.
%%======================================================

%%======================================================
%% Tag the file
%%======================================================
-module(mailDB).
-compile(export_all).
-author("Stephen Bailey").
-email("Stephen.Bailey@stackingit.com").
-vsn( "0.0.0.1" ).
%% get the query list comprehension tied in so we can use it
-include_lib("stdlib/include/qlc.hrl").

%%=====================================================
%% Defines the DB data
%%
%% TODO : Find out how to put this in a hrl file and 
%%        referenced
%%======================================================
-record( conversation, { 
            id,         %% unique primary Key 
            author,     %% User : The person that started this %% information about the mail object
            subject,    %% string : heading for the conversation
            message,    %% string : the content 
            listeners,  %% [User,User,...] of listeners.
            time        %% when the message was created 
        } ).


%%=====================================================
%% Create the DB for the First Time.. this should be
%% called only once
%%=====================================================
createDB() ->
    mnesia:create_schema( [ node() ] ).    
    
%%=====================================================
%% Start our DB
%% We are only using disk we we might not need to go fast, and we wil use up space
%% quite quickly being mail.. in time I can look at some cunning way to pull the current
%% conversation into memeory since they will be active and the old ones archived off, but for 
%% now I just need to get stuff working :-)
%%=====================================================
startDB() ->
    mnesia:start(),
    %% Okay we are going to make this table a bag so that I can have
    %% multiple "mails" per conversation
    mnesia:create_table( conversation, [ { attributes, record_info( fields, conversation ) } , 
                                         { disc_only_copies, [ node() ] },
                                         { type, bag } ] ).    

%%=====================================================
%% Save a new Conversation
%%=====================================================
addConversation( Author, Subject, Message, Listeners ) ->
    %% Make a new unique conversation record
    Conversation = #conversation{ id={now(),node()}, author=Author , subject=Subject, message=Message, listeners=Listeners, time=now() },
    %% Make the act of saving it a method
    F = fun() ->
            mnesia:write( Conversation )
        end,
    %% Perform the save in a transaction
    mnesia:transaction(F).    
    
    

%%=====================================================
%% Retrieve a Conversation by ConversationID
%%=====================================================
getConversation( ConversationID ) ->
    %% Get the conversation started with this ID
    F = fun() ->
            %%NOTE: qlc:q gives you a handle to the query
            %%      qlc:e gives you all the answers
            qlc:e( qlc:q( [ X || X <- mnesia:table( conversation ), X#conversation.id =:= ConversationID ] ) ) 
        end,
    {atomic, Val } = mnesia:transaction( F ),
    Val.
    
%%=====================================================
%% Get any new or active conversations for the user
%%=====================================================
getActiveConversations( User ) ->
    % look through the DB to find any conversations that 
    % this user is still involved in and return them all
    todo.

%%=====================================================
%% Set up a debugging environment
%%=====================================================
startDebug() ->
    %% get a tracer up for just calls
    tracer:trace([?MODULE],[c]),
    %% sort out the Mnesia DB
    mnesia:stop(),      % make sure there is not one running already
    createDB(),         % create a DB for this node 
    startDB(),          % create the structure        
    mnesia:clear_table( conversation ), % nuke any data if we already had a DB set up
    %%put in test data for the DB
    addConversation( "Stephen", "Subject", "Some interseting Message", [ "Sue" , "bob" ] ),
    addConversation( "Stephen", "test", "Testing", [ "Sue" ] ),
    %% get a viewing tool up to look at the DB data
    tv:start().
    
    
    