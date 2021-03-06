%%=====================================================
%% Abstract
%%
%% A mail server that allow you to opt out of a mail
%% trail.. you know kind of like a real conversation
%% when you can leave when you want... okay so I cant
%% help with the fact that you may feel rude but hey
%% this is just software :-)
%%
%%=====================================================

%%======================================================
%% Tag the file
%%======================================================
-module(converseServer).
-compile(export_all).
-author("Stephen Bailey").
-email("Stephen.Bailey@stackingit.com").
-vsn( "0.0.0.1" ).
-export( [start/0] ).
-include( "DebugMacros.hrl" ).

%%=====================================================
%% Start and register the process
%%=====================================================
start() ->
    register( converse_server, spawn_link( fun() -> loop() end ) ).

%%=====================================================
%% This is here for a quick way to start
%% everything to help with debugging
%% NOTE : this will NUKE your DB so be careful
%%=====================================================
debugStart()->    
    tracer:trace([?MODULE],[c]),
    converseDB:debugStart(),
    start().

%%=====================================================
%% Main Message loop
%%=====================================================
loop() ->
    receive
        { startConversation, PID, User, Subject, Message, Listeners } ->
            PID ! createNewMail( User, Listeners, Subject, Message ),
            ?MODULE:loop();
        { checkConversations, PID, User } ->
            PID ! getConversations( User ),
            ?MODULE:loop()
    end.

%%=====================================================
%% See if there are any mails that the client has waiting for him
%%=====================================================
getConversations( User ) ->
    converseDB:getConversation( User ).
    
%%=====================================================
%% Find that message and make sure this persons is removed from that mail trail 
%% in future ( I guess there is nothing stopping someone adding them back in )
%%=====================================================
optOut( MessageId, ClientPid ) ->
    ?TODO( { optOut, MessageId, ClientPid } ).
	
%%=====================================================
%% Make a mail and store it for each of the listeners
%% get a unique number for it
%%=====================================================
createNewMail( Author, Listeners, Subject, Message ) ->		
    %~ {ok, File} = file:open( integer_to_list( getNewMessageID() ), [write] ),
    %~ io:format(File, "~p\n", [ { {author,Author}, {listeners,Listeners}, {subject,Subject}, {messsage,Message} } ] ),	
    %~ file:close(File).
    converseDB:addConversation( Author, Subject, Message, Listeners ),
    { newConversationCreated, ok }.
	
%%=====================================================        
%% Note, you cannot remove people, they must opt out if they dont want to play anymore
%%=====================================================
replyMail( AuthorPid, MessageID, AddListeners,  Message  )->
    ?TODO( { replyMail, AuthorPid, MessageID, AddListeners,  Message }  ).

%%=====================================================
%% gets a new ID super incrementing
%%=====================================================
getNewMessageID() ->			
    case file:consult("MessageIdFile") of
            {ok, [{Id}]} -> 
                    saveNewMessageId( Id+1 ); 			
            _ -> 			
                    saveNewMessageId( 1 )			
    end.	

%%=====================================================
%% Do the udpate and store the "state" on disk
%%=====================================================
saveNewMessageId( Id ) ->
    {ok, File} = file:open("MessageIdFile", write),			
    io:format( File, "~p.\n", [{Id}] ),
    file:close(File),
    Id.
	
        