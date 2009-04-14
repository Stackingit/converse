%%=====================================================
%% Abstract
%%
%% This reprsents a client that sends mails and recives
%% mails.
%% The client will maily be a push but clearly there 
%% must be an option to pull ( why wait if you want it
%% now!
%%
%% The more I think about it the less I think mail 
%% shold be a push. Pull FTW
%%=====================================================

%%======================================================
%% Tag the file
%%======================================================
-module( converseClient ).
-author("Stephen Bailey").
-email("Stephen.Bailey@stackingit.com").
-vsn( "0.0.0.1" ).
-export( [start/0] ).


-compile(export_all ).

%%=====================================================
%% Get the client up and waiting for any messagse
%%=====================================================
start() ->
    %% I dont bother to register becuase I will always
    %% b hitting the server so it will have my PID
    spawn_link( fun() -> ?MODULE:loop() end ).

%%=====================================================
%% Get the client up and waiting for any messagse with 
%% debugging help on
%%=====================================================
debugStart()->
    tracer:trace([?MODULE],[c.m]),
    start().
	
%%=====================================================
%% waiting for messages
%%=====================================================
loop() ->
    receive 
        { newConversation, Author, Listeners, Subject, Message, MessageId} -> 
            showConversation( Author, Listeners, Subject, Message, MessageId );
        { newConversationCreated, ok } ->
            mail_Successfully_created
            
    end.

%%=====================================================
%% This is the tuple need to send a message to the 
%% server. This just give me one nice place to change
%% the node when this is deployed... not that by then
%5 there will not be a better way :-)
%%=====================================================
sendServer()->
    {converse_server, 'converse_Server@Steve-eee'}.

%%=====================================================
%% Send a new message to the Listeners
%%=====================================================
startConversation( User, Subject, Message, Listeners )->
    sendServer() ! { startConversation, self(), User, Subject, Message, Listeners }.

%%=====================================================
%% Connect to the server so it knows to talk to me
%% when there is a converstation that I am involved in
%%=====================================================
connect() ->
    todo.

%%=====================================================
%% Ask the server is there is any more mail for me, it 
%% should send aback a nice newMail message
%%=====================================================
checkConversations( User ) ->
    server ! { checkConversations, self(), User }.

%%=====================================================
%% Send a reply to a current conversation.
%% Eiher just send the reply or send the reply and 
%% adjust the listeners ( wow I have to think about 
%% that concept a bit ?? )
%%=====================================================
reply( ConversationId, Reply ) ->
    reply( ConversationId, Reply, [], [] ).
    
reply( ConversationId, Reply, NewListeners ) ->
    reply( ConversationId, Reply, NewListeners, [] );
reply( ConversationId, Reply, RemoveListeners ) ->
    reply( ConversationId, Reply, [], RemoveListeners ).
    
reply( ConversationId, Reply, NewListeners, RemoveListeners ) ->
    todo.
	
%%=====================================================
%% Getout of this converstation
%%=====================================================
optOut( ConversationId ) ->
    todo.

%%=====================================================
%% Getout of this converstation, but still listen
%%=====================================================
llistenOnly( ConversationId ) ->
    todo.

%%=====================================================
%% Some Prettyness to make this look like a client
%%=====================================================
showConversation( Author, Listeners, Subject, Message, ConversationId ) ->
    io:format( "MessageId: ~p~n" , [ ConversationId ] ),
    io:format( "Subject : ~s~n" , [ Subject ] ),
    io:format( "From : ~s~n" , [ Author ] ),
    io:format( "Listeners:"),
    [ io:format( "~s ", [Name] ) || Name <- Listeners ],        
    io:format( "~n ~s ~n" , [ Message ] ).

%%=====================================================
%% Helpful tset methods to save some typing
%%=====================================================
test_showConversation() ->
    showConversation( "Steve", [ "Sue@sue","joe@way" ], "Some Subject", "Some Message", 4).
