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
    author,     %% UserID : The person that started this %% information about the mail object
    subject,    %% string : heading for the conversation
    message,    %% string : the content 
    talkers,     %% [userID,userID,...] people still active in the convesation
    listeners,  %% [UserID,UserID,...] of listeners not active in the conversation
    time        %% when the message was created 
} ).

%% Details about the user
-record( user, {
    userId,                     %% primary key for the user
    name,                       %% the person
    password                    %% umm duh the password     
} ).

%% This will only keep the current information, to do history  trauls, you need to hunt
%% down the conversations.. since this is a less likely use case
%%
%% This will be a bag table so that the primary key is user, but each
%% listening and following mail is unqiue.
-record( userConversation, {
    userId,
    talkingConversation,        %% an active conversation ID
    listeningConversation       %% an listening conversation ID
} ).

%%=====================================================
%% this just returns a nice list of all of our tables
%% so if I need to do anything with all of htem like nuke
%% them for debug, I only have to update in one place
%%=====================================================
tables() ->
    [ user, conversation, userConversation ].

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
                                         { type, set } ] ),
                                             
    %% now for the user table
    %% we make this a set so that only userid is unique 
    mnesia:create_table( user, [ { attributes, record_info( fields, user ) } , 
                                 { disc_only_copies, [ node() ] },
                                 { type, set } ] ),
                                 
    %% now for the userConversation table
    %% we make this a bag so the userid is the key for all "actvie conversations" 
    mnesia:create_table( userConversation, [ { attributes, record_info( fields, userConversation ) } , 
                                             { disc_only_copies, [ node() ] },
                                             { type, bag } ] ).

%%=====================================================
%% Add a new user 
%%=====================================================
addUser( Name, Password ) ->
    UserRecord = #user{ userId={now(),node()} , name=Name, password=Password },
    F = fun() ->
            mnesia:write( UserRecord )
        end,
    mnesia:transaction( F ).

%%=====================================================
%% Save a new Conversation
%%=====================================================
addConversation( Author, Subject, Message, Talkers ) ->
    %% right now we are in a catch 22 we need the userid and the conversation ID but we only get these
    %% after each other.
 
    %%Get our authors ID
    AuthorId = getUserId( Author ),
    
    %% Get the userid for these people in the mail
    TalkerUserIds = getUserIds( Talkers ),
    
    %%Define a unique conversationID
    ConversationId = {now(),node()},
    
    %% Make a new unique conversation record
    Conversation = #conversation{ id=ConversationId, 
                                  author=AuthorId , 
                                  subject=Subject, 
                                  message=Message, 
                                  talkers=TalkerUserIds, 
                                  time=now() },
                                  
    %% Now we need to create records for each of our people
    %% involved in this conversation... since the author is just
    %% a talker who is likely to opt out last, we can add this
    %% in there now
    AllTalkerIDs = [ TalkerUserIds | AuthorId ],
                                  
    %% Make the act of saving it a method
    F = fun() ->
            %% add the conversation
            mnesia:write( Conversation ),
            %% add the conversation to each talkers
            writeUserConversationsRecord( AllTalkerIDs, ConversationId )
        end,
    %% Perform the save in a transaction
    mnesia:transaction(F).   

%%=====================================================
%% This will return a list full of new user records
%% for the conversation they have been added to
%%=====================================================
writeUserConversationsRecord( [UserId|RemainingIDs], ConversationId ) ->
    UserConversationRecord = #userConversation{ userId=UserId, talkingConversation=ConversationId},
    mnesia:write( UserConversationRecord ),
    writeUserConversationsRecord( RemainingIDs, ConversationId ).

%%=====================================================
%% This will return the Id of a single user
%%=====================================================
getUserId( UserName ) ->
    todo.
    
    
%%=====================================================
%% This will return a list of ids given a list of 
%% list of usernames
%%=====================================================
getUserIds( UserName ) ->
    todo.

%%=====================================================
%% Retrieve a Conversation by ConversationID
%% -- hmm I dont know if I really like this anymore
%% -- when would you ever do this.. since I always want
%% -- to get all of them... lets leave this one for now
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
    F = fun() ->
        qlc:e(  qlc:q( [ X || X <- mnesia:table( conversation ) ] ) )
    end,
    {atomic, Val } = mnesia:transaction( F ),
    Val.

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
    
    % nuke any data if we already had a DB set upv
    [ mnesia:clear_table( Table ) || Table <- tables() ], 

    %%create some users
    addUser( "Stephen", "test"),
    addUser( "Bob", "test"),
    addUser( "Sue", "test"),
    
    %%put in test data for the DB
    addConversation( "Stephen", "Subject", "Some interseting Message", [ "Sue" , "bob" ] ),
    addConversation( "Stephen", "test", "Testing", [ "Sue" ] ),
    
    %% get a viewing tool up to look at the DB data
    tv:start().
    
    
    