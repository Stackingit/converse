%%=====================================================
%% Abstract
%%
%% This is the module that abstracts away the DB layer
%% and make the information available from the ether
%%
%% TODO: Use Mnesia now to learn it, but next look to
%%       use CouchDB... not sure if it adds any real 
%%       value, but intersting none the less to try.
%%======================================================

%%======================================================
%% Tag the file
%%======================================================
-module(converseDB).
-compile(export_all).
-author("Stephen Bailey").
-email("Stephen.Bailey@stackingit.com").
-vsn( "0.0.0.1" ).
%% get the query list comprehension tied in so we can use it
-include_lib("stdlib/include/qlc.hrl").
-include("DebugMacros.hrl").

%%=====================================================
%% Defines the DB data
%%
%% TODO : Find out how to put this in a hrl file and 
%%        referenced
%%======================================================
-record( tb_conversation, { 
    id,                 %% unique primary Key 
    author,             %% UserID : The person that started this %% information about the mail object
    subject,            %% string : heading for the conversation
    message,            %% string : the content 
    talkers   = [],     %% [userID,userID,...] people still active in the convesation
    listeners = [],     %% [UserID,UserID,...] of listeners not active in the conversation
    time                %% when the message was created 
} ).

%% Details about the user
-record( tb_user, {
    name,                     %% primary key for the user, since the name must be unique
    userId,                   %% the person's id so they can change their name if they want to
    password                  %% umm duh the password     
} ).

%% This will only keep the current information, to do history  trauls, you need to hunt
%% down the conversations.. since this is a less likely use case
%%
%% This will be a bag table so that the primary key is user, but each
%% listening and following mail is unqiue.
-record( tb_userConversation, {
    userId,
    talker   = [],      %% an active conversation ID
    listener = []       %% an listening conversation ID
} ).

%%=====================================================
%% this just returns a nice list of all of our tables
%% so if I need to do anything with all of htem like nuke
%% them for debug, I only have to update in one place
%%=====================================================
tables() ->
    [ tb_user, tb_conversation, tb_userConversation ].

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
    mnesia:create_table( tb_conversation, [ { attributes, record_info( fields, tb_conversation ) } , 
                                            { disc_only_copies, [ node() ] },
                                            { type, set } ] ),
                                             
    %% now for the user table
    %% we make this a set so that only userid is unique 
    mnesia:create_table( tb_user, [ { attributes, record_info( fields, tb_user ) } , 
                                    { disc_only_copies, [ node() ] },
                                    { type, set } ] ),
                                 
    %% now for the userConversation table
    %% we make this a bag so the userid is the key for all "actvie conversations" 
    mnesia:create_table( tb_userConversation, [ { attributes, record_info( fields, tb_userConversation ) } , 
                                                { disc_only_copies, [ node() ] },
                                                { type, set } ] ).

%%=====================================================
%% Add a new user 
%%=====================================================
addUser( Name, Password ) ->
    %% check if this user already exists
    case validateUser( Name ) of
        { user, exists } ->
            { user, already_exists };
        { user, does_not_exist } ->
            addUpdateUser(Name, Password );
        ERROR -> 
            exit( { 'converseDB:addUser', ERROR} )
    end.

%%=====================================================
%% This will either add a new user or update an existing
%% user
%%=====================================================
addUpdateUser( Name, Password ) ->
    UserRecord = #tb_user{ name=Name, userId={now(),node()} , password=Password },
    F = fun() ->
            mnesia:write( UserRecord )
        end,
        
    case mnesia:transaction( F ) of
        {atomic,ok} -> 
            { user, ok };
        Error -> 
            exit( {'converseDB:addUser'}, Error )
    end.
    
%%=====================================================
%% Validate the user exits
%%=====================================================
validateUser( Name ) ->
    %% Define the method to check for the user
    F = fun() ->
            qlc:e( qlc:q( [ X#tb_user.userId || X <- mnesia:table( tb_user ), X#tb_user.name =:= Name ] ) )            
        end,
    
    %% do the lookup
    case mnesia:transaction(F) of
        {aborted, Reason } ->
            %% What happen here ?? .. lets get out of here
            exit( {'converseDB:validateUser', Reason } );
        {atomic, Result } -> 
            case Result of 
                [] -> { user, does_not_exist };
                _  -> { user, exists }
            end
    end.

%%=====================================================
%% Validate the user password
%%=====================================================
validatePassword( Name, Password ) ->
    F = fun() ->
            qlc:e( qlc:q( [ X#tb_user.password || X <- mnesia:table( tb_user ), X#tb_user.name =:= Name ] ) )
        end,
        
    case mnesia:transaction(F) of 
        {aborted, Reason } ->
            %% What happen here ?? .. lets get out of here
            exit( {'converseDB:validatePassword', Reason } );
        {atomic, [Password] } -> 
            {validate, pass};
        {atomic, _ } ->
            {validate, fail}
    end.
    
%%=====================================================
%% Save a new Conversation
%%=====================================================
addConversation( Author, Password, Subject, Message, Talkers ) ->
    %%Validate the password - unless you are the correct person
    %%you dont get to impersonate them !!
    case validatePassword( Author, Password ) of
        { validate, fail } ->
            { conversation, authentication_failed };
        {validate, pass } ->    
            %%Get our authors ID
            AuthorId = getUserId( Author ),
            
            %% Get the userid for these people in the mail
            TalkerUserIds = lists:map( fun(X)-> getUserId( X ) end, Talkers ),
            
            %%Define a unique conversationID
            ConversationId = {now(),node()},
            
            %% Make a new unique conversation record
            Conversation = #tb_conversation{ id=ConversationId, 
                                             author=AuthorId , 
                                             subject=Subject, 
                                             message=Message, 
                                             talkers=TalkerUserIds, 
                                             time=now() },
                                              
            %% Now we need to create records for each of our people
            %% involved in this conversation... since the author is just
            %% a talker who is likely to opt out last, we can add this
            %% in there now
            AllTalkerIDs = [ AuthorId | TalkerUserIds ],
                                          
            
            %% Make the act of saving it a method
            F = fun() ->
                    %% add the conversation
                    mnesia:write( Conversation ),
                    %% update each of the users with the new conversations
                    %% that they are now involved in
                    [ mapUserAsTalker( UserId, ConversationId ) || UserId <- AllTalkerIDs ]
                end,
                
            %% Perform the save in a transaction
            case mnesia:transaction(F) of 
                { atomic, _ } ->
                    { conversation, ok };
                ERROR ->
                    exit( { 'converseDB:addConversation', ERROR } )
            end
    end.

%%=====================================================
%% This will add a conversation ID to a particular user
%%=====================================================
mapUserAsTalker( UserId, ConversationId ) ->
    F = fun() ->
            case mnesia:read( tb_userConversation, UserId ) of
                [] -> 
                    %%just add a new record
                    NewUserConversation = #tb_userConversation{ userId=UserId, talker=[ConversationId] },
                    mnesia:write( NewUserConversation );
                
                [ActiveConversations] ->
                    Talking = ActiveConversations#tb_userConversation.talker,
                    %%now save it back
                    UpdatedConversations = ActiveConversations#tb_userConversation{talker=[ConversationId | Talking ] },
                    mnesia:write( UpdatedConversations )
            end
        end,
    
    mnesia:transaction(F).
    
    
%%===================================================== 
%% This will return the Id of a single user
%%=====================================================
getUserId( UserName ) ->
    F = fun() ->
            qlc:e( qlc:q( [ X#tb_user.userId || X <- mnesia:table( tb_user ), X#tb_user.name =:= UserName ] ) )
        end,
    {atomic, [Val]} = mnesia:transaction(F),
    Val.
    
    
%%=====================================================
%% Get any new or active conversations for the user by returning
%% only the ID, subject and author%%=====================================================
getActiveConversations( User, Password ) ->
    %%Validate the password
    case validatePassword( User, Password ) of
        { validate, fail } ->
            { conversation, authentication_failed };
        {validate, pass } ->    
            % look through the DB to find any conversations that 
            % this user is still involved in and return them all),
            F = fun() ->
                qlc:e( qlc:q( [ { {talking, X#tb_userConversation.talker}, 
                                  {listening, X#tb_userConversation.listener} } 
                                || X <- mnesia:table( tb_userConversation ), 
                                X#tb_userConversation.userId =:= getUserId( User ) ] ) )
                end,
            {atomic, [ActiveConversations] } = mnesia:transaction( F ),
            ActiveConversations
    end.
            
            
%%=====================================================
%% This will return a complete conversation
%%=====================================================
getConversation( ConversationId )->
    F = fun()->
            mnesia:read( tb_conversation, ConversationId )
        end,
        
    case mnesia:transation(F) of
        {atomic, [Conversation] } ->
            Conversation;    
        Error ->
            exit( { 'converseDB:getConversation', Error } )
    end.
        

%%=====================================================
%% Set up a debugging environment
%%=====================================================
debugStart() ->
    %% get a tracer up for just calls
    tracer:trace([?MODULE],[c]),
    
    %% sort out the Mnesia DB
    mnesia:stop(),      % make sure there is not one running already
    mnesia:delete_schema( node() ),
    createDB(),         % create a DB for this node 
    startDB(),          % create the structure        
    
    %%create some users
    addUser( "Stephen", "test"),
    addUser( "Bob", "test"),
    addUser( "Sue", "test"),
    
    %%put in test data for the DB
    addConversation( "Stephen", "test", "Subject", "Some interseting Message", [ "Sue" , "Bob" ] ),
    addConversation( "Stephen", "test", "test", "Testing", [ "Sue" ] ),
    
    %% get a viewing tool up to look at the DB data
    tv:start().
    
    
    