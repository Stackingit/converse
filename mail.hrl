%%======================================================
%% ABSTRACT
%%
%% This define the data to be stored in the DB, 
%%======================================================

%%======================================================
%% information about the mail object
%% id : increasnig value and primary Key 
%% suthor : PID of the person that started this
%% subject : string : heading for the converstation
%% message : string : the content 
%% listeners : [email,email,...] of listeners.
%%======================================================
-record( conversation, 
            { id, 
              author, 
              subject, 
              message, 
              listeners } 
).


