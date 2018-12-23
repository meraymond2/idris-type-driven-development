module Main

import Data.Vect

-- This implementation deliberately avoids using records

data DataStore : Type where
     MkData : (size : Nat) ->
              (items : Vect size String) ->
              DataStore

size : DataStore -> Nat
size (MkData size' items) = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (x :: xs) = x :: addToData xs

initialStore : DataStore
initialStore = MkData 0 []

data Command = Add String
             | Get Integer
             | Size
             | Search String
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str    = Just (Add str)
parseCommand "get" val    = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "size" args  = Just Size
parseCommand "search" str = Just (Search str)
parseCommand "quit" args  = Just Quit
parseCommand _ _          = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                         case integerToFin pos (size store) of
                              Nothing => Just ("Out of range\n", store)
                              Just id => Just (index id store_items ++ "\n", store)

searchStore : (str : String) -> (store : DataStore) -> String
searchStore str store = let search_res = filter (isInfixOf str) (items store)
                            strings    = toList (snd search_res)
                         in unwords (map fmt strings)
                           where fmt : String -> String
                                 fmt s = case findIndex (== s) (items store) of
                                              Nothing => ""
                                              Just i => show (finToInteger i) ++ ": " ++ s

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse inp of
                              Nothing           => Just ("Invalid command\n", store)
                              Just (Add item)   => Just ("ID: " ++ show (size store) ++ "\n", addToStore store item)
                              Just (Get pos)    => getEntry pos store
                              Just Size         => Just (show (size store) ++ " entries.\n", store)
                              Just (Search str) => Just (searchStore str store ++ "\n", store)
                              Just Quit         => Nothing

main : IO ()
main = replWith initialStore "Command: " processInput
