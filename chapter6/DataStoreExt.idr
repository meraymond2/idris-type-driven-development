module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

-- You can't match on types, but you can match on data values, hence the need for SString, etc.
SchemaToType : Schema -> Type
SchemaToType SString = String
SchemaToType SInt = Int
SchemaToType SChar = Char
SchemaToType (x .+. y) = (SchemaToType x, SchemaToType y)

record DataStore where
       constructor MkData
       schema : Schema
       size : Nat
       items : Vect size (SchemaToType schema)

addToStore : (store : DataStore) -> SchemaToType (schema store) -> DataStore
addToStore (MkData schema size items) newItem
   = MkData schema _ (addToData items)
  where
    addToData : Vect oldSize (SchemaToType schema) -> Vect (S oldSize) (SchemaToType schema)
    addToData [] = [newItem]
    addToData (x :: xs) = x :: addToData xs

-- I still don't where schema comes from when it's not an arg...
data Command : Schema -> Type where
  Add : SchemaToType schema -> Command schema
  Get : Integer -> Command schema
  GetAll : Command schema
  SetSchema : (newschema : Schema) -> Command schema
  Quit : Command schema

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs) =
  case xs of
       [] => Just SString
       _  => do xs_sch <- parseSchema xs
                Just (SString .+. xs_sch)
parseSchema ("Int" :: xs) =
  case xs of
       [] => Just SInt
       _  => do xs_sch <- parseSchema xs
                Just (SInt .+. xs_sch)
parseSchema ("Char" :: xs) =
  case xs of
       [] => Just SChar
       _  => do xs_sch <- parseSchema xs
                Just (SChar .+. xs_sch)

parseSchema _ = Nothing

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaToType schema, String)
parsePrefix SString inp = getQuoted (unpack inp)
  where getQuoted : List Char -> Maybe (String, String)
        getQuoted ('"' :: xs) = case span (/= '"') xs of
                                     (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                                     _ => Nothing
        getQuoted _ = Nothing
parsePrefix SInt inp = case span isDigit inp of
                            ("", rest) => Nothing
                            (num, rest) => Just (cast num, ltrim rest)
parsePrefix SChar inp = getSingleQuoted (unpack inp)
  where getSingleQuoted : List Char -> Maybe (Char, String)
        getSingleQuoted (''' :: xs) = case xs of
                                           (c :: ''' :: rest) => Just (c, ltrim (pack rest))
                                           _ => Nothing
        getSingleQuoted _ = Nothing
parsePrefix (l .+. r) inp = do (l_val, inp') <- parsePrefix l inp
                               (r_val, inp'') <- parsePrefix r inp'
                               Just ((l_val, r_val), inp'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaToType schema)
parseBySchema schema inp = case parsePrefix schema inp of
                                Just (res, "") => Just res
                                Just _ => Nothing
                                Nothing => Nothing

parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "add" inp    = do ty <- parseBySchema schema inp
                                      Just (Add ty)
parseCommand schema "get" ""     = Just GetAll
parseCommand schema "get" val    = if all isDigit (unpack val) then Just (Get (cast val)) else Nothing
parseCommand schema "schema" sch = do newSchema <- parseSchema (words sch)
                                      Just (SetSchema newSchema)
parseCommand schema "quit" args  = Just Quit
parseCommand _ _ _               = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                          (cmd, args) => parseCommand schema cmd (ltrim args)

display : SchemaToType schema -> String
display {schema = SString} string = string
display {schema = SInt} int = show int
display {schema = SChar} char = show char
display {schema = (s1 .+. s2)} (a, b) = (display a) ++ ", " ++ (display b)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                         case integerToFin pos (size store) of
                              Nothing => Just ("Out of range\n", store)
                              Just id => Just (display (index id store_items) ++ "\n", store)

getAll : (store : DataStore) -> String
getAll store = unlines (toList (map display (items store)))

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z => Just (MkData schema _ [])
                              (S k) => Nothing

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse (schema store) inp of
                              Nothing              => Just ("Invalid command\n", store)
                              Just (Add item)      => Just ("ID: " ++ show (size store) ++ "\n", addToStore store item)
                              Just (Get pos)       => getEntry pos store
                              Just GetAll          => Just ((getAll store), store)
                              Just (SetSchema sch) => case setSchema store sch of
                                                           Nothing => Just ("Cannot update schema.\n", store)
                                                           Just store' => Just ("Ok.\n", store')
                              Just Quit            => Nothing

initialStore : DataStore
initialStore = MkData SString _ []

main : IO ()
main = replWith initialStore "Command: " processInput
