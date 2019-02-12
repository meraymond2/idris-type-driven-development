import DataStore

testStore : DataStore (SString .+. SString .+. SInt)
testStore = addToStore ("Mercury", "Mariner 10", 1974)
            (addToStore ("Venus", "Venera", 1961)
            (addToStore ("Uranus", "Voyager 2", 1986)
            (addToStore ("Pluto", "New Horizons", 2015)
            empty)))

-- You can use $ to mean wrap the following in parens
testStore2 : DataStore (SString .+. SString .+. SInt)
testStore2 = addToStore ("Mercury", "Mariner 10", 1974) $
            addToStore ("Venus", "Venera", 1961) $
            addToStore ("Uranus", "Voyager 2", 1986) $
            addToStore ("Pluto", "New Horizons", 2015) $
            empty

-- I'm not sure I prefer that, but if you don't have paredit..

listItems : DataStore schema -> List (SchemaType schema)
listItems input with (storeView input)
  listItems input | SNil = []
  listItems (addToStore value restStore) | (SAdd rec) = value :: (listItems restStore)

{- Assumes the schema is String, String, Int, like String: (String, Int) -}
filterKeys : (test : SchemaType val_schema -> Bool) ->
             DataStore (SString .+. val_schema) -> List String
filterKeys test input with (storeView input)
  filterKeys test input | SNil = []
  filterKeys test (addToStore (key, value) restStore) | (SAdd rec) =
    if test value
      then key :: filterKeys test restStore | rec
      else filterKeys test restStore | rec

getValues : DataStore (SString .+. val_schema) -> List (SchemaType val_schema)
getValues input with (storeView input)
  getValues input | SNil = []
  getValues (addToStore (key, value) restStore) | (SAdd rec) =
    value :: getValues restStore
