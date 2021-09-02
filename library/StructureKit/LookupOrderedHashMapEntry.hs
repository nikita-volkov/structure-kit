module StructureKit.LookupOrderedHashMapEntry
  ( LookupOrderedHashMapEntry,
    missing,
    present,
    insert,
    inc,
    dec,
    entryKey,
    select,
  )
where

import StructureKit.Prelude hiding (empty, insert, lookup)

data LookupOrderedHashMapEntry k v
  = MissingLookupOrderedHashMapEntry Int k
  | PresentLookupOrderedHashMapEntry Int k v

missing :: k -> LookupOrderedHashMapEntry k v
missing =
  MissingLookupOrderedHashMapEntry 1

present :: k -> v -> LookupOrderedHashMapEntry k v
present =
  PresentLookupOrderedHashMapEntry 0

insert :: v -> LookupOrderedHashMapEntry k v -> (Maybe v, LookupOrderedHashMapEntry k v)
insert value =
  \case
    PresentLookupOrderedHashMapEntry count key presentValue ->
      (Just presentValue, PresentLookupOrderedHashMapEntry count key value)
    MissingLookupOrderedHashMapEntry count key ->
      (Nothing, PresentLookupOrderedHashMapEntry count key value)

inc :: LookupOrderedHashMapEntry k v -> (Maybe v, LookupOrderedHashMapEntry k v)
inc =
  \case
    PresentLookupOrderedHashMapEntry count key value ->
      ( Just value,
        PresentLookupOrderedHashMapEntry (succ count) key value
      )
    MissingLookupOrderedHashMapEntry count key ->
      ( Nothing,
        MissingLookupOrderedHashMapEntry (succ count) key
      )

dec :: LookupOrderedHashMapEntry k v -> (Maybe v, Maybe (LookupOrderedHashMapEntry k v))
dec =
  \case
    PresentLookupOrderedHashMapEntry count entryKey value ->
      if count == 0
        then (Just value, Nothing)
        else (Just value, Just (PresentLookupOrderedHashMapEntry (pred count) entryKey value))
    MissingLookupOrderedHashMapEntry count entryKey ->
      if count == 0
        then (Nothing, Nothing)
        else (Nothing, Just (MissingLookupOrderedHashMapEntry (pred count) entryKey))

entryKey :: LookupOrderedHashMapEntry k v -> k
entryKey =
  \case
    MissingLookupOrderedHashMapEntry count entryKey -> entryKey
    PresentLookupOrderedHashMapEntry count entryKey value -> entryKey

select :: Eq k => k -> LookupOrderedHashMapEntry k v -> Maybe (LookupOrderedHashMapEntry k v)
select key entry =
  if entryKey entry == key
    then Just entry
    else Nothing
