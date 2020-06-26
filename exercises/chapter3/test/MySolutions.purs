module Test.MySolutions where

import Prelude

import Data.AddressBook (AddressBook, Entry)
import Data.List (head, filter, null, nubBy)
import Data.Maybe (Maybe)


findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.address.street == street

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = not null <<< filter byName
  where
    byName :: Entry -> Boolean
    byName entry = entry.firstName == firstName || entry.lastName == lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy namesEqual
  where 
    namesEqual :: Entry -> Entry -> Boolean
    namesEqual n1 n2 = n1.firstName == n2.firstName && n1.lastName == n2.lastName 