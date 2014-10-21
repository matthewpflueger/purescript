module Data.PhoneBook where

import Data.List
import Data.Maybe
import Control.Plus (empty)

type Entry = { firstName :: String, lastName :: String, phone :: String }

type PhoneBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName ++ ", " ++
                  entry.firstName ++ ": " ++
                  entry.phone

emptyBook :: PhoneBook
emptyBook = empty

insertEntry :: Entry -> PhoneBook -> PhoneBook
insertEntry = Cons

findEntry :: String -> String -> PhoneBook -> Maybe Entry
findEntry firstName lastName =
  head <<< filter (\e -> e.lastName == lastName && e.firstName == firstName)

findEntryByPhone :: String -> PhoneBook -> Maybe Entry
findEntryByPhone phone = filter (\e -> e.phone == phone) >>> head

hasEntry :: String -> String -> PhoneBook -> Boolean
hasEntry firstName lastName book = isJust $ findEntry firstName lastName book

removeDuplicates :: PhoneBook -> PhoneBook
removeDuplicates = nubBy (\a b -> a.firstName == b.firstName && a.lastName == b.lastName && a.phone == b.phone)

