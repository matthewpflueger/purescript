module FileOperations where

import Data.Path
import Data.Array
import Data.Foldable
import Data.Maybe
import Data.Maybe.Unsafe
import Control.MonadPlus
import Debug.Trace


allFiles :: Path -> [Path]
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> [Path]
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> [Path]
onlyFiles file = if isDirectory file then concatMap onlyFiles (ls file) else [file]

sizeCompare :: (Number -> Number -> Boolean) -> Path -> Path -> Path
sizeCompare pred a b =
  let getSize f = fromJust $ size f
  in
    if isDirectory a then b else if getSize a `pred` getSize b then a else b

smallest :: Path -> Path
smallest file = foldl (sizeCompare (<)) file $ onlyFiles file

biggest :: Path -> Path
biggest file = foldl (sizeCompare (>)) file $ onlyFiles file

whereIs :: String -> Maybe Path
whereIs name =
  let
      equals path = if name == filename path then Just path else Nothing
      whereIs' _ j@(Just _) = j
      whereIs' [] Nothing = Nothing
      whereIs' (x : xs) Nothing = whereIs' xs $ whereIs' (ls x) $ equals x
  in
      whereIs' (ls root) (equals root)
