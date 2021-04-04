{-# LANGUAGE QuasiQuotes, TypeFamilies, OverloadedStrings, MultiParamTypeClasses, TemplateHaskell, ViewPatterns #-}
module Main where
import Yesod
import Network.HTTP.Types.Status
import Data.Map (Map, fromList, insertWith, toList, empty, lookup)
import Data.IORef
import Data.Aeson
import Data.List hiding (lookup)
import Prelude hiding (lookup)
import System.Environment
import Data.Char

data AnagramServer = AnagramServer (IORef (Map String [String]))

mkYesod "AnagramServer" [parseRoutes|
/ RootR GET
/#String WordR GET
|]

instance Yesod AnagramServer

getRootR :: Handler Value
getRootR = do
    (AnagramServer ref) <- getYesod
    dict <- liftIO $ readIORef ref
    pure $ toJSON dict

getWordR :: String -> Handler Value
getWordR word = do
    (AnagramServer ref) <- getYesod
    dict <- liftIO $ readIORef ref
    case lookup (wordKey word) dict of
      Just anagrams -> pure $ toJSON $ filter (\w -> upperCase w /= upperCase word) anagrams
      Nothing -> pure $ toJSON ([] :: [String])

dictionary = foldl addEntry empty
    where addEntry dict word = insertWith (++) (wordKey word) [word] dict

upperCase = map toUpper
wordKey   = sort . upperCase

main :: IO ()
main = do
    args <- getArgs
    let file = case length args of
                 1 -> args!!0
                 0 -> error "missing dictionary file name"
    dict <- dictionary <$> lines <$> readFile file
    ref <- newIORef $ dict
    warp 4000 (AnagramServer ref)
