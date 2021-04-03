{-# LANGUAGE QuasiQuotes, TypeFamilies, OverloadedStrings, MultiParamTypeClasses, TemplateHaskell, ViewPatterns #-}
module Main where

import Yesod
import Network.HTTP.Types.Status
import Data.Map as M
import Data.IORef
import Data.Aeson
import Data.List as L
import System.Environment

data AnagramServer = AnagramServer (IORef (M.Map String [String]))

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
    case M.lookup (L.sort word) dict of
      Just anagrams -> pure $ toJSON anagrams
      Nothing -> notFound

dictionary :: [String] -> Map String [String]
dictionary = fromList . removeSingletons . toList . L.foldl addEntry M.empty
    where
        addEntry dict word = insertWith (++) (sort word) [word] dict
        removeSingletons   = L.filter ((1 <) . L.length . snd)

main :: IO ()
main = do
    args <- getArgs
    let file = case L.length args of
                 1 -> args!!0
                 0 -> error "missing dictionary file name"
    dict <- dictionary <$> L.lines <$> readFile file
    ref <- newIORef $ dict
    warp 4000 (AnagramServer ref)
