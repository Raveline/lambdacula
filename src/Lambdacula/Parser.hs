module Lambdacula.Parser
    ( 
        processInput
    )
where

import Data.List
import Data.List.Split
import Data.Char
import Data.String
import Lambdacula.Action


articles = ["the", "a", "an"]

processInput :: String -> [Verb] -> PlayerAction
processInput str vs = buildAction verb objects 
    where 
        verb = findVerb vs statement 
        objects = findObjects (tail statement) verb
        statement = words . map (toLower) $ str
        
        buildAction :: Maybe Verb -> [String] -> PlayerAction
        buildAction (Just v) [] = SimpleAction (actionType v) 
        buildAction (Just v) [o] = Interaction (actionType v) o
        buildAction (Just v) (o:o':[])
                | checkForReversedVerb v  = Complex (actionType v) o' o
                | otherwise = Complex (actionType v) o o'
        buildAction Nothing _ = SimpleAction Zilch

        checkForReversedVerb :: Verb -> Bool
        checkForReversedVerb (Phrasal _ _ _ _ _ True) = True
        checkForReversedVerb _ = False

-- Given a list of verbs, try and find there is one in a given sentence
findVerb :: [Verb] -> [String] -> Maybe Verb
findVerb [] s = Nothing
findVerb (v:vs) s
    | verbInSentence v s = Just v
    | otherwise = findVerb vs s
    where
        verbInSentence :: Verb -> [String] -> Bool
        verbInSentence (Transitive _ form prepositions _) s = form `elem` s
                                                    && prepositions `contains` s
        verbInSentence (Phrasal _ form phrasal prepositions _ _) s = form `elem` s 
                                                        && phrasal `elem` s 
                                                        && prepositions `contains` s

contains [] _ = True
contains xs ys = intersect xs ys /= []

analyseObjects :: [String] -- A sentence, without the initial verb
                -> [String] -- The first possible separators
                -> [String] -- A list of objects
analyseObjects sentence separators =   filter (/= "") . map(trim) . splitOn "___" . unwords . filter (notArticles) $ parse sentence separators
    where
        -- Parse a sentence, replace separators by "_"
        parse :: [String] -> [String] -> [String]
        parse [] _ = []
        parse (w:ws) sep 
            | w `elem` sep = "___":(parse ws sep)
            | otherwise = w:(parse ws sep)
        -- Remove if starts with separator
        removeHead :: [String] -> [String]
        removeHead [] = []
        removeHead ("___":xs) = xs
        removeHead xs = xs
        -- Trim
        trim :: String -> String
        trim = trimRight . trimLeft
            where
                trimLeft :: String -> String
                trimLeft (' ':ws) = trimLeft ws
                trimLeft ws = ws
                trimRight :: String -> String
                trimRight ws
                    | length ws == 0 = ws
                    | (last ws) == ' ' = trimRight $ take ((length ws)- 1) ws
                    | otherwise = ws
        notArticles :: String -> Bool
        notArticles s = not ((map toLower s) `elem` articles)


findObjects :: [String] -> Maybe Verb -> [String]
findObjects _ Nothing = []
findObjects sentence (Just (Transitive _ form prep comp)) =
    analyseObjects sentence (prep ++ comp)
findObjects sentence (Just (Phrasal _ form phrasal prep comp _)) =
    analyseObjects sentence (phrasal:prep ++ comp)

transformIntoAction :: Maybe Verb -> Maybe String -> Maybe String -> PlayerAction
transformIntoAction Nothing _ _ = SimpleAction Zilch
transformIntoAction (Just v) Nothing _ = SimpleAction $ actionType v
transformIntoAction (Just v) (Just o) Nothing = Interaction (actionType v) o
transformIntoAction (Just v) (Just o) (Just o') = Complex (actionType v) o o'

