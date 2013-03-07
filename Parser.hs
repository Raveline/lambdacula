module Parser
    ( 
        processInput
    )
where

import Data.List
import Data.Char
import Action


articles = ["the", "a", "an"]

processInput :: String -> [Verb] -> PlayerAction
processInput str vs = buildAction verb objects 
    where 
        verb = findVerb vs statement 
        objects = findObjects (tail statement) verb
        statement = words . map (toLower) $ str
        
        buildAction :: Maybe Verb -> [String] -> PlayerAction
        buildAction Nothing _ = SimpleAction Zilch
        buildAction (Just v) [] = SimpleAction (getVerbAction v) 
        buildAction (Just v) [o] = Interaction (getVerbAction v) o
        buildAction (Just v) (o:o':[]) = Complex (getVerbAction v) o o'

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
        verbInSentence (Phrasal _ form phrasal prepositions _) s = form `elem` s 
                                                        && phrasal `elem` s 
                                                        && prepositions `contains` s

contains [] _ = True
contains xs ys = intersect xs ys /= []

-- Given a sentence without its first word (that would be the verb !), try and find
-- the objects in this sentence.
-- We're trying to find two potential objects, when there can be none, one or two.
-- "Give the apple to the cook" = ["the apple", "the cook"]
-- "Turn on the lights" = ["the lights"]
-- "Yawn" = []
analyseObjects :: [String] -> [String] -> [String] -> [String]
analyseObjects sentence fstWds sndWds = matcher sentence posFst posSnd
    where 
        matcher :: [String] -> Maybe Int -> Maybe Int -> [String]
        matcher [] _ _ = []
        matcher s Nothing Nothing = [concatAndNoArticles s]
        matcher s Nothing (Just x) = [concatAndNoArticles . take x $ s] ++ [concatAndNoArticles . drop x $ s]
        matcher s (Just 0) Nothing = [concatAndNoArticles . tail $ s]
        matcher s (Just x) Nothing = [concatAndNoArticles . take x $ s]
        matcher s (Just 0) (Just x) = [concatAndNoArticles . take (x-1) . tail $ s] ++ [concatAndNoArticles . drop (x+1) $ s]
        matcher s (Just x) (Just y) = [concatAndNoArticles . init . take (x-1) $ s] ++ [concatAndNoArticles . drop (y+1) $ s]
        
        posFst = findPositionOf fstWds sentence
        posSnd = findPositionOf sndWds sentence
        
        findPositionOf :: [String] -> [String] -> Maybe Int
        findPositionOf prepos sentence = findIndex (`elem` prepos) sentence 

        concatAndNoArticles = unwords . (flip (\\) articles)

findObjects :: [String] -> Maybe Verb -> [String]
findObjects _ Nothing = []
findObjects sentence (Just (Transitive _ form prep comp)) =
    analyseObjects sentence prep comp
findObjects sentence (Just (Phrasal _ form phrasal prep comp)) =
    analyseObjects sentence (phrasal:prep) comp

transformIntoAction :: Maybe Verb -> Maybe String -> Maybe String -> PlayerAction
transformIntoAction Nothing _ _ = SimpleAction Zilch
transformIntoAction (Just v) Nothing _ = SimpleAction $ getVerbAction v
transformIntoAction (Just v) (Just o) Nothing = Interaction (getVerbAction v) o
transformIntoAction (Just v) (Just o) (Just o') = Complex (getVerbAction v) o o'

