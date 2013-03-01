import Data.List
import Data.Char

data Verb = Transitive { form :: String , preposition :: [String] }
            | Phrasal { form :: String, phrasal :: String , preposition :: [String] }
                deriving (Show, Eq)

verbExample = Transitive "speak" ["with", "to"]
verbExample' = Transitive "ask" ["about"]
verbExample'' = Phrasal "look" "for" []
verbs = [verbExample, verbExample', verbExample'']

-- Will most likely have to add other kind of objects.
-- And a map for potential actions on each objects.
data Object = Object { name :: String }

data Action =   Simple Verb
                | Interaction Verb Object
                | Complex Verb Object 

verbInSentence :: Verb -> [String] -> Bool
verbInSentence (Transitive form prepositions) s = form `elem` s
                                                && prepositions `contains` s
verbInSentence (Phrasal form phrasal prepositions) s = form `elem` s 
                                                    && phrasal `elem` s 
                                                    && prepositions `contains` s

findVerb :: [Verb] -> [String] -> Maybe Verb
findVerb [] s = Nothing
findVerb (v:vs) s
    | verbInSentence v s = Just v
    | otherwise = findVerb vs s

contains [] _ = True
contains xs ys = intersect xs ys /= []

processPredicate s = findVerb verbs prepare
    where prepare = words . map (toLower) $ s 
