module Action 
    (
        Object(..),
        Action(..),
        PlayerAction(..),
        Verb(..),
        getVerbAction
    )
where 

-- Transitive verbs are simple : the verb itself, then eventually some preposition, and its objects.
-- Phrasal are a bit twisty : the form + the phrasal will change the meaning of this.
data Verb = Transitive { actionType :: Action, form :: String , preposition :: [String], complement :: [String] }
            | Phrasal { actionType :: Action, form :: String, phrasal :: String , preposition :: [String], complements :: [String] }
                deriving (Show, Eq)

getVerbAction :: Verb -> Action
getVerbAction (Transitive a _ _ _) = a
getVerbAction (Phrasal a _ _ _ _) = a

-- Will most likely have to add other kind of objects.
-- And a map for potential actions on each objects.
data Object = Object { name :: String }
    deriving(Show,Eq)

data PlayerAction =   SimpleAction Action
                | Interaction Action Object
                | Complex Action Object Object 
    deriving(Show,Eq)

data Action = Examine | Talk | Move | Open | Close | TurnOn | TurnOff | Take | Search | Zilch
    deriving (Eq, Show, Ord)

