module Action 
    (
        Action(..),
        PlayerAction(..),
        Verb(..),
        getVerbAction
    )
where 

-- Transitive verbs are simple : the verb itself, then eventually some preposition, and its objects.
-- Phrasal are a bit twisty : the form + the phrasal will change the meaning of this.
data Verb = Transitive {    actionType :: Action
                            , form :: String
                            , preposition :: [String]
                            , complement :: [String] 
                        }
            | Phrasal {     actionType :: Action
                            , form :: String
                            , phrasal :: String 
                            , preposition :: [String]
                            , complements :: [String] 
            }
                deriving (Show, Eq)

getVerbAction :: Verb -> Action
getVerbAction (Transitive a _ _ _) = a
getVerbAction (Phrasal a _ _ _ _) = a

data PlayerAction =   SimpleAction Action
                | Interaction Action String
                | Complex Action String String 
    deriving(Show,Eq)

data Action = Examine | Talk | Move | Open | Close | TurnOn | TurnOff | Take | Search | QuitGame | Eat | Zilch
    deriving (Eq, Show, Ord)

