module Lambdacula.Action 
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

data Action = Examine       -- Look at something
            | Inventorize   -- Special action : check inventory
            | Talk          -- Communicate with something, ideally someone.
            | Move          -- Move something.
            | Open          -- Open something.
            | Close         -- Close something.
            | TurnOn        -- Starting anything that can be started.
            | TurnOff       -- Stopping anaything that can be stopped.
            | Take          -- Pick up stuff.
            | Search        -- Look for something.
            | QuitGame      -- Leave this wonderful game.
            | Eat           -- Experiment with your mouth.
            | Show          -- Special, more like an event. Display stuff on screen
            | Use           -- Basic interaction (press something, combine objects, etc.)
            | Zilch         -- Unknown action.
    deriving (Eq, Show, Ord)

