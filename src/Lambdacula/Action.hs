module Lambdacula.Action 
    (
        Action(..),
        PlayerAction(..),
        Verb(..)
    )
where 

-- Transitive verbs are simple : the verb itself, then eventually some preposition, and its objects.
-- Phrasal are a bit twisty : the form + the phrasal will change the meaning of this.
data Verb = Transitive {    actionType :: Action
                            , form :: String
                            , preposition :: [String]
                            , complement :: [String] 
                            , reversed :: Bool
                        }
            | Phrasal {     actionType :: Action
                            , form :: String
                            , phrasal :: String 
                            , preposition :: [String]
                            , complements :: [String] 
                            , reversed :: Bool
            }
                deriving (Show, Eq)

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
            | Lift          -- Lift something
            | Take          -- Pick up stuff.
            | Search        -- Look for something.
            | QuitGame      -- Leave this wonderful game.
            | Eat           -- Experiment with your mouth.
            | Show          -- Special, more like an event. Display stuff on screen
            | Use           -- Basic interaction (press something, combine objects, etc.)
            | Attack        -- Attack someone or even something
            | Give          -- Give something
            | Flee          -- Leave the current room for the previous one
            | Zilch         -- Unknown action.
    deriving (Eq, Show, Ord)

