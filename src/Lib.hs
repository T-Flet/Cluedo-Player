---- Cluedo Player Data Types and Related Functions
--
--   Description:
--          This package contains all the used data types and all their related
--          functions.
--
--   Sections:
--       0 - Imports
--       1 - Equipment Related Data Types
--       2 - Gameplay Related Data Types
--       3 - State Related Data Types
--       4 - Utility Functions
--



---- 0 - IMPORTS ---------------------------------------------------------------

module Lib where

import qualified Data.Map.Strict as M
import Data.Tuple (swap)



---- 1 - EQUIPMENT RELATED DATA TYPES ------------------------------------------

data Suspect = MissScarlett | ColonelMustard | MrsWhite | ReverendGreen | MrsPeacock | ProfessorPlum
    deriving (Eq, Ord, Enum, Show, Read)

data Weapon = Candlestick | Dagger | LeadPipe | Revolver | Rope | Spanner
    deriving (Eq, Ord, Enum, Show, Read)

data Room = Hall | Lounge | DiningRoom | Kitchen | Ballroom | Conservatory | BilliardRoom | Library | Study
    deriving (Eq, Ord, Enum, Show, Read)

data Card = Sus Suspect | Wea Weapon | Roo Room
    deriving (Eq, Ord, Enum, Show, Read)

data Circumstances = Circumstances { suspect :: Suspect, weapon :: Weapon, room :: Room }


-- Functions --

-- Maps from Equipment to Unique Strings
susStrs :: M.Map Suspect String
susStrs = M.fromList [(MissScarlett, "MS"), (ColonelMustard, "CM"), (MrsWhite, "MW"), (ReverendGreen, "RG"), (MrsPeacock, "MP"), (ProfessorPlum, "PP")]
strSuss = invertMap susStrs :: M.Map String Suspect

weaStrs :: M.Map Weapon String
weaStrs = M.fromList [(Candlestick, "Ca"), (Dagger, "Da"), (LeadPipe, "LP"), (Revolver, "Re"), (Rope, "Ro"), (Spanner, "Sp")]
strWeas = invertMap weaStrs :: M.Map String Weapon

rooStrs :: M.Map Room String
rooStrs = M.fromList [(Hall, "Ha"), (Lounge, "Lo"), (DiningRoom, "DR"), (Kitchen, "Ki"), (Ballroom, "Ba"), (Conservatory, "Co"), (BilliardRoom, "BR"),
                      (Library, "Li"), (Study, "St")]
strRoos = invertMap rooStrs :: M.Map String Room

                    

---- 2 - GAMEPLAY RELATED DATA TYPES -------------------------------------------

data PlayerStatus = NPC | Player | Eliminated

data Suggestion = Suggestion { circumstances :: Circumstances, resolver :: Suspect }

data Notebook = Notebook { suspects :: M.Map Suspect (Maybe Suspect),
                           weapons  :: M.Map Weapon  (Maybe Suspect),
                           rooms    :: M.Map Room    (Maybe Suspect) }



---- 3 - STATE RELATED DATA TYPES ----------------------------------------------

data LocationName = Corridor | StartOf Suspect | In Room
    deriving (Eq, Show, Read)

data Location = Location { lName :: LocationName, distances :: M.Map Location Int }

    -- Note that player statuses are set by giving them clues and by accusations
data Event = GameStart | GiveClues Suspect [Card] | Roll Suspect Int | Move Suspect Location
                | Suggest Suspect Circumstances | Resolve (Maybe Suspect)
                | Accuse  Suspect Circumstances | GameEnd

data State = State { event :: Event,
                     players :: M.Map Suspect PlayerStatus,
                     notebooks :: M.Map Suspect Notebook,
                     susLocs :: M.Map Suspect Location,
                     weaLocs :: M.Map Weapon Location }



---- 4 - UTILITY FUNCTIONS -----------------------------------------------------

-- Invert a Map with guaranteed unique values
invertMap :: Ord v => M.Map k v -> M.Map v k
invertMap = M.fromList . map swap . M.toList
