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

import Data.Map.Strict (Map, fromList, toList, (!))
import Data.Tuple (swap)



---- 1 - EQUIPMENT RELATED DATA TYPES ------------------------------------------

data Suspect = MissScarlett | ColonelMustard | MrsWhite | ReverendGreen | MrsPeacock | ProfessorPlum
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Weapon = Candlestick | Dagger | LeadPipe | Revolver | Rope | Spanner
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Room = Hall | Lounge | DiningRoom | Kitchen | Ballroom | Conservatory | BilliardRoom | Library | Study
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Card = Sus Suspect | Wea Weapon | Roo Room
    deriving (Eq, Ord, Show, Read)
instance Enum Card where
    toEnum i
        | i <= 5    = Sus (toEnum  i       :: Suspect)
        | i <= 11   = Wea (toEnum (i - 6)  :: Weapon)
        | otherwise = Roo (toEnum (i - 12) :: Room)
    fromEnum (Sus s) = fromEnum s
    fromEnum (Wea w) = fromEnum w + 6
    fromEnum (Roo r) = fromEnum r + 12

data Circumstances = Circumstances { suspect :: Suspect, weapon :: Weapon, room :: Room }


-- Functions --

-- Lists of all Cards and Equipment
allCards = enumFromTo (Sus MissScarlett) (Roo Study) 
allSuspects = enumFrom MissScarlett
allWeapons = enumFrom Candlestick
allRooms = enumFrom Hall


-- Maps from Equipment to Unique Strings
fromSuspect :: Map Suspect String
fromSuspect = fromList [(MissScarlett, "MS"), (ColonelMustard, "CM"), (MrsWhite, "MW"), (ReverendGreen, "RG"), (MrsPeacock, "MP"), (ProfessorPlum, "PP")]
toSuspect = invertMap fromSuspect :: Map String Suspect

fromWeapon :: Map Weapon String
fromWeapon = fromList [(Candlestick, "Ca"), (Dagger, "Da"), (LeadPipe, "LP"), (Revolver, "Re"), (Rope, "Ro"), (Spanner, "Sp")]
toWeapon = invertMap fromWeapon :: Map String Weapon

fromRoom :: Map Room String
fromRoom = fromList [(Hall, "Ha"), (Lounge, "Lo"), (DiningRoom, "DR"), (Kitchen, "Ki"), (Ballroom, "Ba"), (Conservatory, "Co"), (BilliardRoom, "BR"),
                      (Library, "Li"), (Study, "St")]
toRoom = invertMap fromRoom :: Map String Room
                    


---- 2 - GAMEPLAY RELATED DATA TYPES -------------------------------------------

data PlayerStatus = NPC | Player | Eliminated

data Suggestion = Suggestion { circumstances :: Circumstances, resolver :: Suspect }

    -- The Map values are the Suspects (Players) who possibly possess that clue
data Notebook = Notebook { suspects :: Map Suspect [Suspect],
                           weapons  :: Map Weapon  [Suspect],
                           rooms    :: Map Room    [Suspect] }
-- ADD FIELDS FOR owned CLUES (3 lists) AND FOR solution (3 Maybes)

-- Functions --

emptyNotebook = Notebook
    (fromList [(s, allSuspects) | s <- allSuspects])
    (fromList [(w, allSuspects) | w <- allWeapons])
    (fromList [(r, allSuspects) | r <- allRooms])

-- UPDATE FUNCTION USING adjust



---- 3 - STATE RELATED DATA TYPES ----------------------------------------------

data LocationName = Corridor | StartOf Suspect | In Room
    deriving (Eq, Show, Read)

data Location = Location { lName :: LocationName, distances :: Map Location Int }

    -- Note that player statuses are set by giving them clues and by accusations
data Event = GameStart | GiveClues Suspect [Card] | Roll Suspect Int | Move Suspect Location
                | Suggest Suspect Circumstances | Resolve (Maybe Suspect)
                | Accuse  Suspect Circumstances | GameEnd

data State = State { event :: Event,
                     players :: Map Suspect PlayerStatus,
                     notebooks :: Map Suspect Notebook,
                     susLocs :: Map Suspect Location,
                     weaLocs :: Map Weapon Location }



---- 4 - UTILITY FUNCTIONS -----------------------------------------------------

-- Invert a Map with guaranteed unique values
invertMap :: Ord v => Map k v -> Map v k
invertMap = fromList . map swap . toList
