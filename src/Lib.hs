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

import Data.Map.Strict (Map, fromList, toList, (!), adjust, mapWithKey)
import qualified Data.Map.Strict as M (filter) 
import Data.Tuple (swap)
import Data.List ((\\), intercalate, delete)



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
                    

-- Split Cards into each equipment type
toEquip :: [Card] -> ([Suspect], [Weapon], [Room])
toEquip = foldr addEquip ([], [], [])
    where addEquip (Sus s) (ss, ws, rs) = (s:ss, ws, rs)
          addEquip (Wea w) (ss, ws, rs) = (ss, w:ws, rs)
          addEquip (Roo r) (ss, ws, rs) = (ss, ws, r:rs)



---- 2 - GAMEPLAY RELATED DATA TYPES -------------------------------------------

data PlayerStatus = NPC | Player | Eliminated

data Suggestion = Suggestion { circumstances :: Circumstances, resolver :: Suspect }

    -- The [[Card]] value is the list of possible sets of cards each player could have
    -- The [Suspect] values are the Players who possibly possess that clue
data Notebook = Notebook { suspectCards :: Map Suspect [[Card]],
                           suspectMap :: Map Suspect [Suspect],
                           weaponMap  :: Map Weapon  [Suspect],
                           roomMap    :: Map Room    [Suspect] }
    deriving (Eq)
instance Show Notebook where
    show nb = " \n\
\   NOTEBOOK: \n\n\
\   Known Owned Cards: \n\
\       " ++ showMap (suspectCards nb) ++ " \n\n\
\   Suspects Possible Owners: \n\
\       " ++ showMap (suspectMap nb) ++ " \n\n\
\   Weapons Possible Owners: \n\
\       " ++ showMap (weaponMap nb) ++ " \n\n\
\   Rooms Possible Owners: \n\
\       " ++ showMap (roomMap nb) ++ " \n\n\
\ "
        where showMap :: (Show k, Show v) => Map k v -> String
              showMap = intercalate "\n\
\       " . map showEntry . toList
              showEntry (k,v) = show k ++ ": " ++ show v


-- Functions --

solved :: Show k => Map k [Suspect] -> Maybe k
solved equipMap = case map fst . toList $ M.filter null equipMap of
    [sol] -> Just sol
    []    -> Nothing
    more  -> error $ "Something went wrong; there are multiple supposed solutions: " ++ show more

known :: Map k [Suspect] -> Map k [Suspect]
known = M.filter ((== 1) . length)

unknown :: Map k [Suspect] -> Map k [Suspect]
unknown = M.filter ((/= 1) . length)

ownedBy :: Suspect -> Map k [Suspect] -> Map k [Suspect]
ownedBy sus = M.filter (== [sus])
-- CHANGE TO USE suspectCards!!!!!!!!!!!!


emptyNotebook = Notebook
    (fromList [(s,[]) | s <- allSuspects])
    (fromList [(s, allSuspects) | s <- allSuspects])
    (fromList [(w, allSuspects) | w <- allWeapons])
    (fromList [(r, allSuspects) | r <- allRooms])



    
-- INITIAL SET-UP OF Notebook BY REMOVING ONESELF AND NPCS (AND ADDING CLUES)?
-- ADDING A Suggestion TO THE NOTEBOOK

-- WRITE TESTS FOR COMPLEX FUNCTIONS


    -- Add some Suspects not owning some Cards to the Notebook
    -- I.e.: Remove given possible owners from given equipment and the equipment cards from the owners
doNotOwn :: [Suspect] -> [Card] -> Notebook -> Notebook
doNotOwn suss cards (Notebook sC sM wM rM) = Notebook newSusCards newSMap newWMap newRMap
    where newSusCards = foldr (adjust (map (\\ cards))) sC suss
          newSMap = foldr (adjust (\\ suss)) sM ss
          newWMap = foldr (adjust (\\ suss)) wM ws
          newRMap = foldr (adjust (\\ suss)) rM rs
          (ss, ws, rs) = toEquip cards

    -- Add a single certain newly-acquired clue to the Notebook
    -- I.e.: Set the Card's equipment owner to the player and add the owned card the owner
owns :: Suspect -> Card -> Notebook -> Notebook
owns sus card (Notebook sC sM wM rM) = Notebook newSusCards newSMap newWMap newRMap 
    where newSusCards = mapWithKey updateOwners sC
          updateOwners s css
            | s == sus  = if [card] `elem` css then css else [card]:css 
            | otherwise = map (delete card) css
          (newSMap, newWMap, newRMap) = case card of
            Sus s -> (adjust (\_-> [sus]) s sM, wM, rM)
            Wea w -> (sM, adjust (\_-> [sus]) w wM, rM)
            Roo r -> (sM, wM, adjust (\_-> [sus]) r rM)



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
