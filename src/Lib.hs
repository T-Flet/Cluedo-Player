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

import Data.Map.Strict (Map, keys, fromList, toList, (!), adjust, mapWithKey, partitionWithKey, keys)
import qualified Data.Map.Strict as M (filter, foldr, partition) 
import Data.Tuple (swap)
import Data.List ((\\), intercalate, nub)
import Data.Set (Set, empty, singleton, delete, insert)
import qualified Data.Set as S ((\\), fromList, toList, map, filter)



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

-- instance Show (Set Card) where
    -- show = intercalate ", " . S.toList . S.map show
    -- FIND A WAY TO ACHIEVE THIS EFFECT!!!!!!!!!!!!!!!!!!!!!!!!!!!


data Circumstances = Circumstances { suspect :: Suspect, weapon :: Weapon, room :: Room }
    deriving (Eq, Read)
instance Show Circumstances where
    show (Circumstances sus wea roo) = "It was " ++ show sus ++ " with the " ++ show wea ++ " in the " ++ show roo ++ "!"


-- Functions --

-- Lists of all Cards and Equipment
allCards = enumFromTo (Sus MissScarlett) (Roo Study) 
allSuspects = enumFrom MissScarlett
allWeapons = enumFrom Candlestick
allRooms = enumFrom Hall


-- Maps (NOT FUNCTIONS; use: map ! key) from Equipment to Unique Strings and vice-versa
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
                    

toCard :: String -> Card
toCard str
    | str `elem` keys toSuspect = Sus $ toSuspect ! str
    | str `elem` keys toWeapon  = Wea $ toWeapon  ! str
    | str `elem` keys toRoom    = Roo $ toRoom    ! str
    | otherwise = error $ "Invalid unique string: " ++ str


-- Split Cards into each equipment type
toEquip :: [Card] -> ([Suspect], [Weapon], [Room])
toEquip = foldr addEquip ([], [], [])
    where addEquip (Sus s) (ss, ws, rs) = (s:ss, ws, rs)
          addEquip (Wea w) (ss, ws, rs) = (ss, w:ws, rs)
          addEquip (Roo r) (ss, ws, rs) = (ss, ws, r:rs)



---- 2 - GAMEPLAY RELATED DATA TYPES -------------------------------------------

data Player = Player { pName :: String, pStatus :: PlayerStatus, pCardNum :: Int }
    deriving (Eq, Show)

data PlayerStatus = NPC | Playing | Eliminated
    deriving (Eq, Show, Read)

data Suggestion = Suggestion { circumstances :: Circumstances, suggester :: Suspect, unhelpful :: [Suspect], resolver :: Suspect }
    deriving (Eq)
instance Show Suggestion where
    show (Suggestion circs sugg unhe resv) = " \n\
\   SUGGESTION by " ++ show sugg ++ ":   " ++ show circs ++ "\n\n\
\       Could not help: " ++ intercalate ", " (map show unhe) ++ "\n\n\
\       Helped: " ++ show resv ++ "\n\n\
\ " 


    -- The [[Card]] value is the list of possible sets of cards each player could have
    -- The [Suspect] values are the Players who possibly possess that clue
data Notebook = Notebook { suspectCards :: Map Suspect (Set (Set Card)),
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
-- ADD Xs BEFORE LISTS IF THEY ARE OF LENGTH ONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


-- Functions --

-- Notebook Card Queries

    -- Return all cards known to be possessed by a specific player and then all the other known to be possessed ones
playerOthersKnownCards :: Suspect -> Notebook -> ([Card], [Card])
playerOthersKnownCards sus = tupleApply allKnown . partitionWithKey (\k _-> k == sus) . suspectCards
    where allKnown = M.foldr onlyKnown []
          onlyKnown possCards acc = singletons possCards ++ acc


-- Notebook Maps Queries

solved :: Show k => Map k [Suspect] -> Maybe k
solved equipMap = case map fst . toList $ M.filter null equipMap of
    [sol] -> Just sol
    []    -> Nothing
    more  -> error $ "Something went wrong; there are multiple supposed solutions: " ++ show more


    -- Return all known and unknown equipment from an equipment map
knownUnknown :: Map k [Suspect] -> ([k],[k])
knownUnknown = tupleApply keys . M.partition ((== 1) . length)


-- Interface Notebook modifiers

-- Notebook creating and updating functions
emptyNotebook = Notebook
    (fromList [(s,empty) | s <- allSuspects])
    (fromList [(s, allSuspects) | s <- allSuspects])
    (fromList [(w, allSuspects) | w <- allWeapons])
    (fromList [(r, allSuspects) | r <- allRooms])

    
    -- Initial Notebook set-up: remove npcs and oneself as possible owners and then add one's own cards
initNotebook :: [Suspect] -> Suspect -> [Card] -> Notebook -> Notebook
initNotebook npcs owner cards nb = foldr (owns owner) (doNotOwn (owner:npcs) allCards nb) cards


    -- Add a suggestion to Notebook
addSuggestion :: Suggestion -> Notebook -> Notebook
addSuggestion (Suggestion (Circumstances s w r) sugg unhe resv) nb
    | null resvUnknownCs = nnb
    | otherwise = Notebook (adjust (insert (S.fromList resvUnknownCs)) resv sC) sM wM rM
        where nnb@(Notebook sC sM wM rM) = doNotOwn unhe suggCs  nb
              resvUnknownCs = suggCs \\ otherKnownCs
              (resvKnownCs, otherKnownCs) = playerOthersKnownCards resv nb
              suggCs = [Sus s, Wea w, Roo r]


-- Low-level Notebook Modifiers

    -- Add some Suspects not owning some Cards to the Notebook
    -- I.e.: Remove given possible owners from given equipment and the equipment cards from the owners
doNotOwn :: [Suspect] -> [Card] -> Notebook -> Notebook
doNotOwn suss cards (Notebook sC sM wM rM) = Notebook newSusCards newSMap newWMap newRMap
    where newSusCards = foldr (adjust (S.map (S.\\ cardSet))) sC suss
          newSMap = foldr (adjust (\\ suss)) sM ss
          newWMap = foldr (adjust (\\ suss)) wM ws
          newRMap = foldr (adjust (\\ suss)) rM rs
          (ss, ws, rs) = toEquip cards
          cardSet = S.fromList cards


    -- Add a single certain newly-acquired clue to the Notebook
    -- I.e.: Set the Card's equipment owner to the player and add the owned card the owner
owns :: Suspect -> Card -> Notebook -> Notebook
owns sus card (Notebook sC sM wM rM) = Notebook newSusCards newSMap newWMap newRMap 
    where newSusCards = mapWithKey updateOwners sC
          updateOwners s css
            | s == sus  = insert (singleton card) css 
            | otherwise = S.map (delete card) css
          (newSMap, newWMap, newRMap) = case card of
            Sus s -> (adjust (const [sus]) s sM, wM, rM)
            Wea w -> (sM, adjust (const [sus]) w wM, rM)
            Roo r -> (sM, wM, adjust (const [sus]) r rM)



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


    -- Return all singletons from a Set of Sets
singletons :: Ord a => Set (Set a) -> [a]
singletons = concat . S.map S.toList . S.filter ((== 1) . length)


    -- Apply a function to both elements of a tuple
tupleApply :: (a -> b) -> (a,a) -> (b,b)
tupleApply f (x,y) = (f x, f y)
