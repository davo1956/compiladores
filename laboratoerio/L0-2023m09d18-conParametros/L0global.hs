module L0global
    ( showListSep
    , showListMN
    , showShortList
    )
--
-- Global functions
--
--mcb
where
-- Modules already defined in Haskell: ------------------------------
-- import Data.Set as S
-- import Data.List as L
--
-- Modules defined in this project: ---------------------------------

--
--
-- Global functions: -------------------------------------------------
--

showListSep  :: (Show b) => String -> String -> [b] -> String
showListSep ind sep lx =
    case lx of
         []     -> ""
         x:lx'  -> ind ++ show x ++ sep
                       ++ showListSep ind sep lx'

--

--
showShortList  :: (Show b) => String->String-> [b] -> String
showShortList ind sep lx =
    if lxSize <= 4
       then showListSep ind sep lx
       else showListSep ind sep lxShortStr
    where
    lxSize      = length lx
    lxShort     = (take 3 lx) ++ [last lx]
    lxShortStr  = [   (show (lxShort!!0))
                    , (show (lxShort!!1))
                    , (show (lxShort!!2))
                    , "..."
                    , (show (lxShort!!3))
                    ]
--

showListMN  :: (Show b) => String->String-> Int->Int-> [b] -> String
showListMN ind sep m n lx =
    if m+n >= lxSize
       then showListSep ind sep lx
       else showListSep ind sep lxMNstr
    where
    lxSize      = length lx
    lxFirstm    = take m lx
    lxLastn     = drop (lxSize - n) lx
    lxMNstr     =  [show x | x  <- lxFirstm]
                ++ ["..."]
                ++ [show x | x  <- lxLastn]

--     lxShort     = (take 3 lx) ++ [last lx]
--     lxShortStr  = [   (show (lxShort!!0))
--                     , (show (lxShort!!1))
--                     , (show (lxShort!!2))
--                     , "..."
--                     , (show (lxShort!!3))
--                     ]
--
