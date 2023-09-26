module L0stateGraphs
    ( StateGraph(..)
    , LabTran
    , NumStm
    , showStateGraph
    , showStateGraphMN
    )
--
-- StateGraphs
--
--mcb
where
-- Modules already defined in Haskell: ------------------------------
-- import Data.Set as S
-- import Data.List as L
--
-- Modules defined in this project: ---------------------------------
import L0sintaxis
--
import L0estados
--
import L0global
--
-- Semántica de L0 -------------------------------------------------
--

-- Instrucciones numeradas con Int
type NumStm b   = (Int, Stm b)

-- Transiciones etiquetadas con instrucciones numeradas.
type LabTran b  = (Estado b, NumStm b, Estado b)

-- Gráfica de transiciones de estados de un programa
data StateGraph b   = StateGraph ([Estado b], [LabTran b])
--
instance (Show b) => Show (StateGraph b) where
    show = showStateGraph
--
showStateGraph :: (Show b) => StateGraph b -> String
showStateGraph (StateGraph (stateList, tranList ))=
        "States:\n"
    ++ showListSep ind1 sep1 stateList
    ++  "Transitions:\n"
    ++ showListSep ind1 sep1 tranList
    where
    ind1= "    " -- 4 espacios
    sep1= "\n"   -- newline
--
--

showStateGraphMN :: (Show b) => Int->Int-> StateGraph b -> String
showStateGraphMN m n (StateGraph (stateList, tranList ))=
        "States (short list):\n"
    ++ showListMN ind1 sep1 m n stateList
    ++  "Transitions:\n"
    ++ showListMN ind1 sep1 m n tranList
    where
    ind1= "    " -- 4 espacios
    sep1= "\n"   -- newline
--
