module L0show
    ( showProg
    , showVarList
    , showStmList
    , showEstado
    , showVal, showVal4
    , showShortList
    )
--
-- Funciones show para L0
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
-- import L0semantica
--
-- Funciones show para L0 -----------------------------------------------------
--
--
----------------------------------------------------
--
showVal4 :: Val4 -> String
-- Digitos de Val4 a string
showVal4 d = case d of
             D0     -> "0"
             D1     -> "1"
             D2     -> "2"
             _      -> error $ "showVal4: NO implementado aun para d= " ++ (show d)
--

showVal :: Val -> String
showVal = showVal4

showEstado :: (Show b) => Estado b -> String
showEstado sigma = show sigma -- XXX
--

showVar :: Var -> String
showVar v = show v -- XXX
--

showShortList  :: (Show b) => [b] -> String
showShortList lx =
    if lxSize <= 4
       then show lx
       else lxShortStr
    where
    lxSize      = length lx
    lxShort     = (take 3 lx) ++ [last lx]
    lxShortStr  = "[" ++ (show (lxShort!!0)) ++ ", "
                      ++ (show (lxShort!!1)) ++ ", "
                      ++ (show (lxShort!!2)) ++ ", ..., "
                      ++ (show (lxShort!!3)) ++ "]"

showVarList :: String -> VarList -> String
showVarList ind lv =
    case lv of
         VLvar  v       -> ind ++ showVar v ++ "\n"
         VLlv v lv'     -> ind ++ showVar v ++ ";\n"
                               ++ showVarList ind lv'
--

showStm :: (Show b) => Stm b -> String
showStm s = show s -- XXX
--

showStmList :: (Show b) => String -> StmList b -> String
showStmList ind ls = --show lv -- XXX
    case ls of
         SLstm  s       -> ind ++ showStm s ++ "\n"
         SLls s ls'     -> ind ++ showStm s ++ ";\n"
                               ++ showStmList ind ls'
--

--
-- showStmList :: (Show b) => StmList b -> String
-- showStmList ls = show ls -- XXX
--

--
showProg :: (Show b) => Prog b  -> String
showProg (Prog (lv,ls)) =   -- XXX
        "L0-Program " ++ "NOMBRE XXX" ++ "\n"
    ++ ind1 ++ "VAR\n"
    ++      showVarList ind2 lv
    ++ ind1 ++ "PROG\n"
    ++      showStmList ind2 ls
    where
        ind1= "    " -- 4 espacios
        ind2= ind1++ind1
--
