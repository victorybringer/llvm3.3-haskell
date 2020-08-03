{-# LANGUAGE FlexibleInstances,TypeSynonymInstances,MultiParamTypeClasses,NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -XUndecidableInstances #-}

module LLVM.Slicing.Data.SliceType ( 
  -- * Types
  LabelType, LabelSet, 
  SliceTable, 
  SliceMetrics(..),
  -- * Constructor
  lkpSli, mrgSli,
  updSli, updSli2, updSlices,  
  xtdSli, xtdSli2, xtdSlices,    
  unionLkpSli, 
  computeSliMetrics,
  -- * Visualization
  showSlices, showSlices2, 
  showMapWith, showMetricMap
  )
 where
 
import Data.List  (delete,foldl',intersperse,foldl1') 
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Map (Map)
import qualified Data.Map as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Text.Printf  (printf)


type Name = String
type LabelType = Int
type LabelSet = IntSet  
type SliceTable = Map Name LabelSet 


instance {-# OVERLAPPING #-} Show SliceTable where
 show ds = showSlices ds
 
showSlices :: SliceTable -> String
showSlices = showSlices' IS.toList
showSlices2 = showSlices' id
showSlices' = showMapWith "\n Variable      SrcLineNumbers  "

showMapWith :: Show b => String -> (a -> [b]) -> Map String a -> String
showMapWith header toList ds = 
  if M.null ds then "\nThe final table is NULL !" 
  else  header ++ "\n------------------------------\n" ++ res
 where res = unlines . buildTable $ mapSnds (concat . (intersperse ","). (map show). toList) ds'
       ds' = M.toAscList ds



-- lkpSli n ds = list of labels that reference n in ds
lkpSli :: Name -> SliceTable -> LabelSet
lkpSli x ls =  {-# SCC lkpSli #-} M.findWithDefault IS.empty x ls
--    case M.lookup x ls of
--     Nothing -> IS.empty  --[]
--     Just ls -> ls
{-# INLINE lkpSli #-}

-- updSli n ls ds = updates ds assigning ls to n
-- {-# SCC "updSlice" #-}
updSli :: Name -> LabelSet -> SliceTable -> SliceTable
updSli =  M.insert   

updSli2 :: [Name] -> LabelSet -> SliceTable -> SliceTable           
updSli2 args ls ds
           = foldl'
              (\r arg -> updSli arg ls r) ds args

updSlices :: [Name] -> [LabelSet] -> SliceTable -> SliceTable
updSlices args [] ds = ds
--           = foldl'
--              (\r arg -> updSli arg IS.empty r) ds args              
updSlices args lcs ds
           = foldl'
              (\r (arg,lc)-> updSli arg lc r) ds
                [(arg,lc) | (arg,lc) <- zip args lcs]

-- xtdSli n ls ds = extends ds assigning ls to n
xtdSli :: Name -> LabelSet -> SliceTable -> SliceTable
xtdSli = M.insertWith' IS.union 
--xtdSli x ls [] = [(x,ls)]
--xtdSli x ls ((y,ls'):ds) 
--    | x == y    = (x,IS.union ls ls'):ds
--    | otherwise = (y,ls'):xtdSli x ls ds
    
xtdSli2 :: [Name] -> LabelSet -> SliceTable -> SliceTable
xtdSli2 args ls ds
           = foldl'
              (\r arg -> xtdSli arg ls r) ds args

xtdSlices :: [Name] -> [LabelSet] -> SliceTable -> SliceTable
xtdSlices args [] ds = ds
--           = foldr(\arg r -> xtdSli arg IS.empty r) ds args
xtdSlices args lcs ds
           = foldl'
              (\r (arg,lc) -> xtdSli arg lc r) ds
                [(arg,lc) | (arg,lc) <- zip args lcs]
              
-- mrgSli ss ss' = merges two slices ss and ss'
mrgSli ::  SliceTable -> SliceTable -> SliceTable
mrgSli = {-# SCC mrgSli #-} M.unionWith IS.union
{-# INLINE mrgSli #-}

-- unionLkpSli ns ls = union of the slices corresponding
--                     to each variable v in ns 
unionLkpSli :: [Name] -> SliceTable -> LabelSet
unionLkpSli vs ls = {-# SCC unionLkpSli #-} 
  IS.unions. M.elems $ M.filterWithKey (\k _ -> elem k vs) ls
--    IS.unions [lkpSli v ls | v <- vs]    
{-# INLINE unionLkpSli #-}
 

------- Utils
buildTable :: [(String,String)] -> [String]
buildTable ps = map f ps where
    f (x,y) = " " ++ x ++ replicate (bs - length x) ' ' 
              ++ replicate 8 ' ' ++ "{" ++ y ++ "}"
    bs = maximum (map (length . fst) ps)

mapSnds :: (a -> b) -> [(c,a)] -> [(c,b)]
mapSnds g xs = [(x, g y) | (x,y) <- xs]


------- Test
s1 :: SliceTable
s1 = M.fromList [("ta",IS.fromList [1]),("bd",IS.fromList [1,3,2,3,4,4,4,4,4,4]),
      ("0cd",IS.fromList [2,3,1]),("d",IS.fromList [4])]

s2 = updSli2 ["ta","hello"] IS.empty s1


-------------------------
----- Slice-based Metrics
data SliceMetrics = Metrics {
                   tightness :: !Double,
                   coverage :: !Double,
                   minCover :: !Double,
                   maxCover :: !Double,
                   overlap  :: !Double,
                   coupling :: !Double } deriving (Eq)

instance Show SliceMetrics where
--  show Empty = "N/A: empty slice table!"
  show (Metrics t c ic ac o p) = printf "(%.2f, %.2f, %.2f, %.2f, %.2f, %.3f)" t c ic ac o p

showMetricMap :: Show a => Map String a -> String
showMetricMap = showMapWith
    "\n Function    (Tightness,Coverage,Min-Cover,Max-Cover,Overlap,Coupling)  "  (:[])

computeSliMetrics :: [IntSet] -> Int -> SliceMetrics  
computeSliMetrics fSlices' fSize' =  if null fSlices then Metrics 0 0 0 0 0 0
     else Metrics tightness coverage minCover maxCover overlap coupling
  where   
    tightness = fSliInters / fSize
    coverage = (sum fSliSizes) / fSize / fOutSize
    minCover = (minimum fSliSizes) / fSize
    maxCover = (maximum fSliSizes ) / fSize
    overlap  = (sum $ map (fSliInters/) fSliSizes) / fOutSize
    coupling = fSliUnions / fSize
    --
    fSlices = filter (not . IS.null) fSlices'
    fSize = fromIntegral fSize'   
    fSliSizes = map (fromIntegral . IS.size) fSlices
    fSliInters = fromIntegral . IS.size $ intersections fSlices
    fSliUnions = fromIntegral . IS.size $ IS.unions fSlices'
    fOutSize = fromIntegral $ length fSlices    

intersections :: [IntSet] -> IntSet
intersections iss = case iss of
  []  ->  IS.empty
  [is] -> is
  _   -> foldl1' IS.intersection iss