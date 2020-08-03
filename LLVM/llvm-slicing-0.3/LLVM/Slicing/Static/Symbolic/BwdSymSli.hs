{-# LANGUAGE BangPatterns,ViewPatterns,DeriveGeneric,RankNTypes,TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields -rtsopts #-}

module LLVM.Slicing.Static.Symbolic.BwdSymSli (
 -- * Types
 SliceSummary(..),
 -- * Slice Computing
 computeSlice,
 genSliceTable, genSliceTable2 
 )
 where

import GHC.Generics ( Generic )
import Control.Arrow
import Control.DeepSeq
import Control.DeepSeq.Generics ( genericRnf )
import Control.Lens ( Lens', makeLenses, (.~), (%~), (^.) )
import Control.Monad.RWS.Strict (RWS)
import qualified Control.Monad.RWS.Strict as RW

import Data.Map ( Map )
import qualified Data.Map as M
import Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as IM
import Data.Set ( Set )
import qualified Data.Set as S
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IS
import Data.Maybe -- ( fromMaybe )
import Data.Monoid
import Data.List -- (foldl') 
import Data.Char ( isLetter )
--import qualified Data.Text as T (unpack)

import LLVM.Analysis
import LLVM.Analysis.CFG
import LLVM.Analysis.CallGraphSCCTraversal
import LLVM.Analysis.Dataflow
import LLVM.Analysis.CallGraph
import LLVM.Analysis.PointsTo
import LLVM.Analysis.PointsTo.TrivialFunction

import LLVM.Slicing.Util.Utils
import LLVM.Slicing.Data.SliceType
--import qualified LLVM.Slicing.Data.HBDD as BDD
--import Options.Applicative


import Text.Printf
import Debug.Trace
import Data.Time

--debug :: c -> String -> c
--debug = flip trace




------
data SliceSummary =
  SliceSummary {  _bwdSliceTable :: SliceTable
                , _procTblSummary :: IntMap SliceTable
                , _traceSize :: Int
--                , _sliceDiagnostics :: Diagnostics
                }
  deriving (Generic,Show)

$(makeLenses ''SliceSummary)

instance Eq SliceSummary where
  (SliceSummary bs1 ps1 _) == (SliceSummary bs2 ps2 _) =
    bs1 == bs2 && ps1 == ps2   

instance Monoid SliceSummary where
  mempty = SliceSummary mempty mempty 0 
  mappend (SliceSummary bs1 ps1 tr1) (SliceSummary bs2 ps2 tr2) =
    SliceSummary (mrgSli bs1 bs2)(IM.unionWith mrgSli ps1 ps2)(tr1+tr2)

instance NFData SliceSummary where
  rnf = genericRnf

--
data SliceAnalysis = SliceAnalysis { _sliceSumm :: SliceSummary }
  deriving (Eq, Generic)

$(makeLenses ''SliceAnalysis)

instance NFData SliceAnalysis where
  rnf = genericRnf

instance Monoid SliceAnalysis where
  mempty = SliceAnalysis { _sliceSumm = mempty }
  mappend a1 a2 =
    SliceAnalysis { _sliceSumm = _sliceSumm a1 `mappend` _sliceSumm a2 }

---------------
type Analysis = RWS SliceEnv String SliceState

runAnalysis :: RWS r b s a -> r -> s -> a 
runAnalysis a r s = fst $ RW.evalRWS a r s
analysisEnvironment = RW.asks
analysisGet = RW.get
analysisPut = RW.put
analysisLocal = RW.local

----

data SliceEnv = SEnv { procSliTbl :: !(IntMap SliceTable)  
--                     , pta :: IndirectCallSummary   -- pta  
                     , paraValMap :: !(IntMap (Value,Int))
                     , instCtrMap :: IntMap ValueIds
--                     , callCtrMap :: IntMap ValueIds 
                     }                      

data SliceState = SState { traces :: !Int      --  [Instruction]
--                         , instDepMap :: IntMap ValueIds
                         } deriving (Generic)

data SliceInfo = SInfo { _sliceTable :: !SliceTable   
                       , _instDeps :: IntMap ValueIds  
                       }
              deriving (Eq,Ord,Show,Generic)

$(makeLenses ''SliceInfo)

instance NFData SliceState where
  rnf = genericRnf

instance NFData SliceInfo where
  rnf = genericRnf

top :: SliceInfo
top = SInfo mempty mempty


meetSliceInfo :: SliceInfo -> SliceInfo -> SliceInfo
meetSliceInfo (SInfo !s1 !d1) (SInfo !s2 !d2) = 
          SInfo (mrgSli s1 s2) (IM.unionWith IS.union d1 d2)   -- IM.unionWith IS.union

sliceTransfer :: SliceInfo -> Instruction -> Analysis SliceInfo
sliceTransfer si i = do -- 
  cdM <- analysisEnvironment instCtrMap
  addTrace i 
  case i of    
    StoreInst {storeAddress = ptr, storeValue = sv} -> 
      updSliInfo i (ptr,sv) si cdM
    AtomicRMWInst {atomicRMWPointer = ptr, atomicRMWValue = av} ->
      updSliInfo i (ptr,av) si cdM
    AtomicCmpXchgInst {atomicCmpXchgPointer = ptr, atomicCmpXchgNewValue = nv} ->
      updSliInfo i (ptr,nv) si cdM
      
    InsertValueInst {insertValueAggregate = a, insertValueValue = iv} -> 
      updSliInfo i (a,iv) si cdM
----    PhiNode {} -> updSliInfo i (toValue i, toValue i) si cdM
      
    CallInst { callFunction = fv, callArguments = avs } ->
      callTransfer cdM si i fv (map fst avs)
    InvokeInst { invokeFunction = fv, invokeArguments = avs } ->
      callTransfer cdM si i fv (map fst avs)
      
    _ -> if isCtrInst i then setTrCtrDep cdM si i     -- setTrCtrDep si i IS.empty 
         else return si           
{-# INLINE sliceTransfer #-}

updSliInfo :: Instruction -> (Value,Value) -> SliceInfo -> IntMap ValueIds -> Analysis SliceInfo
updSliInfo i (ptr,v) si cdM
--  | instructionIsPhiNode i = addTrInstDep si' i l'
  | null (refs ptr) = return si    -- addTrInstDep si i l'      
  | otherwise = case valueContent ptr of      
      InstructionC PhiNode {phiIncomingValues = (map fst -> ivs)} -> do
         let ptNames = mapMaybe toVarName (map memAccessBase ivs)  
             !si2 = if null ptNames then si 
                    else (sliceTable %~ xtdSli2 ptNames l') si    -- si'
         return si2    -- addTrInstDep si2 i l'
      InstructionC SelectInst {selectTrueValue = tv, selectFalseValue = fv} -> do
         let ptNames = mapMaybe toVarName (map memAccessBase [tv,fv])
             !si2 = if null ptNames then si 
                    else (sliceTable %~ xtdSli2 ptNames l') si    -- si'
         return si2   -- addTrInstDep si2 i l'
      _ -> return si'   -- addTrInstDep si' i l'
  where
    !l' = unionLs cdM si i v
    base = memAccessBase ptr
    baseName = toVarName base
    updOrXtdSli = if isAggregate ptr || isAggType base
                  then xtdSli else updSli
    !si' = if isNothing baseName then si
           else (sliceTable %~ updOrXtdSli (fromJust baseName) l') si


callTransfer :: IntMap ValueIds -> SliceInfo -> Instruction -> Value -> [Value] -> Analysis SliceInfo
callTransfer cdM si i fv cargs =  do    -- dbgIt i $ 
  ptM <- analysisEnvironment procSliTbl 
  paraMap <- analysisEnvironment paraValMap
--  cdM <- analysisEnvironment instCtrMap
  case valueContent' fv of
    FunctionC f -> do
      let    -- fName = identifierAsString . functionName $ f  
          fID = functionUniqueId f
          procSli = IM.findWithDefault M.empty fID ptM 
          argMap = IM.filterWithKey mapF paraMap
             where mapF n (v,k) = k == -1 || elem n frmlIds
                   frmlIds = map valueUniqueId (functionParameters f)
          isArgIn v = lkpSli (toVarName' v) procSli == IS.singleton (- valueUniqueId v)
          noChgArgs = [toVarName' v | (v,k) <- IM.elems argMap, isArgIn v, k /= -1]
          noChgGlbs = [toVarName' v | (v,k) <- IM.elems argMap, isArgIn v, k == -1]
          iID = instructionUniqueId i
          iCtrDep = IS.insert iID $ findCD cdM si i      -- lkpCtrDep i si
          --
          !procSli' = M.mapWithKey fillF procSli 
             where fillF var !lx 
                     | elem var noChgGlbs = IS.empty
                     | elem var noChgArgs = iCtrDep
                     | otherwise  =  IS.unions $ [lx1,iCtrDep] ++ lxs   
                     where (lx1,lx2) = IS.partition (>0) lx
                           lxs = map (lci. negate) $ IS.toList lx2     
                   lci n = case IM.lookup n argMap of                
                            Just (gv,-1) -> unionLs cdM si i gv
                            Just (v,k) -> unionLs cdM si i (cargs !! k)
                            _  -> IS.singleton (-n)
          chgActArgs = [ (toVarName' (toActVar v k), lkpSli (toVarName' v) procSli') 
                        | (v,k) <- IM.elems argMap, not (isArgIn v)] 
             where toActVar v k = if k == -1 then v else memAccessBase (cargs !! k) 
          !si' = (sliceTable %~ updSlices argNames lxs. M.unionWithKey mapF procSli') si  
             where (argNames,lxs) = unzip chgActArgs
                   mapF k lx' lx = if (head k == '@') && not(IS.null lx') 
                                   then lx' else IS.union lx' lx
          si2 = if M.null procSli then si else si'
--          iCallInfls = IS.insert iID $ IM.findWithDefault IS.empty iID cCtrMap 
--          si2' = addInstDep2 iCallInfls (iCtrDep) si2     -- for call dependency
      return si2    -- setTrCtrDep si2 i IS.empty  
    ExternalFunctionC ef      
      | isMemCMS ef   -> updSliInfo i (cargs!!0,cargs!!1) si cdM
      | isC99Scanf ef -> updSliInfo i (cargs!!1,cargs!!0) si cdM
      | isC99Read ef  -> updSliInfo i (cargs!!0,undef) si cdM
      | otherwise     -> return si   -- setTrCtrDep si i IS.empty
      where  undef = ConstantC UndefValue {constantType=TypeVoid, constantUniqueId = 0}
    _ -> return si   -- setTrCtrDep si i IS.empty  

identifySlice ::    
       (FuncLike funcLike, HasFunction funcLike, HasCFG funcLike)
        => Module -> Lens' compositeSummary SliceSummary
        -> ComposableAnalysis compositeSummary funcLike
identifySlice m lns =
  composableAnalysisM runner (sliceAnalysis m) lns
  where
    runner a = runAnalysis a constData cache
    constData = SEnv mempty mempty mempty   -- instCtrsM callCtrsM undefined 
    cache = SState 0   -- mempty
    
sliceAnalysis :: (FuncLike funcLike, HasCFG funcLike,HasFunction funcLike)
               => Module -> funcLike -> SliceSummary -> Analysis SliceSummary
sliceAnalysis m funcLike s@(SliceSummary bs ptM _) = do  
  let envMod e = e { procSliTbl = ptM
                   , paraValMap = IM.fromList $ zip allParaIds allParaVals  
                   , instCtrMap = genCtrDepMap f   -- genFunCtrMap
--                   , callCtrMap = genFunCallCtrMap cg f
                   }
      !fact0 = (sliceTable %~ xtdSlices fArgStrs fArgSlis) top
      analysis = fwdDataflowAnalysis top meetSliceInfo sliceTransfer  -- fwdDataflowAnalysis
  localInfo <- analysisLocal envMod (dataflow funcLike analysis fact0)   -- dataflow
  tr <- getTrace 
  let trStr = "\n" ++ fName ++ fStrLn ++ "\'s #Insts_traced = " ++ show tr -- (length tr)
      fStrLn = case getFunctionLine f of
               Just (src, ln) -> "(Defined at " ++ src ++ ":" ++ show ln ++ ")"
               Nothing        -> ""  
      fName = identifierAsString (functionName f)
      SInfo bs' ideps = dataflowResult localInfo
  return $ (procTblSummary %~ IM.insertWith mrgSli fID bs')   -- (\_ y -> y)
--         $ (instDepSummary %~ IM.union ideps')   --  IM.unionWith IS.union   
         $ (traceSize %~ (+) tr)
         $! (bwdSliceTable %~ mrgSli bs') s   -- `debug` trStr
  where
    f = getFunction funcLike 
    fID = functionUniqueId f 
    (fArgStrs,fArgSlis) = unzip $ map initSlice fParaVals
      where initSlice (v,n) 
              | n == -2   =  (toVarName' v, IS.empty) 
              | otherwise =  (toVarName' v, IS.singleton $ - valueUniqueId v) 
    fParaIds = map (valueUniqueId . fst) fParaVals        
    fParaVals = frmlVals f ++ globalVals ++ allocVals f
    --
    allParaIds = map (valueUniqueId . fst) allParaVals
    allParaVals = globalVals ++ (concatMap frmlVals $ moduleDefinedFunctions m)
    frmlVals fn = zip (map toValue $ functionParameters fn) [0..]
    globalVals = zip (map toValue $ moduleGlobalVariables m) (repeat (-1))
    allocVals fn = zip (map toValue $ funcAllocInsts fn) (repeat (-2))

type IsParallel = Bool
computeSlice :: IsParallel -> Module -> SliceSummary
computeSlice isPar m = _sliceSumm res  -- `showGraph` (cg,mName)
  where
    cg = callGraph m ics []
    ics = runPointsToAnalysis m
--    mName = T.unpack $ moduleIdentifier m
    analyses :: [ComposableAnalysis SliceAnalysis Function]
    analyses = [ identifySlice m sliceSumm ]
    analysisFunc = callGraphComposeAnalysis analyses   
    res = cgTraversal cg analysisFunc mempty 
    cgTraversal = if isPar then parallelCallGraphSCCTraversal else callGraphSCCTraversal

-------------
genSliceTable2 :: IsParallel -> Module -> (Map String [String],Map String [String])
genSliceTable2 isPar m = (toSrcLns bwdST, toSrcLns fwdST)
  where (bwdST,fwdST) = genSliceTable isPar m
        valMap = genValueMap m
        toSrcLns = M.map (toSrcLnStr valMap)

genSliceTable :: IsParallel -> Module -> (SliceTable,SliceTable)    
genSliceTable isPar m = (bwdSlices, error "BwdSymSli don't support forward slicing, please try SymSlicer!")
                       `debug` trStr 
  where summary = computeSlice isPar m
        aliasMap = genAliasMap m
        valMap = genValueMap m
        bwdSlices = reduceKey. addAlias aliasMap $ _bwdSliceTable summary
--        fwdSlices = reduceKey. addAlias aliasMap $ _fwdSliceTable summary
        reduceKey = M.mapKeys (drop 1). M.filterWithKey (\(_:k:_) _->(isLetter k || k == '_'))
        trStr = "\n\tIts trace Info: #Insts_bwdSym = " ++ show (_traceSize summary)
     
addAlias :: Map String String -> SliceTable -> SliceTable
addAlias am s = updSlices aliNames aliSlices s
  where  
    (aliNames,aliSlices) = unzip . concatMap fromAlias $ M.toList am
    fromAlias (v1,v2) = [(v1,unionSli),(v2,unionSli)] 
      where unionSli = IS.union (lkpSli v1 s) (lkpSli v2 s)

getBwdSliTbl = fst . genSliceTable False     
    
----------------------------------
------
unionLs :: IsValue a => IntMap ValueIds -> SliceInfo -> Instruction -> a -> ValueIds
unionLs cdM si i v = unionL' l l0 (v,ss)
  where !l0 = findCD cdM si i  
        iID = instructionUniqueId i 
        !l = IS.insert iID $ lkpCtrDep i si       -- IS.singleton iID
        ss = si ^. sliceTable
{-# INLINE unionLs #-}

unionL' :: IsValue a => ValueIds -> ValueIds -> (a,SliceTable) -> ValueIds
unionL' l l0 (i,s) = IS.unions [l, l0, refIds, unionLkpSli refStrs s]
  where !refValues = refVals i   -- valueRefs2 
        !refIds = IS.fromList . HS.toList . HS.map valueUniqueId $ refValues 
        !refStrs = mapMaybe toVarName . HS.toList $ refValues    -- refs
{-# INLINE unionL' #-}

---
setTrCtrDep :: IntMap ValueIds -> SliceInfo -> Instruction -> Analysis SliceInfo  
setTrCtrDep cdM si i = do
--  addTrace i
  let !l' = unionLs cdM si i i
      iID = instructionUniqueId i
      !si' = addInstDep iID l' si
  return $! si' 

addTrInstDep :: SliceInfo -> Instruction -> ValueIds -> Analysis SliceInfo
addTrInstDep si i l' = do 
--  addTrace i
  return $! addInstDep (instructionUniqueId i) l' si 

addInstDep :: UniqueId -> ValueIds -> SliceInfo -> SliceInfo
addInstDep n d = instDeps %~ IM.insert n d     -- IM.insertWith' IS.union
{-# INLINE addInstDep #-}

addInstDep2 :: ValueIds -> ValueIds -> SliceInfo -> SliceInfo
addInstDep2 ns d si = IS.foldl' (\si' n -> addInstDep n d si') si ns   
{-# INLINE addInstDep2 #-}

lkpCtrDep ::  Instruction -> SliceInfo -> ValueIds
lkpCtrDep i si = 
   IM.findWithDefault IS.empty (instructionUniqueId i) $ si ^. instDeps
{-# INLINE lkpCtrDep #-}

findCD ::  IntMap ValueIds -> SliceInfo -> Instruction -> ValueIds
findCD cdM si i = cds
  where cds = IS.unions [lkpCD ci | ci <- IS.toList cis]
        lkpCD n = IM.findWithDefault IS.empty n $ si ^. instDeps
        cis = IM.findWithDefault IS.empty (instructionUniqueId i) cdM
{-# INLINE findCD #-}

---
getTrace :: Analysis Int -- [Instruction]
getTrace = do {s <- analysisGet; return (traces s) }  

addTrace :: Instruction -> Analysis ()
addTrace i = do 
  s <- analysisGet  
  let tr' = 1 + traces s   -- i : traces s -- ++ [i]
  analysisPut s { traces = tr'}







