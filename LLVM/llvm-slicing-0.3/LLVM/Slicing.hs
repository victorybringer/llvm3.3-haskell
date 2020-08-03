{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields -rtsopts #-}

module LLVM.Slicing (
  -- * Types
  module LLVM.Slicing.Static.SDG.SDGType,
  module LLVM.Slicing.Data.SliceType,  
  module LLVM.Slicing.Util.Utils,
  -- * Slice operators 
  genSliceTableWith,
  compareSliceMethod,compareSliceMethod',
  compareSliceTable,compareSliceTable',
  -- * Show related graphs
  genSDGgraph, genGraphs, genGraphs',
  genICFGgraph, genCGgraph, 
  vizGraphDot, vizGraphPng, vizGraphHtml,
  vizGraphPngHtml, vizGraphPngHtml',
  mkCFGs, mkCDGs, mkPDTs, mkDTs, mkCG, mkICfg,mkSDG,
  -- * Print Debug Info.
  printDbgInfo,
  -- * Print Metrics
  printMetrics
 ) where

import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IS
import Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as IM
import Text.Printf  ( printf )
import Data.Char ( isLetter )
import Data.Maybe ( maybeToList )
import Data.List ( nub,delete )
import System.FilePath (takeBaseName)

import qualified Data.Text.Lazy as L ( unpack )
import qualified Data.Text as T (unpack)
import qualified Data.Graph.Inductive as G   -- ( grev,nmap )
import qualified Data.GraphViz as G  ( printDotGraph,GraphvizOutput(..) ) 
import Control.Arrow ( (&&&) )

import LLVM.Analysis 

import LLVM.Analysis.CFG
import LLVM.Analysis.CDG
import LLVM.Analysis.CallGraph
import LLVM.Analysis.Dominance
import qualified LLVM.Analysis.PointsTo.TrivialFunction as PT
import LLVM.Analysis.PointsTo.Andersen
import LLVM.Analysis.PointsTo
import LLVM.VisualizeGraph
import LLVM.Slicing.Static.SDG.IFDS.ICFG ( mkICFG,ICFG )

import LLVM.Slicing.Util.Utils
import LLVM.Slicing.Data.SliceType 
import LLVM.Slicing.Static.SDG.SDGType 
--import LLVM.Slicing.Static.Symbolic.SymADT  
--import LLVM.Slicing.Data.ValueDepType  
import qualified LLVM.Slicing.Static.Symbolic.SymSlicer as Sym
import qualified LLVM.Slicing.Static.Symbolic.BwdSymSli as BwdSym
--import qualified LLVM.Slicing.Static.Symbolic.SymSlicer2 as SymBDD
import qualified LLVM.Slicing.Static.SDG.SDGSlicer as SDG
import qualified LLVM.Slicing.Static.SDG.IFDS.IFDSSlicer as IFDS
import qualified LLVM.Slicing.Static.Weiser.MWeiser as MW
import qualified LLVM.Slicing.Static.InfoFlow.InfoFlowSlicer as IF  

    
-- | Get backward/forward slice tables with a given method
genSliceTableWith :: String -> Bool -> Module -> (SliceTable,SliceTable)
genSliceTableWith method isPar m
  | elem method ["sym","symbolic","Symbolic","Sym","SymSlicer"] =  -- Sym.genSliceTable isPar m 
       (fst $! BwdSym.genSliceTable isPar m, snd $ Sym.genSliceTable isPar m) 
  | elem method ["Sym2","sym2"] = Sym.genSliceTable isPar m  
  | elem method ["Info","info","if","InfoFlow","infoFlow"] = IF.genSliceTable isPar m
  | elem method ["sdg","SDG","SDGSlicer","sdgSlicer"] =  SDG.genSliceTable isPar m
  | elem method ["ifds","IFDS","IFDSSlicer","ifdsSlicer"] =  IFDS.genSliceTable isPar m
  | elem method ["weiser","Weiser","MWeiser","WeiserSlicer"] =  
        if isPar then error "Weiser cann't support to compute slices parallelly!" 
        else MW.genSliceTable m
  | otherwise  = error "Unknown the slice method (Symbolic,SDG,IFDS or Weiser)!"

  
-- | Compare the final results of two slice methods
compareSliceMethod :: Module -> (String,String) -> String  
compareSliceMethod m (md1,md2) = sliceDiffs  
  where 
   (bwdSliDiff,fwdSliDiff) = compareSliceMethod' m (md1,md2)   -- timeIt "Both" $    
   weisers = ["weiser","Weiser","MWeiser","WeiserSlicer"]
   isWeiser = elem md1 weisers || elem md2 weisers
   sliceDiffs = if isWeiser then bwdSliDiff   
                else bwdSliDiff ++ "\n\n" ++ fwdSliDiff 

compareSliceMethod' :: Module -> (String,String) -> (String,String)  
compareSliceMethod' m (md1,md2) = (bwdSliDiff,fwdSliDiff)  
  where
   (bwd1,fwd1) = genSliceTableWith md1 False m     -- timeIt md1 $ 
   (bwd2,fwd2) = genSliceTableWith md2 False m     -- timeIt md2 $ 
   valMap = genValueMap m
   toSrcLns = M.map (toSrcLnStr valMap)
   st1 *-* st2 = compareSliceTable m st1 st2
   bwdSliDiff = printf ("\nBwdSliceTable_1(%s): %s \n BwdSliceTable_2(%s): %s \
          \ \nBwdSliceDiff_1(%s - %s): %s \nBwdSliceDiff_2(%s - %s): %s ")
           md1 (showSlices2 $ toSrcLns bwd1) md2 (showSlices2 $ toSrcLns bwd2)
           md1 md2 (showSlices2 diff1) md2 md1 (showSlices2 diff2)
          where   (diff1, diff2) = (bwd1 *-* bwd2)
   fwdSliDiff = printf ("FwdSliceTable_1(%s): %s \nFwdSliceTable_2(%s): %s \
          \ \nFwdSliceDiff_1(%s - %s): %s \nFwdSliceDiff_2(%s - %s): %s ")
           md1 (showSlices2 $ toSrcLns fwd1) md2 (showSlices2 $ toSrcLns fwd2)
           md1 md2 (showSlices2 diff1) md2 md1 (showSlices2 diff2)
          where   (diff1, diff2) = (fwd1 *-* fwd2)


-- | Compare two slice tables
compareSliceTable :: Module -> SliceTable -> SliceTable 
                      -> (M.Map String [String],M.Map String [String])  
compareSliceTable m st1 st2 = (toSrcLns $ diffSli st1, toSrcLns $ diffSli st2)
  where  
    allFunIds = IS.fromList $ map valueUniqueId (moduleDefinedFunctions m) 
                           ++ map valueUniqueId (moduleExternalFunctions m)
    bothSli = M.intersectionWith IS.intersection st1 st2  
    diffSli st = M.differenceWith mapF st bothSli
    mapF s1 s2 = let s' = s1 IS.\\ s2 IS.\\ allFunIds in
                 if IS.null s' then Nothing else Just s' 
    valMap = genValueMap m
    toSrcLns = M.map (toSrcLnStr valMap)
    

compareSliceTable' :: SliceTable -> SliceTable -> (SliceTable,SliceTable)  
compareSliceTable' st1 st2 = (diffSli st1, diffSli st2)
  where 
    bothSli = M.intersectionWith IS.intersection st1 st2  
    diffSli st = M.differenceWith mapF st bothSli
    mapF s1 s2 = let s' = s1 IS.\\ s2 in
                 if IS.null s' then Nothing else Just s' 

--showDiffWith opt fn = do
--  let !m = file2ModuleWith ["-g"] opt fn
--      res = compareSliceMethod m ("Sym","IFDS")
--  putStrLn res
  
  
--------------
-- | Compute slice-based Metrics
printMetrics :: FilePath -> IO ()
printMetrics fn = do
  let instDepCoh = "\n\tMetrics with InstDepTable: " ++ showMetricMap instCohMap
      bwdSliCoh = "\n\n\tMetrics with BwdSliceTable: " ++ 
                  showMetricMap (sliCohMapWith bwdSliTbl)
      fwdSliCoh = "\n\tMetrics with FwdSliceTable: " ++  
                  showMetricMap (sliCohMapWith fwdSliTbl)      
      bothSliCoh = "\n\tMetrics with (BwdSli + FwdSli) Table: " ++  
                  showMetricMap (sliCohMapWith $ mrgSli bwdSliTbl fwdSliTbl)     
      bwdSliCoup = "\n\n\tCoupling(f,g) with BwdSliceTable: " ++ 
                  showCoupMap (sliCoupMapWith bwdSliTbl)
      bothSliCoup = "\n\n\tCoupling(f,g) with (BwdSli + FwdSli) Table: " ++ 
                  showCoupMap (sliCoupMapWith $ mrgSli bwdSliTbl fwdSliTbl) 
      bwdSrcLnCoh = "\n\n\tMetrics with SrcLine-based BwdSliceTable: " ++ 
                  showMetricMap (srcLnCohMapWith bwdSliTbl)         
      bothSrcLnCoh = "\n\tMetrics with SrcLine-based (BwdSli + FwdSli) Table: " ++  
                  showMetricMap (srcLnCohMapWith $ mrgSli bwdSliTbl fwdSliTbl)  
      bwdSrcLnCoup = "\n\n\tCoupling(f,g) with SrcLine-based BwdSliceTable: " ++ 
                  showCoupMap (srcLnCoupMapWith bwdSliTbl)
      bothSrcLnCoup = "\n\n\tCoupling(f,g) with SrcLine-based (BwdSli + FwdSli) Table: " ++ 
                  showCoupMap (srcLnCoupMapWith $ mrgSli bwdSliTbl fwdSliTbl)
      allOutVars = "\n\tOutput_variable Table: " ++ showVarMap outVarMap
      bwdSliCoh2 = "\n\n\tMetrics with SDG's BwdSliceTable: " ++ 
                  showMetricMap (sliCohMapWith bwdSliTbl2)
      fwdSliCoh2 = "\n\tMetrics with SDG's FwdSliceTable: " ++  
                  showMetricMap (sliCohMapWith fwdSliTbl2)
      bwdSliCoup2 = "\n\n\tCoupling(f,g) with SDG's BwdSliceTable: " ++ 
                  showCoupMap (sliCoupMapWith bwdSliTbl2)     
      bothSliCoh2 = "\n\tMetrics with SDG's (BwdSli + FwdSli) Table: " ++  
                  showMetricMap (sliCohMapWith $ mrgSli bwdSliTbl2 fwdSliTbl2)
      bothSliCoup2 = "\n\n\tCoupling(f,g) with SDG's (BwdSli + FwdSli) Table: " ++ 
                  showCoupMap (sliCoupMapWith $ mrgSli bwdSliTbl2 fwdSliTbl2)
      bwdSliCoh3 = "\n\n\tMetrics with Weiser's BwdSliceTable: " ++ 
                  showMetricMap (sliCohMapWith bwdSliTbl3)
      bwdSliCoup3 = "\n\n\tCoupling(f,g) with Weiser's BwdSliceTable: " ++ 
                  showCoupMap (sliCoupMapWith bwdSliTbl3)     
      metricInfo = concat ["\nIts slice-based metric results:",
                           instDepCoh,allOutVars,
                           bwdSliCoh,fwdSliCoh,bothSliCoh,bwdSliCoup,bothSliCoup,
                           bwdSliCoh2,fwdSliCoh2,bothSliCoh2,bwdSliCoup2,bothSliCoup2,
                           bwdSliCoh3,bwdSliCoup3, "\n\n"]
      metricInfo2 = concat [">> Comparsion of Source-line and IR-instruction slice-based metrics <<",
                           instDepCoh,allOutVars,
                           bwdSliCoh,bwdSrcLnCoh,bothSliCoh,bothSrcLnCoh,
                           bwdSliCoup,bwdSrcLnCoup,bothSliCoup,bothSrcLnCoup, "\n\n"]
--  writeFile (fn ++ ".SrcLnMetrics") metricInfo2  
  writeFile (fn ++ ".SliceMetrics") metricInfo 
  putStrLn metricInfo  
--  putStrLn metricInfo2  
--  putStrLn (show bwdSliTbl)
 where
   m = fn2mod fn  
   valMap = genValueMap m 
   !summ = Sym.computeSlice False m
   procValDepMap  =  Sym._procInstDepSumm summ  
   instDepMap =  Sym._instDepSummary summ  
   (bwdSliTbl,fwdSliTbl) = Sym.getSliceTable summ valMap m
   (bwdSliTbl2,fwdSliTbl2) = SDG.genSliceTable False m
   (bwdSliTbl3,_) = MW.genSliceTable m
--   instDepRel = imis2r instDepMap  
--   srcLnDepMap = instDep2SrcLnDep' valMap instDepMap
--   srcLnDepRel = imis2r srcLnDepMap 

   instCohMap = M.fromList . zip fNames $ map fInstCohension fs
     where fInstCohension f = getInstCohension instDepMap (fOutValIDs f) f
   sliCohMapWith sliTbl = M.fromList . zip fNames $ map fSliCohension fs
     where fSliCohension f = (getSliCohension sliTbl (fOutVars f) f)
                              {coupling = fCouplingWith sliTbl f}
   srcLnCohMapWith sliTbl = M.fromList . zip fNames $ map fSrcLnCohension fs
     where fSrcLnCohension f = (getSrcLnSliCoh valMap sliTbl (fOutVars f) f)
                              {coupling = fCouplingWith2 sliTbl f}
                              
   sliCoupMapWith sliTbl = M.fromList [(fgShow f g, [fgCouplingWith sliTbl f g]) 
                         | f <- fs, g <- fs, valueUniqueId f /= valueUniqueId g]  
     where fgShow f g = show (functionName f) ++ " -> " ++ show (functionName g)  
   srcLnCoupMapWith sliTbl = M.fromList [(fgShow f g, [fgCouplingWith2 sliTbl f g]) 
                         | f <- fs, g <- fs, valueUniqueId f /= valueUniqueId g]  
     where fgShow f g = show (functionName f) ++ " -> " ++ show (functionName g)
   showCoupMap = showMapWith "\n    f -> g         Coupling(f,g)  " id 
   
   fCouplingWith sliTbl f = fAllCouplings / allSize 
     where fAllCouplings = sum [(fgCouplingWith sliTbl f g) * (fSize g) | g <- fs'] 
           allSize = sum $ map fSize fs'
           fs' = delete f fs            
   fCouplingWith2 sliTbl f = fAllCouplings / allSize 
     where fAllCouplings = sum [(fgCouplingWith2 sliTbl f g) * (fSize2 g) | g <- fs'] 
           allSize = sum $ map fSize2 fs'
           fs' = delete f fs  
   fgCouplingWith :: SliceTable -> Function -> Function -> Double
   fgCouplingWith sliTbl f g = ((fgCouple f g)* fsize +(fgCouple g f)* gsize)/(fsize + gsize)
     where  fgCouple f g = coupling $ getSliCohension sliTbl (fOutVars g) f
            fsize = fSize f
            gsize = fSize g
   fgCouplingWith2 sliTbl f g = ((fgCouple f g)* fsize +(fgCouple g f)* gsize)/(fsize + gsize)
     where  fgCouple f g = coupling $ getSrcLnSliCoh valMap sliTbl (fOutVars g) f
            fsize = fSize2 f
            gsize = fSize2 g
   --        
   outVarMap = M.fromList . zip fNames $ map (HS.toList. fOutVars) fs
   showVarMap = showMapWith "\n Function    Output Variables  " id 
   fs = moduleDefinedFunctions m
   fNames = map (show . functionName) fs   -- identifierAsString
   glbVals = map toValue $ moduleGlobalVariables m
   fSize = fromIntegral . length . functionInstructions
   fSize2 = fromIntegral . length . concatMap valueLine . functionInstructions
   fOutVars =  HS.map (drop 1. toVarName') . fOutVals
   fOutValIDs = hsToIS . fOutVals
   fOutVals = genFunOutVars procValDepMap outputValMap glbVals
   outputValMap = M.fromList . zip fNames' $ map funcOutputVars fs' 
   fs' = moduleDefinedFunctions $ fn2mod' fn
   fNames' = map (show . functionName) fs'


getSliCohension :: SliceTable -> HashSet String -> Function -> SliceMetrics
getSliCohension sliTbl vars f = computeSliMetrics fSlices fSize  -- `debug` (show fSize)
  where 
    fInsts = functionInstructions f
    fInstIDs = IS.fromList $ map valueUniqueId fInsts
    fSize = length fInsts
    fSlices = map (IS.intersection fInstIDs) $ M.elems fSliceMap                 
    fSliceMap = M.filterWithKey (\k _ -> HS.member k vars) sliTbl 

getSrcLnSliCoh :: IntMap Value -> SliceTable -> HashSet String -> Function -> SliceMetrics
getSrcLnSliCoh valMap sliTbl vars f = computeSliMetrics fSlices fSize  
  where  
    fSrcLns = IS.fromList . concatMap valueLine $ functionInstructions f
    fSize = IS.size fSrcLns
    fSlices = map (IS.intersection fSrcLns) $ M.elems fSliceMap                 
    fSliceMap = M.map toSrcLns $ M.filterWithKey (\k _ -> HS.member k vars) sliTbl 
    toSrcLns = IS.fromList . concatMap valueLine . findVals valMap 

getInstCohension :: IntMap IntSet -> IntSet -> Function -> SliceMetrics
getInstCohension instDepMap varIDs f = computeSliMetrics fInstDeps fSize  
  where 
    fInsts = functionInstructions f
    fInstIDs = IS.fromList $ map valueUniqueId fInsts
    fSize = length fInsts
    fInstDeps = map (IS.intersection fInstIDs) $ IM.elems fInstDepMap                 
    fInstDepMap = IM.filterWithKey (\k _ -> IS.member k varIDs) instDepMap    
    
genFunOutVars :: IntMap (IntMap IntSet) -> M.Map String (HashSet Value)
                  -> [Value] -> Function -> HashSet Value     
genFunOutVars procValDepMap outputMap glbVals f = HS.union modifiedVals fOutputs 
  where 
    fID = functionUniqueId f
    fFmls = map toValue (functionParameters f)
    fValDep = IM.findWithDefault IM.empty fID procValDepMap
    isArgOut v = lkpValDep (memAccessBase v) fValDep /= IS.singleton (- valueUniqueId v)
    lkpValDep v vdM = IM.findWithDefault IS.empty (valueUniqueId v) vdM
    modifiedVals = HS.fromList $ filter isArgOut (fFmls ++ glbVals)
    fOutputs = M.findWithDefault (funcOutputVars f) (show $ functionName f) outputMap
--    fRetVal = HS.fromList. filter isValidVar . maybeToList $ do
--          rv <- funcExitValue f 
--          if isPhiNode rv then phi2Var rv
--          else return rv 
  
genFunOutVars2 :: IntMap (HashSet Value) -> Function -> HashSet Value     
genFunOutVars2 funModSetMap f = HS.unions [fRetVal,modifiedVals,funcOutputVars f]
  where 
    fID = functionUniqueId f
    modifiedVals = IM.findWithDefault HS.empty fID funModSetMap
    fRetVal = HS.fromList. filter isValidVar $ maybeToList (funcExitValue f)   
       
-- | Print some debug info.
printDbgInfo :: FilePath -> IO ()
printDbgInfo fn = printInfo m >> putStrLn trInfo  
  where 
    m = fn2mod fn
    trInfo = concat ["Its analysis trace Info.:",
                     sdgSize,varSliced,bwdSymTr,bwdSDGTr,bwdIFDSTr,weiserTr,"\n"]
    sdg = IFDS.genSDG False m
    sdgSize = printf "\n\tSDG(#Nodes,#Edges) = (%s,%s)" 
             (show $ G.noNodes sdg)(show . length $ G.labEdges sdg)
    varSliced = "\n\t#Vars_sliced = " ++ show (length allVals')
    bwdSymTr = "\n\t#Insts_bwdSym = " ++ show (BwdSym._traceSize $ BwdSym.computeSlice False m)
    bwdSDGTr = "\n\t#Insts_bwdSDG = " ++ show (SDG._traceSize $ SDG.computeSlice False m)
    bwdIFDSTr = "\n\t#Insts_bwdIFDS = " ++ show (IFDS._traceSize $ IFDS.computeSlice False m)
--    fwdSymTr = "\n\t#Insts_fwdSym = " ++ show (_traceSize $ Sym.computeSlice False m)
    weiserTr = "\n\t#Insts_Weiser = " ++ show (sum $ map 
                 (MW._traceSize. MW.computeSlice m. addAlias aliasMap) allVals' )
    fs = moduleDefinedFunctions m     
    allVals =  map toValue (concatMap functionParameters fs)
            ++ map toValue (moduleGlobalVariables m) 
            ++ map toValue (concatMap funcAllocInsts fs)
    allVals' = filter ((\(k:_)->(isLetter k || k == '_')). drop 1. toVarName') allVals
    aliasMap = genAliasMap' m
    addAlias am v = v : (maybeToList $ HM.lookup v am)

-- | Generate a SDG graph
genSDGgraph :: Module -> String
genSDGgraph m = toDotString (G.grev sdg)
  where
    valMap = genValueMap m    
    sdg = G.nmap (convertNode2 (valMap ^!)) sdg' 
    sdg' = SDG.genSDG False m      -- IFDS.genSDG False m   
    
-- | Generate a ICFG graph
genICFGgraph :: Module -> String
genICFGgraph m = toDotString icfg 
  where
    icfg = mkICFG m pta initps
    pta = PT.runPointsToAnalysis m
    initps = case findMain m of 
       Just mf -> [mf]
       Nothing -> take 1 $ moduleDefinedFunctions m

-- | Generate a call graph
genCGgraph :: Module -> String
genCGgraph m = toDotString cg  
  where
    pta = PT.runPointsToAnalysis m
    cg = callGraph m pta []       
       
toDotString :: ToGraphviz a => a -> String    
toDotString = L.unpack . G.printDotGraph . toGraphviz 



----- 
genGraphs :: FilePath ->IO ()
genGraphs fn = do
  m <- getModuleFromFile fn
  genGraphs' fn m 

genGraphs' :: FilePath ->  Module -> IO ()
genGraphs' fn m = do
  vizGraphPngHtml' fn m "SDG" mkSDG
  vizGraphPngHtml' fn m "ICFG" mkICfg
  vizGraphPngHtml' fn m "CG" mkCG       
  vizGraphPngHtml' fn m "CFG" mkCFGs
  vizGraphPngHtml' fn m "CDG" mkCDGs 
  vizGraphPngHtml' fn m "PDT" mkPDTs
  vizGraphPngHtml' fn m "DT" mkDTs 


-----
vizGraphPng, vizGraphDot, vizGraphHtml,vizGraphPngHtml' :: 
        ToGraphviz a => FilePath -> Module -> String -> (Module -> [(String, a)]) -> IO ()
vizGraphPng = vizGraphModuleWith (FileOutput G.Png)
vizGraphDot = vizGraphModuleWith (FileOutput G.DotOutput)    -- (G.XDot Nothing)
vizGraphHtml = vizGraphModuleWith HtmlOutput

vizGraphPngHtml fn s mk = do 
   m <- getModuleFromFile fn
   vizGraphPngHtml' fn m s mk
   
vizGraphPngHtml' fn m s mk = do
   vizGraphDot fn m s mk
   vizGraphPng fn m s mk
   vizGraphHtml fn m s mk


vizGraphModuleWith :: ToGraphviz a => 
        OutputType -> FilePath -> Module -> String -> (Module -> [(String, a)]) -> IO ()
vizGraphModuleWith fmt ofn m s  = visualizeGraphFromModule m (Just outFile) fmt 
  where
    outFile = case ofn of 
       ""  -> getModuleName m ++ "_" ++ s 
       _   -> ofn ++ "_" ++ s  
--    optOptions = [ "-mem2reg", "-basicaa" ]




----
mkPDTs :: Module -> [(String, PostdominatorTree)]
mkPDTs m = map (getFuncName &&& toTree) fs
  where
    fs = moduleDefinedFunctions m
    toTree = postdominatorTree . controlFlowGraph

mkDTs :: Module -> [(String, DominatorTree)]
mkDTs m = map (getFuncName &&& toTree) fs
  where
    fs = moduleDefinedFunctions m
    toTree = dominatorTree . controlFlowGraph

mkCG :: Module -> [(String, CallGraph)]
mkCG m = [("Call Graph", callGraph m aa [])]
  where
    aa = PT.runPointsToAnalysis m
    str = getModuleName m ++ "\' Call Graph"

mkCFGs :: Module -> [(String, CFG)]
mkCFGs m = map (getFuncName &&& controlFlowGraph) fs
  where
    fs = moduleDefinedFunctions m

mkCDGs :: Module -> [(String, CDG)]
mkCDGs m = map (getFuncName &&& toCDG) fs
  where
    fs = moduleDefinedFunctions m
    toCDG = controlDependenceGraph . controlFlowGraph

--mkPTG :: Module -> [(String, Andersen)]
--mkPTG m = [("Module", runPointsToAnalysis m)]

mkICfg :: Module -> [(String,ICFG)]
mkICfg m = [("ICFG Graph", mkICFG m pta initps)]
  where
    str = getModuleName m ++ "\' ICFG Graph"
    pta = PT.runPointsToAnalysis m
    initps = case findMain m of 
       Just mf -> [mf]
       Nothing -> take 1 $ moduleDefinedFunctions m 

mkSDG :: Module -> [(String,SDGGraphType2)]
mkSDG m = [("SDG Graph", G.grev sdg)]
  where
    str = getModuleName m ++ "\' SDG Graph"
    valMap = genValueMap m    
    sdg = G.nmap (convertNode2 (valMap ^!)) sdg' 
    sdg' = SDG.genSDG False m      -- IFDS.genSDG False m  


    
getFuncName :: Function -> String
getFuncName = identifierAsString . functionName     -- show

getModuleName :: Module -> String
getModuleName = takeBaseName . T.unpack . moduleIdentifier