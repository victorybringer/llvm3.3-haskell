module Main where

import qualified Data.Map as M
import qualified Data.IntSet as IS  ( size,toList )
import Options.Applicative
import System.Environment ( getArgs )
import System.Directory 
import System.FilePath
import Data.Monoid ((<>))

import LLVM.Slicing 
--import LLVM.Slicing.Static.Symbolic.SymSlicer     ( getSrcLnDepTable2 )

-----
---
data SliceMethod = Symbolic | SDG | Weiser | IFDS | Sym2 | InfoFlow
                  deriving (Read, Ord, Eq, Show)
data Direction = Fwd | Bwd | Both deriving (Read, Ord, Eq, Show)
data GrType = Sdg | Icfg | Cfg | Cdg | Cg | Pdt | Dt | All  deriving (Read, Ord, Eq, Show)

data Opts = Opts { 
                   criterion :: [String]
                 , direction :: Direction
                 , sliceMethod :: SliceMethod
                 , isParallel :: Bool
                 , timeout :: Int
                 , grType :: Maybe GrType
                 , outputFile :: Maybe FilePath
                 , cmpSlices  :: [String]
                 , isMetrics  :: Bool
                 , inputFile :: FilePath
                 } deriving (Show)

cmdOpts :: Parser Opts
cmdOpts = Opts
 <$> many (strOption
      ( long "criterion"
      <> short 'c'
      <> metavar "VARIABLEs"
      <> help "The criterion variables (with the form of Var@Fun,e.g. x@main) for slicing. If null, just output the slice table for all single variables."))
  <*> option auto
      ( long "direction"
      <> short 'd'
      <> metavar "DIRECTION"
      <> help "The type of output to slice: Fwd, Bwd or Both. Default: Bwd"
      <> value Bwd)   
  <*> option auto
      ( long "method"
      <> short 'm'
      <> metavar "SLICE_METHOD"
      <> help "The slice algorithm: Symbolic,Weiser,InfoFlow,SDG or IFDS. Default: Symbolic"
      <> value Symbolic)    
  <*> option auto
      ( long "isParallel"
      <> short 'p'
      <> metavar "True/False"
      <> help "Whether or not travelling SCC in callgraph in parallel. Default: False"
      <> value False)     
  <*> option auto
      ( long "timeout"
      <> short 't'
      <> metavar "TIMEOUT"
      <> help "The timeout (sec.) for running slicer. Default: 1800" 
      <> value 1800)  
  <*> optional (option auto
      ( long "graph"
      <> short 'g'
      <> metavar "GRAPH_SHOW"
      <> help "Print related graphs: Sdg,Cg,Cdg,Cfg,Icfg,Pdt,Dt, or All." ))
  <*> optional (strOption
     ( long "output"
     <> short 'o'
     <> metavar "FILE/DIR"
     <> help "The destination of a file output"))
  <*> many (strOption
      ( long "compare"
--      <> short 'c'
      <> metavar "SLICE_METHODs"
      <> help "Compare the final results of two slice methods. If null, compare Symbolic and IFDS"))
  <*> option auto
      ( long "isMetrics"
--      <> short 'p'
      <> metavar "True/False"
      <> help "Whether or not printing slice-based Metrics. Default: False"
      <> value False)
--  <*> switch
--     ( long "enable-slice-based-metrics"
--      <> help "Printing slice-based Metrics" )
  <*> argument str 
      ( metavar "FILE"  
      <> help "The input file which can be bitcode,llvm assembly, or C/CPP sourcecode")
 
--main :: IO ()
main = execParser args >>= realMain       -- irSliceMain
  where
    args = info (helper <*> cmdOpts)
      ( fullDesc
      <> progDesc "Generate static slices based on several methods for FILE (which can be bitcode,llvm assembly, or C/CPP sourcecode)"
      <> header "LLVMSlicing - Static program slicers for LLVM IR")

realMain :: Opts -> IO ()
realMain opts = (if timeout opts < 0 then id else timeoutWith' (timeout opts)).
 timeIO $! do 
    let dir = direction opts
        inFile = inputFile opts
        outFile = outputFile opts 
        sliceVars = criterion opts 
        method = sliceMethod opts
        isPar = isParallel opts
    m <- getModuleFromFile inFile
    let valMap = genValueMap m    
        (bwdSlices,fwdSlices) = genSliceTableWith (show method) isPar m                     
        res = if null sliceVars then tblRes  else sliRes 
        tblRes = case dir of 
           Fwd -> fwdTbl
           Bwd -> bwdTbl
           Both -> bwdTbl ++ "\n" ++ fwdTbl                   
          where bwdTbl = "\nBackward Static SliceTable:" ++ showSliSize bwdSlices 
                        ++ showSlices2 (toSrcLns bwdSlices)
                fwdTbl = "\nForward Static SliceTable:" ++ showSliSize fwdSlices 
                        ++ showSlices2 (toSrcLns fwdSlices)
                toSrcLns = M.map (toSrcLnStr valMap) 
                showSliSize s = "\n\t#Insts_sliced = " ++ show (allSlices s) 
                            ++ " (Average: " ++ show (avgSize s) ++ ")\n"    
                avgSize m = round $ (allSlices m)/(fromIntegral $ M.size m)  
                allSlices = fromIntegral . sum . map IS.size . M.elems           
        sliRes = case dir of
           Fwd -> fwdSli
           Bwd -> bwdSli
           Both -> bwdSli ++ "\n" ++ fwdSli                   
          where bwdSli = "Backward Static Slice for " ++ show sliceVars ++ showIRSli bwdSlices
                fwdSli = "Forward Static Slice for " ++ show sliceVars ++ showIRSli fwdSlices  
                              
        showIRSli slices = ":\n" ++ srcLines ++ valStrings  --  show vals
          where vals = unionLkpSli sliceVars  slices
                srcLines = " <SourceLines> " ++ show (toSrcLnStr valMap vals) ++ ": \n"
                valStrings = concatMap showVal (findVals valMap vals)
                showVal v = "  " ++ show v ++ "\n" 
    --
--    let srcLnDepMap = getInstDepTable m 
--        instDepRes = "\nSource-Line Dependence Table:\n" ++ showSlices2 srcLnDepMap
--        res' = res ++ instDepRes
    case outFile of  
      Nothing -> writeFile fn res
               where fn = inFile ++ ".SliceResult_" ++ show dir ++ "-" ++ show method 
      Just ofile -> appendFile ofile res
    putStrLn res   
    printInfo m    
--    printValueMap m 
--    printGr inFile $ R.printRel "LDG" srcInflRel
--    printGr (takeBaseName inFile ++ "-inst") (showInstRel instInflRel) 

    -- printing graphs   
    let prntGr ty mk = vizGraphPngHtml' inFile m ty mk
    case grType opts of 
     Nothing -> return ()
     Just Sdg -> prntGr "SDG" mkSDG
     Just Icfg -> prntGr "ICFG" mkICfg
     Just Cg -> prntGr "CG" mkCG       
     Just Cfg -> prntGr "CFG" mkCFGs
     Just Cdg -> prntGr "CDG" mkCDGs 
     Just Pdt -> prntGr "PDT" mkPDTs
     Just Dt -> prntGr "DT" mkDTs
     Just All -> genGraphs' inFile m
     
    -- compare slice tables 
    let cmps = case cmpSlices opts of 
          [md1]  -> compareSliceMethod m (md1, "Symbolic")
          md1 : md2 : _ -> compareSliceMethod m (md1,md2)
          _  -> compareSliceMethod m ("Symbolic","IFDS")    -- SDG
    writeFile (inFile ++ ".SliceDiffResult") cmps     
    
    -- printing slice-based metrics
    if isMetrics opts then  printMetrics inFile  else return ()
    
    
main1 = do  
  ps <- getArgs  
  btfs <- getFilesFromDirs ps
  sequence_ $ fmap cmd btfs 
 where
   cmd fn = do 
       putStrLn $ "Now generate graphs for " ++ fn ++ "......"
--       m <- getModuleFromFile fn  
       genGraphs fn     
--       printGr' fn "CG" (genCGgraph m)
--       printGr' fn "SDG" (genSDGgraph m)
--       printGr' fn "ICFG" (genICFGgraph m) 

main1' = do
  fn : mds <- getArgs    
  m <- getModuleFromFile fn
  let res = case mds of 
          md1 : md2 : _ -> compareSliceMethod m (md1,md2)
          _  -> compareSliceMethod m ("SDG","IFDS")    -- SDG
  writeFile (fn ++ ".SliceDiffResult") res
--  printGr fn "SDG" (genSDGgraph m)
--  putStrLn res

main2' = do  
  ps <- getArgs  
  btfs <- getFilesFromDirs ps
  sequence_ $ fmap cmd btfs 
 where
   cmd fn = do 
       putStrLn $ "Now SliceDiff_SDG-IFDS for " ++ fn ++ "......"
       m <- getModuleFromFile fn
       let res = compareSliceMethod m ("SDG","IFDS") 
       writeFile (fn ++ ".SliceDiff_SDG-IFDS") res
       printGr fn "SDG" (genSDGgraph m)

main3 = do
  fs <- getArgs  
  mapM_ genGraphs fs

main3' = autoTestWith genGraphs  "Graphs"     -- genGraphs

main4' = autoTestWith printDbgInfo "Basic Info."

main5 = do
  fs <- getArgs  
  sequence_ $ map printMetrics fs

main5' = autoTestWith printMetrics "Slice-based Metrics"

autoTestWith run info = do 
  ps <- getArgs  
  btfs <- getFilesFromDirs ps
  sequence_ $ map cmd btfs 
 where
   cmd fn = timeoutWith' 18000 $! do 
       putStrLn $ "Now print " ++ info ++ " for " ++ fn ++ "......"
       run fn       -- genGraphs


--- final IR Slicer

irSliceMain :: Opts -> IO ()
irSliceMain opts = do
    let dir = direction opts
        inFile = inputFile opts
        outFile = outputFile opts
        sliceVars = criterion opts 
        method = sliceMethod opts
        isPar = isParallel opts
        tm = timeout opts
    let 
      showIRSlices = showMapWith "\n Variable      IR_Unique_Numbers  " IS.toList
      runMain fn = (if tm < 0 then id else timeoutWith' tm) . timeIO $! do  
        m <- getModuleFromFile fn 
        let valMap = genValueMap m    
            (bwdSlices,fwdSlices) = genSliceTableWith (show method) isPar m                     
            res = if null sliceVars then tblRes  else sliRes 
            tblRes = case dir of 
               Fwd -> fwdTbl
               Bwd -> bwdTbl
               Both -> bwdTbl ++ "\n" ++ fwdTbl                   
              where bwdTbl = "\nBackward Static SliceTable:" ++ showSliSize bwdSlices 
                            ++ showIRSlices bwdSlices ++ showSlices2 (toSrcLns bwdSlices)
                    fwdTbl = "\nForward Static SliceTable:" ++ showSliSize fwdSlices 
                            ++ showIRSlices fwdSlices ++ showSlices2 (toSrcLns fwdSlices)
                    toSrcLns = M.map (toSrcLnStr valMap) 
                    showSliSize s = "\n\t#Insts_sliced = " ++ show (allSlices s) 
                                ++ " (Average: " ++ show (avgSize s) ++ ")\n"    
                    avgSize m = round $ (allSlices m)/(fromIntegral $ M.size m)  
                    allSlices = fromIntegral . sum . map IS.size . M.elems           
            sliRes = case dir of
               Fwd -> fwdSli
               Bwd -> bwdSli
               Both -> bwdSli ++ "\n" ++ fwdSli                   
              where bwdSli = "Backward Static Slice for " ++ show sliceVars ++ showIRSli bwdSlices
                    fwdSli = "Forward Static Slice for " ++ show sliceVars ++ showIRSli fwdSlices  
                                  
            showIRSli slices = ":\n" ++ srcLines ++ valStrings  --  show vals
              where vals = unionLkpSli sliceVars  slices
                    srcLines = " <SourceLines> " ++ show (toSrcLnStr valMap vals) ++ ": \n"
                    valStrings = concatMap showVal (findVals valMap vals)
                    showVal v = "  " ++ show v ++ "\n" 
      ---   
--        let srcLnDepMap = getSrcLnDepTable2 m 
--            showLnDeps = showMapWith "\n SourceLine        SrcLineNumbers  " id
--            instDepRes = "\nSource-Line Dependence Table:\n" ++ showLnDeps srcLnDepMap
--            res' = res ++ instDepRes
        case outFile of  
          Nothing -> writeFile fname res 
                   where fname = fn ++ ".SliceResult_" ++ show dir ++ "-" ++ show method 
          Just ofile -> appendFile ofile res
        putStrLn res   
        printInfo m         
--        printValueMap m 
        --    
        let prntGr ty mk = vizGraphDot inFile m ty mk >> vizGraphPng inFile m ty mk
        case grType opts of 
         Nothing -> return ()
         Just Sdg -> printGr inFile "SDG" (genSDGgraph m) 
         Just Icfg -> printGr inFile "ICFG" (genICFGgraph m)  -- prntGr "ICFG" mkICfg 
         Just Cg -> printGr inFile "CG" (genCGgraph m)  --  prntGr "CG" mkCG         
         Just Cfg -> prntGr "CFG" mkCFGs
         Just Cdg -> prntGr "CDG" mkCDGs 
         Just Pdt -> prntGr "PDT" mkPDTs
         Just Dt -> prntGr "DT" mkDTs  
         
    isDirectory <- doesDirectoryExist inFile
    fs <- if isDirectory then getFilesFromDirs [inFile]
          else return [inFile]
    let cmd fn = do 
           putStrLn $ "Now slice testing for " ++ fn ++ "......"
           runMain fn     
--           printMetrics fn  
    sequence_ $ map cmd fs  
    

