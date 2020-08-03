{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module LLVM.Slicing.Static.SDG.IFDS.ICFG (
  -- * Types
  ICFG(..),
  ICFGEdge(..),
  ICFGNode(..),
  CFGEdge(..),
  ICFGGraphType,
  -- * Constructor
  mkICFG,
  -- * Visualization
  icfgGraphvizRepr
  ) where

import Data.Graph.Inductive hiding ( Gr, UGr )
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz
import qualified Data.Set as S
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.Monoid ( mempty )
import Control.Arrow

import Debug.Trace.LocationTH

import Text.Printf
import Data.Maybe

import LLVM.Analysis
import LLVM.Analysis.CFG  hiding (CFGEdge(..))
import LLVM.Analysis.PointsTo
--import LLVM.Analysis.PointsTo.TrivialFunction
--import LLVM.Analysis.PointsTo.Andersen

data CFGEdge = TrueEdge
             | FalseEdge
             | EqualityEdge Value
             | IndirectEdge
             | UnwindEdge
             | OtherEdge
             deriving (Eq)

instance Show CFGEdge where 
  show TrueEdge = "True"
  show FalseEdge = "False"
  show (EqualityEdge v) = "== " ++ show v
  show IndirectEdge = "Indirect"
  show UnwindEdge = "Unwind"
  show OtherEdge = ""

instance Labellable CFGEdge where
  toLabelValue = toLabelValue . show

data ICFGNode = InstNode Instruction
              | ReturnNode Instruction
              | ExternalNode (Maybe ExternalFunction)
              deriving (Show)

data ICFGEdge = CallToEntry Instruction
              | ReturnToCall Instruction
              | CallToReturn
              | IntraEdge CFGEdge

instance Show ICFGEdge where
  show (CallToEntry v) = printf "(_[%s]" (show (toValue v))
  show (ReturnToCall v) = printf ")_[%s]" (show (toValue v))
  show CallToReturn = "<call-to-return>"
  show (IntraEdge ce) = show ce

instance Labellable ICFGEdge where
  toLabelValue = toLabelValue . show

type ICFGGraphType = Gr ICFGNode ICFGEdge

data ICFG = ICFG { icfgGraph :: ICFGGraphType  -- Gr ICFGNode ICFGEdge
                 , icfgEntryPoints :: [Function]
                 , icfgModule :: Module
--                 , icfgInstMap :: IntMap Instruction
                 , icfgUnknownNode :: Maybe Node
                 }

-- | Build the interprocedural CFG for the given 'Module'.  The ICFG
-- has all of the instructions in the module as nodes, augmented by a
-- special "return" node for each call node.  
mkICFG :: (PointsToAnalysis a) => Module
          -> a          -- ^ A points-to analysis
          -> [Function] -- ^ Entry points.  This could be just main or a larger list for a library
          -> ICFG
mkICFG m pta entryPoints =
  ICFG { icfgGraph = mkGraph allNodes allEdges
       , icfgEntryPoints = entryPoints
       , icfgModule = m
--       , icfgInstMap = instMap
       , icfgUnknownNode = unknownCallNodeId
       }
  where
    initialData = (externNodes, [])
    (allNodes, allEdges) =
      foldr localBuilder initialData (moduleDefinedFunctions m)
    localBuilder :: Function -> ([LNode ICFGNode], [LEdge ICFGEdge]) -> ([LNode ICFGNode], [LEdge ICFGEdge])
    localBuilder = buildLocalGraph convertEdge convertCallEdge
                      transformCallToReturnNode convertNode
                      (buildCallEdges pta unknownCallNodeId)

    unknownCallNodeId = case (entryPoints, usesDlopen m) of
      ([_], False) -> Nothing
      _ -> Just $ moduleNextId m
    -- ^ With a single entry point we have a closed system and
    -- "unknown" functions can only be introduced through calls like
    -- dlopen and its Windows equivalents.  Otherwise we need to
    -- represent calls to unknown functions explicitly.
    externNodes =
      let ns = map mkExternNode (moduleExternalFunctions m)
      in case unknownCallNodeId of
        Nothing -> ns
        Just uid -> (uid, ExternalNode Nothing) : ns

mkExternNode :: ExternalFunction -> LNode ICFGNode
mkExternNode ef = (externalFunctionUniqueId ef, ExternalNode (Just ef))

usesDlopen :: Module -> Bool
usesDlopen m = foldr dlfold False (moduleExternalFunctions m)
  where
    dlfold ef acc = acc || show (externalFunctionName ef) == "@dlopen"


-- | The major workhorse that constructs interprocedural edges.  For
-- the given call/invoke node, create an edge from the call to the
-- entry of the callee AND an edge from the return node of the callee
-- to the "return" pseudo-node for the call instruction.
--
-- This function will add edges to the special "unknown function" for
-- calls through function pointers in 'Module's that do not have a
-- single entry point.  Single-entry point modules (without calls to
-- dlopen) are closed systems where there are no unknown functions.
buildCallEdges :: (PointsToAnalysis a)
                  => a
                  -> Maybe Node
                  -> Instruction
                  -> [LEdge ICFGEdge]
buildCallEdges pta unknownCallNode inst =
  case (isDirectCall inst, unknownCallNode) of
    (_, Nothing) -> callEdges'
    (True, _) -> callEdges'
    (False, Just uid) -> unknownEdges uid ++ callEdges'
  where
    instid = instructionUniqueId inst
    unknownEdges uid = [ (instid, uid, CallToEntry inst)
                       , (uid, -instid, ReturnToCall inst)
                       ]
    calledFuncs = pointsTo pta (calledValue inst)
    callEdges = foldr mkCallEdge [] calledFuncs
    callEdges' = (instid, -instid, CallToReturn) : callEdges
    mkCallEdge :: Value -> [LEdge ICFGEdge] -> [LEdge ICFGEdge]
    mkCallEdge cf acc =
      case valueContent cf of
        FunctionC f ->
          let calleeEntryId = instructionUniqueId (functionEntryInstruction f)
              calleeExitId = -- instructionUniqueId $ fromJust (functionExitInstruction f)
                   case functionExitInstruction f of
                      Just ei -> instructionUniqueId ei
                      Nothing -> -(functionUniqueId f)
          in (instid, calleeEntryId, CallToEntry inst) :
             (calleeExitId, -instid, ReturnToCall inst) : acc
        ExternalFunctionC ef ->
          let calleeEntryId = externalFunctionUniqueId ef
          in (instid, calleeEntryId, CallToEntry inst) :
             (calleeEntryId, -instid, ReturnToCall inst) : acc
        GlobalAliasC GlobalAlias { globalAliasTarget = t } -> mkCallEdge t acc
        _ -> $failure ("Expected a function value: " ++ show cf)

-- | Get the value called by a Call or Invoke instruction
calledValue :: Instruction -> Value
calledValue CallInst { callFunction = v } = v
calledValue InvokeInst { invokeFunction = v } = v
calledValue i = $failure ("Expected Call or Invoke instruction: " ++ show i)

-- | Return True if the given call (or invoke) instruction is a call
-- to a statically known function (rather than a function pointer).
isDirectCall :: Instruction -> Bool
isDirectCall ci = isDirectCall' cv
  where
    cv = calledValue ci
    isDirectCall' :: Value -> Bool
    isDirectCall' v = case valueContent v of
      FunctionC _ -> True
      ExternalFunctionC _ -> True
      GlobalAliasC GlobalAlias { globalAliasTarget = t } -> isDirectCall' t
      InstructionC BitcastInst { castedValue = c } -> isDirectCall' c
      _ -> False

-- | Given a call node, create the corresponding return-site node.
-- This is implemented by simply negating the node ID (since all
-- normal node IDs are positive ints, this is fine.)
transformCallToReturnNode :: Instruction -> Maybe (LNode ICFGNode)
transformCallToReturnNode i = Just (-(instructionUniqueId i), ReturnNode i)

convertEdge :: LEdge CFGEdge -> LEdge ICFGEdge
convertEdge (src, dst, lbl) = (src, dst, IntraEdge lbl)

convertCallEdge :: LEdge CFGEdge -> LEdge ICFGEdge
convertCallEdge (src, dst, lbl) = (-src, dst, IntraEdge lbl)

convertNode :: LNode Instruction -> LNode ICFGNode
convertNode (nid, nv) = (nid, InstNode nv)


-- Visualization

data ICFGCluster = CUnknown
                 | CExternalFunction !ExternalFunction
                 | CFunction !Function
                 | CBlock !BasicBlock
                 deriving (Eq, Ord)

-- | Generate a Graph Identifier for each cluster node for the ICFG
clusterIdent :: ICFGCluster -> GraphID
clusterIdent CUnknown = Str "unknown"
clusterIdent (CExternalFunction ef) = Num . Int $ externalFunctionUniqueId ef     -- 
clusterIdent (CFunction f) = Num . Int $ functionUniqueId f       --  
clusterIdent (CBlock b) = Num . Int $ basicBlockUniqueId b        --  

instance Show ICFGCluster where
  show CUnknown = "Unknown"
  show (CExternalFunction ef) = show (externalFunctionName ef)
  show (CFunction f) = show (functionName f)
  show (CBlock b) = show (basicBlockName b)

-- | A set of graphviz parameters suitable for visualizing an 'ICFG'.
-- These graph settings group instructions into basic blocks and basic
-- blocks into functions using GraphViz clusters.
icfgParams :: GraphvizParams Node ICFGNode ICFGEdge ICFGCluster ICFGNode -- ICFGEdge ICFGCluster ICFGNode
icfgParams =
  defaultParams { fmtNode = formatNode
                , fmtEdge = formatEdge
                , clusterBy = nodeCluster
                , clusterID = clusterIdent
                , fmtCluster = formatCluster
                }
  where
    formatCluster CUnknown = [GraphAttrs [textLabel "UnknownFunction"]]
    formatCluster (CExternalFunction ef) = [GraphAttrs { attrs = [toLabel (show (externalFunctionName ef))] } ]
    formatCluster (CFunction f) = [GraphAttrs { attrs = [toLabel (show (functionName f))] } ]
    formatCluster (CBlock b) = [GraphAttrs { attrs = [toLabel (show (basicBlockName b))] } ]
    nodeCluster l@(_, ExternalNode Nothing) = C CUnknown (N l)
    nodeCluster l@(_, ExternalNode (Just ef)) = C (CExternalFunction ef) (N l)
    nodeCluster l@(_, ReturnNode i) = C (CFunction f) (C (CBlock bb) (N l))
      where  -- f = instructionFunction2 i
        bb = instructionBasicBlock2 i
        f = basicBlockFunction bb
    nodeCluster l@(_, InstNode i) = C (CFunction f) (C (CBlock bb) (N l))
      where -- f = instructionFunction2 i
        bb = instructionBasicBlock2 i
        f = basicBlockFunction bb
    formatNode (_,l) = case l of
      ExternalNode Nothing -> [textLabel "UnknownFunction"]
      ExternalNode (Just ef) -> [toLabel ("ExternalFunction: " ++ show (externalFunctionName ef))]
      InstNode i -> [toLabel (toValue i), shape BoxShape]
      ReturnNode _ -> [textLabel "ReturnNode", shape BoxShape]
    formatEdge (_,_,l) =
      let lbl = toLabel l
      in case l of
        CallToEntry _ -> [lbl, style dashed, color DeepSkyBlue]
        ReturnToCall _ -> [lbl, style dashed, color Crimson]
        CallToReturn -> [lbl, style dotted, color ForestGreen]
        IntraEdge _ -> [lbl]

-- | Suitable parameters for visualizing an ICFG with graphviz.
icfgGraphvizRepr :: ICFG -> DotGraph Node
icfgGraphvizRepr = graphToDot icfgParams . icfgGraph

instance ToGraphviz ICFG where
  toGraphviz = icfgGraphvizRepr
  

instructionBasicBlock2 :: Instruction -> BasicBlock
instructionBasicBlock2 i = case instructionBasicBlock i of
     Just bb -> bb
     Nothing -> $failure "IFDS.ICFG.instructionBasicBlock2: NoInstBB"  
  

------------------------
-- | Build the local control flow nodes and edges for the given
-- 'Function'.  The nodes and edges are placed on the front of the
-- input lists.
--
-- Note: This function is only exposed to make the ICFG construction
-- more efficient without copying code.
buildLocalGraph :: (LEdge CFGEdge -> LEdge a)        -- ^ A function to convert CFG edges to another type of edge
                   -> (LEdge CFGEdge -> LEdge a)     -- ^ A function to convert a CFG edge from a Call or Invoke to another type of edge
                   -> (Instruction -> Maybe (LNode b)) -- ^ A function to generate an extra node from a Call or Invoke edge
                   -> (LNode Instruction -> LNode b) -- ^ A function to convert CFG nodes to another type of node
                   -> (Instruction -> [LEdge a])     -- ^ A function to apply to Call and Invoke instructions to generate extra edges
                   -> Function                      -- ^ The current function
                   -> ([LNode b], [LEdge a])        -- ^ Accumulator
                   -> ([LNode b], [LEdge a])
buildLocalGraph edgeF callEdgeF callEdgeN nodeF callF f acc =
  foldr (buildBlockGraph edgeF callEdgeF callEdgeN nodeF callF) acc (functionBody f)

buildBlockGraph :: (LEdge CFGEdge -> LEdge a)        -- ^ A function to convert CFG edges to another type of edge
                   -> (LEdge CFGEdge -> LEdge a)     -- ^ A function to convert a CFG edge from a Call or Invoke to another type of edge
                   -> (Instruction -> Maybe (LNode b)) -- ^ A function to generate an extra node from a Call or Invoke edge
                   -> (LNode Instruction -> LNode b) -- ^ A function to convert CFG nodes to another type of node
                   -> (Instruction -> [LEdge a])     -- ^ A function to apply to Call and Invoke instructions to generate extra edges
                   -> BasicBlock
                   -> ([LNode b], [LEdge a])
                   -> ([LNode b], [LEdge a])
buildBlockGraph edgeF callEdgeF callEdgeN nodeF callF bb acc =
  foldr (buildGraphInst edgeF callEdgeF callEdgeN nodeF callF) acc instsAndSuccessors
  where
    blockInsts = basicBlockInstructions bb
    (_:successors) = blockInsts
    instsAndSuccessors = case null successors of
      True -> terminator
      False -> offsetPairs ++ terminator
    offsetPairs = zip blockInsts $ map Just successors
    terminator = [(last blockInsts, Nothing)]

buildGraphInst :: (LEdge CFGEdge -> LEdge a)        -- ^ A function to convert CFG edges to another type of edge
                  -> (LEdge CFGEdge -> LEdge a)     -- ^ A function to convert a CFG edge from a Call or Invoke to another type of edge
                  -> (Instruction -> Maybe (LNode b)) -- ^ A function to generate an extra node from a Call or Invoke edge
                  -> (LNode Instruction -> LNode b) -- ^ A function to convert CFG nodes to another type of node
                  -> (Instruction -> [LEdge a])     -- ^ A function to apply to Call and Invoke instructions to generate extra edges
                  -> (Instruction, Maybe Instruction)    -- ^ Current instruction and successor (if any)
                  -> ([LNode b], [LEdge a])        -- ^ Accumulator
                  -> ([LNode b], [LEdge a])
buildGraphInst edgeF callEdgeF callEdgeN nodeF callF (inst, Nothing) (nodeAcc, edgeAcc) =
   -- Note, when adding the edges, put the accumulator second in the
   -- list append so that only the short list (new edges) needs to be
   -- reallocated
  case (callEdgeN inst, inst) of
    (Just en, InvokeInst { }) -> (nodeF thisNode : en : nodeAcc, allEdges ++ edgeAcc)
    _ -> (nodeF thisNode : nodeAcc, allEdges ++ edgeAcc)
  where
    thisNodeId = instructionUniqueId inst
    thisNode = (thisNodeId, inst)
    allEdges = case inst of
      InvokeInst { } -> callF inst ++ map callEdgeF theseEdges
      _ -> map edgeF theseEdges

    -- Note: branch targets are all basic blocks.  The
    -- lookup function handles grabbing the first real
    -- instruction from the basic block.
    theseEdges = case inst of
      -- Returns have no intraprocedural edges
      RetInst {} -> []
      -- Unwinds also have no intraprocedural edges
--      UnwindInst {} -> []
      -- A single target (no label needed)
      UnconditionalBranchInst { unconditionalBranchTarget = tgt } ->
        [ (thisNodeId, jumpTargetId tgt, OtherEdge) ]
      -- Two edges (cond is true, cond is false)
      BranchInst { branchCondition = cond
                 , branchTrueTarget = tTarget
                 , branchFalseTarget = fTarget
                 } ->
        [ (thisNodeId, jumpTargetId tTarget, TrueEdge)
        , (thisNodeId, jumpTargetId fTarget, FalseEdge)
        ]
      SwitchInst { switchValue = cond
                 , switchDefaultTarget = defTarget
                 , switchCases = cases
                 } ->
        ( thisNodeId, jumpTargetId defTarget, OtherEdge) :
        map (caseEdge thisNodeId cond) cases
      IndirectBranchInst { indirectBranchAddress = addr
                         , indirectBranchTargets = targets
                         } ->
        map (indirectEdge thisNodeId addr) targets
      InvokeInst { invokeNormalLabel = n
                 , invokeUnwindLabel = u
                 } ->
        [ (thisNodeId, jumpTargetId n, OtherEdge)
        , (thisNodeId, jumpTargetId u, UnwindEdge)
        ]
      -- No code after unreachable instructions is executed
      UnreachableInst {} -> []
      -- The resume instruction resumes propagating exceptions, so
      -- control will transfer to the caller.  In theory, another
      -- handler in the same function could pick it up...  Resolving
      -- that might require some more sophisticated analysis.
      ResumeInst {} -> []
      _ -> $failure ("Last instruction in a block should be a terminator: " ++ show (toValue inst))
buildGraphInst edgeF callEdgeF callEdgeN nodeF callF (inst, Just successor) (nodeAcc, edgeAcc) =
  case (callEdgeN inst, inst) of
    (Just en, CallInst { }) -> (nodeF thisNode : en : nodeAcc, theseEdges ++ edgeAcc)
    _ -> (nodeF thisNode : nodeAcc, theseEdges ++ edgeAcc)
  where
    thisNodeId = instructionUniqueId inst
    thisNode = (thisNodeId, inst)
    thisEdge = (thisNodeId, instructionUniqueId successor, OtherEdge)
    theseEdges = case inst of
      CallInst { } -> callEdgeF thisEdge : callF inst
      _ -> [edgeF thisEdge]

-- | Only BasicBlocks are targets of jumps.  This function finds the
-- identifier for the given block.
jumpTargetId :: BasicBlock -> Int
jumpTargetId bb = instructionUniqueId t
  where
    (t:_) = basicBlockInstructions bb

caseEdge :: Node -> Value -> (Value, BasicBlock) -> LEdge CFGEdge
caseEdge thisNodeId cond (val, dest) =
  (thisNodeId, jumpTargetId dest, EqualityEdge val)

indirectEdge :: Node -> Value -> BasicBlock -> LEdge CFGEdge
indirectEdge thisNodeId addr target =
  (thisNodeId, jumpTargetId target, IndirectEdge)
