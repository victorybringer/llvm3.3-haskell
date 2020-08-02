{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module LLVM.Analysis.ICFG (
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
import Data.GraphViz
import qualified Data.Set as S
import Debug.Trace.LocationTH

import Text.Printf

import LLVM.Analysis
import LLVM.Analysis.CFG
import LLVM.Analysis.PointsTo
import LLVM.Analysis.Internal.PatriciaTree

data ICFGNode = InstNode Instruction
              | ReturnNode Instruction
              | ExternalNode (Maybe ExternalFunction)
              deriving (Show)

data ICFGEdge = CallToEntry Instruction
              | ReturnToCall Instruction
              | CallToReturn
              | IntraEdge CFGEdge

instance Show ICFGEdge where
  show (CallToEntry v) = printf "(_[%s]" (show (Value v))
  show (ReturnToCall v) = printf ")_[%s]" (show (Value v))
  show CallToReturn = "<call-to-return>"
  show (IntraEdge ce) = show ce

instance Labellable ICFGEdge where
  toLabelValue = toLabelValue . show

type ICFGGraphType = Gr ICFGNode ICFGEdge

data ICFG = ICFG { icfgGraph :: ICFGGraphType
                 , icfgEntryPoints :: [Function]
                 , icfgModule :: Module
                 , icfgUnknownNode :: Maybe Node
                 }

-- | Build the interprocedural CFG for the given 'Module'.  The ICFG
-- has all of the instructions in the module as nodes, augmented by a
-- special "return" node for each call node.  There are several types
-- of edges:
--
-- * Standard intraprocedural edges as in a CFG
--
-- * Edges from calls to the entry node of the target function(s)
--
-- * Edges from ret instructions to the special "return" node of the
--   corresponding call node that led to the function
--
-- * Edges from call nodes to their corresponding special "return"
--   node.  These are used to propagate information about local
--   variables
--
-- * Edges to and from called external functions.  The external
--   function is represented by two nodes: a fake entry and fake exit.
--
-- This graph is meant for use in interprocedural analysis.  The
-- @entryPoints@ parameter directs interprocedural analyses where they
-- should start.  If no entry points are specified, the 'Module' will
-- be considered to be a library and all functions will be considered
-- as entry points (with multiple entries permitted for each).
--
-- Additionally, if no entry points are specified (or there are calls
-- to dlopen), there will be edges to Unknown functions that are called
-- through function pointers.
mkICFG :: (PointsToAnalysis a) => Module
          -> a          -- ^ A points-to analysis
          -> [Function] -- ^ Entry points.  This could be just main or a larger list for a library
          -> ICFG
mkICFG m pta entryPoints =
  ICFG { icfgGraph = mkGraph allNodes allEdges
       , icfgEntryPoints = entryPoints
       , icfgModule = m
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
    calledFuncs = S.elems $ pointsToValues pta (calledValue inst)
    callEdges = foldr mkCallEdge [] calledFuncs
    callEdges' = (instid, -instid, CallToReturn) : callEdges
    mkCallEdge :: Value -> [LEdge ICFGEdge] -> [LEdge ICFGEdge]
    mkCallEdge cf acc =
      case valueContent cf of
        FunctionC f ->
          let calleeEntryId = instructionUniqueId (functionEntryInstruction f)
              calleeExitId = instructionUniqueId (functionExitInstruction f)
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
clusterIdent (CExternalFunction ef) = Int $ externalFunctionUniqueId ef
clusterIdent (CFunction f) = Int $ functionUniqueId f
clusterIdent (CBlock b) = Int $ basicBlockUniqueId b

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
      where
        Just bb = instructionBasicBlock i
        f = basicBlockFunction bb
    nodeCluster l@(_, InstNode i) = C (CFunction f) (C (CBlock bb) (N l))
      where
        Just bb = instructionBasicBlock i
        f = basicBlockFunction bb
    formatNode (_,l) = case l of
      ExternalNode Nothing -> [textLabel "UnknownFunction"]
      ExternalNode (Just ef) -> [toLabel ("ExternalFunction: " ++ show (externalFunctionName ef))]
      InstNode i -> [toLabel (Value i), shape BoxShape]
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