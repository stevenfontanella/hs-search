import Debug.Trace
import Data.String

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc

-- runParser :: DynFlags -> String -> P a -> ParseResult a
-- runParser flags str parser = unP parser parseState
--   where
--     filename = "<interactive>"
--     location = mkRealSrcLoc (mkFastString filename) 1 1
--     buffer = stringToStringBuffer str
--     parseState = mkPState flags buffer location

data Def info = Def
  deriving (Show)
data Parsed info = Program [Def info]
  deriving (Show)

type Name = String
data Table = Table Main.Name [SrcSpan]

-- data Loc = Loc
--   { locLine :: !Int
--   , locCol  :: !Int
--   } deriving (Show, Eq)

-- data Range = Range
--   { rngFile  :: !String
--   , rngStart :: !Loc
--   , rngEnd   :: !Loc
--   } deriving (Show, Eq)

type Failure = String

declToDef :: Decl SrcSpanInfo -> Def info
declToDef decl = trace "\n" $ traceShow decl $ case decl of
  PatBind{}  -> Def
  TypeSig{}  -> Def
  FunBind{}  -> Def
  TypeDecl{} -> Def
  DataDecl{} -> Def

parseFromString :: String -> Either Failure (Parsed info)
parseFromString str = case parseModule str of
  ParseOk (Module _ mbHead pragma imports decls) -> Right $ Program $ fmap declToDef decls
  ParseOk _         -> Left "parseFromString: Unexpected parse result"
  ParseFailed loc s -> Left $ "parseFromString: " <> s <> " at " <> show loc

parseFromFile :: String -> IO (Parsed info)
parseFromFile = fmap (either error id) . fmap parseFromString . readFile

main = do
  fname <- getLine
  -- putStrLn contents
  print =<< parseFromFile fname
  -- putStrLn contents