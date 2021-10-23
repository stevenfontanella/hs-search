import Debug.Trace
import Data.Map

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
newtype Table = Table
 { toMap :: Map Main.Name [SrcSpan]
 } deriving (Show)

-- data Loc = Loc
--   { locLine :: !Int
--   , locCol  :: !Int
--   } deriving (Show, Eq)

-- data Range = Range
--   { rngFile  :: !String
--   , rngStart :: !Loc
--   , rngEnd   :: !Loc
--   } deriving (Show, Eq)

type HName = Language.Haskell.Exts.Syntax.Name

type Failure = String

n :: HName a -> (a, Main.Name)
n (Ident a s) = (a, s)
n (Symbol a s) = (a, s)

foldMapDecl :: Monoid m => (a -> Main.Name -> m) -> Decl a -> m
foldMapDecl acc decl = case decl of
  FunBind _ matches -> mconcat [uncurry acc (n fnName) | (Match a fnName pats rhs mbBinds) <- matches]
  -- big TODO
  -- TypeDecl a dh ty -> _
  -- TypeFamDecl a kdh m_rs m_ii -> _
  -- ClosedTypeFamDecl a dh m_rs m_ii tes -> _
  -- DataDecl a don m_con dh qcds des -> _
  -- GDataDecl a don m_con dh m_ty gds des -> _
  -- DataFamDecl a m_con dh m_rs -> _
  -- TypeInsDecl a ty ty' -> _
  -- DataInsDecl a don ty qcds des -> _
  -- GDataInsDecl a don ty m_ty gds des -> _
  -- ClassDecl a m_con dh fds m_cds -> _
  -- InstDecl a m_over ir m_ids -> _
  -- DerivDecl a m_ds m_over ir -> _
  -- InfixDecl a as m_n ops -> _
  -- DefaultDecl a tys -> _
  -- SpliceDecl a exp -> _
  -- TSpliceDecl a exp -> _
  -- TypeSig a nas ty -> _
  -- PatSynSig a nas m_tvbs m_con m_tvb's m_con' ty -> _
  -- FunBind a mas -> _
  -- PatBind a pat rhs m_bi -> _
  -- PatSyn a pat pat' psd -> _
  -- ForImp a cc m_sa m_s na ty -> _
  -- ForExp a cc m_s na ty -> _
  -- RulePragmaDecl a rus -> _
  -- DeprPragmaDecl a x0 -> _
  -- WarnPragmaDecl a x0 -> _
  -- InlineSig a b m_ac qn -> _
  -- InlineConlikeSig a m_ac qn -> _
  -- SpecSig a m_ac qn tys -> _
  -- SpecInlineSig a b m_ac qn tys -> _
  -- InstSig a ir -> _
  -- AnnPragma a an -> _
  -- MinimalPragma a m_bf -> _
  -- RoleAnnotDecl a qn ros -> _
  -- CompletePragma a nas m_qn -> _

foldMapMod :: Monoid m => (a -> Main.Name -> m) -> Module a -> m
foldMapMod acc (Module _ _ _ _ decls) = mconcat $ foldMapDecl acc <$> decls
foldMapMod _ _ = error "foldMapMod: Not a module"

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