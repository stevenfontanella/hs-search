module Control.Parse (
    foldMapMod
  , parseFromString
  , parseFromFile
  , symsFromModule
  , symsFromFile
  , Mod
  , Module
) where

import Util

import Data.SymTable
import qualified Data.SymTable as T

import Control.Exception
import Control.DeepSeq

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts (knownExtensions)

import Debug.Trace

type Failure = String
type PName = String
type HName = Language.Haskell.Exts.Syntax.Name

n :: HName a -> (a, PName)
n (Ident a s) = (a, s)
n (Symbol a s) = (a, s)

mconcatMap :: Monoid b => (a -> b) -> [a] -> b
mconcatMap f = mconcat . map f

-- TODO: classes + instance declarations
foldMapConDecl :: Monoid m => (a -> PName -> m) -> ConDecl a -> m
foldMapConDecl acc conDecl = case conDecl of
  ConDecl a na tys -> addName na
  InfixConDecl a ty na ty' -> addName na
  RecDecl a na fds -> addName na <> mconcat [mconcatMap addName names | FieldDecl _ names _ <- fds]
  where
    addName = uncurry acc . n

foldMapDecl :: Monoid m => (a -> PName -> m) -> Decl a -> m
foldMapDecl acc decl = case decl of
  FunBind _ matches -> mconcat [addName fnName | (Match a fnName pats rhs mbBinds) <- matches]
  PatBind _ (PVar _ name) rhs m_bi -> addName name
  DataDecl _ don m_con head consts des -> handleDeclHead head <> mconcat [foldMapConDecl acc con | QualConDecl _ _ _ con <- consts]
  _ -> mempty
  where
    handleDeclHead head = case head of
      DHead a na -> addName na
      DHInfix a tvb na -> addName na
      DHParen a dh -> handleDeclHead dh
      DHApp a dh tvb -> mempty

    addName = uncurry acc . n
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

foldMapMod :: Monoid m => (a -> PName -> m) -> Module a -> m
foldMapMod acc (Module _ _ _ _ decls) = mconcat $ foldMapDecl acc <$> decls
foldMapMod _ _ = error "foldMapMod: Not a module"

type Mod = Module SrcSpanInfo

exts :: [Extension]
exts = map EnableExtension [NamedFieldPuns, TupleSections, CPP, RecordWildCards, ScopedTypeVariables, BangPatterns, MultiParamTypeClasses, ExistentialQuantification, GADTs, LambdaCase, TemplateHaskell]

parseFromString :: FilePath -> String -> Either Failure Mod
-- TODO parseModuleWithMode to preserve filename
parseFromString fname str = case parseModuleWithMode defaultParseMode{parseFilename=fname, extensions=exts, baseLanguage=Haskell2010} str of
  ParseOk m@(Module _ mbHead pragma imports decls) -> Right m
  ParseOk _         -> Left "parseFromString: Unexpected parse result"
  ParseFailed loc s -> Left $ "parseFromString: " <> s <> " at " <> show loc

parseFromFile :: FilePath -> IO (Either Failure Mod)
parseFromFile path = do
  -- force lazy I/O
  mybeContents <- try $ (evaluate . force) =<< readFile path :: IO (Either SomeException String)
  case mybeContents of
    Left e -> pure $ Left $ displayException e
    Right content -> pure $ parseFromString path content

symsFromModule :: Module info -> Table PName info
symsFromModule = foldMapMod (flip T.singleton)

symsFromFile :: FilePath -> IO (Either Failure SymTable)
symsFromFile = fmap symsFromModule <.> parseFromFile
