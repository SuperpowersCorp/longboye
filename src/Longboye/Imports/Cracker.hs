module Longboye.Imports.Cracker
       ( crack
       , crackE
       ) where

import           Prelude hiding        ( drop
                                       , take
                                       , length
                                       )

import           Control.Applicative   ( (<$>), (<|>), (*>), empty)
import           Control.Monad         ( void )
import           Data.Char             ( Char )
import           Data.Functor.Identity ( Identity )
import           Data.Text             ( Text
                                       , take
                                       , drop
                                       , splitOn
                                       , length
                                       , pack
                                       )
import qualified Data.Text             as Text
import           Data.Maybe            ( fromJust
                                       , isJust
                                       )
import           Longboye.Import       ( Import )
import qualified Longboye.Import       as Import
import           Longboye.Member       ( Member( ClassMember
                                               , FnMember
                                               , OpMember
                                               )
                                       )
import qualified Longboye.Parsers      as Parsers
import           Overture
import           Text.Megaparsec       ( Dec
                                       , ParsecT
                                       , between
                                       , choice
                                       , lookAhead
                                       , many
                                       , manyTill
                                       , optional
                                       , sepBy
                                       , sepBy1
                                       )
import qualified Text.Megaparsec       as P
import           Text.Megaparsec.Char  ( anyChar
                                       , alphaNumChar
                                       , char
                                       , lowerChar
                                       , spaceChar
                                       , string
                                       , upperChar
                                       )
import qualified Text.Megaparsec.Char  as C
import           Text.Megaparsec.Error ( ParseError
                                       )
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  ( Parser )

type Language = (Text, [Import], Text)

crack :: String -> Text -> Maybe Language
crack = (eitherToMaybe .) . crackE

crackE :: String -> Text -> Either (ParseError Char Dec) Language
crackE filename = Parsers.parse filename language

language :: ParsecT Dec Text Identity Language
language = sc >> importsOnlyHaskell

importsOnlyHaskell :: Parser Language
importsOnlyHaskell = (,,) <$> preImports <*> imports <*> postImports

preImports :: Parser Text
preImports = pack <$> manyTill anyChar (lookAhead import')

imports :: Parser [Import]
imports = error "imports not implemented."

-- import           Foo
-- import           Foo.Bar
-- import           Foo.Bar as Bar
-- import           Foo.Bar as Baz
-- import           Foo.Bar as Baz.Bif
-- import           Foo.Bar as Baz.Bif        ( bam )
-- import           Foo.Bar                   ( baz )
-- import           Foo.Bar                   ( Foo(..), Bar(bif, bam), baz )
-- import           Foo.Bar as Baz            ( bif )
-- import           Foo.Bar            hiding ( baz, bif )
-- import           Foo.Bar as Baz     hiding ( bif )
-- above + qualified

import' :: Parser Import
import' = do
  void $ string "import"
  void sc
  q   <- maybeToBool <$> optional (string "qualified")
  void sc
  mod <- importedModule
  void sc
  as  <- optional asClause
  h   <- maybeToBool <$> optional (string "hiding")
  void sc
  m   <- optional membersList
  return $ Import.from q mod as h m
  where maybeToBool Nothing = False
        maybeToBool _       = True

asClause :: Parser Text
asClause = do
  void . lexeme . string $ "as"
  void sc
  modName

membersList :: Parser [Member]
membersList = between lparen rparen $ member `sepBy` lexeme (char ',')

member :: Parser Member
member = classMemb <|> fnMemb <|> opMemb

classMemb :: Parser Member
classMemb = do
  className <- typeClassName
  classOps  <- optional (between lparen rparen tcOpList)
  return $ ClassMember className classOps

typeClassName :: Parser Text
typeClassName = do
  firstLetter <- upperChar
  rest        <- many cnChars
  return . Text.pack $ firstLetter:rest
  where cnChars = alphaNumChar <|> char '_' -- TODO: anything else?

tcOpList :: Parser [Text]
tcOpList = tcOp `sepBy` comma

tcOp :: Parser Text
tcOp = error "tcOp not implemented."

fnMemb :: Parser Member
fnMemb = FnMember <$> identifer
  where identifer = do
          firstLetter <- lowerChar
          rest        <- many idChars
          return . Text.pack $ firstLetter:rest
        idChars = alphaNumChar <|> char '_' -- TODO: others?

opMemb :: Parser Member
opMemb = between lparen rparen (mk <$> p)
  where mk      = OpMember . Text.pack
        p       = many . choice . map C.char $ opChars
        opChars = "<>!@#$%^&*-+." :: String

importedModule :: Parser Text
importedModule = modName

modName :: Parser Text
modName = Text.intercalate "." <$> modSeg `sepBy1` char '.'

modSeg :: Parser Text
modSeg = do
  firstLetter <- upperChar
  rest        <- many modChars
  return . Text.pack $ firstLetter:rest
  where modChars = alphaNumChar <|> char '_' -- TODO: anything else?

postImports :: Parser Text
postImports = error "postImports not implemented."

-- | sc: space consumer
sc :: ParsecT Dec Text Identity ()
sc = L.space (void spaceChar) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lparen = char '('
rparen = char ')'
comma  = char ','
