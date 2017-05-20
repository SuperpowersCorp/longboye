module Longboye.Member
       ( Member(..)
       , fromDecl
       , render
       ) where

import           Data.Monoid                   ( (<>) )
import           Data.Ord                      ()
import           Data.Text                     ( Text )
import qualified Data.Text             as Text
import           Language.Haskell.Exts         ( CName( ConName
                                                      , VarName
                                                      )
                                               , ImportSpecList( ImportSpecList )
                                               , ImportSpec( IAbs
                                                           , IThingAll
                                                           , IThingWith
                                                           , IVar
                                                           )
                                               , Name( Ident
                                                     , Symbol
                                                     )
                                               , Namespace( NoNamespace
                                                          , TypeNamespace
                                                          , PatternNamespace
                                                          )
                                               , SrcSpanInfo
                                               )

data Member
  = NamedMember Text
  | OpMember Text [Text]
  deriving (Eq, Ord, Read, Show)

fromDecl :: ImportSpecList SrcSpanInfo -> [Member]
fromDecl (ImportSpecList _ _ specs) = map fromSpec specs
  where fromSpec (IVar _ name)              = NamedMember (renderName name)
        fromSpec (IThingAll _ name)         = NamedMember (renderName name)
        fromSpec (IThingWith _ name cnames) = OpMember (renderName name) (map cnameText cnames)
        fromSpec (IAbs _ ns name)           = NamedMember $ nsPre <> renderName name
          where nsPre =
                  case ns of
                    NoNamespace _      -> ""
                    TypeNamespace _    -> "[TYPE_NS]." -- TODO: TypeNamespace
                    PatternNamespace _ -> "[PATT_NS]." -- TODO: PatternNamespace

render :: Text -> Member -> Text
render _   (NamedMember name)   = name
render _   (OpMember name [])   = name
render sep (OpMember name ops) = name <> renderedOps
  where renderedOps = if null ops
                        then ""
                        else "( " <> Text.intercalate sep' ops <> lastPadding <> ")"
        sep'        = "\n" <> Text.replicate n " " <> Text.tail sep
        sep''       = Text.tail . Text.init . Text.init $ sep'
        n           = 2 + nameLength
        nameLength  = Text.length name
        lastPadding
          | null ops        = ""
          | length ops == 1 = " "
          | otherwise       = "\n" <> sep''

renderName :: Name a -> Text
renderName (Ident _ n)  = Text.pack n
renderName (Symbol _ n) = "(" <> Text.pack n <> ")"

cnameText :: CName a -> Text
cnameText (VarName _ name) = renderName name
cnameText (ConName _ name) = renderName name
