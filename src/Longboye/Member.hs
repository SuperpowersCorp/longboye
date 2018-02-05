{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Longboye.Member
       ( Member(..)
       , fromDecl
       , opCount
       , render
       , sort
       ) where

import           Longboye.Prelude              hiding ( sort )

import qualified Data.Text             as Text
import           Language.Haskell.Exts                ( CName( ConName
                                                             , VarName
                                                             )
                                                      , ImportSpec( IAbs
                                                                  , IThingAll
                                                                  , IThingWith
                                                                  , IVar
                                                                  )
                                                      , ImportSpecList( ImportSpecList )
                                                      , Name( Ident
                                                            , Symbol
                                                            )
                                                      , Namespace( NoNamespace
                                                                 , PatternNamespace
                                                                 , TypeNamespace
                                                                 )
                                                      , SrcSpanInfo
                                                      )

data Member
  = NamedMember Text Bool
  | OpMember Text [Text]
  deriving (Eq, Ord, Read, Show)

opCount :: Member -> Int
opCount (NamedMember _ _) = 0
opCount (OpMember _ xs)   = length xs

fromDecl :: ImportSpecList SrcSpanInfo -> [Member]
fromDecl (ImportSpecList _ _ specs) = map fromSpec specs
  where
    fromSpec (IVar _ name)              = NamedMember (renderName name) False
    fromSpec (IThingAll _ name)         = NamedMember (renderName name) True
    fromSpec (IThingWith _ name cnames) = OpMember (renderName name) (map cnameText cnames)
    fromSpec (IAbs _ ns name)           = NamedMember (nsPre <> renderName name) False
      where
        nsPre = case ns of
          NoNamespace      _ -> ""
          TypeNamespace    _ -> notSupportedYet "TypeNamespace"
          PatternNamespace _ -> notSupportedYet "PatternNamespace"
        notSupportedYet x = panic $ x <> " not supported yet."

render :: Text -> Member -> Text
render _   (NamedMember name False) = name
render _   (NamedMember name True)  = name <> "(..)"
render _   (OpMember name [])       = name
render sep (OpMember name ops)      = name <> renderedOps
  where
    renderedOps = if null ops
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

sort :: [Member] -> [Member]
sort = sortBy (comparing sortKey)
  where
    sortKey (NamedMember name b) =
      name <> if b then "(..)" else ""
    sortKey (OpMember name ms) =
      name <> if null ms then "" else Text.intercalate ", " ms
