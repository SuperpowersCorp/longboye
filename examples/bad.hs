import           Data.Text                              ( Text )
import qualified Data.Text                    as Text
import           Language.Haskell.Exts                  ( Module( Module
                                                                , XmlHybrid
                                                                , XmlPage
                                                                )
                                                        , SrcSpanInfo
                                                        , srcSpanEndLine
                                                        , srcSpanStartLine
                                                        , srcInfoSpan
                                                        , importAnn
                                                        )
import           Language.Haskell.Exts.Parser           ( ParseResult( ParseOk
                                                                     , ParseFailed
                                                                     )
                                                        , defaultParseMode
                                                        , parseFilename
                                                        )
import qualified Language.Haskell.Exts.Parser as Parser
import           Language.Haskell.Exts.Syntax           ( ImportDecl )
import           Longboye.Import                        ( Import )
import qualified Longboye.Import              as Import
import           Overture
import           Preluding                              hiding  ( head
                                                        , init
                                                        )
