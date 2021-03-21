module Text.Pretty(
      module Text.PrettyPrint
    , Pretty(..)
    , dot
    , tab
    , vsep
    , commas
) where

import Text.PrettyPrint
import Data.List        (intersperse)

class Pretty a where
    pretty   :: a -> Doc
    toString :: a -> String
    toString = render . pretty
    prettyPrint :: a -> IO ()
    prettyPrint = putStrLn . toString

instance Pretty Doc where
    pretty = id

instance Pretty String where
    pretty = text

instance Pretty Int where
    pretty = int

instance Pretty Float where
    pretty = float

instance Pretty a => Pretty (Maybe a) where
    pretty = maybe (text "Nothing") pretty

dot :: Doc
dot = char '.'

tab :: Doc -> Doc
tab = nest 4

vsep :: Pretty a => [a] -> Doc
vsep = foldr (($+$) . pretty) empty

commas :: Pretty a => [a] -> Doc
commas = hcat . intersperse comma . map pretty