module Stella.Check.Utils (Pretty(..)) where
import Data.Text (Text)
import qualified Data.Text as T

class Pretty a where
  pp :: a -> Text

instance Pretty Int where pp = show'
instance Pretty Integer where pp = show'
instance Pretty Text where pp = id
instance Pretty String where pp = T.pack
instance Pretty a => Pretty [a]
  where pp ls = "[ " <> T.intercalate "\n, " (map pp ls) <> " ]"



show' :: Show a => a -> Text
show' = T.pack . show
