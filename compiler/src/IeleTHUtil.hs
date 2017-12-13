module IeleTHUtil where
import Data.List(stripPrefix)
import Data.Maybe(maybeToList)
import Data.Char(isUpper,toLower)
import Language.Haskell.TH
import Control.Lens
import Control.Lens.TH

makeFieldsWithNamer namer tyName =
  makeLensesWith (defaultFieldRules & lensField .~ namer) tyName

makeFieldsForPrefix :: String -> Name -> DecsQ
makeFieldsForPrefix prefix tyName = makeFieldsWithNamer namer tyName
 where
  namer tyName _ field = maybeToList $ do
    fieldPart <- stripPrefix prefix (nameBase field)
    method    <- case fieldPart of
      (x:xs) | isUpper x -> Just (toLower x:xs)
      _ -> Nothing
    let cls = "Has" ++ fieldPart
    return (MethodName (mkName ("Has"++fieldPart)) (mkName method))

makeFieldsFor :: [(String,String)] -> Name -> DecsQ
makeFieldsFor mapping tyName = makeFieldsWithNamer (lookingupNamer mapping) tyName
