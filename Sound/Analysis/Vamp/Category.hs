  -- , pluginKey
  -- , parseCategoryFile
  -- , readCategoryFile
  -- , getCategoryMap
  -- , apiVersion

import           System.FilePath
import           Text.Regex

type Key            = String
type Category       = [String]
type CategoryMap    = [(Key, Category)]

mkPluginKey :: FilePath -> String -> Key
mkPluginKey path name = (takeBaseName path) ++ ":" ++ name

pluginKey :: PluginDescriptor -> Key
pluginKey p = mkPluginKey (pluginLibraryPath p) (pluginIdentifier p)

-- Categories

parseCategoryFile :: String -> CategoryMap
parseCategoryFile =
    mapMaybe (\x -> case matchCat x of
                Just [key, cat] -> Just (key, splitCat cat)
                _ -> Nothing) . lines
    where
        matchCat = matchRegex (mkRegex "^vamp:([^:]+:[^:]+)::(.*)")
        splitCat = splitRegex (mkRegex "[\t ]*>[\t ]*")

readCategoryFile :: FilePath -> IO CategoryMap
readCategoryFile = liftM parseCategoryFile . readFile

getCategoryMap :: IO CategoryMap
getCategoryMap = getCategoryFiles >>= liftM concat . mapM readCategoryFile
