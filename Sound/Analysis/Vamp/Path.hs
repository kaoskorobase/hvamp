{-# LANGUAGE CPP #-}
module Sound.Analysis.Vamp.Path (
    expandEnv,
    getVampPath,
    getPluginPath,
    getPluginLibraries,
    getCategoryPath,
    getCategoryFiles
) where

import System.Directory
import System.Environment
import System.FilePath
import Text.Regex

-- utilities

-- | Regular expression matching environment variable reference.
-- On windows of the form /%VARIABLE%/, on other systems /$VARIABLE/.
envVarRegex :: Regex
#if defined(mingw32_HOST_OS)
envVarRegex = mkRegex "%([A-Za-z0-9_]+)%"
#else
envVarRegex = mkRegex "\\$([A-Za-z0-9_]+)"
#endif

-- | Expand environment variables matching 'envVarRegex' from env in str.
expandEnv :: [(String, String)] -> String -> String
expandEnv env str =
    case matchRegexAll envVarRegex str of
        Just (before, match, after, [sub]) ->
            let replacement = case lookup sub env of
                                Nothing -> match
                                Just value -> value
            in before ++ replacement ++ (expandEnv env after)
        _ -> str

-- | Get full paths of directory contents.
getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' dir = do
    files <- catch (getDirectoryContents dir) (\_ -> return [])
    return $ map (combine dir) (filter (flip notElem [".", ".."]) files)

-- | Get files in path list matching filter f.
getFilesInPath :: (FilePath -> Bool) -> [FilePath] -> IO [FilePath]
getFilesInPath f p = mapM getDirectoryContents' p >>= return . filter f . concat

defaultVampPath :: String
#if defined(darwin_HOST_OS)
defaultVampPath = "$HOME/Library/Audio/Plug-Ins/Vamp:/Library/Audio/Plug-Ins/Vamp"
#elif defined(mingw32_HOST_OS)
defaultVampPath = "%ProgramFiles%\\Vamp Plugins"
#else
defaultVampPath = "$HOME/vamp:$HOME/.vamp:/usr/local/lib/vamp:/usr/lib/vamp"
#endif

getVampPath :: IO String
getVampPath = catch (getEnv "VAMP_PATH") (\_ -> return defaultVampPath)

getPluginPath :: IO [FilePath]
getPluginPath = do
    env <- getEnvironment
    vampPath <- getVampPath
    return $ map (expandEnv env) (splitSearchPath vampPath)

getCategoryPath :: IO [FilePath]
getCategoryPath = getPluginPath >>= return . map mkCategoryPath
    where mkCategoryPath p = subRegex (mkRegex "\\/lib\\/") p "/share/"

pluginSuffix :: String
#if defined(darwin_HOST_OS)
pluginSuffix = ".dylib"
#elif defined(mingw32_HOST_OS)
pluginSuffix = ".dll"
#else
pluginSuffix = ".so"
#endif

isPluginLibrary :: FilePath -> Bool
isPluginLibrary path = takeExtension path == pluginSuffix

getPluginLibraries :: IO [FilePath]
getPluginLibraries = getFilesInPath isPluginLibrary =<< getPluginPath

categoryFileSuffix :: String
categoryFileSuffix = ".cat"

isCategoryFile :: FilePath -> Bool
isCategoryFile path = takeExtension path == categoryFileSuffix

getCategoryFiles :: IO [FilePath]
getCategoryFiles = getFilesInPath isCategoryFile =<< getCategoryPath
