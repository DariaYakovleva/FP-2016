

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

type RepoPath   = Text
type PathReader = ReaderT RepoPath IO RepoPath


gitRoot = "/.git/"

getPathToBranches :: Text -> Text
getPathToBranches pathToRepo =
    "Branches: " <> pathToRepo <> gitRoot <> "branches"

getPathToBranches :: PathReader
getPathToBranches = do
    pathToRepo <- ask
    return $ "Branches: " <> pathToRepo <> gitRoot <> "branches"

getPathToHooks :: Text -> Text
getPathToHooks pathToRepo =
    "Hooks: " <> pathToRepo <> gitRoot <> "hooks"

showRepoInternalDirectories :: Text -> Text
showRepoInternalDirectories pathToRepo =
    let pathToBranches = getPathToBranches pathToRepo
        pathToHooks    = getPathToHooks pathToRepo
    in unlines [ pathToBranches, pathToHooks ]


showRepoInternalDirectories :: PathReader 
showRepoInternalDirectories = do
    pathToBranches <- getPathToBranches
    pathToHooks    <- getPathToHooks
    return $ unlines [ pathToBranches, pathToHooks ]

main :: IO ()
main = do
    pathToRepo <- readFile "my.conf"
    let cleanPath = strip pathToRepo
    let finalInfo = showRepoInternalDirectories cleanPath
    putStrLn finalInfo

main :: IO ()
main = do
    pathToRepo <- readFile "my.conf"
    let cleanPath = strip pathToRepo
    finalInfo <- runReaderT showRepoDirectories cleanPath
    putStrLn finalInfo


-- ===========
emailIsValid :: String -> Bool
emailIsValid email = '@' `elem` email

askEmail :: IO (Maybe String)
askEmail = do
    putStrLn "Input your email, please:"
    email <- getLine
    return $ if emailIsValid email
             then Just email 
             else Nothing 


askEmail :: MaybeT IO String
askEmail = do
    lift $ putStrLn "Input your email, please:"
    email <- lift getLine
    guard $ emailIsValid email
    return email


main :: IO ()
main = do
    email <- askEmail
    case email of
        Nothing     -> putStrLn "Wrong email."
        Just email' -> putStrLn $ "OK, your email is " ++ email'    

main :: IO ()
main = do
    email <- runMaybeT askEmail
    case email of
        Nothing     -> putStrLn "Wrong email."
        Just email' -> putStrLn $ "OK, your email is " ++ email'

main :: IO ()
main = do
    Just email <- runMaybeT $ asum $ repeat askEmail
    putStrLn $ "OK, your email is " ++ email        

asum :: (Foldable t, Alternative f) => t (f a) -> f a
asum = foldr (<|>) empty
