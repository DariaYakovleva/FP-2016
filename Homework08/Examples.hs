newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

get :: (Monad m) => StateT s m s
get = state $ \ s -> (s, s)

put :: (Monad m) => s -> StateT s m ()
put s = state $ \ _ -> ((), s)

modify :: (Monad m) => (s -> s) -> StateT s m ()
modify f = state $ \ s -> ((), f s)

gets :: (Monad m) => (s -> a) -> StateT s m a
gets f = state $ \ s -> (f s, s)

class MonadTrans t where
    lift :: (Monad m) => m a -> t m a



foo :: ReaderT Int (State [Int]) Int  -- or StateT [Int] (Reader Int) Int
foo i = do
    baseCounter <- ask
    let newCounter = baseCounter + i
    put [baseCounter, newCounter]
    return newCounter

newtype MaybeIO a = MaybeIO {
   runMaybeIO :: IO (Maybe a)
}

instance Monad MaybeIO where
    return x = MaybeIO (return (Just x))
    MaybeIO action >>= f = MaybeIO $ do
        result <- action
        case result of
            Nothing -> return Nothing
            Just x  -> runMaybeIO (f x)
result <- runMaybeIO $ do
    c1 <- MaybeIO $ tryConnect "host1"
    c2 <- MaybeIO $ tryConnect "host2" 

transformIO2MaybeIO :: IO a -> MaybeIO a
transformIO2MaybeIO action = MaybeIO $ do
    result <- action
    return (Just result)

result <- runMaybeIO $ do
  c1 <- MaybeIO $ tryConnect "host1"
  transformIO2MaybeIO $ print "Hello"
  c2 <- MaybeIO $ tryConnect "host2"
  ...

MaybeT transformer

newtype MaybeT m a = MaybeT {
   runMaybeT :: m (Maybe a)
}

newtype MaybeIO a = MaybeIO {
   runMaybeIO :: IO (Maybe a)
}

instance Monad m => Monad (MaybeT m) where
    return x = MaybeT (return (Just x))
    MaybeT action >>= f = MaybeT $ do
        result <- action
        case result of
            Nothing -> return Nothing
            Just x  -> runMaybeT (f x)

transformToMaybeT :: Monad m => m a -> MaybeT m a
transformToMaybeT action = MaybeT $ do
    result <- action
    return (Just result)

General version of transform

transformToMaybeT  :: Monad m => m a -> MaybeT    m a
transformToEitherT :: Monad m => m a -> EitherT l m a

class MonadTrans t where    -- t :: (* -> *) -> * -> *
    lift :: Monad m => m a -> t m a
instance MonadTrans MaybeT where
    lift = transformToMaybeT

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

type RepoPath   = Text
type PathReader = ReaderT RepoPath IO RepoPath

getPathToBranches :: PathReader
getPathToBranches = do
    pathToRepo <- ask
    return $ "Branches: " <> pathToRepo <> gitRoot <> "branches"

showRepoInternalDirectories :: PathReader 
showRepoInternalDirectories = do
    pathToBranches <- getPathToBranches
    pathToHooks    <- getPathToHooks
    return $ unlines [ pathToBranches, pathToHooks ]

main :: IO ()
main = do
    pathToRepo <- readFile "my.conf"
    let cleanPath = strip pathToRepo
    finalInfo <- runReaderT showRepoDirectories cleanPath
    putStrLn finalInfo

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

type         Reader r a = ReaderT r        Identity a
type PurePathReader     = Reader  RepoPath          Text
type     PathReader     = ReaderT RepoPath IO       Text

instance (Monad m) => Monad (ReaderT r m) where
    return  = lift . return
    m >>= f = ReaderT $ \r -> do
        a <- runReaderT m r
        runReaderT (f a) r

instance MonadTrans ReaderT where
   lift m = ReaderT (const m)    -- lift ma = ReaderT $ \r -> ma            

-- ListT defined mutually recursive with `Step`
newtype ListT m a = ListT { next :: m (Step m a) }

data Step m a = Cons a (ListT m a) | Nil

class (Monad m) => MonadIO m where
    liftIO :: IO a -> m a

instance MonadIO IO where
    liftIO = id

instance MonadIO m => MonadIO (StateT s m) where
    liftIO = lift . liftIO

instance MonadIO m => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO   