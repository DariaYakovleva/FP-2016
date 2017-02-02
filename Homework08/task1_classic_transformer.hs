--TASK
--Implement instance Monad & MonadTrans for
--StateT, WriterT & EitherT

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
-- {-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}

--import Control.Monad.State
import Control.Monad.Trans.Class 
import Control.Applicative
import Control.Monad

main :: IO()
main = do
	putStrLn $ "HW8.1"	

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
    fmap f m = StateT $ \ s -> fmap (\ ~(a, s') -> (f a, s')) $ runStateT m s

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure a = StateT $ \ s -> return (a, s)
    StateT mf <*> StateT mx = StateT $ \ s -> do
        ~(f, s') <- mf s
        ~(x, s'') <- mx s'
        return (f x, s'')

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

mapWriterT :: (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f m = WriterT $ f (runWriterT m)
{-# INLINE mapWriterT #-}

instance (Functor m) => Functor (WriterT w m) where
    fmap f = mapWriterT $ fmap $ \ ~(a, w) -> (f a, w)
    {-# INLINE fmap #-}

instance (Foldable f) => Foldable (WriterT w f) where
    foldMap f = foldMap (f . fst) . runWriterT
    {-# INLINE foldMap #-}

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
    pure a  = WriterT $ pure (a, mempty)
    {-# INLINE pure #-}
    f <*> v = WriterT $ liftA2 k (runWriterT f) (runWriterT v)
      where k ~(a, w) ~(b, w') = (a b, w `mappend` w')
    {-# INLINE (<*>) #-}

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Monad m => Functor (EitherT e m) where
  fmap f = EitherT . liftM (fmap f) . runEitherT
  {-# INLINE fmap #-}

instance Monad m => Applicative (EitherT e m) where
  pure a  = EitherT $ return (Right a)
  {-# INLINE pure #-}
  EitherT f <*> EitherT v = EitherT $ f >>= \mf -> case mf of
    Left  e -> return (Left e)
    Right k -> v >>= \mv -> case mv of
      Left  e -> return (Left e)
      Right x -> return (Right (k x))
  {-# INLINE (<*>) #-}
-- ========================================================================
--class Monad m where   -- m :: * -> *
--    return :: a -> m a                  -- return
--    (>>=)  :: m a -> (a -> m b) -> m b  -- bind

instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \s -> return (a, s)    
    m >>= f = StateT $ \s -> do
        			(a, s') <- runStateT m s
        			runStateT (f a) s'

--lift :: (Monad m) => m a -> t m a	
instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> do
    	res <- m
    	return (res, s)    

instance (Monoid w, Monad m) => Monad (WriterT w m) where
    return a = WriterT $ return (a, mempty)
    m >>= f  = WriterT $ do
                    (a, w) <- runWriterT m
                    (a', w') <- runWriterT (f a) 
                    return (a', mappend w w')

instance (Monoid w) => MonadTrans (WriterT w) where
    lift m = WriterT $ do
        res <- m
        return (res, mempty)

-- newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }
-- data Either a b  =  Left a | Right b

instance Monad m => Monad (EitherT l m) where
    return a = EitherT $ return $ Right a
    m >>= f = EitherT $ do
                res <- runEitherT m
                case res of
                    Right r -> runEitherT (f r)
                    Left e -> return $ Left e

instance MonadTrans (EitherT l) where
    lift m = EitherT $ do
                res <- m
                return $ Right res