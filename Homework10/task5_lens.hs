{-# LANGUAGE RankNTypes #-}

import Control.Monad.Identity
import Control.Applicative

main :: IO()
main = do
	putStrLn $ "HW10.5"

--True lenses have type:

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t

--Lecture presented Van Laarhoven Lenses or Simple Lenses:

type Lens' s a  = Lens s s a a

--Implement next basic lens functions
set  :: Lens' s a -> a -> s -> s         -- set    value (setter)
set ln value thing = runIdentity $ ln (\_ -> Identity value) thing

view :: Lens' s a -> s -> a              -- lookup value (getter)
view ln th = getConst $ ln (\x -> Const x) th

over :: Lens' s a -> (a -> a) -> s -> s  -- change value (modifier)
over ln f th = set ln (f (view ln th)) th
