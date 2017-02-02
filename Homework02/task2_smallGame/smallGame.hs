main = do
	let p1 = Player {pName="ivan", pHealth=5, pAttack=5, pDefense=5}
	let m1 = Monster {mName="monstr1", mHealth=1, mAttack=1, mDefense=1, mBonus=Health 3}
	let m2 = Monster {mName="monstr2", mHealth=2, mAttack=2, mDefense=2, mBonus= Attack 4}
	print $ p1
	print $ gloriousBattle p1 [m1] 
	print $ gloriousBattle p1 [m1, m2] 
	print $ "END"

data Equipment = Health Int | Attack Int | Defense Int deriving Show

class Add a where
	(!+) :: a -> Player -> Player

instance Add Equipment where
	(!+) (Health a) player = player {pHealth = sum(pHealth player, a)}
	(!+) (Attack a) player = player {pAttack = sum(pAttack player, a)}
	(!+) (Defense a) player = player {pDefense = sum(pDefense player, a)}


data Player = Player
	{ pName :: String
	, pHealth :: Int
	, pAttack :: Int
	, pDefense :: Int
} deriving Show
data Monster = Monster 
	{ mName :: String
	, mHealth :: Int
	, mAttack :: Int
	, mDefense :: Int
	, mBonus :: Equipment
	}

type Result = Maybe Player

gloriousBattle :: Player -> [Monster] -> Result
gloriousBattle p [] = Just p
gloriousBattle p (x:xs) = let res = fight p x in case res of
												 	Nothing -> Nothing
												 	Just pp -> gloriousBattle pp xs

fight :: Player -> Monster -> Result
fight p m
	| pHealth p <= 0 = Nothing
	| mHealth m <= 0 = Just $ (!+) (mBonus m) p
	| otherwise = case strike p m 2 of
		Nothing -> Nothing
		Just (pp, mm) -> if (mHealth mm <= 0) 
						 then Just $ (!+) (mBonus mm) pp
						 else let res = (strike' mm pp 2) in case res of
						 										Nothing -> Just $ (!+) (mBonus mm) pp
						 										Just r -> (fight (fst r) (snd r))

strike :: Player -> Monster -> Int -> Maybe (Player, Monster)
strike p m x
	| pAttack p < x = Nothing
	| pAttack p <= 0 = Nothing
	| mDefense m >= x = Just (p {pAttack = pAttack p - x}, m {mDefense = mDefense m - x})
	| otherwise = Just (p {pAttack = pAttack p - x}, m {mHealth = mHealth m - (x - mDefense m),  mDefense = 0})

strike' :: Monster -> Player -> Int ->  Maybe (Player, Monster)
strike' m p x
	| mAttack m < x = Nothing
	| mAttack m <= 0 = Nothing
	| pDefense p >= x = Just (p {pDefense = pDefense p - x}, m {mAttack = mAttack m - x})
	| otherwise = Just (p {pHealth = pHealth p - (x - pDefense p),  pDefense = 0}, m {mAttack = mAttack m - x})
