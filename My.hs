module My where

class MyFunctor f where
  myFmap :: (a -> b) -> f a -> f b

  -- Functor laws:
  -- fmap id = id
  -- fmap (g . f) = fmap g . fmap f

class MyFunctor f => MyApplicative f where
  myPure :: a -> f a
  myApply :: f (a -> b) -> f a -> f b
  myLiftA2 :: (a -> b -> c) -> f a -> f b -> f c
  myLiftA2 f x = myApply $ myApply (myPure f) x

class MyApplicative m => MyMonad m where
  myBind :: m a -> (a -> m b) -> m b
  x `myBind` y = myJoin (myFmap y x)

  myJoin :: m (m a) -> m a
  myJoin x = x `myBind` id

  myReturn :: a -> m a
  myReturn = myPure

  myFail :: String -> m a
  myFail = error

  -- Monad laws:
  -- m >>= return    = m
  -- return x >>= f  = f x
  -- (m >>= f) >>= g = m >>= (\x -> f x >>= g)

myAp :: MyMonad m => m (a -> b) -> m a -> m b
myAp f x = f `myBind` \f' ->
           x `myBind` \x' ->
           myReturn $ f' x'

class MyApplicative f => MyAlternative f where
  myEmpty :: f a
  myOr :: f a -> f a -> f a

  -- Alternative laws:
  -- empty <|> u = u
  -- u <|> empty = u
  -- u <|> (v <|> w) = (u <|> v) <|> w

class MyMonad m => MyMonadPlus m where
  myMzero :: m a
  myMplus :: m a -> m a -> m a

  -- MonadPlus laws:
  -- mzero `mplus` m = m
  -- m `mplus` mzero = m
  -- m `mplus` (n `mplus` o) = (m `mplus` n) `mplus` o
  -- mzero >>= f = mzero
  -- m >> mzero = mzero

mySequence :: (MyMonad m) => [m a] -> m [a]
mySequence [] = myReturn []
mySequence (x:xs) = x `myBind` \x' ->
                    mySequence xs `myBind` \xs' ->
                    myReturn (x':xs')

instance MyFunctor Maybe where
  myFmap f Nothing  = Nothing
  myFmap f (Just x) = Just (f x)

instance MyApplicative Maybe where
  myPure x = Just x

  myApply (Just f) (Just x) = Just (f x)
  myApply _ _               = Nothing

instance MyMonad Maybe where
  myBind Nothing _  = Nothing
  myBind (Just x) f = f x

  myFail _ = Nothing

instance MyAlternative Maybe where
  myEmpty = Nothing

  myOr x@(Just _) _ = x
  myOr Nothing  x   = x

instance MyFunctor [] where
  myFmap = map

instance MyApplicative [] where
  myPure x = [x]

  myApply fs xs = go fs xs []
    where go [] xs acc = acc
          go (f:fs) xs acc = go fs xs (acc ++ (myFmap f xs))

instance MyMonad [] where
  myBind = flip concatMap

  myFail _ = []

instance MyAlternative [] where
  myEmpty = []
  myOr = (++)

newtype MyState s a = MyState { runMyState :: s -> (a, s) }

instance MyFunctor (MyState s) where
  myFmap f p = MyState $ \s0 -> let (x, s1) = runMyState p s0
                                in (f x, s1)

instance MyApplicative (MyState s) where
  myPure x = MyState (\s -> (x, s))

  myApply f p = MyState $ \s0 -> let (f', s1) = runMyState f s0
                                     (x, s2) = runMyState p s1
                                 in (f' x, s2)

instance MyMonad (MyState s) where
  myBind p f = MyState $ \s0 -> let (x, s1) = runMyState p s0
                                in runMyState (f x) s1

myPut newState = MyState $ \_ -> ((), newState)

myGet = MyState $ \s -> (s, s)

myEvalState :: MyState s a -> s -> a
myEvalState p s = fst $ runMyState p s

execState :: MyState s a -> s -> s
execState p s = snd $ runMyState p s

myModify :: (s -> s) -> MyState s ()
myModify f = myGet `myBind` \s ->
             myPut (f s)

myGets :: (s -> a) -> MyState s a
myGets f = myGet `myBind` \s ->
           myReturn (f s)

newtype MyMaybeT m a = MyMaybeT { runMyMaybeT :: m (Maybe a) }

instance MyMonad m => MyFunctor (MyMaybeT m) where
  myFmap f x = MyMaybeT $ runMyMaybeT x `myBind` \maybe ->
                          myReturn $ myFmap f maybe

instance MyMonad m => MyApplicative (MyMaybeT m) where
  myPure = myReturn
  myApply = myAp

instance MyMonad m => MyMonad (MyMaybeT m) where
  myReturn = MyMaybeT . myReturn . Just

  -- myBind :: MyMaybeT m a -> (a -> MyMaybeT m b) -> MyMaybeT m b
  x `myBind` f = MyMaybeT $ runMyMaybeT x `myBind` \maybe ->
                            case maybe of
                              Nothing -> myReturn Nothing
                              Just value -> runMyMaybeT $ f value
