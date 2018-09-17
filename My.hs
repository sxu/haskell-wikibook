module My where


-- Functor
-- fmap id = id
-- fmap (g . f) = fmap g . fmap f

class MyFunctor f where
  myFmap :: (a -> b) -> f a -> f b

class MyFunctor f => MyApplicative f where
  myPure :: a -> f a
  myApply :: f (a -> b) -> f a -> f b
  myLiftA2 :: (a -> b -> c) -> f a -> f b -> f c
  myLiftA2 f x = myApply $ myApply (myPure f) x

-- Monad laws:
-- m >>= return    = m
-- return x >>= f  = f x
-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)

class MyApplicative m => MyMonad m where
  myBind :: m a -> (a -> m b) -> m b
  x `myBind` f = myJoin (myFmap f x)

  myJoin :: m (m a) -> m a
  myJoin x = x `myBind` id

  myReturn :: a -> m a
  myReturn = myPure

  myFail :: String -> m a
  myFail = error

myAp :: MyMonad m => m (a -> b) -> m a -> m b
myAp f x = f `myBind` \f' ->
           x `myBind` \x' ->
           myReturn $ f' x'

myLiftM :: MyMonad m => (a -> b) -> m a -> m b
myLiftM f x = x `myBind` \x' -> myReturn $ f x'

mySequence :: (MyMonad m) => [m a] -> m [a]
mySequence [] = myReturn []
mySequence (x:xs) = x `myBind` \x' ->
                    mySequence xs `myBind` \xs' ->
                    myReturn (x':xs')

-- Alternative laws:
-- empty <|> u = u
-- u <|> empty = u
-- u <|> (v <|> w) = (u <|> v) <|> w

class MyApplicative f => MyAlternative f where
  myEmpty :: f a
  myOr :: f a -> f a -> f a

myGuard :: MyAlternative f => Bool -> f ()
myGuard True  = myPure ()
myGuard False = myEmpty

-- MonadPlus laws:
-- mzero `mplus` m = m
-- m `mplus` mzero = m
-- m `mplus` (n `mplus` o) = (m `mplus` n) `mplus` o
-- mzero >>= f = mzero
-- m >> mzero = mzero

class MyMonad m => MyMonadPlus m where
  myMzero :: m a
  myMplus :: m a -> m a -> m a

class MyMonadTrans t where
  myLift :: MyMonad m => m a -> t m a

-- Instances of existing types

instance MyFunctor IO where
  myFmap = fmap

instance MyApplicative IO where
  myPure = pure
  myApply = (<*>)

instance MyMonad IO where
  myBind = (>>=)

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

-- State

newtype MyState s a = MyState { runMyState :: s -> (a, s) }

myState = MyState

instance MyFunctor (MyState s) where
  myFmap f p = MyState $ \s0 -> let (x, s1) = runMyState p s0
                                in (f x, s1)

instance MyApplicative (MyState s) where
  myPure x = MyState (\s -> (x, s))

  myApply f p = MyState $ \s0 -> let (f', s1) = runMyState f s0
                                     (x, s2) = runMyState p s1
                                 in (f' x, s2)

instance MyMonad (MyState s) where
  (MyState p) `myBind` f = MyState $ \s0 -> let (x, s1) = p s0
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

-- MaybeT

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

instance MyMonad m => MyAlternative (MyMaybeT m) where
  myEmpty = MyMaybeT $ myReturn Nothing

  x `myOr` y = MyMaybeT $ runMyMaybeT x `myBind` \maybeX ->
                          runMyMaybeT y `myBind` \maybeY ->
                          myReturn $ maybeX `myOr` maybeY

instance MyMonad m => MyMonadPlus (MyMaybeT m) where
  myMzero = myEmpty
  myMplus = myOr

instance MyMonadTrans MyMaybeT where
  myLift = MyMaybeT . (myLiftM Just)

-- Identity

newtype MyIdentity a = MyIdentity { runMyIdentity :: a }

instance MyFunctor MyIdentity where
  myFmap f = MyIdentity . f . runMyIdentity

instance MyApplicative MyIdentity where
  myPure = MyIdentity
  myApply f = MyIdentity . runMyIdentity f . runMyIdentity

instance MyMonad MyIdentity where
  myReturn = MyIdentity
  (MyIdentity x) `myBind` f = f x

newtype MyIdentityT m a = MyIdentityT { runMyIdentityT :: m a }

instance MyMonad m => MyMonad (MyIdentityT m) where
  myReturn = MyIdentityT . myReturn
  
  (MyIdentityT x) `myBind` f = MyIdentityT $ x `myBind` (runMyIdentityT . f)

instance MyMonad m => MyApplicative (MyIdentityT m) where
  myPure = myReturn
  myApply = myAp

instance MyMonad m => MyFunctor (MyIdentityT m) where
  myFmap = myLiftM

instance MyMonadTrans MyIdentityT where
  myLift = MyIdentityT

-- StateT

newtype MyStateT s m a = MyStateT { runMyStateT :: (s -> m (a,s)) }

instance MyMonad m => MyMonad (MyStateT s m) where
  myReturn x = MyStateT (\s -> myReturn (x, s))

  (MyStateT p) `myBind` f = MyStateT $ \s0 ->
                              p s0 `myBind` \(x, s1) ->
                              runMyStateT (f x) s1

instance MyMonad m => MyApplicative (MyStateT s m) where
  myPure = myReturn
  myApply = myAp

instance MyMonad m => MyFunctor (MyStateT s m) where
  myFmap = myLiftM

type MyState' s = MyStateT s MyIdentity

myState' :: (s -> (a, s)) -> MyState' s a
myState' f = MyStateT $ \s -> myReturn $ f s
