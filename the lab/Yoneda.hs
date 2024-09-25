{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

newtype Yoneda f a = Yoneda {runYoneda :: forall b. (a -> b) -> f b}


instance Functor (Yoneda f) where  
  fmap :: (a -> b) -> Yoneda f a -> Yoneda f b
  fmap f y = Yoneda $ \ab -> runYoneda y (ab . f)