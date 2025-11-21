module Password where

newtype PwdOp a = PwdOp { runPwd :: String -> (a, String) }

instance Functor PwdOp where
    fmap f (PwdOp g) = PwdOp $ \s -> let (a, s') = g s in (f a, s')

instance Applicative PwdOp where
    pure x = PwdOp $ \s -> (x, s)
    (PwdOp f) <*> (PwdOp g) = PwdOp $ \s ->
        let (fab, s') = f s
            (a, s'') = g s'
        in (fab a, s'')

instance Monad PwdOp where
    (PwdOp m) >>= k = PwdOp $ \s ->
        let (a, s') = m s
            PwdOp m' = k a
        in m' s'

setPassword :: String -> PwdOp ()
setPassword p = PwdOp $ \_ -> ((), p)

checkPassword :: String -> PwdOp Bool
checkPassword p = PwdOp $ \s -> (p == s, s)

runPwdOp :: PwdOp a -> a
runPwdOp (PwdOp f) = fst (f "")





