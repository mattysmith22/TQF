module TQF.TypeCheck.Monad
    ( TypeCheck
    , runTypeCheck
    , block
    , curFacts
    , setCurFacts
    , exitWith
    ) where

import           Control.Arrow
import           Control.Monad.Trans.Class

data Info f t
    = Info
    { exitWithType  :: t
    , exitWithFacts :: f
    , nominalFacts  :: f
    }

newtype TypeCheck f t m a
    = TypeCheck
    { runTypeCheck' :: f -> m (a, Info f t)
    }

instance Functor m => Functor (TypeCheck f t m) where
    fmap f x = TypeCheck $ fmap (first f) . runTypeCheck' x

instance (Monoid f, Monoid t, Monad m) => Applicative (TypeCheck f t m) where
    pure x = TypeCheck $ \f -> return (x, Info
        { exitWithType = mempty
        , exitWithFacts = mempty
        , nominalFacts = f
        })

    fa <*> fb = TypeCheck $ \f -> do
        (a,ainfo) <- runTypeCheck' fa f
        (b,binfo) <- runTypeCheck' fb $ nominalFacts ainfo
        let mergedfacts = Info
                { exitWithType = exitWithType ainfo <> exitWithType binfo
                , exitWithFacts = exitWithFacts ainfo <> exitWithFacts binfo
                , nominalFacts = nominalFacts binfo
                }
        return (a b, mergedfacts)

instance (Monoid f, Monoid t, Monad m) => Monad (TypeCheck f t m) where
    ma >>= f = TypeCheck $ \facts -> do
        (a,ainfo) <- runTypeCheck' ma facts
        (b,binfo) <- runTypeCheck' (f a) $ nominalFacts ainfo
        let mergedfacts = Info
                { exitWithType = exitWithType ainfo <> exitWithType binfo
                , exitWithFacts = exitWithFacts ainfo <> exitWithFacts binfo
                , nominalFacts = nominalFacts binfo
                }
        return (b, mergedfacts)

instance (Monoid f, Monoid t) => MonadTrans (TypeCheck f t) where
    lift mx = TypeCheck $ \facts -> do
        x <- mx
        return (x, Info
            { exitWithFacts = mempty
            , exitWithType = mempty
            , nominalFacts = facts
            })

runTypeCheck
    :: (Monoid f, Monoid t, Monad m)
    => TypeCheck f t m a
    -> m a
runTypeCheck x
    = fst <$> runTypeCheck' x mempty

block
    :: (Monoid f, Monoid t, Monad m)
    => TypeCheck f t m a
    -> TypeCheck f t m (t,a)
block x = TypeCheck $ \facts -> do
    (x,info) <- runTypeCheck' x facts
    let nominalFacts' = exitWithFacts info <> nominalFacts info
    let info' = Info
            { exitWithType = mempty
            , exitWithFacts = mempty
            , nominalFacts = nominalFacts'
            }
    return ((exitWithType info, x), info')

curFacts
    :: (Monoid f, Monoid t, Monad m)
    => TypeCheck f t m f
curFacts = TypeCheck $ \facts -> runTypeCheck' (pure facts) facts

setCurFacts
    :: (Monoid f, Monoid t, Monad m)
    => f
    -> TypeCheck f t m ()
setCurFacts facts = TypeCheck $ \_ -> runTypeCheck' (pure ()) facts

exitWith
    :: (Monoid f, Monoid t, Monad m)
    => f
    -> t
    -> TypeCheck f t m ()
exitWith exitFacts exitType = TypeCheck $ \f -> return ((), Info
    { exitWithFacts = exitFacts
    , exitWithType = exitType
    , nominalFacts = f
    })
