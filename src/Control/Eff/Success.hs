{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Safe #-}
-- | Success-effect for exiting a computation early with a result.
--
--  This is similar to the Exception effect with the difference that the
-- intention of this effect is that the early termination is desired.
module Control.Eff.Success ( Success (..)
                            , Done
                            , success
                            , success_
                            , done
                            , runSuccess
                            , execSuccess
                            , execSuccessDef
                            , doOnSuccess
                            , execSuccessEx
                            , filterSuccess
                            , mapSuccess
                            , mapTopSuccess
                            ) where

import Control.Eff.Internal
import Data.OpenUnion

import Control.Monad (when)
import Control.Monad.Base
import Control.Monad.Trans.Control

newtype Success s v = Success s
type Done = Success ()

instance ( MonadBase m m
         , SetMember Lift (Lift m) r
         , MonadBaseControl m (Eff r)
         ) => MonadBaseControl m (Eff (Success s ': r)) where
    type StM (Eff (Success s ': r)) a = StM (Eff r) (Either s a)
    liftBaseWith f = raise $ liftBaseWith $ \runInBase ->
                       f (runInBase . runSuccess)
    restoreM x = do r :: Either s a <- raise (restoreM x)
                    either success return r

-- | Abort the computation with a success-value, the type is inferred
success :: Member (Success s) r => s -> Eff r a
success s = send (Success s)
{-# INLINE success #-}

success_ :: Member (Success s) r => s -> Eff r ()
success_ s = send (Success s)
{-# INLINE success_ #-}

done :: Member Done r => Eff r ()
done = success ()

-- | Run a computation that might produce a computation result.
runSuccess :: Eff (Success s ': r) a -> Eff r (Either s a)
runSuccess = handle_relay
  (return . Right)
  (\(Success e) _ -> return (Left e))

-- | Runs a success-effect, such that a successful computation returns 'Just'
--   the success value, and 'Nohting' if no value has been produced.
execSuccess :: Eff (Success s ': r) a -> Eff r (Maybe s)
execSuccess = fmap (either Just (const Nothing)) . runSuccess
{-# INLINE execSuccess #-}

execSuccessDef :: s -> Eff (Success s ': r) a -> Eff r s
execSuccessDef s = fmap (either id (const s)) . runSuccess
{-# INLINE execSuccessDef #-}


execSuccessEx :: Eff (Success s ': r) a -> Eff r s
execSuccessEx = execSuccessDef $
  error "Control.Eff.Success.execSuccessEx no success"


-- TODO: comment
doOnSuccess :: Member (Success s) r => Eff r a -> (s -> Eff r a) -> Eff r a
doOnSuccess m handle = interpose return (\(Success s) _ -> handle s) m

-- | map the success
mapSuccess :: Member (Success s) r => (s -> s) -> Eff r a -> Eff r a
mapSuccess f = interpose return (\(Success s) _ -> success $ f s)

-- | map a success-effect where the success is on the top-level
mapTopSuccess
  :: Member (Success s) r
  => (s0 -> s)
  -> Eff (Success s0 ': r) a
  -> Eff r a
mapTopSuccess f m = runSuccess m >>= either (success . f) return

-- | Discard the result of a succesful computation depending on a predicate
filterSuccess :: Member (Success s) r => (s -> Bool) -> Eff r a -> Eff r ()
filterSuccess p = interpose (const $ return ())
    (\(Success s) _ -> when (p s) $ success s)
