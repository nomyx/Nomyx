
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeFamilies                     #-}

import Data.Typeable
:q

main = undefined


class Signal a where
  data SignalType a :: *
  getSignal :: a -> SignalType a

instance Signal Radio a where
  data SignalType a [(a, String)] =
  getSignal

