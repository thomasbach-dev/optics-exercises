module SelfCorrectingLenses
  ( ProducePrices(..)
  , limePrice1
  , lemonPrice1
  , limePrice2
  ) where

import Control.Lens (Lens', lens)
import GHC.Generics (Generic)

data ProducePrices = ProducePrices
  { _limePrice  :: Float
  , _lemonPrice :: Float
  } deriving (Eq, Generic, Show)

limePrice1 :: Lens' ProducePrices Float
limePrice1 = lens getter setter
  where
    getter = _limePrice
    setter price newPrice = price { _limePrice = max 0 newPrice }

lemonPrice1 :: Lens' ProducePrices Float
lemonPrice1 = lens getter setter
  where
    getter = _lemonPrice
    setter price newPrice = price { _lemonPrice = max 0 newPrice }

limePrice2 :: Lens' ProducePrices Float
limePrice2 = lens getter setter
  where
    getter = _limePrice
    setter prices newPrice =
      let newPrice' = max 0 newPrice
      in ProducePrices
           { _limePrice = newPrice'
           , _lemonPrice = roundTo50Cents (_lemonPrice prices) newPrice'
           }

roundTo50Cents :: Float -> Float -> Float
roundTo50Cents current other
  | current < other = max current (other - acceptedDelta)
  | otherwise = min current (other + acceptedDelta)

acceptedDelta :: Float
acceptedDelta = 0.5
