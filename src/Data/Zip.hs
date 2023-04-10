module Data.Zip (zipPadded) where

zipPadded :: a -> b -> [a] -> [b] -> [(a, b)]
zipPadded _ _ [] []           = []
zipPadded dl _ [] rs          = zip (repeat dl) rs
zipPadded _ dr ls []          = zip ls (repeat dr)
zipPadded dl dr (l:ls) (r:rs) = (l, r): zipPadded dl dr ls rs
