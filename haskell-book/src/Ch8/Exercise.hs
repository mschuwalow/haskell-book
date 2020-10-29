module Ch8.Exercise where
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 1 f x = f x
applyTimes n f x = f (applyTimes (n - 1) f x)

-- applyTimes 5 (+1) 5
