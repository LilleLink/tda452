import Test.QuickCheck
data Grid a = Grid [[a]]

grids :: Arbitrary a => Gen (Grid a)
grids = do
    rX <- choose (1,10)
    rY <- choose (1,10)
    Grid <$> (vectorOf rY . vectorOf rX $ arbitrary)

instance Arbitrary a => Arbitrary (Grid a) where
    arbitrary = grids