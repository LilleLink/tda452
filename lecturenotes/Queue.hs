module Queue_Work where



-- For testing we use the "slow" queue as out model
import Test.QuickCheck
import qualified SlowQueue as Slow

------------------------------------------------------------
-- Interface

empty   :: Q a
add     :: a -> Q a -> Q a
remove  :: Q a -> Q a
front   :: Q a -> a
isEmpty :: Q a -> Bool

------------------------------------------------------------
-- Implementation

data Q a = Q [a] [a]
  deriving Show


-- invariant: front should not be empty when back isn't
invariant :: Q Int -> Bool
invariant (Q front back) = null back || (not (null front))


empty                     = Q [] []
add a (Q front back)      = fixQ front (a:back)
remove (Q (a:front) back) = fixQ front back
front (Q (a:front) back)  = a
isEmpty (Q front back)    = null front && null back

-- move back to the front when front is empty
-- (i.e. fix the invariant)

fixQ :: [a] -> [a] -> Q a
fixQ [] back    = Q (reverse back) []
fixQ front back = Q front back

-- Test the operations by comparing to the slow queue
-- constraint that a has to be in artibrary aswell
instance Arbitrary a => Arbitrary (Q a) 
  where
    arbitrary = do
        front <- arbitrary
        back  <- arbitrary
        let back' = if null front then [] else back
        return (Q front back')

contents :: Q Int -> Slow.Q Int
contents (Q front back) = Slow.Q (front ++ reverse back)

prop_empty :: Bool
prop_empty     = contents empty == Slow.empty

prop_add a q   = contents (add a q) == Slow.add a (contents q)

prop_remove q  =
    not (isEmpty q) ==>
      contents (remove q) == Slow.remove (contents q)

prop_front q   =
    not (isEmpty q) ==>
      front q == Slow.front (contents q)

prop_isEmpty q = isEmpty q == Slow.isEmpty (contents q)

prop_invQ q = invariant q

-- Requirements:
--
--   * Generator should satisfy invariant
--   * Q operations should *preserve* invariant

prop_empty_inv    = invariant empty
prop_add_inv a q  = invariant (add a q)
prop_remove_inv q = not (isEmpty q) ==> invariant (remove q)

main = do
    quickCheck prop_invQ
    quickCheck prop_empty
    quickCheck prop_add
    quickCheck prop_remove
    quickCheck prop_front
    quickCheck prop_isEmpty
    quickCheck prop_empty_inv
    quickCheck prop_add_inv
    quickCheck prop_remove_inv


