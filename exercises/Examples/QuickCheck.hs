module Examples.QuickCheck where

import Test.QuickCheck
import Data.Char
import Data.List

-- reverse
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = xs ++ [x]
-- rev (x:xs) = (rev xs) ++ [x]
-- 이렇게 되어야하는게 아닌가? 잘 모르겠다

-- QuickCheck에서 가져온 함수
-- (===) :: (Eq a, Show a) => a -> a -> Property

-- quickCheck : 임의로 테스트

-- 하나 테스트함. 근데 그러려고 quickCheck 쓰는 거 아님
propRevSmall :: Property
propRevSmall = rev [1,2] === [2,1]

-- *Examples.QuickCheck> quickCheck propRevSmall
-- +++ OK, passed 1 test.

-- 100개 테스트함
propRevTwice :: [Int] -> Property
propRevTwice xs = rev (rev xs) === xs

-- *Examples.QuickCheck> quickCheck propRevTwice
-- +++ OK, passed 100 tests.

-- verboseCheck : 무슨 값으로 테스트하는지 보여줌

-- *Examples.QuickCheck> verboseCheck propRevTwice
-- Passed:
-- []
-- [] == []

-- Passed:
-- [1]
-- [1] == [1]

-- Passed:
-- [-2,1,-1]
-- [-2,1,-1] == [-2,1,-1]
-- -- lots of output
-- +++ OK, passed 100 tests.

-- quickCheck에 값을 줄 수도 있음

-- *Examples.QuickCheck> quickCheck (propRevTwice [1,2,3])
-- +++ OK, passed 1 test.

-- ??
propRevMedium :: Property
propRevMedium = conjoin [rev [1,2,2] === [2,2,1],
                         rev [1,2,3] === [3,2,1]]

-- *Examples.QuickCheck> quickCheck propRevMedium
-- *** Failed! Falsified (after 1 test):
-- [2,3,1] /= [3,2,1]

propRevTwo :: [Int] -> [Int] -> Property
propRevTwo xs ys = rev (xs ++ ys) === rev ys ++ rev xs

-- *Examples.QuickCheck> quickCheck propRevTwo
-- *** Failed! Falsified (after 5 tests and 8 shrinks):
-- [0]
-- [0,1]
-- [0,1,0] /= [1,0,0]

propLast :: [Int] -> Property
propLast xs = last xs === head (reverse xs)

propLastFixed :: NonEmptyList Int -> Property
propLastFixed (NonEmpty xs) = last xs === head (reverse xs)

propCycle :: NonEmptyList Int -> NonNegative Int -> Property
propCycle (NonEmpty xs) (NonNegative n) =
  cycle xs !! n === xs !! (mod n (length xs))

propToUpperChanges :: Char -> Property
propToUpperChanges c = toUpper c =/= c

propToUpperChangesLetter :: Property
propToUpperChangesLetter = forAll (elements ['a'..'z']) propToUpperChanges

listHasZero :: [Int] -> Bool
listHasZero xs = elem 0 xs

-- *Examples.QuickCheck> quickCheck (listHasZero [1,0,2])
-- +++ OK, passed 1 test.
-- *Examples.QuickCheck> quickCheck listHasZero
-- *** Failed! Falsified (after 1 test):
-- []

propSort :: NonEmptyList Int -> Property
propSort (NonEmpty xs) =
  forAll (elements xs) (\x -> elem x (sort xs))

propRevTwo' :: [Int] -> [Int] -> Property
propRevTwo' xs ys =
  let input = xs ++ ys
  in counterexample ("Input: " ++ show input) $
     rev input === rev ys ++ rev xs

someLetters :: Gen String
someLetters = do
  c <- elements "xyzw"
  n <- choose (1,10)
  return (replicate n c)

data Switch = On | Off
  deriving (Show, Eq)

toggle :: Switch -> Switch
toggle On = Off
toggle Off = On

propToggleTwice :: Switch -> Property
propToggleTwice s = s === toggle (toggle s)

instance Arbitrary Switch where
  arbitrary = elements [On,Off]
