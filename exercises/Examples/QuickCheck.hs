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


-- quickCheck의 verbose가 부족하면 추가 출력문을 넣을 수 있다
-- counterexample :: Testable prop => String -> prop -> Property

propRevTwo' :: [Int] -> [Int] -> Property
propRevTwo' xs ys =
  let input = xs ++ ys
  in counterexample ("Input: " ++ show input) $
     rev input === rev ys ++ rev xs

-- *Examples.QuickCheck> quickCheck propRevTwo'
-- *** Failed! Falsified (after 4 tests and 5 shrinks):
-- [0]
-- [0,1]
-- Input: [0,0,1]
-- [0,1,0] /= [1,0,0]



-- Gen 은 Monad 다. 진짜 지긋지긋하네
-- sample 을 쓰면 어떤 임의의 값을 입력하는지 볼 수 있다

someLetters :: Gen String
someLetters = do
  c <- elements "xyzw"
  n <- choose (1,10)
  return (replicate n c)

-- *Examples.QuickCheck> sample someLetters
-- "yyyyyyyy"
-- "zzzzzzzzz"
-- "xxxxxxxxx"
-- "yyyyyyy"
-- "yyy"
-- "ww"
-- "xxxxxx"
-- "yyy"
-- "yyyyyyy"
-- "xxxxxxxxxx"
-- "y"

-- Gen 즉 generator 는 Arbitrary 라는 타입 클래스와 연관있다
-- Arbitrary는 QuickCheck이 입력을 자동으로 생성하는지를 보여준다

-- class Arbitrary a where
--   arbitrary :: Gen a
--   shrink :: a -> [a]

-- 내가 만든 데이터 타입으로 테스트하고 싶다면
-- forAll을 쓰거나
-- 그 타입에서 Arbitrary instance를 구현해야함

data Switch = On | Off
  deriving (Show, Eq)

toggle :: Switch -> Switch
toggle On = Off
toggle Off = On

propToggleTwice :: Switch -> Property
propToggleTwice s = s === toggle (toggle s)

-- 여기까지만 하면 에러

instance Arbitrary Switch where
  arbitrary = elements [On,Off]
