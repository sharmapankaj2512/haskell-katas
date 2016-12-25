---------------
-- Recursion
---------------

pow2 0 = 1
pow2 n = 2 * pow2 (n - 1)

repeatString str 0 = ""
repeatString str n = str ++ repeatString str (n - 1)

---------------
-- Lists
---------------

-- return True if the list is empty

empty [] = True
empty _ = False

-- double the value of each element in the list

double [] = []
double (x:xs) = 2 * x : double xs

-- remove odd numbers from the list of ints

removeOdds [] = []
removeOdds (x:xs)
    | mod x 2 == 0 = x : removeOdds xs
    | otherwise = removeOdds xs

---------------
-- Tuples
---------------

headAndLength [] = (Nothing, 0)
headAndLength xs = ((Just $ head xs), length xs)

---------------
-- Let binding
---------------

evenCount xs =
    let evens = removeOdds xs
    in length evens

---------------
-- Where binding
---------------

evenCount' xs = length evens
    where evens = removeOdds xs

---------------
-- Type Synonyms
---------------

type String = [Char]

type Point = (Double, Double)

type Line = (Point, Point)

isHorizontal :: Line -> Bool
isHorizontal ((_, y1), (_, y2)) = y1 == y2

---------------
-- newtype
---------------

newtype APoint = MakeAPoint (Double, Double) deriving (Eq, Show)

center = MakeAPoint (0, 0)

---------------
-- Records
---------------

-- Records in generally should be avoided
-- http://nikita-volkov.github.io/record/

data Person' = Person' {
    firstName' :: Main.String
  , lastName' :: Main.String
} deriving (Eq, Show)

abc = Person' "some" "some"

xyz = abc {lastName' = "none"}

---------------
-- Algebaric Data Types
---------------

type FirstName = Main.String
type LastName  = Main.String

data Person = Person FirstName LastName deriving (Eq, Show)

firstName :: Person -> FirstName
firstName (Person name _) = name

lastName :: Person -> LastName
lastName (Person _ name) = name

---------------
-- Algebaric Data Type Constructors
---------------

data Auth key secret = EmailPasswordAuth key secret
          | FacebookLegacyAuth key secret
          | GoogleLegacyAuth key secret
          | FacebookAuth key secret
          | GoogleAuth key secret


---------------
-- Patameterised Types
---------------

data Option a = Some a | None deriving (Eq, Show)

fromOption :: a -> Option a -> a
fromOption defaultValue None = defaultValue
fromOption _ (Some value) = value

---------------
-- Type class instances
---------------

