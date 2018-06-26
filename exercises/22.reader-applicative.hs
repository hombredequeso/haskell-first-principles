module ReaderApplicative where

-- Basic Types:
newtype HumanName =
    HumanName String
    deriving (Eq, Show)

newtype DogName =
    DogName String
    deriving (Eq, Show)

newtype Address =
    Address String
    deriving (Eq, Show)

-- Data Types:

data Person =
    Person {
            humanName :: HumanName
          , dogName :: DogName
          , address :: Address
           } deriving (Eq, Show)

data Dog = 
    Dog {
        dogsName :: DogName, 
        dogsAddress :: Address
        } deriving (Eq, Show)

-- Data to play with:

pers :: Person
pers =
    Person (HumanName "Big Bird")
    (DogName "Barkley")
    (Address "Sesame Street")

chris :: Person
chris = 
    Person (HumanName "Chris Allen")
    (DogName "Papu")
    (Address "Austin")


-- Let the games begin...

-- without Reader
getDog :: Person -> Dog
getDog p =
    Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR =
    (Dog <$> dogName <*> address)

-- with Reader, non-pointfree
getDogR' :: Person -> Dog
getDogR' person=
    (Dog <$> dogName <*> address) person

-- :t dogName
-- dogName :: Person -> DogName

-- :t (Dog <$> dogName)
-- (Dog <$> dogName) :: Person -> Address -> Dog


-- Dog <$> dogName
-- => fmap Dog dogName
-- map the function Dog (DogName -> Address -> Dog)
--  over the DogName which is wrapped in a function (Person -> DogName)
--  i.e. the reader is a function context which provides a Person
--
    --what is the f of the functor/applicative?
        -- it is a function taking Person (i.e. 
        -- Person ->, 
        --    in other words:
        -- (->) Person )
-- 
-- (Dog <$> dogName <*> address)
--  given :t (Dog <$> dogName) :: Person -> Address -> Dog
--  (Oog <$> dogName) <*> address
-- :t address :: Person -> Address
-- is:  (Person -> Address -> Dog) <*>  (Person -> Address)
-- (i.e. applicative apply a function across a function)
-- If I've got a function that requires a Person and an Address,
-- and another function that gives an Address from a Person,
-- then putting them together the Address can be got rid of
-- because it can be determined from the Person.
-- So I end up with Person -> Dog
--
