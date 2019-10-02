import Data.Time

data DatabaseItem = DbString String
 | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDataBase :: [DatabaseItem]
theDataBase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbString "Hello, world!",
    DbDate(UTCTime (fromGregorian 1909 5 1) (secondsToDiffTime 34123)),
    DbDate(UTCTime (fromGregorian 1950 5 1) (secondsToDiffTime 34123)),
    DbNumber 90213
  ]

checkForDbDate :: DatabaseItem -> Bool
checkForDbDate (DbDate utc) = True
checkForDbDate (DbString _) = False
checkForDbDate (DbNumber _) = False

getUtc :: DatabaseItem -> UTCTime
getUtc (DbDate utc) = utc

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate ds = map getUtc $ filter checkForDbDate ds

filterDbDateAlt :: [DatabaseItem] -> [UTCTime]
filterDbDateAlt = foldr getTimes [] where
  getTimes (DbDate date) dates = date: dates
  getTimes _ dates = dates

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr getNumber [] where
  getNumber (DbNumber num) nums = num : nums
  getNumber _ nums = nums

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr compareTime zeroTime where
  zeroTime = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)
  compareTime (DbDate date) date' = max date date'
  compareTime _ date = date

mostRecentAlt :: [DatabaseItem] -> UTCTime
mostRecentAlt = maximum . filterDbDateAlt

sumBd :: [DatabaseItem] -> Integer
sumBd = foldr (+) 0 . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb ds = (fromIntegral . sumBd $ ds) / ((fromIntegral . length . filterDbNumber) ds)
