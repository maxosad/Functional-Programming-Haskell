module HW1.T1
 ( Day(..)
 , nextDay
 , afterDays
 , isWeekend
 , daysToParty
 ) where
 
import Numeric.Natural (Natural)

data Day  
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving Show
  
nextDay :: Day -> Day
nextDay Monday = Tuesday
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Friday
nextDay Friday = Saturday
nextDay Saturday = Sunday
nextDay Sunday = Monday

afterDays :: Natural -> Day -> Day
afterDays 0 day = day
afterDays x day = afterDays (x - 1) (nextDay day)

isWeekend :: Day -> Bool
isWeekend Sunday = True
isWeekend Saturday = True
isWeekend _ = False

daysToParty :: Day -> Natural
daysToParty Monday = 4
daysToParty Tuesday = 3
daysToParty Wednesday = 2
daysToParty Thursday = 1
daysToParty Friday = 0
daysToParty Saturday = 6
daysToParty Sunday = 5
