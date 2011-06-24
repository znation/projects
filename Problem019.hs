module Problem019 where

answer :: Int
answer = length (filter sunday (filter firstOfMonth (dateRange (Date 1901 1 1) (Date 2000 12 31))))

data Date = Date {  year :: Int,
                    month :: Int,
                    day :: Int }
                    deriving (Eq, Show)
                    
sunday :: Date -> Bool
sunday d = dayOfWeek d == 0

dayOfWeek :: Date -> Int
dayOfWeek = dayOfWeek' (Date 1900 1 1) 1

dayOfWeek' :: Date -> Int -> Date -> Int
dayOfWeek' startingDate counter d = if      (startingDate == d)
                                    then    counter
                                    else    dayOfWeek' (nextDay startingDate) ((counter + 1) `mod` 7) d

firstOfMonth :: Date -> Bool
firstOfMonth d = (day d) == 1

dateRange :: Date -> Date -> [Date]
dateRange d1 d2 =   if      d1 == d2
                    then    [d1]
                    else    d1:(dateRange (nextDay d1) d2)
                    
nextDay :: Date -> Date
nextDay d = if      lastDayOfYear d
            then    Date ((year d) + 1) 1 1
            else    if      lastDayOfMonth d
                    then    Date (year d) (nextMonth (month d)) 1
                    else    Date (year d) (month d) ((day d) + 1)
                    
nextMonth :: Int -> Int
nextMonth 12 = 1
nextMonth x = x + 1

lastDayOfYear :: Date -> Bool
lastDayOfYear d = (month d) == 12 && (day d) == 31

lastDayOfMonth :: Date -> Bool
lastDayOfMonth date =   let m = (month date)
                            d = (day date)
                        in  if      (m == 2)
                            then    if      leapYear (year date)
                                    then    d == 29
                                    else    d == 28
                            else    if      (m == 9 || m == 4 || m == 6 || m == 11)
                                    then    d == 30
                                    else    d == 31

leapYear :: Int -> Bool
leapYear x =    if      x `mod` 100 == 0
                then    x `mod` 400 == 0
                else    x `mod` 4 == 0




