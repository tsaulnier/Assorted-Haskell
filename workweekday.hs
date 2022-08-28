-- various calendar functions

data WorkWeekDay = WorkDay DayOfWeek Int Int | PlayDay DayOfWeek deriving (Eq)

playTime :: WorkWeekDay -> Int
playTime PlayDay _ = 1440
playTime WorkDay _ a b = 1440 - (b - a)

instance Show WorkWeekDay where
 show PlayDay day = day ++ " is a free day"
 show WorkDay day start end
      | start < 720 && end < 720 = dayAndStartChunk ++ "AM-" ++ endChunk ++ "AM"
      | start < 720 && end > 720
      | start > 720 && end > 720
      where
        dayAndStartChunk = day ++ " work " ++ (show(start `div` 60)) ++ ":" ++ (show(start `mod` 60))
        endChunk = (show(end `div` 60)) ++ ":" ++ (show(end `mod` 60))
