module DevEffort where

data DevSystem = DS { capacity  :: Double,
                      fixing    :: Double,
                      improving :: Double,
                      checking  :: Double,
                      problems  :: Double,
                      additional:: Double,
                      required  :: Double,
                      code      :: Double,
                      checks    :: Double,
                      improvements :: Double }
    deriving (Eq, Show)

initial = DS { capacity   =  3.0,
               fixing     =  1.0,
               improving  =  1.0,
               checking   =  1.0,
               problems   = 10.0,
               additional =  0.0,
               required   =  1.0,
               code       =  0.0,
               checks     =  0.0,
               improvements= 0.0 }

coverage m | code m > 0.0 = checks m / code m 
           | otherwise    = 0.0

quality m | code m > 0.0 = improvements m / code m 
          | otherwise    = 0.0

capped m = (max 0) . (min m) 

fix_problems t m = m { fixing    = v , 
                       improving = (r - v) / 2.0,
                       checking  = (r - v) / 2.0 }
    where r = capacity m
          v = capped r t

add_checks t m = m { checking  = v,
                     improving = (r - v) }
    where r = (capacity m - fixing m) 
          v = capped r t 
          
improve_design t m = m { improving = v }
    where r = (capacity m - fixing m - checking m)
          v = capped r t
          
evolve m = m { code   = (code m) + f,
               checks = (checks m) + c,
               improvements = (improvements m) + i }
    where f = fixing m
          c = checking m
          i = improving m 
