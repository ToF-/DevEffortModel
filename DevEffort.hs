module DevEffort where

data DevSystem = DS { capacity  :: Double,
                      fixing    :: Double,
                      improving :: Double,
                      checking  :: Double,
                      coverage  :: Double,
                      quality   :: Double,
                      problems  :: Double,
                      additional:: Double,
                      required  :: Double }
    deriving (Eq, Show)

initial = DS { capacity   =  3.0,
               fixing     =  1.0,
               improving  =  1.0,
               checking   =  1.0,
               coverage   =  0.0,
               quality    =  0.0,
               problems   = 10.0,
               additional =  0.0,
               required   =  1.0 }

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
          
