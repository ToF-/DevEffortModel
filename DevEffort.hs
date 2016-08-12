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


