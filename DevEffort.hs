module DevEffort where
import Text.Printf
import Data.Char
import Data.Maybe

data DevSystem = DS { capacity  :: Double,
                      fixing    :: Double,
                      improving :: Double,
                      checking  :: Double,
                      requests  :: Double,
                      features      :: Double,
                      checks    :: Double,
                      improvements :: Double }
    deriving (Eq, Show)

initial = DS { capacity   =  3.0,
               fixing     =  1.0,
               improving  =  1.0,
               checking   =  1.0,
               requests   =  1.0,
               features       =  0.0,
               checks     =  0.0,
               improvements= 0.0 }

coverage m | features m > 0.0 = checks m / features m 
           | otherwise    = 1.0

quality m | features m > 0.0 = improvements m / features m 
          | otherwise    = 1.0

efficiency m = 1.0 / (quality m * coverage m)

additional m = 2.0 - (quality m + coverage m)
 
capped m = (max 0) . (min m) 

add_requests p m = m { requests = (requests m) + p }

fix_requests t m = m { fixing    = v , 
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
          
evolve m = add_requests 1.0 m'    
    where m'= m { features   = new_features ,
                  checks = new_checks , 
                  improvements = new_improvements,
                  requests = new_requests }
          f = fixing m / efficiency m
          c = checking m / efficiency m
          i = improving m / efficiency m
          new_features = features m + f
          new_checks = capped new_features (checks m + c)
          new_improvements = capped new_features (improvements m + i)
          new_requests = (max 0 (requests m - f)) + additional m

pretty m = [printf "Features/Fixes done:%7.2f Coverage:%3d%% Quality:%3d%%" 
                (features m) ((round (coverage m * 100))::Integer) ((round (quality m * 100))::Integer)
           ,printf "Requests:%7.2f" (requests m)
           ,printf "Budget:  %7.2f" (capacity m)
           ,printf "  Improving Feature/Fixes:    %7.2f" (fixing m)
           ,printf "  Improving Coverage:         %7.2f" (checking m)
           ,printf "  Improving Design:           %7.2f" (checking m)
            ]

prompt = "F)eature <N>  C)overage <N>  D)esign <N>  R)un  Q)uit"  
 
data Command = Feature Double
             | Coverage Double
             | Design Double
             | Run
             | Quit
    deriving (Eq, Show)


parse s = case words (map toUpper s) of
    ["F",v] -> listToMaybe (map fst (reads v)) >>= return . Feature 
    ["C",v] -> listToMaybe (map fst (reads v)) >>= return . Coverage 
    ["D",v] -> listToMaybe (map fst (reads v)) >>= return . Design 
    ["R"]   -> Just Run
    ["Q"]   -> Just Quit
    _       -> Nothing
    
execute :: Command -> DevSystem -> DevSystem
execute (Feature f)  ds = fix_requests f ds
execute (Coverage f) ds = add_checks f ds
execute (Design f)   ds = improve_design f ds
execute Run ds          = evolve ds
