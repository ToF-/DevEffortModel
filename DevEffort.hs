module DevEffort where
import Text.Printf
import Data.Char
import Data.Maybe

data DevSystem = DS { capacity  :: Double,
                      fixing    :: Double,
                      improving :: Double,
                      checking  :: Double,
                      problems  :: Double,
                      code      :: Double,
                      checks    :: Double,
                      improvements :: Double }
    deriving (Eq, Show)

initial = DS { capacity   =  3.0,
               fixing     =  1.0,
               improving  =  1.0,
               checking   =  1.0,
               problems   =  1.0,
               code       =  0.0,
               checks     =  0.0,
               improvements= 0.0 }

coverage m | code m > 0.0 = checks m / code m 
           | otherwise    = 1.0

quality m | code m > 0.0 = improvements m / code m 
          | otherwise    = 1.0

required m = 1.0 / (quality m * coverage m)

additional m = 2.0 - (quality m + coverage m)
 
capped m = (max 0) . (min m) 

add_problems p m = m { problems = (problems m) + p }

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
          
evolve m = m { code   = new_code ,
               checks = new_checks , 
               improvements = new_improvements,
               problems = new_problems }
    where f = fixing m / required m
          c = checking m
          i = improving m 
          new_code = code m + f
          new_checks = capped new_code (checks m + c)
          new_improvements = capped new_code (improvements m + i)
          new_problems = (max 0 (problems m - f)) + additional m

pretty m = [printf "Features/Fixes done:%7.2f Coverage:%3d%% Quality:%3d%%" 
                (code m) ((round (coverage m * 100))::Integer) ((round (quality m * 100))::Integer)
           ,printf "Requests:%7.2f" (problems m)
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
    
