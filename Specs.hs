import Test.Hspec
import Test.QuickCheck
import DevEffort

shouldBe_d a e = (rounded a) `shouldBe` (rounded e)

rounded = (/ 10000.0) . fromInteger . round . (* 10000.0)

main = do 
    hspec $ do
        describe "a dev system" $ do
            describe "should have the following variables" $ do
                let m = initial
                it "capacity (time budget) per cycle" $ do
                    capacity m   `shouldBe_d` 3.0
                it "time spent on fixing requests" $ do
                    fixing m     `shouldBe_d` 1.0
                it "time spent on improving design" $ do
                    improving m  `shouldBe_d` 1.0
                it "time spent on adding checks" $ do
                    checking m   `shouldBe_d` 1.0
                it "relative quantity of checks" $ do
                    coverage m   `shouldBe_d` 1.0
                it "quality of design" $ do
                    quality m    `shouldBe_d` 1.0
                it "remaining requests" $ do
                    requests m   `shouldBe_d` 1.0
                it "quantity of features" $ do
                    features m       `shouldBe_d` 0.0 
                it "quantity of checks" $ do
                    checks m     `shouldBe_d` 0.0 
                it "quantity of improvements" $ do
                    improvements m `shouldBe_d` 0.0 


            describe "updating time spent on fixing requests" $ do
                it "should not allow for negative amount" $ do
                    let m = fix_requests (-2.5) initial
                    fixing m `shouldBe_d` 0.0
                it "should be capped by capacity" $ do
                    let m = fix_requests 4.0 initial
                    fixing m `shouldBe_d` 3.0
                it "should impact time spent improving design" $ do
                    let m = fix_requests 2.0 initial
                    improving m `shouldBe_d` 0.5
                it "should impact time spent adding checks" $ do
                    let m = fix_requests 2.0 initial
                    checking m `shouldBe_d` 0.5

            describe "updating time spent on adding checks" $ do
                it "should not allow for negative amount" $ do
                    let m = add_checks (-2) $ fix_requests 1.8 initial
                    checking m `shouldBe_d` 0.0
                it "should be capped by capacity minus time spent fixing requests" $ do
                    let m = add_checks 2 $ fix_requests 1.8 initial
                    checking m `shouldBe_d` 1.2
                it "should impact time spent improving design" $ do
                    let m = add_checks 1.0 $ fix_requests 1.8 initial
                    improving m `shouldBe_d` 0.2

            describe "updating time spent on improving design" $ do
                it "should not allow for negative amount" $ do
                    let m = improve_design (-1) $ add_checks 1 $ fix_requests 1.8 initial
                    improving m `shouldBe_d` 0.0
                it "should be capped by capacity minus time spent fixing requests and adding checks" $ do
                    let m = improve_design 1 $ add_checks 1 $ fix_requests 1.8 initial
                    improving m `shouldBe_d` 0.2

            describe "adding requests" $ do
                it "increases the number of requests" $ do
                    let m = add_requests 1.0 initial
                    requests m `shouldBe_d` 2.0
                    
            describe "evolves" $ do
                describe "implicitely" $ do 
                    it "brings new requests" $ do
                        let m = evolve initial
                        requests m `shouldBe_d` 1.0 
                describe "features" $ do
                    it "increases with time spent on fixing requests" $ do
                        let m = evolve $ fix_requests  1 initial
                        features m `shouldBe_d` 1.0
                describe "checks" $ do
                    it "increases with time spent on adding checks" $ do
                        let m = evolve $ add_checks 1 $ fix_requests  1 initial
                        checks m `shouldBe_d` 1.0
                describe "improvements" $ do
                    it "increases with time spent on improving design" $ do
                        let m = evolve $ improve_design 1 $ add_checks 1 $ fix_requests  1 initial
                        improvements m `shouldBe_d` 1.0
            describe "pretty print" $ do
                it "shows the current values" $ do
                    let m = initial
                    (pretty m) `shouldBe` 
                        ["Features/Fixes done:   0.00 Coverage:100% Quality:100%"
                        ,"Requests:   1.00"
                        ,"Budget:     3.00"
                        ,"  Improving Feature/Fixes:       1.00"
                        ,"  Improving Coverage:            1.00"
                        ,"  Improving Design:              1.00"
                        ]
            describe "prompt" $ do
                it "shows available commands" $ do
                    prompt `shouldBe` "F)eature <N>  C)overage <N>  D)esign <N>  R)un  Q)uit"  

            describe "parse" $ do
                it "parses a command" $ do
                    parse "f  1.4" `shouldBe` (Just $ Feature 1.4)
                    parse "F  2.4" `shouldBe` (Just $ Feature 2.4)
                    parse "f  foo" `shouldBe` Nothing
                    parse "c  1.0" `shouldBe` (Just $ Coverage 1.0)  
                    parse "d  1.0" `shouldBe` (Just $ Design 1.0)  
                    parse "r" `shouldBe` (Just Run)
                    parse "q" `shouldBe` (Just Quit)
                    parse "foo" `shouldBe` Nothing

            describe "execute" $ do
                it "executes a command" $ do
                    let m = execute (Feature 0.4) initial 
                    fixing m `shouldBe_d` 0.4
                    let m = execute (Coverage 0.4) initial 
                    checking m `shouldBe_d` 0.4
                    let m = execute (Design 0.4) initial 
                    improving m `shouldBe_d` 0.4
                    let m = execute Run (execute (Feature 1.0 ) initial) 
                    features m `shouldBe_d` 1.0

    putStr "\ntime spent is capped by capacity :\n\t"   
    quickCheck $ \m -> (fixing m + checking m + improving m) <= capacity m 

    putStr "\ncoverage is capped to 100pct :\n\t"        
    quickCheck $ \m -> checks m <= features m 

    putStr "\ncoverage equals checks on features :\n\t"      
    quickCheck $ \m -> coverage m == 0.0 || coverage m == checks m / features m

    putStr "\ndesign quality is capped to 100pct :\n\t"        
    quickCheck $ \m -> improvements m <= features m 

    putStr "\nquality equals improvements over features :\n\t" 
    quickCheck $ \m -> quality m == improvements m / features m 

    putStr "\nefficiency equals 1 over quality x coverage :\n\t" 
    quickCheck $ \m -> efficiency m == 1.0 / (quality m * coverage m)

    putStr "\nadditional requests equals 2 - quality - coverage :\n\t"
    quickCheck $ \m -> rounded (additional m) == rounded (2.0 - quality m - coverage m)
     
    putStr "\nnumber of requests to solve is positive :\n\t" 
    quickCheck $ \m -> requests m >= 0.0 

    putStr "\nnumber of requests reduce with time spent fixing and grows with additional requests\n\t"
    quickCheck $ \m -> let m' = evolve m
        in rounded (requests m') == rounded (max 0 (requests m - (fixing m / efficiency m)) + additional m)
 
instance Arbitrary DevSystem where
    arbitrary = do 
                d <- fmap ((1+).(capped 3.0)) arbitrary
                c <- fmap ((1+).(capped 3.0)) arbitrary
                f <- fmap ((1+).(capped 3.0)) arbitrary
                return (evolve (improve_design d (add_checks c (fix_requests f initial)))) 

