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
                it "time spent on fixing problems" $ do
                    fixing m     `shouldBe_d` 1.0
                it "time spent on improving design" $ do
                    improving m  `shouldBe_d` 1.0
                it "time spent on adding checks" $ do
                    checking m   `shouldBe_d` 1.0
                it "relative quantity of checks" $ do
                    coverage m   `shouldBe_d` 0.0
                it "quality of design" $ do
                    quality m    `shouldBe_d` 0.0
                it "remaining problems" $ do
                    problems m   `shouldBe_d` 10.0
                it "additional problems" $ do
                    additional m `shouldBe_d` 0.0
                it "required time per problem" $ do
                    required m   `shouldBe_d` 1.0 
                it "quantity of code" $ do
                    code m       `shouldBe_d` 0.0 
                it "quantity of checks" $ do
                    checks m     `shouldBe_d` 0.0 
                it "quantity of improvements" $ do
                    improvements m `shouldBe_d` 0.0 


            describe "updating time spent on fixing problems" $ do
                it "should not allow for negative amount" $ do
                    let m = fix_problems (-2.5) initial
                    fixing m `shouldBe_d` 0.0
                it "should be capped by capacity" $ do
                    let m = fix_problems 4.0 initial
                    fixing m `shouldBe_d` 3.0
                it "should impact time spent improving design" $ do
                    let m = fix_problems 2.0 initial
                    improving m `shouldBe_d` 0.5
                it "should impact time spent adding checks" $ do
                    let m = fix_problems 2.0 initial
                    checking m `shouldBe_d` 0.5

            describe "updating time spent on adding checks" $ do
                it "should not allow for negative amount" $ do
                    let m = add_checks (-2) $ fix_problems 1.8 initial
                    checking m `shouldBe_d` 0.0
                it "should be capped by capacity minus time spent fixing problems" $ do
                    let m = add_checks 2 $ fix_problems 1.8 initial
                    checking m `shouldBe_d` 1.2
                it "should impact time spent improving design" $ do
                    let m = add_checks 1.0 $ fix_problems 1.8 initial
                    improving m `shouldBe_d` 0.2

            describe "updating time spent on improving design" $ do
                it "should not allow for negative amount" $ do
                    let m = improve_design (-1) $ add_checks 1 $ fix_problems 1.8 initial
                    improving m `shouldBe_d` 0.0
                it "should be capped by capacity minus time spent fixing problems and adding checks" $ do
                    let m = improve_design 1 $ add_checks 1 $ fix_problems 1.8 initial
                    improving m `shouldBe_d` 0.2
            describe "evolves" $ do
                describe "code" $ do
                    it "increases with time spent on fixing problems" $ do
                        let m = evolve $ fix_problems  1 initial
                        code m `shouldBe_d` 1.0
                describe "checks" $ do
                    it "increases with time spent on adding checks" $ do
                        let m = evolve $ add_checks 1 $ fix_problems  1 initial
                        checks m `shouldBe_d` 1.0
                describe "improvements" $ do
                    it "increases with time spent on improving design" $ do
                        let m = evolve $ improve_design 1 $ add_checks 1 $ fix_problems  1 initial
                        improvements m `shouldBe_d` 1.0
    
    putStr "time spent is capped by capacity :"   
    quickCheck $ \m -> (fixing m + checking m + improving m) <= capacity m 

    putStr "coverage is capped to 100pct :"        
    quickCheck $ \m -> checks m <= code m 

    putStr "coverage equals checks on code :"      
    quickCheck $ \m -> coverage m == 0.0 || coverage m == checks m / code m

    putStr "design quality is capped to 100pct :"        
    quickCheck $ \m -> improvements m <= code m 

    putStr "quality equals improvements on code :" 
    quickCheck $ \m -> quality m == improvements m / code m 


instance Arbitrary DevSystem where
    arbitrary = do 
                d <- fmap ((1+).(capped 3.0)) arbitrary
                c <- fmap ((1+).(capped 3.0)) arbitrary
                f <- fmap ((1+).(capped 3.0)) arbitrary
                return (evolve (improve_design d (add_checks c (fix_problems f initial)))) 

