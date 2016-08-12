import Test.Hspec
import DevEffort

main = hspec $ do
        describe "a dev system" $ do
            describe "should have the following variables" $ do
                let m = initial
                it "capacity (time budget) per cycle" $ do
                    capacity m   `shouldBe` 3.0
                it "time spent on fixing problems" $ do
                    fixing m     `shouldBe` 1.0
                it "time spent on improving design" $ do
                    improving m  `shouldBe` 1.0
                it "time spent on adding checks" $ do
                    checking m   `shouldBe` 1.0
                it "relative quantity of checks" $ do
                    coverage m   `shouldBe` 0.0
                it "quality of design" $ do
                    quality m    `shouldBe` 0.0
                it "remaining problems" $ do
                    problems m   `shouldBe` 10.0
                it "additional problems" $ do
                    additional m `shouldBe` 0.0
                it "required time per problem" $ do
                    required m `shouldBe` 1.0 
