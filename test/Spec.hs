module Main where

import Atividade04
import Test.Tasty
import Test.Tasty.HUnit
import Atividade04
import Test.QuickCheck
import Atividade04 (redl)

-- Usei Chat GPT

prop_redl_equivalent_to_foldl :: [Int] -> Bool
prop_redl_equivalent_to_foldl xs =
  let f = (+) 
      initAcc = 0
  in redl f initAcc (arratToList xs) == foldl f initAcc xs

instance Arbitrary Sem where
    arbitrary  = elements [Green, Yellow, Red]

semaforo :: Gen Sem
semaforo = arbitrary
semaforos :: Gen [Sem]
semaforos = listOf semaforo

prop_timelist_redlvs :: [Sem] -> Bool
prop_timelist_redlvs xs = timeList (arratToList xs) == timeListRedl (arratToList xs)


main :: IO ()
main = do
  print "QuickCheck redl"
  quickCheck prop_redl_equivalent_to_foldl
  print "QuickCheck timelist"
  quickCheck prop_timelist_redlvs
  print (show(timeList (arratToList([Yellow, Green, Green]))) ++ " | " ++ show (timeListRedl (arratToList([Yellow, Green, Green]))))
  print "Regular Test"
  defaultMain tests
  
  

tests :: TestTree
tests = (testGroup "Testes da Atividade 4" [nextTest, timeListTest, sumRedlTest, timeListRedlTest, timeBTTest, bestBTTest, timeTTest, bestTTest])



nextTest = testGroup "next Test" 
            [testCase "test1" (assertEqual "Test 1" Yellow (next Green)),
             testCase "test2" (assertEqual "Test 2" Red (next Yellow)),
             testCase "test3" (assertEqual "Test 3" Green (next Red))
             ]
             
timeListTest = testGroup "timeList Test" 
            [testCase "test1" (assertEqual "Test 1" 5 (timeList (arratToList [Green, Yellow, Red]))),
             testCase "test2" (assertEqual "Test 2" 7 (timeList (arratToList [Green, Yellow, Red, Green]))),
             testCase "test3" (assertEqual "Test 3" 10 (timeList (arratToList [Green, Yellow, Red, Green, Green]))),
             testCase "test4" (assertEqual "Test 4" 3 (timeList (arratToList [Green, Yellow]))),
             testCase "test5" (assertEqual "Test 5" 5 (timeList (arratToList [Yellow, Red])))
             ]

sumRedlTest = testGroup "sunRedl Test" 
            [testCase "test1" (assertEqual "Test 1" 5 (soma (2 :- 3 :- Nil)))
             ]

timeListRedlTest = testGroup "timeListredl Test" 
            [testCase "test1" (assertEqual "Test 2" 7 (timeListRedl (arratToList [Green, Yellow, Red, Green]))),
             testCase "test2" (assertEqual "Test 3" 10 (timeListRedl (arratToList [Green, Yellow, Red, Green, Green]))),
             testCase "test3" (assertEqual "Test 4" 3 (timeListRedl (arratToList [Green, Yellow]))),
             testCase "test4" (assertEqual "Test 5" 5 (timeListRedl (arratToList [Yellow, Red])))
             ]



timeBTTest = testGroup "timeBT Test" 
            [testCase "test1" (assertEqual ("Test 1 "++ show(bt) ++ " || " ++ show(caminhosArvore bt)) 4 (timeBT bt))
             ]
bestBTTest = testGroup "bestBT Test" 
            [testCase "test1" (assertEqual ("Test 1 "++ show(bt) ++ " || " ++ show(caminhosArvore bt)) [Green, Yellow, Green] (bestBT bt))
             ]

timeTTest = testGroup "timeT Test" 
            [testCase "test1" (assertEqual ("Test 1 "++ show(tree) ++ " || " ++ show(caminhosArvoreT tree)) 5 (timeT tree))
             ]
bestTTest = testGroup "bestT Test" 
            [testCase "test1" (assertEqual ("Test 1 "++ show(tree) ++ " || " ++ show(caminhosArvoreT tree)) [Green, Green, Red] (bestT tree))
             ]