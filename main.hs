{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
import Text.XML.HXT.Core
import System.Random
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans

import RandomShuffle

data Guest = Guest { firstName, lastName :: String }
  deriving (Show, Eq)

type Answer = String
data Question = Question { description :: String, answers :: [Answer] }
                    deriving (Show, Eq)
type Test = [Question]
type AnswerSet = [(Bool, Answer)]

-- State monad helper
hoistState :: Monad m => State s a -> StateT s m a
hoistState = StateT . (return .) . runState

-- XHT helpers
atTag tag = deep (isElem >>> hasName tag)
text = getChildren >>> getText

-- Code
getQuestion = atTag "question" >>>
    proc q -> do
        description <- text <<< atTag "description" -< q
        answer1 <- text <<< atTag "answer1" -< q
        answer2 <- text <<< atTag "answer2" -< q
        answer3 <- text <<< atTag "answer3" -< q
        answer4 <- text <<< atTag "answer4" -< q
        returnA -< Question { description = description, 
                              answers = [answer1,answer2,answer3,answer4] }

loadFile = runX (readDocument [withValidate no] "sip-simple.xml" >>> getQuestion)

pickRandomQuestion :: Test -> State StdGen Question
pickRandomQuestion test = do
    gen <- get
    let (num, gen2) = randomR (0, length test - 1) gen
    put gen2
    return (test !! num)
    
randomizeAnswers :: Question -> State StdGen AnswerSet
randomizeAnswers q = do
    gen <- get
    let answList = zip [True,False,False,False] $ answers q
    let (answSet, gen2) = randomShuffle gen answList
    put gen2
    return answSet

printAnswerSet :: AnswerSet -> IO ()
printAnswerSet answSet = do
    sequence_ $
        map (\(num, (correctness, text)) ->
            print $ (show num) ++ ". " ++ text)
        numberedAnswSet
    where numberedAnswSet = zip [1..] answSet

testLoop :: Test -> StateT StdGen IO ()
testLoop test = do
    -- pick a random question
    q <- hoistState $ pickRandomQuestion test
    -- shuffle the answers
    answerSet <- hoistState $ randomizeAnswers q
    -- print the message to the user
    liftIO $ do
        print $ description q
        printAnswerSet answerSet 
        -- read the choice
        a <- getLine 
        let choice = read a :: Int
   
        -- TODO: error checking
        if fst $ answerSet !! (choice - 1)
            then print "OK!"
            else print "Not OK!"

    testLoop test

main = do
    test <- loadFile
    runStateT (testLoop test) (mkStdGen 10)
