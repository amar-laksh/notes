module Chapter2 (
    lastUsingNthElement,
    lastUsingHeadReverse,
    initUsingTake,
    initUsingTailReverse,
    errorMsg,
)
where

errorMsg = "only accepts non-empty list"

lastUsingNthElement :: [el] -> el
lastUsingNthElement [] = error errorMsg
lastUsingNthElement el = el !! (length el - 1)

lastUsingHeadReverse :: [el] -> el
lastUsingHeadReverse [] = error errorMsg
lastUsingHeadReverse el = head (reverse el)

initUsingTake :: [el] -> [el]
initUsingTake [] = error errorMsg
initUsingTake el = take (length el - 1) el

initUsingTailReverse :: [el] -> [el]
initUsingTailReverse [] = error errorMsg
initUsingTailReverse el = reverse (tail (reverse el))
