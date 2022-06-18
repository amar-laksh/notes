module Chapter2
  ( lastUsingNthElement,
    lastUsingHeadReverse,
    initUsingTake,
    initUsingTailReverse,
  )
where

lastUsingNthElement :: [el] -> el
lastUsingNthElement [] = error "only accepts non-empty list"
lastUsingNthElement el = el !! (length el - 1)

lastUsingHeadReverse :: [el] -> el
lastUsingHeadReverse [] = error "only accepts non-empty list"
lastUsingHeadReverse el = head (reverse el)

initUsingTake :: [el] -> [el]
initUsingTake [] = error "only accepts non-empty list"
initUsingTake el = take (length el - 1) el

initUsingTailReverse :: [el] -> [el]
initUsingTailReverse [] = error "only accepts non-empty list"
initUsingTailReverse el = reverse (tail (reverse el))
