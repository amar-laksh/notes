module Chapter10
  ( putStr',
  )
where

putStr' :: String -> IO ()
putStr' str = sequence_ [putChar chr | chr <- str]
