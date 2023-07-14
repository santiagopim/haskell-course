import Data.List
import System.CPUTime (getCPUTime)
import System.Directory (doesFileExist, listDirectory)
import Text.XHtml (thead)

{-
We imported some functions that you'll need to complete the homework.
FilePath is just a synonym for String. Although, make sure to follow
the standard path representation when using them
(https://en.wikipedia.org/wiki/Path_(computing).

getCPUTime    :: IO Integer
doesFileExist :: FilePath -> IO Bool
listDirectory :: FilePath -> IO [FilePath]

You can hover over the functions to know what they do.
-}

{-
-- Question 1 -- Define an IO action that counts the number of files
in the current directory and prints it to the terminal inside a string
message.
-}

-- getCPUTime    :: IO Integer
-- doesFileExist :: FilePath -> IO Bool
-- listDirectory :: FilePath -> IO [FilePath]

listFiles :: IO ()
listFiles = listDirectory "./"
            >>= (\filePaths ->
                   putStrLn $ "Number of files: " ++ show (length filePaths))
--listFiles = listDirectory "./" >>= (\filePaths -> print $ length filePaths)
--listFiles = listDirectory "./" >>= (\filePaths -> putStrLn $ show filePaths)

{-
-- Question 2 -- Write an IO action that asks the user to type
something, then writes the message to a file called msg.txt, and after
that, it reads the text from the msg.txt file and prints it back. Use
the writeFile and readFile functions.
-}

createMsg :: IO ()
createMsg = do
  putStrLn "Type something:"
  something <- getLine
  print something
  writeFile "msg.txt" something
  putStrLn "... file written"
  resomething <- readFile "msg.txt"
  print resomething

{-
-- Context for Questions 3 and 4 -- In cryptography, prime numbers
(positive integers only divisible by themselves and 1) play a
fundamental role in providing unbreakable mathematical structures.
These structures, in turn, are leveraged to establish secure
encryption. But, generating primes is a computational straining
problem, as we will measure in the following exercise. This is
because, to know whether a number is a prime number, you first need to
know all the previous primes and then check that they are not a
divisor of this number. So, this problem gets bigger and bigger! Our
lead cryptographer provided us with 3 different algorithms (primes1,
primes2, and primes3). All three correctly produce a list of all the
prime numbers until a limit (that we provide as a parameter). Our job
is not to understand these algorithms but to measure which is the
fastest and print the largest prime number below our limit. Do it step
by step, starting with question 3.
-}

primes1 :: Integer -> [Integer]
primes1 m = sieve [2 .. m]
 where
  sieve [] = []
  sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

primes2 :: Integer -> [Integer]
primes2 m = sieve [2 .. m]
 where
  sieve (x : xs) = x : sieve (xs \\ [x, x + x .. m])
  sieve [] = []

primes3 :: Integer -> [Integer]
primes3 m = turner [2 .. m]
 where
  turner [] = []
  turner (p : xs) = p : turner [x | x <- xs, x < p * p || rem x p /= 0]

{-
-- Question 3 -- Define an IO action that takes an IO action as input
and calculates the time it takes to execute. Use the getCPUTime :: IO
Integer function to get the CPU time before and after the IO action.
The CPU time here is given in picoseconds (which is 1/1000000000000th
of a second).
-}

timeIO :: IO a -> IO ()
timeIO action = do
  init <- getCPUTime
  --print init
  action
  fint <- getCPUTime
  --print fint
  print $ divMod (fint - init) 10_000_000_000
  


{-
-- Question 4 -- Write an action that retrieves a value from the
standard input, parses it as an integer, and compares the time all
three algorithms take to produce the largest prime before the limit.
Print the number and time to the standard output.
-}

benchmark :: IO ()
benchmark = do
  putStrLn "Give me some number:"
  value <- getLine
  let limit = (read value :: Integer)
  --print limit
  timeIO (print (primes1 limit))
  timeIO (print (primes2 limit))
  timeIO (print (primes3 limit))


{-
 -- Question 5 -- EXTRA CREDITS -- (In case the previous ones were too
easy) Write a program that prints the directory tree structure from
the current folder. Below you can see an example output of how such a
structure looks like:

.
├── foo1.hs
├── foo2.hs
├── bar1
    ├── bar2
    ├── foo3.hs
    ├── foo4.hs
    └── bar3
        └── foo5.hs
└── bar5
    ├── bar6
    ├── foo6.hs
    └── foo7.hs

HINT: You can use the function doesFileExist, which takes in a
FilePath and returns True if the argument file exists and is not a
directory, and False otherwise.
-}

{-
The printDirectory function gets the path to be printed, prints a dot
".", and calls getFiles with the path as parameter.

The getFiles function gets the path to be printed as parameter,
creates a list with all files from that path, and passes that list to
the printFiles function to print them.

The printFiles function gets a path and a list of files from that path
as parameters, checks recursively if every file in the list is a file
or a directory, in case it is a file just print it, in case it is a
directory then calls getFiles again with the new sub-path.

Interenstingly the path parameter in getFiles and printFiles functions
is a list of the directories that traverses to that path, i.e, the
"./one/foo/bar" path is listed as ["one", "foo", "bar"], so the level
to indent every line is acquired just with the length of the list.

The getPath and printLine functions are auxiliary.
-}
printDirectory :: String -> IO()
printDirectory p = do
  putStrLn "."
  getFiles [p]

getFiles :: [String] -> IO ()
getFiles p = listDirectory (getPath p) >>= (\files -> printFiles p (sort files))

getPath :: [String] -> String
getPath []     = ""
getPath (p:ps) = p ++ "/" ++ getPath ps

printLine :: Bool -> Int -> String -> IO()
printLine e l f = do
  putStr $ replicate ((l - 1) * 4) ' '
  if e
    then putStrLn ("└── " ++ f)
    else putStrLn ("├── " ++ f)

printFiles :: [String] -> [String] -> IO ()
printFiles _    []     = putStr "" -- return ()
printFiles path (f:fs) = do
  let p = getPath path ++ "/" ++ f
  let e = length fs == 0
  let l = length path
  doesFileExist p >>= (\exists ->
                          if exists
                          then printLine e l f
                          else do
                            printLine e l f
                            getFiles (path ++ [f]))
  printFiles path fs

