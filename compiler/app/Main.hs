-- Issues:
-- 1. There are no comments in the language.
-- 2. Call -- need arbitrary length prefix to disambiguate.
-- 3. Right now everything is case-sensitive, is this intended?
-- 5. I should wrap IntToken in Constant and use that
-- 6. Why iele names that start with digits cannot have letters, but iele names
--    that start with -digit can?
-- 7. The sstore/sload syntax is broken, there is no size arg, need to change Theo's grammar.
-- 8. Lambda productions are risky, because they may generate infinite loops.
--    I should remove them.

module Main where

import qualified Data.ByteString as B
import Data.Char
import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import System.Environment
import System.Exit
import System.IO

import IeleParser (ieleParser)
import IeleAssembler (assemble)
import IeleTypes
import IelePrint
import IeleDesugar

import Codec.Binary.Base16(b16Enc)

stringToParse = "\n\
\@beneficiary = global 0\n\
\@auction.start = global 0\n\
\@auction.end = global 0\n\
\\n\
\@highest.bidder = global 0\n\
\@highest.bid = global 0\n\
\\n\
\@ended = global false\n\
\\n\
\define @init(%beneficiary, %bidding.time) {\n\
\entry:\n\
\    @beneficiary = %beneficiary\n\
\    @auction.start = call @iele.timestamp()\n\
\    @auction.end = add @auction_start, %bidding.time\n\
\}\n\
\\n\
\define @bid() {\n\
\entry:\n\
\    %1 = call @iele.timestamp()\n\
\    %2 = cmp ge %1, @auction.end\n\
\    br %2, label %exit\n\
\\n\
\bb.1:\n\
\    %bid.value = call @iele.callvalue()\n\
\    %3 = cmp le %bid.value, @highest.bid\n\
\    br %3, label %exit\n\
\\n\
\bb.2:\n\
\    %4 = cmp eq @highest.bid, 0\n\
\    br %4, label %bb.4\n\
\\n\
\bb.3:\n\
\    send @highest.bid to @highest.bidder\n\
\\n\
\bb.4:\n\
\    @highest.bidder = call @iele.caller()\n\
\    @highest.bid = %bid.value\n\
\\n\
\exit:\n\
\    ret void\n\
\}\n\
\\n\
\define @finish.auction() {\n\
\entry:\n\
\    %1 = call @iele.timestamp()\n\
\    %2 = cmp lt %1, @auction.end\n\
\    br %2, label %exit\n\
\\n\
\bb.1:\n\
\    br @ended, label %exit\n\
\\n\
\bb.2:\n\
\    @ended = true\n\
\    send @highest.bid to @beneficiary\n\
\    ret void\n\
\\n\
\exit:\n\
\    ret void\n\
\}\n\
\"

readFileArg :: FilePath -> IO String
readFileArg file =
  case file of
    "-" -> hGetContents stdin
    _ -> readFile file

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--parse",file] -> do
      contents <- readFileArg file
      case parse ieleParser file contents of
        Left err  -> do
          hPrint stderr err
          exitWith (ExitFailure 1)
        Right cs  -> do
          putStr (show (prettyContractsP cs))
    ["--desugar",file] -> do
      contents <- readFileArg file
      case parse ieleParser file contents of
        Left err  -> do
          hPrint stderr err
          exitWith (ExitFailure 1)
        Right cs  -> do
          putStr (show (prettyContractsD (map processContract cs)))
    [file] -> do
      contents <- readFileArg file
      case parse ieleParser file contents of
        Left err  -> do
          hPrint stderr err
          exitWith (ExitFailure 1)
        Right cs  -> do
          {-
          writeFile "test1.iele" (show (prettyContract c))
          let (defs,c') = processContract c
          mapM_ print defs
          writeFile "test2.iele" (show (prettyContract c'))
           -}
          B.putStr . b16Enc . assemble . compileContracts $ cs
    _ -> putStrLn "Usage: iele-assemble [--parse | --desugar] FILE"

--parse anyChar "" "a"
