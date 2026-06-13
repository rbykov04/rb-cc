module Error where
import Data.List
import AST

import Text.Pretty.Simple

import Data.Text.Lazy (unpack)
import Data.IntMap.Lazy (IntMap, (!))
import qualified Data.IntMap.Lazy as IntMap

data Error = ErrorCode Int
           | ErrorText String
           | ErrorType Token String Obj
           | ErrorLoc Int String
           | ErrorToken Token String
           | ErrorLex Int String
           | ErrorScope (Node Parsed) String
           deriving Show

--                                   begin  end
--                                    |     |
getLines :: String -> Int -> Int -> [(Int, Int)]
getLines []     begin cur   = []
getLines ('\n':ss) begin cur   =  (begin, cur) : getLines ss (cur+1) (cur+1)
getLines (s:ss)    begin cur   = getLines ss begin (cur+1)


mkTokError tok err = Left $ ErrorToken tok err
mkTextError err = Left $ ErrorText err



errorAt :: String -> Int -> String -> [String]
errorAt current_input loc text = [current_input] ++ [replicate loc ' ' ++ "^ " ++ text]

errorTok :: String -> Token -> String -> [String]
errorTok input tok text = errorAt input (tokenLoc tok) $ text ++ "\n"  ++ show tok


printError :: String -> Error -> [String]
printError input (ErrorType t text obj) =
        let
          lines = getLines input 0 0
          iLines = zip[0..] lines
          line = ignoreUnknownLine (find (finder (tokenLoc t)) iLines)
          a = "stdin:" ++ show (fst line) ++ ": " ++ text ++ " " ++ (show (tokenKind t))
          b = errorTok input t text
          dump = unpack (pShow obj)
        in [a] ++ b ++ [dump]
          where
            finder :: Int -> ((Int, (Int, Int)) -> Bool)
            finder x (_, (_, e)) = x < e

            -- FIXME: unreachiable - think about it
            ignoreUnknownLine (Just x) = x
            ignoreUnknownLine Nothing = (0,(0, 0))
printError input (ErrorToken t text) =
        let
          lines = getLines input 0 0
          iLines = zip[0..] lines
          line = ignoreUnknownLine (find (finder (tokenLoc t)) iLines)
          a = "stdin:" ++ show (fst line) ++ ": " ++ text ++ " " ++ (show (tokenKind t))
          b = errorTok input t text
        in [a] ++ b
          where
            finder :: Int -> ((Int, (Int, Int)) -> Bool)
            finder x (_, (_, e)) = x < e

            -- FIXME: unreachiable - think about it
            ignoreUnknownLine (Just x) = x
            ignoreUnknownLine Nothing = (0,(0, 0))
printError input (ErrorLoc loc text) = errorAt input loc text
printError input (ErrorText text) = [text]
printError input (ErrorLex loc text) = errorAt input loc text
