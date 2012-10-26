import           System.IO

import qualified Text.JSON as JSON

import           Dzen (Dzen)
import qualified Dzen as Dzen
import           I3 (I3)
import qualified I3 as I3

convert' :: [Dzen] -> (I3 -> I3) -> [I3]
convert' ((Dzen.String s) : rest) f =
    f (I3.default_ { I3.fullText = Just s }) : convert' rest f
convert' ((Dzen.FG     c) : rest) _ = convert' rest (\i3 -> i3 { I3.color = c })
convert' (_               : rest) f = convert' rest f
convert' []                       _ = []

convert :: [Dzen] -> [I3]
convert = flip convert' id

main :: IO ()
main = do l <- getLine
          case Dzen.parseDzen l of
              Right dzs -> putStrLn . JSON.encode . JSON.JSArray . map I3.toJson .
                           convert $ dzs
              Left err  -> hPutStr stdout (show err ++ "\n")
          main