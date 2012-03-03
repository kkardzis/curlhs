module Main where

import Distribution.Simple
import Distribution.Simple.Utils          (withUTF8FileContents, writeUTF8File)
import Distribution.Simple.PreProcess
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.PackageDescription    (BuildInfo)
import System.FilePath                    (replaceExtension)
import Text.ParserCombinators.ReadP


-------------------------------------------------------------------------------
main = defaultMainWithHooks simpleUserHooks
  { hookedPreProcessors = ("hscx", ppHscX) : knownSuffixHandlers }


-------------------------------------------------------------------------------
ppHscX :: BuildInfo -> LocalBuildInfo -> PreProcessor
ppHscX bi lbi = PreProcessor
  { platformIndependent = True
  , runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity -> do
      let hscFile = replaceExtension outFile "hsc"
      withUTF8FileContents inFile (writeUTF8File hscFile . processHscX)
      runSimplePreProcessor (ppHsc2hs bi lbi) hscFile outFile verbosity
  }

processHscX :: String -> String
processHscX = unlines . map processHscXL . lines

processHscXL :: String -> String
processHscXL line =
  case (readP_to_S parseLine $ reverse line) of
    [((vmin, vmax, text),[])] -> concat
      [ "#if LIBCURL_VERSION_NUM >= ", vmin
      , " && LIBCURL_VERSION_NUM <= ", vmax
      , "\n", text, "\n", "#endif"]
    _ -> line

parseLine :: ReadP (String, String, String)
parseLine =
  char '|' >> parseVTAG >>= \vmax ->
  char ':' >> parseVTAG >>= \vmin ->
  char '|' >> parseTEXT >>= \text ->
  return (maybe "0x000000" id vmin, maybe "0xFFFFFF" id vmax, text)

parseTEXT :: ReadP String
parseTEXT = char ' ' >> manyTill get eof >>= return . reverse

parseVTAG :: ReadP (Maybe String)
parseVTAG = count 4 get >>= \vtag -> if (vtag == "----") then return Nothing
  else maybe pfail (return . Just) $ lookup (reverse vtag)
    [ ("7200", "0x071400"), ("7201", "0x071401")
    , ("7210", "0x071500"), ("7211", "0x071501"), ("7212", "0x071502")
    , ("7213", "0x071503"), ("7214", "0x071504"), ("7215", "0x071505")
    , ("7216", "0x071506"), ("7217", "0x071507")
    , ("7220", "0x071600")
    , ("7230", "0x071700"), ("7231", "0x071701")
    , ("7240", "0x071800")
    ]

