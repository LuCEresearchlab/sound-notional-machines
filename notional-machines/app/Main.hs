{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List                                                      (intercalate)
import qualified NotionalMachines.Lang.TypedArith.Main                          as TA
import qualified NotionalMachines.Lang.TypedLambdaArith.Main                    as TLA
import qualified NotionalMachines.Lang.TypedLambdaRef.Main                      as TLR
import qualified NotionalMachines.Lang.UntypedArith.Main                        as UA
import qualified NotionalMachines.Lang.UntypedLambda.Main                       as UL
import qualified NotionalMachines.LangInMachine.TypedLambdaRefTAPLMemoryDiagram as TLRT
import qualified NotionalMachines.LangInMachine.UntypedLambdaAlligatorEggs      as ULA
import qualified NotionalMachines.LangInMachine.UntypedLambdaExpressionTree     as ULET
import           Options.Applicative
import           Options.Applicative.Help.Pretty                                (Doc, fillSep,
                                                                                 string, vsep)

data Language = UntypedArith | UntypedLambda | TypedArith | TypedLambdaArith | TypedLambdaRef
  deriving (Bounded, Enum, Eq, Read, Show)

data NotionalMachine = ExpTree | Alligator | TAPLMemoryDiagram
  deriving (Bounded, Enum, Eq, Read, Show)

data Options = Options { optLanguage        :: Language
                       , optNotionalMachine :: Maybe NotionalMachine
                       , optOutputFilePath  :: Maybe FilePath
                       , optDiagramWidth    :: Maybe Int
                       }


languageHelp :: String
languageHelp = "Language (" ++ intercalate ", " (map show supportedLanguages) ++ ")"
    where supportedLanguages :: [Language]
          supportedLanguages = [minBound .. maxBound]

notionalMachineHelp :: String
notionalMachineHelp = "Notional Machine (" ++ intercalate ", " (map show supportedNotionalMachines) ++ ")"
    where supportedNotionalMachines :: [NotionalMachine]
          supportedNotionalMachines = [minBound .. maxBound]

languageParser :: Parser Language
languageParser = option auto
  ( long "language"
 <> short 'l'
 <> metavar "LANGUAGE"
 <> help languageHelp )

notionalMachineParser :: Parser (Maybe NotionalMachine)
notionalMachineParser = option (Just <$> auto)
  ( long "notional-machine"
 <> short 'n'
 <> metavar "NOTIONAL_MACHINE"
 <> help notionalMachineHelp
 <> value Nothing )

outputFilePathParser :: Parser (Maybe FilePath)
outputFilePathParser = option (Just <$> str)
  ( long "output"
 <> short 'o'
 <> metavar "FILE"
 <> help "Output file path for the notional machine diagram"
 <> value Nothing )

diagramWidthParser :: Parser (Maybe Int)
diagramWidthParser = option (Just <$> auto)
  ( long "width"
 <> short 'w'
 <> metavar "WIDTH"
 <> help "Width for the notional machine diagram"
 <> value Nothing )

optionsParser :: Parser Options
optionsParser = Options
  <$> languageParser
  <*> notionalMachineParser
  <*> outputFilePathParser
  <*> diagramWidthParser

supportedCombinations :: Maybe Doc
supportedCombinations = Just $ vsep
  [ (fillSep . map string . words) (
    "Demo of Sound Notional Machines. You can interact with them in the terminal " ++
    "via a series of REPLs for various combinations of languages and notional machines. " ++
    "All languages have REPLs but not all combinations of language and notional machine.")
  , ""
  , "Supported combinations of language and notional machine:"
  , "  - UntypedLambda  + ExpTree (.png, .tif, .bmp, .jpg and .pdf supported)"
  , "  - UntypedLambda  + Alligator (.svg supported)"
  , "  - TypedLambdaRef + TAPLMemoryDiagram (.png, .tif, .bmp, .jpg and .pdf supported)"
  ]

main :: IO ()
main = do
  options <- execParser $ info (optionsParser <**> helper)
    ( fullDesc
   <> header "Demo of Sound Notional Machines"
   <> progDescDoc supportedCombinations )

  case optLanguage options of
    UntypedArith  -> UA.repl
    UntypedLambda ->
        case optNotionalMachine options of
          Nothing        -> UL.repl
          Just Alligator -> handleDiagramOptions options ULA.repl
          Just ExpTree   -> handleDiagramOptions options ULET.repl
          _              -> putStrLn msgInvalidCombination
    TypedArith       -> TA.repl
    TypedLambdaArith -> TLA.repl
    TypedLambdaRef   ->
        case optNotionalMachine options of
          Nothing                -> TLR.repl
          Just TAPLMemoryDiagram -> handleDiagramOptions options TLRT.repl
          _                      -> putStrLn msgInvalidCombination

  where
      handleDiagramOptions options repl =
              case (optOutputFilePath options, optDiagramWidth options) of
                (Just outputFile, Just width) -> repl outputFile width
                _                             -> putStrLn msgFileInfoMandatory
      msgFileInfoMandatory = "Please provide output file path and diagram width."
      msgInvalidCombination = "Invalid combination of language and notional machine."
