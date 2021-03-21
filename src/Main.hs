module Main where

import Polysemy
import Polysemy.State
import Text.Pretty (toString)
import Options.Applicative
import System.Directory  (createDirectoryIfMissing)
import Syntax.Pretty()
import Syntax.Mutate
import Parsing.Parser
import Parsing.Lexer
import Data.Configuration

amount :: Int
amount = 10

mutate :: Configuration -> IO ()
mutate Configuration{..} = do
    content <- readFile fileName
    let ast = (parser fileName . lexer fileName) content
    createDirectoryIfMissing True folder
    --let mutatedFilePath  = \ index -> "../experiments/" ++ folder ++ "/mutated" ++ show index ++ ".oox"
    mapM_ (\ (category, mutation) -> do
        mutatedAsts <- runM (evalState emptyEnvironment (mutateCompilationUnit mutation ast))
        mapM_ (\ (index, mutatedAst) -> do
            let mutatedFilePath = folder ++ "/" ++ category ++ "_" ++ show index ++ ".oox"
            writeFile mutatedFilePath (toString mutatedAst)) (zip [0..] mutatedAsts)
        ) operators
    --mapM_ (generateMutatedFile originalFilePath . mutatedFilePath) [1..amount]

{-generateMutatedFile :: FilePath -> FilePath -> IO ()
generateMutatedFile original mutated = do
    content <- readFile original
    let ast = (parser original . lexer original) content
    mutatedAst <- runM (evalState emptyEnvironment (mutateCompilationUnit ast))
    writeFile mutated (toString mutatedAst)-}

--------------------------------------------------------------------------------
-- Argument parsing and main
--------------------------------------------------------------------------------

parseConfiguration :: Parser Configuration
parseConfiguration = Configuration 
                <$> strArgument 
                    ( help "The input OOX source file" )
                <*> strOption
                    (  help  "The output folder"
                    <> long  "output"
                    <> short 'o'
                    <> metavar "STRING"
                    <> value "./output" )

parserInfo :: ParserInfo Configuration
parserInfo = info (parseConfiguration <**> helper)
              (fullDesc <> header "A Mutation Generation Tool for OOX Programs")

main :: IO ()
main = execParser parserInfo >>= mutate