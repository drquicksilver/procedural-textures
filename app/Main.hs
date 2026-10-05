module Main (main) where

import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Examples (Example (..), defaultExamplesDirectory, loadExamples)
import HtmlOutput (writeGallery)
import Options.Applicative
import Render (ImageFn, writeImage, writeImageRaw)
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath ((<.>), (</>))
import System.IO (hPutStrLn, stderr)
import Texture (textureToImageFn)
import TextureJson (Document (..), decodeDocument, encodeDocumentPretty)

data Command
  = RenderExamples FilePath FilePath Int
  | Gallery FilePath FilePath Int
  | RenderSpec FilePath FilePath Int
  | Benchmark FilePath
  | Format [FilePath]

main :: IO ()
main = do
  parsed <- execParser (info (commandParser <**> helper) (fullDesc <> progDesc "Render procedural textures"))
  case parsed of
    RenderExamples examplesDir outputDir size -> renderExamples examplesDir outputDir size
    Gallery examplesDir outputDir size -> renderGallery examplesDir outputDir size
    RenderSpec specPath outputPath size -> renderSpec specPath outputPath size
    Benchmark examplesDir -> runBenchmark examplesDir
    Format paths -> mapM_ formatDocument paths

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "examples" (info examplesCommand (progDesc "Render every example to PNG (the default)"))
        <> command "gallery" (info galleryCommand (progDesc "Render the HTML gallery"))
        <> command "render" (info renderCommand (progDesc "Render one texture document to PNG"))
        <> command "benchmark" (info benchmarkCommand (progDesc "Time rendering each example"))
        <> command "format" (info formatCommand (progDesc "Rewrite texture documents in canonical form (migrating old versions)"))
    )
    <|> pure (RenderExamples defaultExamplesDirectory "out" 128)
  where
    examplesCommand = RenderExamples <$> examplesOption <*> outOption "out" <*> sizeOption 128
    galleryCommand = Gallery <$> examplesOption <*> outOption "site" <*> sizeOption 512
    renderCommand =
      RenderSpec
        <$> strArgument (metavar "SPEC.json")
        <*> strArgument (metavar "OUT.png")
        <*> sizeOption 512
    benchmarkCommand = Benchmark <$> examplesOption
    formatCommand = Format <$> some (strArgument (metavar "FILE.json..."))

examplesOption :: Parser FilePath
examplesOption =
  strOption (long "examples" <> metavar "DIR" <> value defaultExamplesDirectory <> showDefault <> help "Directory of example documents")

outOption :: FilePath -> Parser FilePath
outOption def =
  strOption (long "out" <> metavar "DIR" <> value def <> showDefault <> help "Output directory")

sizeOption :: Int -> Parser Int
sizeOption def =
  option auto (long "size" <> metavar "N" <> value def <> showDefault <> help "Width and height in pixels")

renderExamples :: FilePath -> FilePath -> Int -> IO ()
renderExamples examplesDir outputDir size = do
  examples <- loadExamples examplesDir
  createDirectoryIfMissing True outputDir
  mapM_ (writeImage size size . exampleImage outputDir) examples

renderGallery :: FilePath -> FilePath -> Int -> IO ()
renderGallery examplesDir outputDir size = do
  examples <- loadExamples examplesDir
  createDirectoryIfMissing True outputDir
  mapM_ (writeImageRaw size size . exampleImage outputDir) examples
  writeGallery
    (outputDir </> "gallery.html")
    "Procedural Textures"
    [ (exampleId example <.> "png", show (documentTexture (exampleDocument example)))
    | example <- examples
    ]

renderSpec :: FilePath -> FilePath -> Int -> IO ()
renderSpec specPath outputPath size = do
  document <- readDocument specPath
  writeImage size size (outputPath, textureToImageFn (documentTexture document))

formatDocument :: FilePath -> IO ()
formatDocument path = do
  document <- readDocument path
  let formatted = encodeDocumentPretty document
  -- Force the encoding before overwriting the file it was read from.
  BL.length formatted `seq` BL.writeFile path formatted
  putStrLn ("Formatted " <> path)

readDocument :: FilePath -> IO Document
readDocument path = do
  bytes <- BL.readFile path
  case decodeDocument bytes of
    Left err -> do
      hPutStrLn stderr (path <> ": " <> err)
      exitFailure
    Right document -> pure document

exampleImage :: FilePath -> Example -> (FilePath, ImageFn)
exampleImage outputDir example =
  (outputDir </> exampleId example <.> "png", textureToImageFn (documentTexture (exampleDocument example)))

runBenchmark :: FilePath -> IO ()
runBenchmark examplesDir = do
  examples <- loadExamples examplesDir
  createDirectoryIfMissing True "out"
  small <- mapM (timeOne 128) examples
  putStrLn "Benchmark: 128x128"
  putStrLn (renderTable small)
  large <- mapM (timeOne 512) examples
  putStrLn "Benchmark: 512x512"
  putStrLn (renderTable large)

timeOne :: Int -> Example -> IO (String, Integer)
timeOne size example = do
  start <- getCurrentTime
  writeImageRaw size size (exampleImage "out" example)
  end <- getCurrentTime
  let ms = round (diffUTCTime end start * 1000.0)
  pure (T.unpack (documentName (exampleDocument example)), ms)

renderTable :: [(String, Integer)] -> String
renderTable rows =
  let nameWidth = maximum (length "Example" : map (length . fst) rows)
      msWidth = maximum (length "Ms" : map (length . show . snd) rows)
      headerRow = formatRow nameWidth msWidth "Example" "Ms"
      separator = replicate (nameWidth + msWidth + 5) '-'
      body = map (\(name, ms) -> formatRow nameWidth msWidth name (show ms)) rows
  in intercalate "\n" (headerRow : separator : body)

formatRow :: Int -> Int -> String -> String -> String
formatRow nameWidth msWidth name ms =
  padRight nameWidth name <> " | " <> padLeft msWidth ms

padRight :: Int -> String -> String
padRight width text =
  text <> replicate (width - length text) ' '

padLeft :: Int -> String -> String
padLeft width text =
  replicate (width - length text) ' ' <> text
