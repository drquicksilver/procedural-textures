module Main (main) where

import qualified Data.ByteString.Lazy as BL
import Examples (Example (..), defaultExamplesDirectory, loadExamples)
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import HtmlOutput (GalleryEntry (..), writeGallery)
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
  | Format [FilePath]

main :: IO ()
main = do
  parsed <- execParser (info (commandParser <**> helper) (fullDesc <> progDesc "Render procedural textures"))
  case parsed of
    RenderExamples examplesDir outputDir size -> renderExamples examplesDir outputDir size
    Gallery examplesDir outputDir size -> renderGallery examplesDir outputDir size
    RenderSpec specPath outputPath size -> renderSpec specPath outputPath size
    Format paths -> mapM_ formatDocument paths

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "examples" (info examplesCommand (progDesc "Render every example to PNG (the default)"))
        <> command "gallery" (info galleryCommand (progDesc "Render the HTML gallery"))
        <> command "render" (info renderCommand (progDesc "Render one texture document to PNG"))
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
    [ GalleryEntry
        { entryImage = exampleId example <.> "png"
        , entryTitle = T.unpack (documentName document)
        , entryDescription = T.unpack (documentDescription document)
        , entryCode = BLC.unpack (encodeDocumentPretty document)
        }
    | example <- examples
    , let document = exampleDocument example
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
