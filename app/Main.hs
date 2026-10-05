{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (Value (..), eitherDecode)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import Examples (Example (..), defaultExamplesDirectory, loadExamples)
import HtmlOutput (GalleryEntry (..), writeGallery)
import Options.Applicative
import RampLibrary (RampLibrary, defaultRampsDirectory, libraryRampToValue, loadRampLibrary, parseLibraryRamp)
import Render (ImageFn, writeImage, writeImageRaw)
import Resolve (resolveDocument)
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName, (<.>), (</>))
import System.IO (hPutStrLn, stderr)
import Texture (textureToImageFn)
import TextureJson (Document (..), decodeDocument, encodeDocumentPretty, encodeValuePretty)

data Command
  = RenderExamples FilePath FilePath Int
  | Gallery FilePath FilePath Int
  | RenderSpec FilePath FilePath Int
  | Format [FilePath]

main :: IO ()
main = do
  (rampsDir, parsed) <- execParser (info (((,) <$> rampsOption <*> commandParser) <**> helper) (fullDesc <> progDesc "Render procedural textures"))
  case parsed of
    RenderExamples examplesDir outputDir size -> do
      library <- loadRampLibrary rampsDir
      renderExamples library examplesDir outputDir size
    Gallery examplesDir outputDir size -> do
      library <- loadRampLibrary rampsDir
      renderGallery library examplesDir outputDir size
    RenderSpec specPath outputPath size -> do
      library <- loadRampLibrary rampsDir
      renderSpec library specPath outputPath size
    Format paths -> mapM_ formatFile paths

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "examples" (info examplesCommand (progDesc "Render every example to PNG (the default)"))
        <> command "gallery" (info galleryCommand (progDesc "Render the HTML gallery"))
        <> command "render" (info renderCommand (progDesc "Render one texture document to PNG"))
        <> command "format" (info formatCommand (progDesc "Rewrite texture documents and library ramps in canonical form (migrating old documents)"))
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

rampsOption :: Parser FilePath
rampsOption =
  strOption (long "ramps" <> metavar "DIR" <> value defaultRampsDirectory <> showDefault <> help "Directory of built-in ramps")

examplesOption :: Parser FilePath
examplesOption =
  strOption (long "examples" <> metavar "DIR" <> value defaultExamplesDirectory <> showDefault <> help "Directory of example documents")

outOption :: FilePath -> Parser FilePath
outOption def =
  strOption (long "out" <> metavar "DIR" <> value def <> showDefault <> help "Output directory")

sizeOption :: Int -> Parser Int
sizeOption def =
  option auto (long "size" <> metavar "N" <> value def <> showDefault <> help "Width and height in pixels")

renderExamples :: RampLibrary -> FilePath -> FilePath -> Int -> IO ()
renderExamples library examplesDir outputDir size = do
  examples <- loadExamples examplesDir
  createDirectoryIfMissing True outputDir
  images <- mapM (exampleImage library outputDir) examples
  mapM_ (writeImage size size) images

renderGallery :: RampLibrary -> FilePath -> FilePath -> Int -> IO ()
renderGallery library examplesDir outputDir size = do
  examples <- loadExamples examplesDir
  createDirectoryIfMissing True outputDir
  images <- mapM (exampleImage library outputDir) examples
  mapM_ (writeImageRaw size size) images
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

renderSpec :: RampLibrary -> FilePath -> FilePath -> Int -> IO ()
renderSpec library specPath outputPath size = do
  document <- readDocument specPath
  imageFn <- documentImage library specPath document
  writeImage size size (outputPath, imageFn)

-- | Rewrite a texture document or a library ramp file in canonical form.
formatFile :: FilePath -> IO ()
formatFile path = do
  bytes <- BL.readFile path
  formatted <-
    case eitherDecode bytes of
      Right json@(Object o)
        | KeyMap.member "texture" o -> either (failWith path) (pure . encodeDocumentPretty) (decodeDocument bytes)
        | otherwise ->
            either (failWith path) (pure . encodeValuePretty . libraryRampToValue) (parseEither (parseLibraryRamp (T.pack (takeBaseName path))) json)
      Right _ -> failWith path "not a JSON object"
      Left err -> failWith path err
  -- Force the encoding before overwriting the file it was read from.
  BL.length formatted `seq` BL.writeFile path formatted
  putStrLn ("Formatted " <> path)

readDocument :: FilePath -> IO Document
readDocument path = do
  bytes <- BL.readFile path
  either (failWith path) pure (decodeDocument bytes)

documentImage :: RampLibrary -> FilePath -> Document -> IO ImageFn
documentImage library path document =
  either (failWith path) (pure . textureToImageFn) (resolveDocument library document)

exampleImage :: RampLibrary -> FilePath -> Example -> IO (FilePath, ImageFn)
exampleImage library outputDir example = do
  imageFn <- documentImage library (exampleId example) (exampleDocument example)
  pure (outputDir </> exampleId example <.> "png", imageFn)

failWith :: FilePath -> String -> IO a
failWith path err = do
  hPutStrLn stderr (path <> ": " <> err)
  exitFailure
