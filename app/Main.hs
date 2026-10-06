{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (Value (..), eitherDecode)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import Examples (Example (..), defaultExamplesDirectory, loadExamples)
import Gallery (GalleryEntry (..))
import HtmlOutput (writeSolidGallery)
import ContactSheet (writeContactSheet)
import Options.Applicative
import RampLibrary (RampLibrary, defaultRampsDirectory, libraryRampToValue, loadRampLibrary, parseLibraryRamp)
import Render (ImageFn, writeImage, writeImageRaw)
import Resolve (resolveDocument)
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName, (<.>), (</>))
import Text.Read (readMaybe)
import System.IO (hPutStrLn, stderr)
import Texture (textureToImageFn)
import Scene (View(..), SliceAxis(..), defaultCamera, defaultView, viewImageFn)
import Geometry (shapes, shapeName)
import TextureJson (Document (..), decodeDocument, encodeDocumentPretty, encodeValuePretty)

data Command
  = RenderExamples FilePath FilePath Int
  | Gallery FilePath FilePath Int Bool
  | RenderSpec FilePath FilePath Int View
  | Format [FilePath]

main :: IO ()
main = do
  (rampsDir, parsed) <- execParser (info (((,) <$> rampsOption <*> commandParser) <**> helper) (fullDesc <> progDesc "Render procedural textures"))
  case parsed of
    RenderExamples examplesDir outputDir size -> do
      library <- loadRampLibrary rampsDir
      renderExamples library examplesDir outputDir size
    Gallery examplesDir outputDir size contactSheet -> do
      library <- loadRampLibrary rampsDir
      renderGallery library examplesDir outputDir size contactSheet
    RenderSpec specPath outputPath size view -> do
      library <- loadRampLibrary rampsDir
      renderSpec library specPath outputPath size view
    Format paths -> mapM_ formatFile paths

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "examples" (info examplesCommand (progDesc "Render every example to PNG (the default)"))
        <> command "gallery" (info galleryCommand (progDesc "Render the gallery as HTML or a contact-sheet PNG"))
        <> command "render" (info renderCommand (progDesc "Render one texture document to PNG"))
        <> command "format" (info formatCommand (progDesc "Rewrite texture documents and library ramps in canonical form (migrating old documents)"))
    )
    <|> pure (RenderExamples defaultExamplesDirectory "out" 128)
  where
    examplesCommand = RenderExamples <$> examplesOption <*> outOption "out" <*> sizeOption 128
    galleryCommand = Gallery <$> examplesOption <*> outOption "site" <*> sizeOption 512
      <*> switch (long "contact-sheet" <> help "Write gallery.png with eight columns of 128x128 previews (ignores --size)")
    renderCommand =
      RenderSpec
        <$> strArgument (metavar "SPEC.json")
        <*> strArgument (metavar "OUT.png")
        <*> sizeOption 512
        <*> viewOption
    formatCommand = Format <$> some (strArgument (metavar "FILE.json..."))

viewOption :: Parser View
viewOption = makeView
  <$> optional (option (eitherReader readShape) (long "shape" <> metavar "SHAPE" <> help "Render a 3D shape: sphere, cube, cylinder, torus, bitten-cube, cut-sphere, cut-cube"))
  <*> option (eitherReader readAxis) (long "axis" <> value XY <> metavar "xy|xz|yz" <> help "Slice orientation")
  <*> option (eitherReader readPosition) (long "slice" <> value 0 <> metavar "POSITION" <> help "Slice position in object coordinates (default 0)")
  where
    makeView (Just shape) _ _ = Scene shape defaultCamera
    makeView Nothing axis position = Slice axis position
    readShape name = case filter ((== name) . shapeName) shapes of
      [shape] -> Right shape
      _ -> Left "Unknown shape"
    readPosition text = case readMaybe text of
      Just n | not (isNaN n || isInfinite n) && n >= (-2) && n <= 2 -> Right n
      _ -> Left "Slice position must be finite and between -2 and 2"
    readAxis "xy" = Right XY
    readAxis "xz" = Right XZ
    readAxis "yz" = Right YZ
    readAxis _ = Left "Axis must be xy, xz or yz"

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

renderGallery :: RampLibrary -> FilePath -> FilePath -> Int -> Bool -> IO ()
renderGallery library examplesDir outputDir size contactSheet = do
  examples <- loadExamples examplesDir
  createDirectoryIfMissing True outputDir
  entries <- mapM galleryEntry examples
  if contactSheet
    then writeContactSheet (outputDir </> "gallery.png") "Procedural Textures · 3D cutaways and XY slices" (concatMap sheetPair entries)
    else do
      htmlEntries <- mapM writePair (zip examples entries)
      writeSolidGallery (outputDir </> "gallery.html") "Procedural Textures · 3D materials" htmlEntries
  where
    galleryEntry example = do
      let document = exampleDocument example
      texture <- either (failWith (exampleId example)) pure (resolveDocument library document)
      pure GalleryEntry
        { entryImage = (viewImageFn defaultView texture, viewImageFn (Slice XY 0) texture)
        , entryTitle = T.unpack (documentName document)
        , entryDescription = T.unpack (documentDescription document)
        , entryCategory = T.unpack (documentCategory document)
        , entryCode = BLC.unpack (encodeDocumentPretty document)
        }
    writePair (example, entry) = do
      let (solid, slice) = entryImage entry
          solidName = exampleId example <> "-solid.png"
          sliceName = exampleId example <> ".png"
      writeImageRaw size size (outputDir </> solidName, solid)
      writeImageRaw size size (outputDir </> sliceName, slice)
      pure entry {entryImage = (solidName, sliceName)}
    -- Adjacent pairs preserve square, pixel-exact 128² previews in the sheet.
    sheetPair entry =
      let (solid,slice) = entryImage entry
      in [entry {entryImage = solid, entryTitle = entryTitle entry <> " · solid"},
          entry {entryImage = slice, entryTitle = entryTitle entry <> " · slice"}]

renderSpec :: RampLibrary -> FilePath -> FilePath -> Int -> View -> IO ()
renderSpec library specPath outputPath size view = do
  document <- readDocument specPath
  texture <- either (failWith specPath) pure (resolveDocument library document)
  writeImage size size (outputPath, viewImageFn view texture)

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
