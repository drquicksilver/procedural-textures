{-# LANGUAGE OverloadedStrings #-}

module HtmlOutput
  ( GalleryEntry (..)
  , writeGallery
  , renderGallery
  , writeSolidGallery
  , renderSolidGallery
  , SiteLink (..)
  , writeSiteIndex
  , renderSiteIndex
  , writeShapePage
  , renderShapePage
  ) where

import Data.List (intercalate)
import Gallery (GalleryEntry (..), groupByCategory, groupByFamily, guideRoleLabel)
import TextureJson (ExampleGuide(..))
import qualified Data.Text as T
import Data.Char (toUpper)

writeGallery :: FilePath -> String -> [GalleryEntry FilePath] -> IO ()
writeGallery path title entries =
  writeFile path (renderGallery title entries)

renderGallery :: String -> [GalleryEntry FilePath] -> String
renderGallery = renderGalleryWith (\path title _ -> singleImage path title)

writeSolidGallery :: FilePath -> String -> [GalleryEntry (FilePath, FilePath)] -> IO ()
writeSolidGallery path title entries = writeFile path (renderSolidGallery title entries)

renderSolidGallery :: String -> [GalleryEntry (FilePath, FilePath)] -> String
renderSolidGallery = renderGalleryWith $ \(solid, slice) title guide ->
  let (axis, position) = maybe ("xy", 0) id (guide >>= guidePreview)
      plane = map toUpper (T.unpack axis)
      coordinate = case T.unpack axis of "xz" -> "y"; "yz" -> "x"; _ -> "z"
      caption = plane <> " slice · " <> coordinate <> "=" <> (if position == 0 then "0" else show position)
  in
  "<div class=\"views\"><figure>" <> singleImage solid (title <> " on a cutaway cube") <>
  "<figcaption>3D cutaway</figcaption></figure><figure>" <> singleImage slice (title <> " as an " <> caption) <>
  "<figcaption>" <> escapeHtml caption <> "</figcaption></figure></div>"

singleImage :: FilePath -> String -> String
singleImage path title = "<img loading=\"lazy\" src=\"" <> escapeHtml path <> "\" alt=\"" <> escapeHtml title <> "\">"

-- | A card on the site index: a page, a preview image and a short blurb.
data SiteLink = SiteLink
  { linkHref :: FilePath
  , linkImage :: FilePath
  , linkTitle :: String
  , linkDescription :: String
  }

writeSiteIndex :: FilePath -> String -> [SiteLink] -> IO ()
writeSiteIndex path title links = writeFile path (renderSiteIndex title links)

renderSiteIndex :: String -> [SiteLink] -> String
renderSiteIndex title links =
  renderPage title linkStyles []
    [ "  <section class=\"grid\">"
    , intercalate "\n" (map renderLink links)
    , "  </section>"
    ]

renderLink :: SiteLink -> String
renderLink link =
  unlines
    [ "    <a class=\"card\" href=\"" <> escapeHtml (linkHref link) <> "\">"
    , "      <div class=\"thumb\">"
    , if null (linkImage link) then "" else "        " <> singleImage (linkImage link) (linkTitle link)
    , "      </div>"
    , "      <div class=\"meta\">"
    , "        <div class=\"title\">" <> escapeHtml (linkTitle link) <> "</div>"
    , "        <p class=\"description\">" <> escapeHtml (linkDescription link) <> "</p>"
    , "      </div>"
    , "    </a>"
    ]

writeShapePage :: FilePath -> String -> FilePath -> String -> [GalleryEntry FilePath] -> IO ()
writeShapePage path title indexHref shape entries = writeFile path (renderShapePage title indexHref shape entries)

-- | One shape in several materials, in the given order, linking back to the index.
renderShapePage :: String -> FilePath -> String -> [GalleryEntry FilePath] -> String
renderShapePage title indexHref shape entries =
  renderPage title linkStyles ["    <nav><a href=\"" <> escapeHtml indexHref <> "\">&larr; All pages</a></nav>"]
    [ "  <section class=\"grid wide\">"
    , intercalate "\n" (map (renderCard (\image name _ -> singleImage image (name <> " on a " <> shape))) entries)
    , "  </section>"
    ]

linkStyles :: [String]
linkStyles =
  [ "    a { color: inherit; }"
  , "    a.card { text-decoration: none; }"
  , "    a.card:hover { border-color: var(--muted); }"
  , "    nav { padding-bottom: 8px; font-size: 13px; }"
  , "    .grid.wide { grid-template-columns: repeat(auto-fit, minmax(min(100%, 420px), 1fr)); }"
  ]

renderGalleryWith :: (image -> String -> Maybe ExampleGuide -> String) -> String -> [GalleryEntry image] -> String
renderGalleryWith thumbnail title entries =
  renderPage title [] [] [if null entries then "<p>No materials in this library.</p>" else intercalate "\n" (map (renderSection thumbnail) (groupByCategory entries))]

-- | The shared page shell: extra style rules, lines above the title and the body.
renderPage :: String -> [String] -> [String] -> [String] -> String
renderPage title extraStyles preamble body =
  unlines $
    [ "<!doctype html>"
    , "<html lang=\"en\">"
    , "<head>"
    , "  <meta charset=\"utf-8\">"
    , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
    , "  <title>" <> escapeHtml title <> "</title>"
    , "  <style>"
    , "    :root {"
    , "      --bg: #f5f5f2;"
    , "      --card: #ffffff;"
    , "      --ink: #1f1f1f;"
    , "      --muted: #5c5c5c;"
    , "      --border: #e0e0dc;"
    , "      --code-bg: #f1f1ec;"
    , "    }"
    , "    body {"
    , "      margin: 0;"
    , "      font-family: \"IBM Plex Mono\", \"SFMono-Regular\", Menlo, Consolas, monospace;"
    , "      color: var(--ink);"
    , "      background: var(--bg);"
    , "    }"
    , "    header {"
    , "      padding: 24px 28px 8px;"
    , "    }"
    , "    h2 {"
    , "      margin: 0;"
    , "      padding: 20px 28px 0;"
    , "      font-size: 18px;"
    , "      color: var(--muted);"
    , "    }"
    , "    h1 {"
    , "      margin: 0;"
    , "      font-size: 28px;"
    , "      letter-spacing: -0.5px;"
    , "    }"
    , "    .grid {"
    , "      display: grid;"
    , "      gap: 24px;"
    , "      padding: 20px 28px 40px;"
    , "      grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));"
    , "    }"
    , "    .card {"
    , "      background: var(--card);"
    , "      border: 1px solid var(--border);"
    , "      border-radius: 12px;"
    , "      overflow: hidden;"
    , "      box-shadow: 0 8px 20px rgba(0, 0, 0, 0.06);"
    , "      display: flex;"
    , "      flex-direction: column;"
    , "    }"
    , "    .thumb {"
    , "      background: #fafaf7;"
    , "      display: flex;"
    , "      align-items: center;"
    , "      justify-content: center;"
    , "      padding: 12px;"
    , "    }"
    , "    .thumb img {"
    , "      width: 100%;"
    , "      height: auto;"
    , "      display: block;"
    , "      border-radius: 8px;"
    , "    }"
    , "    .views { display: grid; grid-template-columns: 1fr 1fr; gap: 8px; width: 100%; }"
    , "    .family > summary { margin: 16px 28px; font-size: 15px; font-weight: 600; }"
    , "    .role, .tags { font-size: 11px; color: var(--muted); }"
    , "    .hint { margin: 0; font-size: 12px; }"
    , "    figure { margin: 0; min-width: 0; }"
    , "    figcaption { font-size: 10px; color: var(--muted); text-align: center; padding-top: 6px; }"
    , "    .meta {"
    , "      padding: 12px 16px 16px;"
    , "      display: flex;"
    , "      flex-direction: column;"
    , "      gap: 8px;"
    , "    }"
    , "    .title {"
    , "      font-size: 15px;"
    , "      font-weight: 600;"
    , "    }"
    , "    .description {"
    , "      margin: 0;"
    , "      font-size: 13px;"
    , "      color: var(--muted);"
    , "    }"
    , "    summary {"
    , "      cursor: pointer;"
    , "      font-size: 12px;"
    , "      color: var(--muted);"
    , "    }"
    , "    pre {"
    , "      margin: 0;"
    , "      padding: 12px;"
    , "      background: var(--code-bg);"
    , "      border-radius: 8px;"
    , "      overflow-x: auto;"
    , "      font-size: 12px;"
    , "      line-height: 1.4;"
    , "      white-space: pre-wrap;"
    , "    }"
    ]
    <> extraStyles
    <>
    [ "  </style>"
    , "</head>"
    , "<body>"
    , "  <header>"
    ]
    <> preamble
    <>
    [ "    <h1>" <> escapeHtml title <> "</h1>"
    , "  </header>"
    ]
    <> body
    <>
    [ "</body>"
    , "</html>"
    ]

renderSection :: (image -> String -> Maybe ExampleGuide -> String) -> (String, [GalleryEntry image]) -> String
renderSection thumbnail (heading, members) =
  unlines
    [ "  <h2>" <> escapeHtml heading <> "</h2>"
    , intercalate "\n" (map renderFamily (groupByFamily members))
    ]
  where
    renderFamily (family, entries) =
      let grid = "<section class=\"grid\">" <> intercalate "\n" (map (renderCard thumbnail) entries) <> "</section>"
      in if null family then grid else "<details class=\"family\"><summary>" <> escapeHtml family <> " · " <> show (length entries) <> " examples</summary>" <> grid <> "</details>"

renderCard :: (image -> String -> Maybe ExampleGuide -> String) -> GalleryEntry image -> String
renderCard thumbnail entry =
  unlines
    [ "    <article class=\"card\">"
    , "      <div class=\"thumb\">"
    , "        " <> thumbnail (entryImage entry) (entryTitle entry) (entryGuide entry)
    , "      </div>"
    , "      <div class=\"meta\">"
    , "        <div class=\"title\">" <> escapeHtml (entryTitle entry) <> "</div>"
    , "        <p class=\"description\">" <> escapeHtml (entryDescription entry) <> "</p>"
    , maybe "" (\guide -> "<span class=\"role\">" <> escapeHtml (guideRoleLabel (guideRole guide)) <> "</span><span class=\"tags\">" <> escapeHtml (T.unpack (T.intercalate " · " (guideTags guide))) <> "</span><p class=\"hint\">" <> escapeHtml (T.unpack (guideHint guide)) <> "</p>") (entryGuide entry)
    , "        <details>"
    , "          <summary>Texture document</summary>"
    , "          <pre><code>" <> escapeHtml (entryCode entry) <> "</code></pre>"
    , "        </details>"
    , "      </div>"
    , "    </article>"
    ]

escapeHtml :: String -> String
escapeHtml =
  concatMap escapeChar

escapeChar :: Char -> String
escapeChar ch =
  case ch of
    '&' -> "&amp;"
    '<' -> "&lt;"
    '>' -> "&gt;"
    '"' -> "&quot;"
    '\'' -> "&#39;"
    _ -> [ch]
