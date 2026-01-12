module HtmlOutput
  ( writeGallery
  ) where

import Data.List (intercalate)

writeGallery :: FilePath -> String -> [(FilePath, String)] -> IO ()
writeGallery path title entries =
  writeFile path (renderGallery title entries)

renderGallery :: String -> [(FilePath, String)] -> String
renderGallery title entries =
  unlines
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
    , "    .meta {"
    , "      padding: 12px 16px 16px;"
    , "      display: flex;"
    , "      flex-direction: column;"
    , "      gap: 8px;"
    , "    }"
    , "    .filename {"
    , "      font-size: 14px;"
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
    , "  </style>"
    , "</head>"
    , "<body>"
    , "  <header>"
    , "    <h1>" <> escapeHtml title <> "</h1>"
    , "  </header>"
    , "  <section class=\"grid\">"
    , intercalate "\n" (map renderCard entries)
    , "  </section>"
    , "</body>"
    , "</html>"
    ]

renderCard :: (FilePath, String) -> String
renderCard (path, code) =
  unlines
    [ "    <article class=\"card\">"
    , "      <div class=\"thumb\">"
    , "        <img src=\"" <> escapeHtml path <> "\" alt=\"" <> escapeHtml path <> "\">"
    , "      </div>"
    , "      <div class=\"meta\">"
    , "        <div class=\"filename\">" <> escapeHtml path <> "</div>"
    , "        <pre><code>" <> escapeHtml code <> "</code></pre>"
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
