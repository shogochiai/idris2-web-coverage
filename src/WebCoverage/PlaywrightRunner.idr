||| Playwright Coverage Runner
|||
||| Executes JavaScript with V8 coverage collection via Playwright browser.
||| Uses system calls to run Node.js Playwright script (no FFI required).
module WebCoverage.PlaywrightRunner

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System
import System.File
import System.Directory

import WebCoverage.Types

%default covering

-- =============================================================================
-- Playwright Script Generation
-- =============================================================================

||| Generate inline Playwright script for coverage collection
playwrightScript : String -> String -> String
playwrightScript jsPath outputPath = unlines
  [ "const { chromium } = require('playwright');"
  , ""
  , "(async () => {"
  , "  const browser = await chromium.launch({ headless: true });"
  , "  const page = await browser.newPage();"
  , ""
  , "  await page.setContent('<html><head></head><body><div id=\"app\"></div></body></html>');"
  , ""
  , "  await page.coverage.startJSCoverage({ resetOnNavigation: false, reportAnonymousScripts: true });"
  , ""
  , "  await page.addScriptTag({ path: '" ++ jsPath ++ "' });"
  , ""
  , "  await new Promise(r => setTimeout(r, 1000));"
  , ""
  , "  const coverage = await page.coverage.stopJSCoverage();"
  , "  await browser.close();"
  , ""
  , "  const fs = require('fs');"
  , "  fs.writeFileSync('" ++ outputPath ++ "', JSON.stringify(coverage));"
  , "})().catch(e => { console.error(e); process.exit(1); });"
  ]

-- =============================================================================
-- V8 JSON Parsing (same as V8Runner)
-- =============================================================================

-- Find pattern in char list
findPattern : List Char -> List Char -> Maybe (List Char)
findPattern _ [] = Nothing
findPattern pattern cs@(_ :: rest) =
  if take (length pattern) cs == pattern
    then Just (drop (length pattern) cs)
    else findPattern pattern rest

-- Take digits from char list
takeDigits : List Char -> Nat
takeDigits cs =
  let digits = takeWhile isDigit (dropWhile (not . isDigit) cs)
  in fromMaybe 0 (parsePositive (pack digits))

-- Parse V8 coverage ranges from JSON
parseV8Ranges : String -> List V8Range
parseV8Ranges json = extractRanges (unpack json)
  where
    extractRanges : List Char -> List V8Range
    extractRanges [] = []
    extractRanges cs =
      case findPattern (unpack "\"startOffset\":") cs of
        Nothing => []
        Just afterStart =>
          let startNum = takeDigits afterStart
          in case findPattern (unpack "\"endOffset\":") afterStart of
               Nothing => []
               Just afterEnd =>
                 let endNum = takeDigits afterEnd
                 in case findPattern (unpack "\"count\":") afterEnd of
                      Nothing => []
                      Just afterCount =>
                        let countNum = takeDigits afterCount
                            range = MkV8Range startNum endNum countNum
                        in range :: extractRanges afterCount

-- Parse script name from URL field
parseScriptUrl : String -> Maybe String
parseScriptUrl json =
  case findPattern (unpack "\"url\":\"") (unpack json) of
    Nothing => Nothing
    Just afterUrl =>
      let url = takeWhile (/= '"') afterUrl
      in Just (pack url)

-- =============================================================================
-- Playwright Coverage Collection
-- =============================================================================

||| Extract project directory from JS path
||| For relative paths (./build/...), returns "."
||| For absolute paths (/path/project/build/...), returns "/path/project"
extractProjectDir : String -> String
extractProjectDir jsPath =
  if isPrefixOf "./build" jsPath then "."
  else if isPrefixOf "build" jsPath then "."
  else "."  -- Always use current directory for simplicity

||| Get global node_modules path via `npm root -g`
getGlobalNodeModules : IO (Maybe String)
getGlobalNodeModules = do
  Right h <- popen "npm root -g 2>/dev/null" Read
    | Left _ => pure Nothing
  Right line <- fGetLine h
    | Left _ => do _ <- pclose h; pure Nothing
  _ <- pclose h
  let path = trim line
  if length path > 0
    then pure $ Just path
    else pure Nothing

||| Check if Playwright is installed (globally or via npx)
checkPlaywrightInstalled : IO (Either String String)
checkPlaywrightInstalled = do
  -- Try global playwright first
  Right h1 <- popen "which playwright 2>/dev/null" Read
    | Left _ => checkNpx
  Right line <- fGetLine h1
    | Left _ => do _ <- pclose h1; checkNpx
  _ <- pclose h1
  if length (trim line) > 0
    then pure $ Right "global"
    else checkNpx
  where
    checkNpx : IO (Either String String)
    checkNpx = do
      -- Check if npx can find playwright
      code <- System.system "npx playwright --version >/dev/null 2>&1"
      if code == 0
        then pure $ Right "npx"
        else pure $ Left $ unlines
          [ "Playwright not found. Install it with:"
          , "  npm install -g playwright"
          , "  npx playwright install chromium"
          , ""
          , "Or for project-local installation:"
          , "  npm install playwright"
          , "  npx playwright install chromium"
          ]

||| Run JavaScript file in Playwright browser and collect V8 coverage
|||
||| @jsPath - Path to compiled JavaScript file
||| @timeout - Timeout in seconds
export
runDomTestCoverage : (jsPath : String) -> IO (Either String V8ScriptCoverage)
runDomTestCoverage jsPath = do
  putStrLn "    [Playwright] Starting browser coverage collection..."

  -- Check Playwright installation first
  Right installType <- checkPlaywrightInstalled
    | Left err => pure $ Left err

  -- Generate unique ID for temp files
  uid <- getUniqueId
  let scriptPath = "/tmp/playwright_cov_" ++ uid ++ ".js"
  let outputPath = "/tmp/playwright_cov_" ++ uid ++ ".json"

  -- Write Playwright script
  let script = playwrightScript jsPath outputPath
  Right () <- writeFile scriptPath script
    | Left err => pure $ Left $ "Failed to write script: " ++ show err

  -- Run Playwright script with Node.js
  -- Set NODE_PATH to include global node_modules for require('playwright')
  globalNodeModules <- getGlobalNodeModules
  let nodePathEnv = case globalNodeModules of
                      Just p  => "NODE_PATH=" ++ p ++ " "
                      Nothing => ""
  let cmd = nodePathEnv ++ "node " ++ scriptPath ++ " 2>&1"
  exitCode <- system cmd

  -- Read coverage output
  Right covJson <- readFile outputPath
    | Left _ => do
        cleanup scriptPath outputPath
        pure $ Left "Failed to read coverage output"

  -- Cleanup temp files
  cleanup scriptPath outputPath

  -- Parse coverage JSON
  let ranges = parseV8Ranges covJson
  let scriptName = fromMaybe jsPath (parseScriptUrl covJson)
  let func = MkV8FunctionCoverage "_aggregate_" ranges False
  pure $ Right $ MkV8ScriptCoverage scriptName [func]
  where
    getUniqueId : IO String
    getUniqueId = do
      Right h <- popen "date +%s%N 2>/dev/null || date +%s" Read
        | Left _ => pure "0"
      Right s <- fGetLine h
        | Left _ => do _ <- pclose h; pure "0"
      _ <- pclose h
      pure (trim s)

    cleanup : String -> String -> IO ()
    cleanup s o = do
      _ <- removeFile s
      _ <- removeFile o
      pure ()

||| Collect Playwright coverage (alias for runDomTestCoverage)
export
collectPlaywrightCoverage : (jsPath : String) -> (timeout : Nat)
                          -> IO (Either String V8ScriptCoverage)
collectPlaywrightCoverage jsPath _ = runDomTestCoverage jsPath
