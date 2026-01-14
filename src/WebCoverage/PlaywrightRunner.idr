||| Playwright Coverage Runner
|||
||| Executes JavaScript with V8 coverage collection via Playwright browser.
||| This provides real browser execution for DOM-based code.
module WebCoverage.PlaywrightRunner

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System.File

import Playwright.Types
import Playwright.FFI
import Playwright.Coverage

import WebCoverage.Types

%default covering

-- =============================================================================
-- Type Conversion
-- =============================================================================

||| Convert Playwright CoverageRange to WebCoverage V8Range
playwrightToV8Range : CoverageRange -> V8Range
playwrightToV8Range r = MkV8Range r.startOffset r.endOffset r.count

||| Convert Playwright CoverageEntry to WebCoverage V8ScriptCoverage
playwrightToV8Script : CoverageEntry -> V8ScriptCoverage
playwrightToV8Script entry =
  let funcs = map convertFunc entry.functions
  in MkV8ScriptCoverage entry.url funcs
  where
    convertFunc : CoverageFunction -> V8FunctionCoverage
    convertFunc f = MkV8FunctionCoverage f.functionName
                      (map playwrightToV8Range f.ranges)
                      f.isBlockCoverage

-- =============================================================================
-- Playwright Coverage Collection
-- =============================================================================

||| Run JavaScript file in browser and collect V8 coverage
|||
||| @jsPath - Path to compiled JavaScript file
||| @timeout - Timeout in seconds (currently unused, for API compatibility)
export
collectPlaywrightCoverage : (jsPath : String) -> (timeout : Nat)
                          -> IO (Either String V8ScriptCoverage)
collectPlaywrightCoverage jsPath timeout = do
  -- Use Playwright's high-level API
  result <- collectBrowserCoverage (defaultConfig jsPath)
  case result of
    Left err => pure $ Left err
    Right covResult =>
      case covResult.entries of
        [] => pure $ Left "No coverage entries collected"
        (entry :: _) => pure $ Right (playwrightToV8Script entry)

||| Run with custom HTML template (for DOM testing)
|||
||| Creates a page with custom HTML, injects the test script,
||| and collects coverage.
export
collectPlaywrightCoverageWithHtml : (jsPath : String)
                                  -> (htmlTemplate : String)
                                  -> (timeout : Nat)
                                  -> IO (Either String V8ScriptCoverage)
collectPlaywrightCoverageWithHtml jsPath htmlTemplate timeout = do
  browser <- launchBrowser
  page <- newPage browser

  -- Set custom HTML content
  setContent page htmlTemplate

  -- Start coverage collection
  startCoverage page

  -- Add the test script
  addScriptTag page jsPath

  -- Stop coverage and get results
  rawJson <- stopCoverageRaw page

  -- Close browser
  closeBrowser browser

  -- Parse and return results
  let entries = parseCoverageEntries rawJson
  case entries of
    [] => pure $ Left "No coverage entries"
    (entry :: _) => pure $ Right (playwrightToV8Script entry)

-- =============================================================================
-- DOM Test Helpers
-- =============================================================================

||| Minimal HTML template for DOM tests
export
minimalDomHtml : String
minimalDomHtml = """
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>DOM Test</title>
</head>
<body>
  <div id="app"></div>
</body>
</html>
"""

||| Run DOM test with coverage
|||
||| Designed for idris2-dom-mvc style tests where the compiled JS
||| creates DOM elements and runs tests.
export
runDomTestCoverage : (jsPath : String) -> IO (Either String V8ScriptCoverage)
runDomTestCoverage jsPath = collectPlaywrightCoverageWithHtml jsPath minimalDomHtml 30
