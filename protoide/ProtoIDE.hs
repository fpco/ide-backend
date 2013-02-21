import Graphics.UI.Gtk
import Data.IORef
import Control.Monad (when, forM_)
import System.IO.Temp (withSystemTempDirectory)
import Data.Maybe (fromJust)
import Data.ByteString.Lazy (fromChunks)
import Data.Monoid (mempty)
import System.FilePath (takeFileName)

import IdeSession
import ModuleName as MN

main :: IO ()
main = withSystemTempDirectory "protoide" $ \tempDir -> do
  initGUI

  -- Create tag
  highlight <- textTagNew (Just "highlight")
  set highlight [ textTagBackground := "#ffff00" ]

  -- Create tag table
  tagTable <- textTagTableNew
  textTagTableAdd tagTable highlight

  -- Create text buffer and text view
  textBuffer <- textBufferNew (Just tagTable)
  textBufferSetText textBuffer "module M where\n\n"

  -- Create text view and give it a fixed-width font
  textView <- textViewNewWithBuffer textBuffer
  font <- fontDescriptionNew
  fontDescriptionSetFamily font "Courier"
  widgetModifyFont textView (Just font)

  -- Create window
  window <- windowNew
  set window [ windowDefaultWidth  := 640
             , windowDefaultHeight := 480
             , containerChild := textView
             ]
  onDestroy window mainQuit

  -- Start IDE session
  let cfg = SessionConfig {
                configDir        = tempDir
              , configStaticOpts = []
              }
  ideSession <- initSession cfg

  -- Whenever the buffer changes, reload the module into ghc
  -- TODO: this is overkill..
  idMapRef <- newIORef mempty
  on textBuffer bufferChanged $ do
    (start, end) <- textBufferGetBounds textBuffer
    src <- textBufferGetByteString textBuffer start end False
    -- TODO: We should be able to vary the module name
    let upd = updateModule (fromJust (MN.fromString "M")) (fromChunks [src])
    updateSession ideSession upd (const $ return ())
    errors <- getSourceErrors ideSession
    idMap  <- getIdMap ideSession
    putStrLn $ "Buffer was changed! " ++ show errors
    writeIORef idMapRef idMap

  -- Highlight the identifier under the cursor
  on textBuffer markSet $ \iter mark -> do
    -- Remove old highlights
    (start, end) <- textBufferGetBounds textBuffer
    textBufferRemoveTag textBuffer highlight start end

    -- Find the IdInfo for the identifier under the cursor
    line   <- textIterGetLine iter
    col    <- textIterGetLineOffset iter
    idMap  <- readIORef idMapRef
    let idInfos = idInfoAtLocation (line + 1) (col + 1) idMap

    -- And highlight if it's defined in the current module
    forM_ idInfos $ \idInfo ->
      case idDefSpan idInfo of
        ProperSpan defSpan | takeFileName (spanFilePath defSpan) == "M.hs" -> do
          iterStart <- textBufferGetIterAtLineOffset textBuffer
                         (spanFromLine   defSpan - 1)
                         (spanFromColumn defSpan - 1)
          iterEnd   <- textBufferGetIterAtLineOffset textBuffer
                         (spanToLine   defSpan - 1)
                         (spanToColumn defSpan - 1)
          textBufferApplyTag textBuffer highlight iterStart iterEnd
        span ->
          putStrLn $ "Unsupported span: " ++ show span

  widgetShowAll window
  mainGUI
