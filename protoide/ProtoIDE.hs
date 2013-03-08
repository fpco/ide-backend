{-# LANGUAGE NamedFieldPuns #-}
import Prelude hiding (span)
import Graphics.UI.Gtk
import Data.IORef
import System.IO.Temp (withSystemTempDirectory)
import Data.ByteString.Lazy (fromChunks)
import Control.Monad.Reader
import System.Process
import qualified Data.Map as Map

#ifndef darwin_HOST_OS
import System.GIO.File.AppInfo
#endif

import IdeSession

main :: IO ()
main = withSystemTempDirectory "protoide" $ \tempDir -> do
  initGUI

  -- Create URL link tag.
  linkTag <- textTagNew (Just "link")
  set linkTag [ -- textTagForeground := "blue"
                textTagUnderline := UnderlineSingle
              ]

  -- Create highlight tag
  highlight <- textTagNew (Just "highlight")
  set highlight [ textTagBackground := "#ffff00" ]

  -- Create tag table
  tagTable <- textTagTableNew
  textTagTableAdd tagTable linkTag
  textTagTableAdd tagTable highlight

  -- Create text buffer and text view
  textBuffer <- textBufferNew (Just tagTable)
  textBufferSetText textBuffer "module M where\n\n"

  -- Create text view and give it a fixed-width font
  textView <- textViewNewWithBuffer textBuffer
  font <- fontDescriptionNew
  fontDescriptionSetFamily font "Courier"
  widgetModifyFont textView (Just font)

  -- Create text view for errors
  errorsBuff <- textBufferNew Nothing
  errorsView <- textViewNewWithBuffer errorsBuff
  textViewSetWrapMode errorsView WrapWord

  -- Create text view for information about ids
  idInfoBuff <- textBufferNew Nothing
  idInfoView <- textViewNewWithBuffer idInfoBuff
  textViewSetWrapMode idInfoView WrapWord

  -- Create a scrolled window to scroll long lines instead of resizing.
  scrolled <- scrolledWindowNew Nothing Nothing
  set scrolled [ scrolledWindowHscrollbarPolicy := PolicyAutomatic
               , scrolledWindowVscrollbarPolicy := PolicyAutomatic
               , containerChild := textView
               ]

  -- Create box to hold id info and errors
  infoAndErrors <- vBoxNew True 5
  boxPackEnd infoAndErrors errorsView PackGrow 0
  boxPackEnd infoAndErrors idInfoView PackGrow 0

  -- Create paned view to contain source and info/errors
  hPaned <- vPanedNew
  panedAdd1 hPaned scrolled
  panedAdd2 hPaned infoAndErrors
  panedSetPosition hPaned 240

  -- Create window
  window <- windowNew
  set window [ windowDefaultWidth  := 640
             , windowDefaultHeight := 480
             , containerChild      := hPaned
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
  idMapRef <- newIORef (error "no ID maps loaded")
  on textBuffer bufferChanged $ do
    (start, end) <- textBufferGetBounds textBuffer
    src <- textBufferGetByteString textBuffer start end False
    -- TODO: We should be able to vary the module name
    let upd = updateModule "M.hs" (fromChunks [src])
    updateSession ideSession upd (const $ return ())
    errors <- getSourceErrors ideSession
    idMaps <- getLoadedModules ideSession
    textBufferSetText errorsBuff (show errors)
    writeIORef idMapRef (Map.elems idMaps)

  -- Highlight the identifier under the cursor
  on textBuffer markSet $ \iter _mark -> do
    -- Remove old highlights
    (start, end) <- textBufferGetBounds textBuffer
    textBufferRemoveTag textBuffer highlight start end
    textBufferRemoveTag textBuffer linkTag   start end

    -- Find the IdInfo for the identifier under the cursor
    line   <- textIterGetLine iter
    col    <- textIterGetLineOffset iter
    idMap  <- readIORef idMapRef
    let idInfos = concatMap (idInfoAtLocation (line + 1) (col + 1)) idMap
        tagSpan sp tag = do
          iterStart <- textBufferGetIterAtLineOffset textBuffer
                         (spanFromLine   sp - 1)
                         (spanFromColumn sp - 1)
          iterEnd   <- textBufferGetIterAtLineOffset textBuffer
                         (spanToLine   sp - 1)
                         (spanToColumn sp - 1)
          textBufferApplyTag textBuffer tag iterStart iterEnd

    -- And highlight if it's defined in the current module
    idInfoText <- forM idInfos $ \(srcSpan, idInfo) -> do
      case idScope idInfo of
        Imported{idImportSpan} -> do
          tagSpan srcSpan linkTag
          case idImportSpan of
            ProperSpan span -> tagSpan span highlight
            _               -> return ()
        Local{idDefSpan} ->
          case idDefSpan of
            ProperSpan span -> tagSpan span highlight
            _               -> return ()
        _ ->
          return ()
      return $ show idInfo ++ " " ++ haddockLink idInfo

    textBufferSetText idInfoBuff (unlines idInfoText)

  textView `on` keyPressEvent $ tryEvent $ do
    "a" <- eventKeyName
    [Control] <- eventModifier
    mark <- liftIO $ textBufferGetInsert textBuffer
    iter <- liftIO $ textBufferGetIterAtMark textBuffer mark
    line <- liftIO $ textIterGetLine iter
    col  <- liftIO $ textIterGetLineOffset iter
    -- Find the IdInfo for the identifier under the tag.
    idMap <- liftIO $ readIORef idMapRef
    let idInfos = concatMap (idInfoAtLocation (line + 1) (col + 1)) idMap
        root = "http://hackage.haskell.org/packages/archive/"
        isImported (_, info) = case idScope info of
          Imported{} -> True
          _ -> False
    case filter isImported idInfos of
      [] -> do
        -- DEBUG: liftIO $ putStrLn $ root ++ "ha: " ++ show (col, line)
        return ()
      (_, info) : _ -> do
        -- DEBUG: liftIO $ putStrLn $ root ++ haddockLink info
        liftIO $ openUrlBySystemTool $ root ++ haddockLink info

  widgetShowAll window
  mainGUI


-- The original openUrlBySystemTool coredumps for me, so here's hack, after
-- https://github.com/keera-studios/hails-templates/blob/master/src/System/Application.hs.
openUrlBySystemTool :: String -> IO ()
openUrlBySystemTool url = do
#ifdef darwin_HOST_OS
  void . system $ "open " ++ url
#else
  infos <- appInfoGetAllForType "text/html"
  -- DEBUG: print (null infos)
  unless (null infos) $ void $ do
    let exe = appInfoGetExecutable $ head infos
    runProcess exe [url] Nothing Nothing Nothing Nothing Nothing
#endif
