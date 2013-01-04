{-# LANGUAGE RecordWildCards, DoRec #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

import IdeSession
import ModuleName (ModuleName)
import qualified ModuleName

import Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.SourceView

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Maybe
import Data.Monoid
import Control.Monad

import System.Environment
import System.Directory
import System.FilePath

main :: IO ()
main = do
    args <- getArgs
    guiMain args

guiMain :: [String] -> IO ()
guiMain args = do

    let configDir = "./ide-session"
        configStaticOpts  = args

    exists <- doesDirectoryExist configDir
    when exists (removeDirectoryRecursive configDir)
    createDirectoryIfMissing False configDir

    ide <- initSession SessionConfig{..}

    initGUI

    updatesRef <- newIORef ([] :: [(IdeSessionUpdate, IO ())])

    rec gui <- makeGUI GuiActions {..}

        let addUpdate str upd act = do
              let act' = act >> appendOutputLog gui str
              modifyIORef updatesRef (++ [(upd, act')])
              appendUpdateLog gui str

            clearUpdates = do
              writeIORef updatesRef []
              clearUpdateLog gui

            onModuleAdd :: ModuleName -> FilePath -> IO ()
            onModuleAdd mn fp = do
              addUpdate ("updateModuleFromFile (" ++ show mn ++ ") " ++ show fp)
                        (updateModuleFromFile mn fp)
                        (do content <- BS.readFile fp
                            addModuleBuffer gui mn content)

            onModuleDelete :: ModuleName -> IO ()
            onModuleDelete mn = do
              addUpdate ("updateModuleDelete (" ++ show mn ++ ")")
                        (updateModuleDelete mn)
                        (deleteModuleBuffer gui mn)

            onBufferUpdate :: ModuleName -> SourceBuffer -> ByteString -> IO ()
            onBufferUpdate mn buffer content = do
              addUpdate ("updateModule (" ++ show mn ++ ") (data)")
                        (updateModule mn (LBS.fromChunks [content]))
                        (textBufferSetModified buffer False)

            onUpdateSession :: IO ()
            onUpdateSession = do
              updates <- readIORef updatesRef
              clearUpdates
              sequence [ act | (_,act) <- updates ]
              let sessionUpdates = mconcat [ upd | (upd,_) <- updates ]
                  handleProgress p = appendOutputLog gui ("Progress: " ++ show p)
              appendOutputLog gui "updateSession"
              updateSession ide sessionUpdates handleProgress
              errs <- getSourceErrors ide
              appendOutputLog gui ("Errors: " ++ show errs)
              ms <- getLoadedModules ide
              appendOutputLog gui ("Loaded:" ++ show ms)

              return ()

    mainGUI

    shutdownSession ide


data GuiActions = GuiActions {
    onModuleAdd    :: ModuleName -> FilePath -> IO (),
    onModuleDelete :: ModuleName -> IO (),
    onBufferUpdate :: ModuleName -> SourceBuffer -> ByteString -> IO (),
    onUpdateSession :: IO ()
  }

data GUI = GUI {
    mainWindow       :: Window,
    moduleListStore  :: ListStore (ModuleName, SourceBuffer),
    moduleEditor     :: SourceView,
    languageManager  :: SourceLanguageManager,
    haskellLang      :: SourceLanguage,
    updatesListStore :: ListStore String,
    outputLogBuffer  :: TextBuffer
  }

makeGUI :: GuiActions -> IO GUI
makeGUI GuiActions{..} = do
    builder <- Gtk.builderNew
    Gtk.builderAddFromFile builder "IdeGuiTest.ui"

    let getWidget cast name = builderGetObject builder cast name

    mainWindow <- getWidget castToWindow "main_window"
    onDestroy mainWindow mainQuit

    -- buttons etc
    updateSessionButton <- getWidget castToButton "update_session_button"
    addModuleButton     <- getWidget castToButton "add_module_button"
    deleteModuleButton  <- getWidget castToButton "delete_module_button"
    updateBufferButton  <- getWidget castToButton "update_buffer_button"

    -- the module list
    moduleListView   <- getWidget castToTreeView "module_list"
    moduleListStore  <- listStoreNew []
    moduleListSelect <- treeViewGetSelection moduleListView

    columnModName    <- treeViewColumnNew
    cellModName      <- cellRendererTextNew

    treeViewColumnSetTitle  columnModName "Module name"
    treeViewColumnPackStart columnModName cellModName True
    treeViewAppendColumn moduleListView columnModName

    treeViewSetModel moduleListView moduleListStore

    cellLayoutSetAttributes columnModName cellModName moduleListStore $ \(modname, buffer) ->
      [ cellText :=> do modified <- textBufferGetModified buffer
                        let modnamestr = ModuleName.toString modname
                        return (if modified then modnamestr ++ " *" else modnamestr) ]

    -- the updates list
    updatesListView   <- getWidget castToTreeView "updates_list"
    updatesListStore  <- listStoreNew []

    columnUpdName    <- treeViewColumnNew
    cellUpdName      <- cellRendererTextNew

    treeViewColumnSetTitle  columnUpdName "Pending updates"
    treeViewColumnPackStart columnUpdName cellUpdName True
    treeViewAppendColumn updatesListView columnUpdName

    treeViewSetModel updatesListView updatesListStore

    cellLayoutSetAttributes columnUpdName cellUpdName updatesListStore $ \str ->
      [ cellText := str ]

    -- the editor
    panedView    <- getWidget castToPaned "paned"
    moduleEditor <- sourceViewNew
    scrollWin    <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy scrollWin PolicyAutomatic PolicyAutomatic
    scrolledWindowSetShadowType scrollWin ShadowIn
    containerAdd scrollWin moduleEditor
    containerAdd panedView scrollWin
    dummybuffer  <- textViewGetBuffer moduleEditor
    textBufferSetText dummybuffer "(no module selected)"

    -- create the appropriate language
    languageManager <- sourceLanguageManagerNew
    Just haskellLang <- sourceLanguageManagerGetLanguage languageManager "haskell"

    -- output log
    outputLogView <- getWidget castToTextView "outputlog"
    outputLogBuffer <- textViewGetBuffer outputLogView

    -- events
    on addModuleButton buttonActivated $ do
        chooseModuleFile mainWindow onModuleAdd

    on deleteModuleButton buttonActivated $ do
      miter <- treeSelectionGetSelected moduleListSelect
      case miter of
        Nothing   -> return ()
        Just iter -> do
           let modIndex = listStoreIterToIndex iter
           (modname, _) <- listStoreGetValue moduleListStore modIndex
           onModuleDelete modname

    on updateBufferButton buttonActivated $ do
      miter <- treeSelectionGetSelected moduleListSelect
      case miter of
        Nothing   -> return ()
        Just iter -> do
           let modIndex = listStoreIterToIndex iter
           (modname, buffer) <- listStoreGetValue moduleListStore modIndex
           start   <- textBufferGetStartIter buffer
           end     <- textBufferGetEndIter buffer
           content <- textBufferGetByteString buffer start end False
           onBufferUpdate modname buffer content

    on moduleListSelect treeSelectionSelectionChanged $ do
      miter <- treeSelectionGetSelected moduleListSelect
      case miter of
        Nothing   -> do
           textViewSetBuffer moduleEditor dummybuffer
        Just iter -> do
           let modIndex = listStoreIterToIndex iter
           (_, buffer) <- listStoreGetValue moduleListStore modIndex
           textViewSetBuffer moduleEditor buffer

    on updateSessionButton buttonActivated $ onUpdateSession

    widgetShowAll mainWindow

    return GUI{..}

addModuleBuffer :: GUI -> ModuleName -> ByteString -> IO ()
addModuleBuffer GUI{..} modname content = do
    buffer <- sourceBufferNewWithLanguage haskellLang
    textBufferSetByteString buffer content
    textBufferSetModified buffer False

    listStoreAppend moduleListStore (modname, buffer)
    return ()

deleteModuleBuffer :: GUI -> ModuleName -> IO ()
deleteModuleBuffer GUI{..} mn = do
    mns <- listStoreToList moduleListStore
    case lookup mn (zip (map fst mns) [0..]) of
      Just i  -> listStoreRemove moduleListStore i
      Nothing -> return ()

appendUpdateLog :: GUI -> String -> IO ()
appendUpdateLog GUI{..} str = do
    listStoreAppend updatesListStore str
    return ()

appendOutputLog :: GUI -> String -> IO ()
appendOutputLog GUI{..} str = do
    end <- textBufferGetEndIter outputLogBuffer
    textBufferInsert outputLogBuffer end (str ++ "\n")

clearUpdateLog :: GUI -> IO ()
clearUpdateLog GUI{..} =
    listStoreClear updatesListStore

chooseModuleFile :: WindowClass win => win
                 -> (ModuleName -> FilePath -> IO ())
                 -> IO ()
chooseModuleFile parent onAddModule = do
  dialog <- fileChooserDialogNew
    (Just "Add a Haskell module")
    (Just (toWindow parent))
    FileChooserActionOpen
    [ ("Cancel",        ResponseCancel)
    , ("Add module", ResponseAccept) ]

  filt <- fileFilterNew
  fileFilterAddPattern filt "*.hs"

  hbox <- hBoxNew False 0
  lab  <- labelNew (Just "Module name:")
  mentry <- entryNew
  set lab [ miscYalign := 0, miscXpad := 10 ]

  boxPackStart hbox lab PackNatural 0
  boxPackStart hbox mentry PackGrow 0

  set dialog [
      fileChooserFilter := filt,
      windowModal := True,
      fileChooserExtraWidget := hbox
    ]
  fileChooserSetCurrentFolder dialog "."

  onResponse dialog $ \resp -> do
    case resp of
      ResponseAccept -> do
        inp   <- fileChooserGetFilename dialog
        mnameEntry <- get mentry entryText
        let mname = if mnameEntry == ""
                    then "Mo" ++ maybe "dule" takeBaseName inp
                    else mnameEntry
        case inp of
          Just file ->
            let errMsg = "wrong module name '" ++ mname ++ "'"
                mn = fromMaybe (error errMsg) $ ModuleName.fromString mname
            in onAddModule mn file
          Nothing   -> return ()
      _             -> return ()
    widgetDestroy dialog

  widgetShowAll dialog
