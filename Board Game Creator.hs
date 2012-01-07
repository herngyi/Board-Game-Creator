import Graphics.UI.Gtk hiding (Action, toAction, Size)
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.EventM
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.IORef
import Data.Array
import Data.Tree
import Data.List
import Data.Maybe (fromJust)
import Data.Either
import System.Directory (doesFileExist)
import System.IO.Error (try)
import Forest
import Contents

type Size = (Int,Int)

data FullIcon = Icon {image :: Pixbuf, path :: FilePath}

instance Eq FullIcon where
  a == b = (path a) == (path b)

type Pointer = Int

data Event = Player Int Pointer
           | Conditions
           | Root {isSource :: Bool
                  ,choiceRT :: Choice}
           | Actions
           | Cond {pointer :: Pointer
                  ,toCondition :: Condition}
           | Act {toAction :: Action}

instance Show Event where
  show (Player t _) = "Player "++(show t)
  show  Conditions  = "Conditions"
  show (Root _ c)   = show c
  show  Actions     = "Actions\nAll"
  show (Cond _ c)   = condToString c
  show (Act a)      =  actToString a

data Choice = All
            | Choice {fromChoice :: Int}
              deriving Eq

instance Show Choice where
  show All        = "All"
  show (Choice x) = show x

data Condition = Click {getButton :: String
                       ,getClicks :: ([Point],[FullIcon])}
               | Hover {getHovers :: ([Point],[FullIcon])}
               | Keys  {getKeys   :: [String]}
               | Conf  {getConfigs :: [Config]}

instance Enum Condition where
  fromEnum (Click _ _) = 0
  fromEnum (Hover   _) = 1
  fromEnum (Keys    _) = 2
  fromEnum (Conf    _) = 3

  toEnum x = nullConditions !! x

nullConditions = [Click "Left-Click" ([],[])
                 ,Hover ([],[]), Keys [], Conf []]


data Config = Config {icon   :: FullIcon
                     ,choice :: Choice
                     ,points :: [Point]}

data Action = GameEnds
            | NextTurn
            | Transform {pointers  :: [Pointer]
                        ,finalIcon :: Maybe FullIcon}
            | Appear    {pointers  :: [Pointer]
                        ,absPoints :: [Point]
                        ,finalIcon :: Maybe FullIcon}
            | Disappear {pointers  :: [Pointer]}

instance Enum Action where
  fromEnum  GameEnds       = 0
  fromEnum  NextTurn       = 1
  fromEnum (Transform _ _) = 2
  fromEnum (Appear  _ _ _) = 3
  fromEnum (Disappear   _) = 4

  toEnum x = nullActions !! x

nullActions = [GameEnds, NextTurn, Transform [] Nothing
              ,Appear [] [] Nothing, Disappear []]

condToString :: Condition -> String
condToString (Click a (b,c)) =
  let x = case b of
            []  -> ""
            [d] -> "the position " ++ (show d)
            _   -> "any of the positions " ++ (commas $ map show b)
      y = case c of
            []  -> ""
            [_] -> "a game piece"
            _   -> "some game pieces"
      z = if (null x)||(null y) then "" else " and "
   in a ++ " on " ++ x ++ z ++ y

condToString (Hover (b,c)) =
  let x = case b of
            []  -> ""
            [d] -> "the position " ++ (show d)
            _   -> "any of the positions " ++ (commas $ map show b)
      y = case c of
            []  -> ""
            [_] -> "a game piece"
            _   -> "some game pieces"
      z = if (null x)||(null y) then "" else " and "
   in "Cursor hovers over " ++ x ++ z ++ y

condToString (Keys a) =
  case a of
    [b] -> "The key combination "++b++" is depressed"
    _   -> "Any of the key combinations "++(commas a)++" are pressed"

condToString (Conf _) = "Any of the specified configurations are reached"
         
commas :: [String] -> String
commas xs = concat $ intersperse ", " xs

keyToString :: ([Modifier],String) -> String
keyToString (mods,name) =
  let modString = concat $ intersperse "+" (map show mods)
   in if null mods
       then name
       else '<' : modString  ++ "> " ++ name

actToString :: Action -> String
actToString GameEnds = "Game ends"
actToString NextTurn = "Next player's turn"
actToString (Transform xs _) = case xs of
                                 [_] -> "Game piece transforms"
                                 _   -> "Game pieces transform"
actToString (Appear   _ _ _) = "Game piece appears"
actToString (Disappear xs  ) = case xs of
                                 [_] -> "Game piece disappears"
                                 _   -> "Game pieces disappear"

main :: IO ()
main = do

  initGUI
  Just xml <- xmlNew "Glade Files/Main Creator.glade"                

-- Infrastructure and Global Variables

  iconList  <- newIORef []
  iconSize  <- newIORef (0,0)
  gridSize  <- newIORef (0,0)
  players   <- newIORef 2

  px <- pixbufNewFromFile "Images/Empty.bmp"
  ePixel <- pixbufAddAlpha px Nothing
  glass    <- pixbufCopy ePixel
  let eIcon = Icon ePixel ""
  let gIcon = Icon glass  ""
  pixbufFill glass 0 0 0 0
  speck <- pixbufScaleSimple glass 1 1 InterpNearest
  emptyIcon <- newIORef eIcon
  glassIcon <- newIORef gIcon
  emptyArray <- newIORef $ array ((1,1),(1,1)) [((1,1),eIcon)]
  pieceArray <- newIORef $ array ((1,1),(1,1)) [((1,1),[])]

  red <- pixbufNewFromFile "Images/Red Pixel.bmp"
  highlight <- newIORef red

  -- The following block is for use in Conditions and Actions

  events   <- newIORef []
  currCond <- newIORef []
  keys     <- newIORef []
  configs  <- newIORef []
  prevIndex   <- newIORef 0
  gamePieces  <- newIORef []
  boardPoints <- newIORef []
  currAct     <- newIORef []
  filteredConds <- newIORef []

  selAll <- pixbufNewFromFile "Images/Select All.bmp"
  desAll <- pixbufNewFromFile "Images/Deselect All.bmp"

  window <- xmlGetWidget xml castToWindow "window1"

-- Help (Contents)

  help <- xmlGetWidget xml castToButton "button10"

  hWindow   <- xmlGetWidget xml castToWindow "window2"
  hClose    <- xmlGetWidget xml castToButton "button11"
  hDisplay  <- xmlGetWidget xml castToButton "button12"
  hContents <- xmlGetWidget xml castToTreeView "treeview2"
  hText     <- xmlGetWidget xml castToTextView "textview1"
  onDestroy hWindow mainQuit
  onClicked hClose (widgetHide hWindow)

  contCol <- treeViewColumnNew
  contRen <- cellRendererTextNew
  treeViewColumnSetTitle contCol " Contents "
  treeViewColumnPackStart contCol contRen False
  treeViewAppendColumn hContents contCol
  contStore <- treeStoreNew contents
  treeViewSetModel hContents contStore
  cellLayoutSetAttributes contCol contRen contStore $ string fst
  treeViewExpandAll hContents

  hBuffer <- textViewGetBuffer hText
  textBufferSetText hBuffer txt_Intro

  onRowActivated hContents $ \path _ -> do
    let (_,txt) = forestGetValue contents path
    textBufferSetText hBuffer txt

  contSel <- treeViewGetSelection hContents
  onSelectionChanged contSel $ do
    paths <- treeSelectionGetSelectedRows contSel
    case paths of
      [path] -> widgetSetSensitive hDisplay True
      _      -> widgetSetSensitive hDisplay False

  onClicked hDisplay $ do
    [path] <- treeSelectionGetSelectedRows contSel
    let (_,txt) = forestGetValue contents path
    textBufferSetText hBuffer txt

  onClicked help $ do
    windowPresent hWindow

-- Main Creater

  onDestroy window (widgetDestroy hWindow >> mainQuit)

-- WIDGETS
-- Page 1

  imageSizeLabel <- xmlGetWidget xml castToLabel "label5"
  editImageSize  <- xmlGetWidget xml castToButton "button1"

  vBox <- xmlGetWidget xml castToVBox "vbox4"
  chooser <- fileChooserWidgetNew FileChooserActionOpen

  sifter <- fileFilterNew
  fileFilterAddPixbufFormats sifter
  fileFilterSetName sifter "Supported Image Files"

  add    <- xmlGetWidget xml castToButton "button3"
  remove <- xmlGetWidget xml castToButton "button4"
  clear  <- xmlGetWidget xml castToButton "button2"

  iconView <- xmlGetWidget xml castToIconView "iconview4"
  set iconView [iconViewSelectionMode := SelectionMultiple
               ,iconViewPixbufColumn := iconDisp]

-- Page 2

  boardNote <- xmlGetWidget xml castToNotebook "notebook2"

  boardSizeLabel <- xmlGetWidget xml castToLabel "label8"
  editBoardSize  <- xmlGetWidget xml castToButton "button5"

  iconMenu <- xmlGetWidget xml castToIconView "iconview1"
  iconViewSetPixbufColumn iconMenu iconDisp

  emptyGrid <- xmlGetWidget xml castToIconView "iconview2"
  set emptyGrid [iconViewSelectionMode := SelectionMultiple
                ,iconViewPixbufColumn := iconDisp]

  pieceGrid <- xmlGetWidget xml castToIconView "iconview3"
  set pieceGrid [iconViewSelectionMode := SelectionMultiple
                ,iconViewPixbufColumn := iconDisp]

-- Page 3

  eventView <- xmlGetWidget xml castToTreeView "treeview1"
  eAddCombo <- xmlGetWidget xml castToComboBox "combobox1"
  eAdd      <- xmlGetWidget xml castToButton "button6"
  eRemove   <- xmlGetWidget xml castToButton "button7"
  eEdit     <- xmlGetWidget xml castToButton "button8"
  eventSel  <- treeViewGetSelection eventView

-- EVENTS AND SETTINGS
-- Page 1

  fileChooserSetSelectMultiple chooser True
  fileChooserAddFilter chooser sifter
  boxPackStart vBox chooser PackGrow 0

  let updateIcons :: [FullIcon] -> IO ()
      updateIcons icons = do
        store <- listStoreNew icons
        treeModelSetColumn store iconDisp image
        iconViewSetModel iconView (Just store)
        iconViewSetModel iconMenu (Just store)

      updateEmptyGrid :: Array Point FullIcon -> IO ()
      updateEmptyGrid arr = do
        store <- listStoreNew (elems arr)
        treeModelSetColumn store iconDisp image
        iconViewSetModel emptyGrid (Just store)

      updatePieceGrid :: Array Point FullIcon
                      -> Array Point [FullIcon]
                      -> IO ()
      updatePieceGrid empties arr = do
        size <- readIORef iconSize
        store <- listStoreNew =<< compose empties arr size
        treeModelSetColumn store iconDisp id
        iconViewSetModel pieceGrid (Just store)

      compose :: Array Point FullIcon
              -> Array Point [FullIcon]
              -> Size
              -> IO [Pixbuf]
      compose arr1 arr2 size = do
        mapM (stack size) $ zip (elems arr1) (elems arr2)

      stack :: Size -> (FullIcon,[FullIcon]) -> IO Pixbuf
      stack size (tile,pieces) = do
        c <- pixbufCopy (image tile)
        forM_ (reverse pieces) $ \i -> blend (image i) c size 255
        return c

  treeSelectionSetMode eventSel SelectionMultiple
  textCol <- treeViewColumnNew
  textRen <- cellRendererTextNew
  treeViewColumnPackStart textCol textRen False
  treeViewAppendColumn eventView textCol

  let updateEventView :: Forest Event -> IO ()
      updateEventView events = do
        store <- treeStoreNew events
        treeViewSetModel eventView store
        cellLayoutSetAttributes textCol textRen store $ string show
        treeViewExpandAll eventView

      clearPageThree = do
        writeIORef events   []
        writeIORef currCond []
        writeIORef keys     []
        writeIORef configs  []
        writeIORef prevIndex 0
        writeIORef gamePieces []
        writeIORef boardPoints []
        writeIORef currAct     []
        updateEventView []

  turnsLabel <- xmlGetWidget xml castToLabel "label9"
  turns <- xmlGetWidget xml castToButton "button9"

  onClicked turns $ do
    initGUI
    Just txml <- xmlNew "Glade Files/Minor Dialogs/Player Number Dialog.glade"
    tWindow <- xmlGetWidget txml castToWindow "window1"
    tOk     <- xmlGetWidget txml castToButton "button1"
    tCancel <- xmlGetWidget txml castToButton "button2"
    tSpin   <- xmlGetWidget txml castToSpinButton "spinbutton1"
    onDestroy tWindow mainQuit
    onClicked tCancel $ widgetDestroy tWindow
    z <- readIORef players
    spinButtonSetValue tSpin (fromIntegral z)
    onClicked tOk $ do
      x' <- spinButtonGetValue tSpin
      let x = truncate x'
      y <- readIORef players
      if x < y
       then
         confirmation
           tWindow
           "Decrease Player Number"
           "WARNING: Decreasing the number of players will\n\
           \irreversibly clear all data in the \"Game Events\"\n\
           \tab. Are you sure you wish to continue?" $ do
             writeIORef players x
             clearPageThree
             widgetDestroy tWindow
       else do
         writeIORef players x
         labelSetText turnsLabel $ show x ++ " Players"
         widgetDestroy tWindow
    widgetShowAll tWindow
    mainGUI

  onClicked editImageSize $ do
    initGUI
    Just ixml <- xmlNew "Glade Files/Minor Dialogs/Edit Image Size Dialog.glade"
    iWindow <- xmlGetWidget ixml castToWindow "window1"
    iApply  <- xmlGetWidget ixml castToButton "button1"
    iCancel <- xmlGetWidget ixml castToButton "button2"
    iSpinW  <- xmlGetWidget ixml castToSpinButton "spinbutton1"
    iSpinH  <- xmlGetWidget ixml castToSpinButton "spinbutton2"
    windowSetTransientFor iWindow window
    onDestroy iWindow mainQuit
    onClicked iCancel $ widgetDestroy iWindow
    (a,b) <- readIORef iconSize
    unless (a == 0) $ do
      spinButtonSetValue iSpinW (fromIntegral a)
      spinButtonSetValue iSpinH (fromIntegral b)
    onClicked iApply $ do
      w' <- spinButtonGetValue iSpinW
      h' <- spinButtonGetValue iSpinH
      let w = truncate w'
          h = truncate h'
      writeIORef iconSize (w,h)
      labelSetText imageSizeLabel $ "Image Size: "++(show w)
                                 ++" by "++(show h)++" Pixels"
      Icon e _ <- readIORef emptyIcon
      Icon g _ <- readIORef glassIcon
      r        <- readIORef highlight
      e' <- pixbufScaleSimple e w h InterpNearest
      g' <- pixbufScaleSimple g w h InterpNearest
      r' <- pixbufScaleSimple r w h InterpNearest
      writeIORef emptyIcon $ Icon e' ""
      writeIORef glassIcon $ Icon g' ""
      writeIORef highlight r'
      oldIcons <- readIORef iconList
      newIcons <- mapScale w h oldIcons
      writeIORef iconList newIcons
      updateIcons newIcons
      oldArray <- readIORef emptyArray
      newArray <- scaleEmpty oldArray w h
      writeIORef emptyArray newArray
      oldArray' <- readIORef pieceArray
      newArray' <- scalePiece oldArray' w h
      writeIORef pieceArray newArray'
      (r,_) <- readIORef gridSize
      unless (r == 0) $ do
         updateEmptyGrid newArray
         updatePieceGrid newArray newArray'
      widgetSetSensitivity add True
      widgetDestroy iWindow
    widgetShowAll iWindow
    mainGUI

  let removers = [remove, clear]

  on iconView selectionChanged $ do
    paths <- iconViewGetSelectedItems iconView
    if null paths
     then widgetSetSensitivity remove False
     else widgetSetSensitivity remove True

  onClicked add $ do
    (w,h)    <- readIORef iconSize
    newPaths <- fileChooserGetFilenames chooser
    oldIcons <- readIORef iconList
    let addedPaths  = newPaths \\ (map path oldIcons)
    blackSheep <- mapM checkFile addedPaths
    let whiteOnes = filter fst blackSheep
        pathToIconPair x = do
          p <- try (pixbufNewFromFileAtScale x w h False)
          case p of
            Right y -> return $ Right (Icon y x)
            Left  s -> return $ Left  (show s,x)
    newIcons <- mapM pathToIconPair $ map snd whiteOnes
    case lefts newIcons of
      [] -> do
        let finalIcons = oldIcons ++ (rights newIcons)
        writeIORef iconList finalIcons
        paths <- iconViewGetSelectedItems iconView
        if null paths
         then widgetSetSensitivity remove False
         else widgetSetSensitivity remove True
        if null finalIcons
         then widgetSetSensitivity clear False
         else widgetSetSensitivity clear True
        updateIcons finalIcons
        mapM_ (iconViewSelectPath iconView) paths
      ls -> do
        let stitch (s,p) = "\nPath: "++p++"\n\nError: "++s
            errors = map stitch ls
            l = length ls
            pl = if l == 1 then "" else "s"
            message = (show l) ++ "of the selected files gave an\
                     \ error during import. The following lists the\
                     \ offending file path"++pl
                         ++" and their error"++pl++":\n"++(concat errors)
        errorDialog window "Import Error" message

  onClicked remove $
    confirmation
      window
      "Remove Images"
      "WARNING: Removing images will remove their copies\n\
      \in the \"Initial Board\" tab and irreversibly clear all data\n\
      \in the \"Game Events\" tab. Are you sure you want to\n\
      \remove these image(s)?" $ do
        paths <- iconViewGetSelectedItems iconView
        oldIcons  <- readIORef iconList
        let blackList = concat paths
            newIcons = removal oldIcons blackList
            removed = map (oldIcons !!) blackList
        writeIORef iconList newIcons
        updateIcons newIcons
        (r,_) <- readIORef gridSize
        unless (r == 0) $ do
          empty <- readIORef emptyIcon
          oldBoard  <- readIORef emptyArray
          oldPieces <- readIORef pieceArray
          let check p = if p `elem` removed
                         then empty
                         else p
              newBoard  = fmap check oldBoard
              newPieces = fmap (filter (`notElem` removed)) oldPieces
          writeIORef emptyArray newBoard
          writeIORef pieceArray newPieces
          updateEmptyGrid newBoard
          updatePieceGrid newBoard newPieces
        widgetSetSensitivity remove False
        if null newIcons
         then widgetSetSensitivity clear False
         else widgetSetSensitivity clear True

  onClicked clear $
    confirmation
      window
      "Clear Selection"
      "WARNING: Clearing your selection empties the boards\n\
      \in the \"Inital Board\" tab and irreversibly clears all data\n\
      \in the \"Game Events\" tab. Are you sure you want to\n\
      \clear your selection?" $ do
        setSensitivities removers [False,False]
        writeIORef iconList []
        updateIcons []
        (r,_) <- readIORef gridSize
        unless (r == 0) $ do
          oldBoard  <- readIORef emptyArray
          oldPieces <- readIORef pieceArray
          empty <- readIORef emptyIcon
          let newBoard  = fmap (const empty) oldBoard
              newPieces = fmap (const  []  ) oldPieces
          writeIORef emptyArray newBoard
          writeIORef pieceArray newPieces
          updateEmptyGrid newBoard
          updatePieceGrid newBoard newPieces
          clearPageThree

  onSizeAllocate iconView $ \(Rectangle _ _ width _ ) -> do
    (w,_) <- readIORef iconSize
    let n = truncate $ (fromIntegral width - 25)/(fromIntegral w + 2)
    iconViewSetColumns iconMenu n

-- Page 2

  onClicked editBoardSize $ do
    initGUI
    Just bxml <- xmlNew "Glade Files/Minor Dialogs/Edit Board Size Dialog.glade"
    bWindow <- xmlGetWidget bxml castToWindow "window1"
    bApply  <- xmlGetWidget bxml castToButton "button2"
    bCancel <- xmlGetWidget bxml castToButton "button1"
    bSpinR  <- xmlGetWidget bxml castToSpinButton "spinbutton1"
    bSpinC  <- xmlGetWidget bxml castToSpinButton "spinbutton2"
    windowSetTransientFor bWindow window
    onDestroy bWindow mainQuit
    (s,t) <- readIORef gridSize
    unless (s == 0) $ do 
      spinButtonSetValue bSpinR (fromIntegral s)
      spinButtonSetValue bSpinC (fromIntegral t)
    onClicked bCancel $ widgetDestroy bWindow
    onClicked bApply $ do
      r' <- spinButtonGetValue bSpinR
      c' <- spinButtonGetValue bSpinC
      let r = truncate r'
          c = truncate c'
      writeIORef gridSize (r,c)
      labelSetText boardSizeLabel $ "Board Size: "++(show r)
                                  ++" Rows and "++(show c)++" Columns"
      empty <- readIORef emptyIcon
      oldArray <- readIORef emptyArray
      let (a,b) = (snd.bounds) oldArray
          picElem x y arr e = if (x <= a)&&(y <= b)
                               then arr ! (x,y)
                               else e
          newArray = array
                       ((1,1),(r,c))
                       [((x,y), picElem x y oldArray empty) | x <- [1..r], y <- [1..c]]
      writeIORef emptyArray newArray
      iconViewSetColumns emptyGrid c
      oldArray' <- readIORef pieceArray
      let (_,(a,b)) = bounds oldArray'
          newArray' = array
                        ((1,1),(r,c))
                        [((x,y), picElem x y oldArray' []) | x <- [1..r], y <- [1..c]]
      writeIORef pieceArray newArray'
      iconViewSetColumns pieceGrid c
      (w,_) <- readIORef iconSize
      unless (w == 0) $ do
        updateEmptyGrid newArray
        updatePieceGrid newArray newArray'
      widgetDestroy bWindow
    widgetShowAll bWindow
    mainGUI

  on iconMenu itemActivated $ \[x] -> do
    icons <- readIORef iconList
    page  <- notebookGetCurrentPage boardNote
    case page of
      0 -> do
        treePaths <- iconViewGetSelectedItems emptyGrid
        oldArray <- readIORef emptyArray
        let newArray = replaceArrayElem oldArray treePaths (icons !! x)
        writeIORef emptyArray newArray
        updateEmptyGrid newArray
        pArr <- readIORef pieceArray
        updatePieceGrid newArray pArr
      1 -> do
        treePaths <- iconViewGetSelectedItems pieceGrid
        oldArray <- readIORef pieceArray
        let newArray = funcOnPieceArray ((icons !! x):) oldArray treePaths
        writeIORef pieceArray newArray
        empties <- readIORef emptyArray
        updatePieceGrid empties newArray
      _ -> return ()

  onSizeAllocate iconMenu $ \(Rectangle _ _ width _ ) -> do
    (w,_) <- readIORef iconSize
    let n = truncate $ (fromIntegral width - 25)/(fromIntegral w + 2)
    iconViewSetColumns iconMenu n

  on emptyGrid keyPressEvent $ do
    name <- eventKeyName
    case name of
      "Delete" -> liftIO $ do
        treePaths <- iconViewGetSelectedItems emptyGrid
        empty     <- readIORef emptyIcon
        oldArray  <- readIORef emptyArray
        let newArray = replaceArrayElem oldArray treePaths empty
        writeIORef emptyArray newArray
        updateEmptyGrid newArray
        return True
      _ -> return False

  on pieceGrid keyPressEvent $ do
    name <- eventKeyName
    case name of
      "Delete" -> liftIO $ do
        treePaths <- iconViewGetSelectedItems pieceGrid
        glass     <- readIORef glassIcon
        oldArray  <- readIORef pieceArray
        let newArray = funcOnPieceArray (drop 1) oldArray treePaths
        writeIORef pieceArray newArray
        empties <- readIORef emptyArray
        updatePieceGrid empties newArray
        return True
      _ -> return False

-- Page 3

  comboBoxSetActive eAddCombo 0

  let getColored :: Array Point FullIcon
                 -> [Point]
                 -> Size
                 -> IO [Pixbuf]
      getColored board ps size =
        let pics = map image (elems board)
            zipped = zip [0..] pics
            tags = pointsToIndices ps (col board)
         in do red <- readIORef highlight
               color tags red size zipped

      editBoardPoints :: Window -> Int -> [Point] -> IO ()
      editBoardPoints win i ps = do
        initGUI
        Just pxml <- xmlNew "Glade Files/Conditions/Board Positions Dialog.glade"
        pWindow <- xmlGetWidget pxml castToWindow "window1"
        pAdd    <- xmlGetWidget pxml castToButton "button1"
        pRemove <- xmlGetWidget pxml castToButton "button2"
        pOk     <- xmlGetWidget pxml castToButton "button3"
        pCancel <- xmlGetWidget pxml castToButton "button4"
        pBoard  <- xmlGetWidget pxml castToIconView "iconview1"
        onDestroy pWindow mainQuit
        onClicked pCancel $ widgetDestroy pWindow
        board <- readIORef emptyArray
        red   <- readIORef highlight
        size  <- readIORef iconSize
        let c = col board
        set pBoard [iconViewSelectionMode := SelectionMultiple
                   ,iconViewPixbufColumn := iconDisp
                   ,iconViewColumns := c]
        let updateBoard ps = do
              store <- listStoreNew =<< getColored board ps size
              treeModelSetColumn store iconDisp id
              iconViewSetModel pBoard (Just store)
        writeIORef boardPoints ps
        updateBoard ps
        onClicked pAdd $ do
          paths <- iconViewGetSelectedItems pBoard
          oldPoints <- readIORef boardPoints
          let newPoints = union oldPoints (treePathsToPoints paths c)
          writeIORef boardPoints newPoints
          updateBoard newPoints
        onClicked pRemove $ do
          paths <- iconViewGetSelectedItems pBoard
          oldPoints <- readIORef boardPoints
          let points = indicesToPoints (concat paths) c
              newPoints = oldPoints \\ points
          writeIORef boardPoints newPoints
          updateBoard newPoints
        on pBoard itemActivated $ \path -> do
          oldPoints <- readIORef boardPoints
          let a@[point] = treePathsToPoints [path] c
              newPoints = if point `elem` oldPoints
                           then oldPoints \\ a
                           else point:oldPoints
          writeIORef boardPoints newPoints
          updateBoard newPoints
        onClicked pOk $ do
          points <- readIORef boardPoints
          if i == 0
           then modifyIORef currCond $ \((Click a (_,b)):xs) ->
                                            (Click a (points,b)):xs
           else modifyIORef currCond $ \(x:(Hover (_,b)):xs) ->
                                            x:(Hover (points,b)):xs
          widgetDestroy pWindow
        widgetShowAll pWindow
        mainGUI

  selCol <- treeViewColumnNew
  picCol <- treeViewColumnNew
  treeViewColumnSetTitle selCol " Chosen? "
  treeViewColumnSetTitle picCol " Game Pieces "
  selRen <- cellRendererToggleNew
  picRen <- cellRendererPixbufNew
  treeViewColumnPackStart selCol selRen False
  treeViewColumnPackStart picCol picRen True

  let editGamePieces :: Window -> Int -> [FullIcon] -> IO ()
      editGamePieces win i fs = do
        initGUI
        Just gxml <- xmlNew "Glade Files/Conditions/Game Pieces Dialog.glade"
        gWindow <- xmlGetWidget gxml castToWindow "window1"
        gOk     <- xmlGetWidget gxml castToButton "button1"
        gCancel <- xmlGetWidget gxml castToButton "button2"
        gSelAll <- xmlGetWidget gxml castToButton "button3"
        gDesAll <- xmlGetWidget gxml castToButton "button4"
        gSelImg <- xmlGetWidget gxml castToImage "image3"
        gDesImg <- xmlGetWidget gxml castToImage "image4"
        gpView  <- xmlGetWidget gxml castToTreeView "treeview1"
        onClicked gCancel $ widgetDestroy gWindow
        gpSel <- treeViewGetSelection gpView
        imageSetFromPixbuf gSelImg selAll
        imageSetFromPixbuf gDesImg desAll
        treeSelectionSetMode gpSel SelectionMultiple
        treeViewAppendColumn gpView selCol
        treeViewAppendColumn gpView picCol
        icons <- readIORef iconList
        let updateGPView gps = do
              paths <- treeSelectionGetSelectedRows gpSel
              store <- treeStoreNew gps
              treeViewSetModel gpView store
              cellLayoutSetAttributes selCol selRen store $ toggle fst
              cellLayoutSetAttributes picCol picRen store $ pixbuf (image.snd)
              selectPaths gpSel paths
            gps = map toggleState icons
                    where toggleState x = leaf (x `elem` fs,x)
        writeIORef gamePieces gps
        updateGPView gps
        cid <- on selRen cellToggled $ \str -> do
                 oldGPs <- readIORef gamePieces
                 let x      = (read str)::Int
                     (a,b)  = rootLabel (oldGPs !! x)
                     newGPs = replace [leaf (not a,b)] oldGPs x
                 writeIORef gamePieces newGPs
                 updateGPView newGPs
        let replaceFst b (_,p) = (b,p)
        onClicked gSelAll $ do
          oldGPs <- readIORef gamePieces
          let newGPs = forestMap (replaceFst True) oldGPs
          writeIORef gamePieces newGPs
          updateGPView newGPs
        onClicked gDesAll $ do
          oldGPs <- readIORef gamePieces
          let newGPs = forestMap (replaceFst False) oldGPs
          writeIORef gamePieces newGPs
          updateGPView newGPs
        onClicked gOk $ do
          gps <- readIORef gamePieces
          let wanted = map snd $ filter fst (map rootLabel gps)
          case i of
            0 -> modifyIORef currCond $ \((Click a (b,_)):xs) ->
                                             ((Click a (b,wanted)):xs)
            1 -> modifyIORef currCond $ \(x:(Hover (b,_)):xs) ->
                                             (x:(Hover (b,wanted)):xs)
          widgetDestroy gWindow
        onDestroy gWindow $ signalDisconnect cid >> mainQuit
        widgetShowAll gWindow
        mainGUI

  keyCol <- treeViewColumnNew
  treeViewColumnSetTitle keyCol " Key Combination "
  keyRen <- cellRendererTextNew
  treeViewColumnPackStart keyCol keyRen False
       
  let editKeys :: [String] -> IO ()
      editKeys ks = do
        initGUI
        Just kxml <- xmlNew "Glade Files/Conditions/Keys Dialog.glade"
        kWindow <- xmlGetWidget kxml castToWindow "window1"
        kOk     <- xmlGetWidget kxml castToButton "button1"
        kCancel <- xmlGetWidget kxml castToButton "button2"
        kAdd    <- xmlGetWidget kxml castToButton "button3"
        kRemove <- xmlGetWidget kxml castToButton "button4"
        keyView <- xmlGetWidget kxml castToTreeView "treeview1"
        onDestroy kWindow mainQuit
        onClicked kCancel $ widgetDestroy kWindow
        keySel <- treeViewGetSelection keyView
        treeViewAppendColumn keyView keyCol
        let updateKeyView keys = do
              store <- treeStoreNew keys
              treeViewSetModel keyView store
              cellLayoutSetAttributes keyCol keyRen store $ string id
            keyChain = listToForest ks
        writeIORef keys keyChain
        updateKeyView keyChain
        onSelectionChanged keySel $ do
          paths <- treeSelectionGetSelectedRows keySel
          if null paths
           then widgetSetSensitivity kRemove False
           else widgetSetSensitivity kRemove True
        on keyView keyPressEvent $ do
          paths <- liftIO $ treeSelectionGetSelectedRows keySel
          case paths of
            []     -> return False
            [path@[x]] -> do
              name <- eventKeyName
              if '_' `elem` name
               then return False
               else do
                 mods <- eventModifier 
                 liftIO $ do
                   oldKeys <- readIORef keys
                   let addedKey = leaf $ keyToString (mods,name)
                   if addedKey `elem` oldKeys
                    then errorDialog
                           kWindow
                           "Repeated Combination"
                           "The depressed key combination has already been listed.\n\
                           \Please try again with another key combination."
                    else do
                      let newKeys = replace [addedKey] oldKeys x
                      writeIORef keys newKeys
                      updateKeyView newKeys
                      treeSelectionSelectPath keySel path
                 return True
        onClicked kAdd $ do
          oldKeys <- readIORef keys
          let newKeys = oldKeys ++ [leaf ("New Key")]
          writeIORef keys newKeys
          paths <- treeSelectionGetSelectedRows keySel
          updateKeyView newKeys
          unless (null paths) $ let [path] = paths
                                 in treeSelectionSelectPath keySel path
        onClicked kRemove $ do
          oldKeys <- readIORef keys
          paths <- treeSelectionGetSelectedRows keySel
          let newKeys = removal oldKeys (concat paths)
          writeIORef keys newKeys
          updateKeyView newKeys
        onClicked kOk $ do
          keyChain <- readIORef keys
          let realKeys = filter (/= "New Key") $ map rootLabel keyChain
          modifyIORef currCond $ \c -> replace [Keys realKeys] c 2
          widgetDestroy kWindow
        widgetShowAll kWindow
        mainGUI

  picRen'   <- cellRendererPixbufNew
  choiceRen <- cellRendererTextNew

  let editSingleConfig :: (Forest Config -> IO ()) -> Maybe (Int,Config) -> IO ()
      editSingleConfig update j = do
        initGUI
        Just sxml <- xmlNew "Glade Files/Conditions/Config Editor Dialog.glade"
        sWindow <- xmlGetWidget sxml castToWindow "window1"
        sOk     <- xmlGetWidget sxml castToButton "button1"
        sCancel <- xmlGetWidget sxml castToButton "button2"
        sAdd    <- xmlGetWidget sxml castToButton "button3"
        sRemove <- xmlGetWidget sxml castToButton "button4"
        sConfig <- xmlGetWidget sxml castToIconView "iconview1"
        sPieces <- xmlGetWidget sxml castToComboBox "combobox1"
        sAlign  <- xmlGetWidget sxml castToAlignment "alignment3"
        onDestroy sWindow mainQuit
        onClicked sCancel $ widgetDestroy sWindow
        icons  <- readIORef iconList
        pieces <- listStoreNew icons
        cellLayoutPackStart sPieces picRen' False
        cellLayoutSetAttributes sPieces picRen' pieces $ pixbuf image
        comboBoxSetModel sPieces (Just pieces)
        case j of
          Nothing    -> comboBoxSetActive sPieces 0
          Just (_,x) -> case (icon x)`elemIndex` icons of
                          Nothing -> comboBoxSetActive sPieces 0
                          Just y  -> comboBoxSetActive sPieces y
        sChoice <- comboBoxNew
        cellLayoutPackStart sChoice choiceRen False
        containerAdd sAlign sChoice
        board <- readIORef emptyArray
        let (r,c) = (snd.bounds) board
        set sConfig [iconViewSelectionMode := SelectionMultiple
                    ,iconViewPixbufColumn := iconDisp
                    ,iconViewColumns := c]
        size <- readIORef iconSize
        let updateConfig :: [Point] -> IO ()
            updateConfig ps = do
              choices <- listStoreNew $ "All":(map show [1..(length ps - 1)])
              comboBoxSetModel sChoice (Just choices)
              cellLayoutSetAttributes sChoice choiceRen choices $ string id
              comboBoxSetActive sChoice 0
              store <- listStoreNew =<< getColored board ps size
              treeModelSetColumn store iconDisp id
              iconViewSetModel sConfig (Just store)
        case j of
          Nothing -> do
            writeIORef boardPoints []
            updateConfig []
          Just x  -> do
            let ps = (points.snd) x
            writeIORef boardPoints ps
            updateConfig ps
        onClicked sAdd $ do
          paths <- iconViewGetSelectedItems sConfig
          oldPoints <- readIORef boardPoints
          let newPoints = union oldPoints (treePathsToPoints paths c)
          writeIORef boardPoints newPoints
          updateConfig newPoints
        onClicked sRemove $ do
          paths <- iconViewGetSelectedItems sConfig
          oldPoints <- readIORef boardPoints
          let points = indicesToPoints (concat paths) c
              newPoints = oldPoints \\ points
          writeIORef boardPoints newPoints
          updateConfig newPoints
        on sConfig itemActivated $ \path -> do
          oldPoints <- readIORef boardPoints
          let a@[point] = treePathsToPoints [path] c
              newPoints = if point `elem` oldPoints
                           then oldPoints \\ a
                           else point:oldPoints
          writeIORef boardPoints newPoints
          updateConfig newPoints
        onClicked sOk $ do
          points <- readIORef boardPoints
          if null points
           then errorDialog
                  sWindow
                  "Positions Needed"
                  "There must be at least one position selected.\n\
                  \Please Select some positions and try again."
           else do
             oldConfigs <- readIORef configs
             icons <- readIORef iconList
             x <- comboBoxGetActive sPieces
             y <- comboBoxGetActive sChoice
             let added = Config (icons !! x) (indexToChoice y) points
                 newConfigs =
                   case j of
                     Nothing    -> oldConfigs ++ [leaf added]
                     Just (i,_) -> replace [leaf added] oldConfigs i
             writeIORef configs newConfigs
             update newConfigs
             widgetDestroy sWindow
        widgetShowAll sWindow
        mainGUI

  imageCol <- treeViewColumnNew
  pointCol <- treeViewColumnNew
  treeViewColumnSetTitle imageCol " Piece "
  treeViewColumnSetTitle pointCol " Positon Information "
  imageRen <- cellRendererPixbufNew
  pointRen <- cellRendererTextNew
  treeViewColumnPackStart imageCol imageRen False
  treeViewColumnPackStart pointCol pointRen False

  let editConfigs :: Window -> [Config] -> IO ()
      editConfigs win cs = do
        initGUI
        Just cxml <- xmlNew "Glade Files/Conditions/Configurations Dialog.glade"
        cWindow <- xmlGetWidget cxml castToWindow "window1"
        cOk     <- xmlGetWidget cxml castToButton "button1"
        cCancel <- xmlGetWidget cxml castToButton "button2"
        cAdd    <- xmlGetWidget cxml castToButton "button3"
        cRemove <- xmlGetWidget cxml castToButton "button4"
        cEdit   <- xmlGetWidget cxml castToButton "button5"
        cConfig <- xmlGetWidget cxml castToTreeView "treeview1"
        onDestroy cWindow mainQuit
        onClicked cCancel $ widgetDestroy cWindow
        configSel <- treeViewGetSelection cConfig
        treeSelectionSetMode configSel SelectionMultiple
        treeViewAppendColumn cConfig imageCol
        treeViewAppendColumn cConfig pointCol
        let updateConfigView :: Forest Config -> IO ()
            updateConfigView configs = do
              store <- treeStoreNew configs
              treeViewSetModel cConfig store
              cellLayoutSetAttributes imageCol imageRen store $ pixbuf (image.icon)
              cellLayoutSetAttributes pointCol pointRen store $ string configShowPoints
            figTrees = listToForest cs
        writeIORef configs figTrees
        updateConfigView figTrees
        let buttons = [cRemove,cEdit]
        onSelectionChanged configSel $ do
          paths <- treeSelectionGetSelectedRows configSel
          case paths of
           []  -> setSensitivities buttons [False,False]
           [_] -> setSensitivities buttons [True ,True ]
           _   -> setSensitivities buttons [True ,False]
        onClicked cAdd $ do
          paths <- treeSelectionGetSelectedRows configSel
          editSingleConfig updateConfigView Nothing
          selectPaths configSel paths
        onClicked cRemove $ do
          paths <- treeSelectionGetSelectedRows configSel
          oldConfigs <- readIORef configs
          let newConfigs = removal oldConfigs (concat paths)
          writeIORef configs newConfigs
          updateConfigView newConfigs
        onClicked cEdit $ do
          [y@[x]] <- treeSelectionGetSelectedRows configSel
          oldConfigs <- readIORef configs
          let chosen = (x, rootLabel $ oldConfigs !! x)
          editSingleConfig updateConfigView (Just chosen)
          treeSelectionSelectPath configSel y
        onClicked cOk $ do
          figTrees <- readIORef configs
          let newConf = Conf $ map rootLabel figTrees
          modifyIORef currCond $ \c -> replace [newConf] c 3
          widgetDestroy cWindow
        widgetShowAll cWindow
        mainGUI

      boardCheck :: Window -> IO () -> IO ()
      boardCheck win code = do
        (r,_) <- readIORef gridSize
        (w,_) <- readIORef iconSize
        if r * w == 0
         then
           errorDialog
             win
             "No Board"
             "Please create a board and try again."
         else code

      pieceCheck :: Window -> IO () -> IO ()
      pieceCheck win code = do
        icons <- readIORef iconList
        if null icons
         then
           errorDialog
             win
             "No Game Pieces"
             "Please add some game pieces and try again."
         else code

      buttons = ["Left-Click","Right-Click"]

      editCondition :: TreePath -> Maybe (Int,Condition) -> IO ()
      editCondition path j = do
        initGUI
        Just cxml <- xmlNew "Glade Files/Conditions/Conditions Dialog.glade"
        cWindow <- xmlGetWidget cxml castToWindow "window1"
        cConfig <- xmlGetWidget cxml castToButton "button1"
        cOk     <- xmlGetWidget cxml castToButton "button2"
        cCancel <- xmlGetWidget cxml castToButton "button3"
        cKeys   <- xmlGetWidget cxml castToButton "button4"
        cCPoint <- xmlGetWidget cxml castToButton "button5"
        cCPiece <- xmlGetWidget cxml castToButton "button6"
        cHPoint <- xmlGetWidget cxml castToButton "button7"
        cHPiece <- xmlGetWidget cxml castToButton "button8"
        cTypes  <- xmlGetWidget cxml castToComboBox "combobox1"
        cMouseB <- xmlGetWidget cxml castToComboBox "combobox2"
        cClkAl  <- xmlGetWidget cxml castToAlignment "alignment2"
        cHvrAl  <- xmlGetWidget cxml castToAlignment "alignment3"
        cKeyAl  <- xmlGetWidget cxml castToAlignment "alignment4"
        cConfAl <- xmlGetWidget cxml castToAlignment "alignment5"
        cClickV <- xmlGetWidget cxml castToVBox "vbox3"
        cHoverV <- xmlGetWidget cxml castToVBox "vbox4"
        onDestroy cWindow mainQuit
        onClicked cCancel $ widgetDestroy cWindow
        case j of
          Nothing    -> do
            writeIORef currCond nullConditions
            writeIORef prevIndex 0
            comboBoxSetActive cTypes 0
            comboBoxSetActive cMouseB 0
          Just (_,c) -> do
            let e = fromEnum c
            writeIORef prevIndex e
            writeIORef currCond $ replace [c] nullConditions e
            comboBoxSetActive cTypes e
            comboBoxSetActive cMouseB $
              case c of
                Click a _ -> fromJust (elemIndex a buttons)
                _         -> 0
        let tools = [cClkAl, cHvrAl, cKeyAl, cConfAl]
        i <- readIORef prevIndex
        widgetSetNoShowAll (tools !! i) False
        on cTypes changed $ do
          x <- comboBoxGetActive cTypes
          i <- readIORef prevIndex
          widgetHide (tools !! i)
          widgetShow (tools !! x)
          writeIORef prevIndex x
        on cMouseB changed $ do
          x <- comboBoxGetActive cMouseB
          let button = buttons !! x
          modifyIORef currCond $ \((Click _ a):xs) ->
                                      (Click button a):xs
        onClicked cCPoint $ do
          cond <- readIORef currCond
          let points = (fst.getClicks.head) cond
          boardCheck cWindow $ editBoardPoints cWindow 0 points
        onClicked cHPoint $ do
          cond <- readIORef currCond
          let points = (fst.getHovers) (cond !! 1)
          boardCheck cWindow $ editBoardPoints cWindow 1 points
        onClicked cCPiece $ do
          cond <- readIORef currCond
          let pieces = (snd.getClicks.head) cond
          pieceCheck cWindow $ editGamePieces cWindow 0 pieces
        onClicked cHPiece $ do
          cond <- readIORef currCond
          let pieces = (snd.getHovers) (cond !! 1)
          pieceCheck cWindow $ editGamePieces cWindow 1 pieces
        onClicked cKeys $ do
          cond <- readIORef currCond
          editKeys $ getKeys $ cond !! 2
        onClicked cConfig $ do
          cond <- readIORef currCond
          let checks = (pieceCheck cWindow).(boardCheck cWindow)
          checks $ editConfigs cWindow $ getConfigs (last cond)
        onClicked cOk $ do
          oldCond <- readIORef currCond
          x <- comboBoxGetActive cTypes
          let chosen = oldCond !! x
          case chosen of
            Click _ ([],[]) ->
              errorDialog
                cWindow
                "Empty Condition"
                "This condition needs at least one specified board position or\n\
                \game piece. Please add some of either requirement and try again."
            Hover   ([],[]) ->
              errorDialog
                cWindow
                "Empty Condition"
                "This condition needs at least one specified board position or\n\
                \game piece. Please add some of either requirement and try again."
            Keys        []  ->
              errorDialog
                cWindow
               "Empty Condition"
               "This condition needs at least one key combination to be\n\
               \specified. Please add some key combinations and try again."
            _               -> do
              oldEvents <- readIORef events
              let p = [head path]
                  fr' = forestInsert (leaf ec) oldEvents path
                  Player t i = forestGetValue fr' p
                  ec = Cond i chosen
                  newEvents =
                    case j of
                      Nothing    -> forestEditNode (Player t (i + 1)) fr' p
                      Just (i,_) -> forestEditNode ec oldEvents path
              writeIORef events newEvents
              updateEventView newEvents
              widgetDestroy cWindow
        widgetShowAll cWindow
        mainGUI

  toggCol <- treeViewColumnNew
  nodeCol <- treeViewColumnNew
  treeViewColumnSetTitle toggCol " Chosen? "
  treeViewColumnSetTitle nodeCol " Conditions with Game Pieces "
  toggRen <- cellRendererPixbufNew
  nodeRen <- cellRendererTextNew
  transRen <- cellRendererPixbufNew
  treeViewColumnPackStart toggCol toggRen False
  treeViewColumnPackStart nodeCol nodeRen False

  let toggPic :: Maybe Bool -> Pixbuf
      toggPic Nothing  = speck
      toggPic (Just b) = if b then selAll else desAll

      toggSet :: Bool
              -> Forest (Maybe Bool,a)
              -> TreePath
              -> Forest (Maybe Bool,a)
      toggSet b fr path =
        case forestGetValue fr path of
          (Just _,c) -> forestEditNode (Just b,c) fr path
          _          -> fr

      piecesViewer :: [Pixbuf] -> IO ()
      piecesViewer pics = do
        initGUI
        Just pvxml <- xmlNew "Glade Files/Actions/Game Pieces Viewer.glade"
        pvWindow <- xmlGetWidget pvxml castToWindow "window1"
        pvViewer <- xmlGetWidget pvxml castToIconView "iconview1"
        onDestroy pvWindow mainQuit
        store <- listStoreNew pics
        treeModelSetColumn store iconDisp id
        set pvViewer [iconViewSelectionMode := SelectionMultiple
                     ,iconViewPixbufColumn := iconDisp
                     ,iconViewColumns := length pics
                     ,iconViewModel := Just store]
        widgetShowAll pvWindow
        mainGUI

      editActPieces :: Int -> Action -> IO ()
      editActPieces i transDisapp = do
        initGUI
        Just xxml <- xmlNew "Glade Files/Actions/Game Pieces Reference Dialog.glade"
        xWindow <- xmlGetWidget xxml castToWindow "window1"
        xOk     <- xmlGetWidget xxml castToButton "button1"
        xCancel <- xmlGetWidget xxml castToButton "button2"
        xSel    <- xmlGetWidget xxml castToButton "button3"
        xUnsel  <- xmlGetWidget xxml castToButton "button4"
        xPics   <- xmlGetWidget xxml castToButton "button5"
        xSelImg <- xmlGetWidget xxml castToImage "image1"
        xDesImg <- xmlGetWidget xxml castToImage "image2"
        xViewer <- xmlGetWidget xxml castToTreeView "treeview1"
        xCombo  <- xmlGetWidget xxml castToComboBox "combobox1"
        xHBox   <- xmlGetWidget xxml castToHBox "hbox2"
        onDestroy xWindow mainQuit
        onClicked xCancel $ widgetDestroy xWindow
        imageSetFromPixbuf xSelImg selAll
        imageSetFromPixbuf xDesImg desAll
        icons  <- readIORef iconList
        case transDisapp of
          Transform _ j -> do
            widgetSetNoShowAll xHBox False
            pieces <- treeStoreNew $ map leaf icons
            cellLayoutPackStart xCombo transRen False
            cellLayoutSetAttributes xCombo transRen pieces $ pixbuf image
            comboBoxSetModel xCombo (Just pieces)
            case j of
              Nothing -> comboBoxSetActive xCombo 0
              Just x  -> case x `elemIndex` icons of
                           Nothing -> comboBoxSetActive xCombo 0
                           Just y  -> comboBoxSetActive xCombo y
          Disappear _   -> return ()
        treeViewAppendColumn xViewer nodeCol
        treeViewAppendColumn xViewer toggCol
        pieceSel <- treeViewGetSelection xViewer
        let updateActPieces pieces = do
              store <- treeStoreNew pieces
              treeViewSetModel xViewer store
              cellLayoutSetAttributes toggCol toggRen store $ pixbuf (toggPic.fst)
              cellLayoutSetAttributes nodeCol nodeRen store $ string (show.snd)
              treeViewExpandAll xViewer
        oldEvents <- readIORef events
        let Node _ (condTree:_) = oldEvents !! i
            targets' = forestFilter hasPieces [condTree]
            xs = pointers transDisapp
            targets = forestMap (pairTogg xs) targets'
        writeIORef filteredConds targets
        updateActPieces targets
        let buttons = [xSel, xUnsel, xPics]
        onSelectionChanged pieceSel $ do
          paths <- treeSelectionGetSelectedRows pieceSel
          case paths of
            []  -> setSensitivities buttons [False,False,False]
            [_] -> setSensitivities buttons [True ,True ,True ]
            _   -> setSensitivities buttons [True ,True ,False]
        onClicked xSel $ do
          paths <- treeSelectionGetSelectedRows pieceSel
          fr <- readIORef filteredConds
          let fr' = foldl (toggSet True) fr paths
          writeIORef filteredConds fr'
          updateActPieces fr'
          selectPaths pieceSel paths
        onClicked xUnsel $ do
          paths <- treeSelectionGetSelectedRows pieceSel
          fr <- readIORef filteredConds
          let fr' = foldl (toggSet False) fr paths
          writeIORef filteredConds fr'
          updateActPieces fr'
          selectPaths pieceSel paths
        onClicked xPics $ do
          paths <- treeSelectionGetSelectedRows pieceSel
          case paths of
            [path] ->
              case snd (forestGetValue targets path) of
                Cond _ c -> piecesViewer (condGetPieces c)
                _        -> return ()
            _        -> return ()
        onClicked xOk $ do
          fr <- readIORef filteredConds
          let pairs = forestSearch toggCheck fr
          if null pairs
           then
             errorDialog
               xWindow
               "Empty Selection"
               "No game pieces have been selected. Please\n\
               \select some game pieces and try again."
           else do
             x <- comboBoxGetActive xCombo
             let ps = map (pointer.snd) pairs
             case transDisapp of
               Transform _ _ ->
                 let ic = icons !! x
                     action = Transform ps (Just ic)
                  in modifyIORef currAct $ \a -> replace [action] a 2
               Disappear _   -> 
                 let action = Disappear ps
                  in modifyIORef currAct $ \a -> replace [action] a 4
             widgetDestroy xWindow
        widgetShowAll xWindow
        mainGUI

  toggCol' <- treeViewColumnNew
  nodeCol' <- treeViewColumnNew
  treeViewColumnSetTitle toggCol' " Chosen? "
  treeViewColumnSetTitle nodeCol' " Conditions with Positions "
  toggRen' <- cellRendererPixbufNew
  nodeRen' <- cellRendererTextNew
  appearRen <- cellRendererPixbufNew
  treeViewColumnPackStart toggCol' toggRen' False
  treeViewColumnPackStart nodeCol' nodeRen' False

  let pointsViewer :: [Point] -> IO ()
      pointsViewer ps = do
        initGUI
        Just pvxml <- xmlNew "Glade Files/Actions/Board Positions Viewer.glade"
        pvWindow <- xmlGetWidget pvxml castToWindow "window1"
        pvViewer <- xmlGetWidget pvxml castToIconView "iconview1"
        onDestroy pvWindow mainQuit
        board <- readIORef emptyArray
        size  <- readIORef iconSize
        store <- listStoreNew =<< getColored board ps size
        treeModelSetColumn store iconDisp id
        set pvViewer [iconViewSelectionMode := SelectionMultiple
                     ,iconViewPixbufColumn := iconDisp
                     ,iconViewColumns := col board
                     ,iconViewModel := Just store]
        widgetShowAll pvWindow
        mainGUI

      editActPoints :: Int -> Action -> IO ()
      editActPoints i (Appear ps1 ps2 j) = do
        initGUI
        Just yxml <- xmlNew "Glade Files/Actions/Board Positions Reference Dialog.glade"
        yWindow <- xmlGetWidget yxml castToWindow "window1"
        yOk     <- xmlGetWidget yxml castToButton "button1"
        yCancel <- xmlGetWidget yxml castToButton "button2"
        ySel    <- xmlGetWidget yxml castToButton "button3"
        yUnsel  <- xmlGetWidget yxml castToButton "button4"
        yPoints <- xmlGetWidget yxml castToButton "button5"
        yAdd    <- xmlGetWidget yxml castToButton "button6"
        yRemove <- xmlGetWidget yxml castToButton "button7"
        ySelImg <- xmlGetWidget yxml castToImage "image1"
        yDesImg <- xmlGetWidget yxml castToImage "image2"
        yCombo  <- xmlGetWidget yxml castToComboBox "combobox1"
        yForest <- xmlGetWidget yxml castToTreeView "treeview1"
        yBoard  <- xmlGetWidget yxml castToIconView "iconview1"
        onDestroy yWindow mainQuit
        onClicked yCancel $ widgetDestroy yWindow
        imageSetFromPixbuf ySelImg selAll
        imageSetFromPixbuf yDesImg desAll
        icons  <- readIORef iconList
        pieces <- treeStoreNew $ map leaf icons
        cellLayoutPackStart yCombo appearRen False
        cellLayoutSetAttributes yCombo appearRen pieces $ pixbuf image
        comboBoxSetModel yCombo (Just pieces)
        case j of
          Nothing -> comboBoxSetActive yCombo 0
          Just x  -> case x `elemIndex` icons of
                       Nothing -> comboBoxSetActive yCombo 0
                       Just y  -> comboBoxSetActive yCombo y
        treeViewAppendColumn yForest nodeCol'
        treeViewAppendColumn yForest toggCol'
        pointSel <- treeViewGetSelection yForest
        let updateActPoints points = do
              store <- treeStoreNew points
              treeViewSetModel yForest store
              cellLayoutSetAttributes toggCol' toggRen' store $ pixbuf (toggPic.fst)
              cellLayoutSetAttributes nodeCol' nodeRen' store $ string (show.snd)
              treeViewExpandAll yForest
        oldEvents <- readIORef events
        let Node _ (condTree:_) = oldEvents !! i
            targets' = forestFilter hasPoints [condTree]
            targets = forestMap (pairTogg ps1) targets'
        writeIORef filteredConds targets
        updateActPoints targets
        onClicked ySel $ do
          paths <- treeSelectionGetSelectedRows pointSel
          fr <- readIORef filteredConds
          let fr' = foldl (toggSet True) fr paths
          writeIORef filteredConds fr'
          updateActPoints fr'
          selectPaths pointSel paths
        onClicked yUnsel $ do
          paths <- treeSelectionGetSelectedRows pointSel
          fr <- readIORef filteredConds
          let fr' = foldl (toggSet False) fr paths
          writeIORef filteredConds fr'
          updateActPoints fr'
          selectPaths pointSel paths
        onClicked yPoints $ do
          paths <- treeSelectionGetSelectedRows pointSel
          case paths of
           [path] ->
             case snd (forestGetValue targets path) of
               Cond _ c -> case condGetPoints c of
                             [] -> return ()
                             x  -> pointsViewer x
               _        -> return ()
           _        -> return ()
        board <- readIORef emptyArray
        let c = col board
        set yBoard [iconViewSelectionMode := SelectionMultiple
                   ,iconViewPixbufColumn := iconDisp
                   ,iconViewColumns := c]
        size <- readIORef iconSize
        let updateAbsPoints ps = do
              store <- listStoreNew =<< getColored board ps size
              treeModelSetColumn store iconDisp id
              iconViewSetModel yBoard (Just store)
        writeIORef boardPoints ps2
        updateAbsPoints ps2
        onClicked yAdd $ do
          paths <- iconViewGetSelectedItems yBoard
          oldPoints <- readIORef boardPoints
          let newPoints = union oldPoints (treePathsToPoints paths c)
          writeIORef boardPoints newPoints
          updateAbsPoints newPoints
        onClicked yRemove $ do
          paths <- iconViewGetSelectedItems yBoard
          oldPoints <- readIORef boardPoints
          let points = indicesToPoints (concat paths) c
              newPoints = oldPoints \\ points
          writeIORef boardPoints newPoints
          updateAbsPoints newPoints
        on yBoard itemActivated $ \path -> do
          oldPoints <- readIORef boardPoints
          let a@[point] = treePathsToPoints [path] c
              newPoints = if point `elem` oldPoints
                           then oldPoints \\ a
                           else point:oldPoints
          writeIORef boardPoints newPoints
          updateAbsPoints newPoints
        onClicked yOk $ do
          fr <- readIORef filteredConds
          ps2' <- readIORef boardPoints
          let pairs = forestSearch toggCheck fr
          if (null ps2')&&(null pairs)
           then
             errorDialog
               yWindow
               "Empty Action"
               "This action needs some board positions to be specified.\n\
               \Please specify some board positions and try again."
           else do
            x <- comboBoxGetActive yCombo
            let ps1' = map (pointer.snd) pairs
                ic = icons !! x
                action = Appear ps1' ps2' (Just ic)
             in modifyIORef currAct $ \a -> replace [action] a 3
            widgetDestroy yWindow
        widgetShowAll yWindow
        mainGUI

  let editAction :: TreePath -> Maybe Action -> IO ()
      editAction path j = do
        initGUI
        Just axml <- xmlNew "Glade Files/Actions/Actions Dialog.glade"
        aWindow <- xmlGetWidget axml castToWindow "window1"
        aOk     <- xmlGetWidget axml castToButton "button1"
        aCancel <- xmlGetWidget axml castToButton "button2"
        aTrans  <- xmlGetWidget axml castToButton "button3"
        aAppear <- xmlGetWidget axml castToButton "button4"
        aDisapp <- xmlGetWidget axml castToButton "button5"
        aTypes  <- xmlGetWidget axml castToComboBox "combobox1"
        aTAlign <- xmlGetWidget axml castToAlignment "alignment2"
        aAAlign <- xmlGetWidget axml castToAlignment "alignment3"
        aDAlign <- xmlGetWidget axml castToAlignment "alignment4"
        aGAlign <- xmlGetWidget axml castToAlignment "alignment5"
        aEAlign <- xmlGetWidget axml castToAlignment "alignment6"
        onDestroy aWindow mainQuit
        onClicked aCancel $ widgetDestroy aWindow
        case j of
          Nothing -> do
            writeIORef currAct nullActions
            writeIORef prevIndex 0
            comboBoxSetActive aTypes 0
          Just a  -> do
            let e = fromEnum a
            writeIORef currAct $ replace [a] nullActions e
            writeIORef prevIndex e
            comboBoxSetActive aTypes e
        let tools = [aGAlign, aEAlign, aTAlign, aAAlign, aDAlign]
        i <- readIORef prevIndex
        comboBoxSetActive aTypes i
        widgetSetNoShowAll (tools !! i) False
        on aTypes changed $ do
          x <- comboBoxGetActive aTypes
          i <- readIORef prevIndex
          widgetHide (tools !! i)
          widgetShow (tools !! x)
          writeIORef prevIndex x
        let p = head path
        onClicked aTrans $ do
          act <- readIORef currAct
          pieceCheck aWindow $ editActPieces p (act !! 2)
        onClicked aAppear $ do
          act <- readIORef currAct
          let checks = (pieceCheck aWindow).(boardCheck aWindow)
          checks $ editActPoints p (act !! 3)
        onClicked aDisapp $ do
          act <- readIORef currAct
          editActPieces p (last act)
        onClicked aOk $ do
          x <- comboBoxGetActive aTypes
          act <- readIORef currAct
          let chosen = act !! x
          case chosen of
            Transform [] _      ->
              errorDialog
                aWindow
                "Empty Action"
                "This action needs both initial and final game pieces to\n\
                \be specified. Please fulfil this requirement and try again."
            Transform _ Nothing ->
              errorDialog
                aWindow
                "Empty Action"
                "This action needs both initial and final game pieces to\n\
                \be specified. Please fulfil this requirement and try again."
            Appear [] [] _      ->
              errorDialog
                aWindow
                "Empty Action"
                "This action needs both board positions and an appearing game\n\
                \piece to be specified. Please fulfil this requirement and try again."
            Appear _ _ Nothing  ->
              errorDialog
                aWindow
                "Empty Action"
                "This action needs both board positions and an appearing game\n\
                \piece to be specified. Please fulfil this requirement and try again."
            Disappear []        ->
              errorDialog
                aWindow
                "Empty Action"
                "This action needs a disappearing game piece to be\n\
                \specified. Please fulfil this requirement and try again."
            _                   -> do
              oldEvents <- readIORef events
              let newEvents =
                    case j of
                      Nothing -> forestInsert (leaf $ Act chosen) oldEvents path
                      Just _  -> forestEditNode (Act chosen) oldEvents path
              writeIORef events newEvents
              updateEventView newEvents
              widgetDestroy aWindow
        widgetShowAll aWindow
        mainGUI

-- Back to creating handlers for signals from buttons on Page 3

  let eButtons = [eRemove, eEdit]

  onSelectionChanged eventSel $ do
    rows <- treeSelectionGetSelectedRows eventSel
    case rows of
     []     -> setSensitivities eButtons [False,False]
     [path] -> do
       oldEvents <- readIORef events
       case forestGetValue oldEvents path of
         Player  _ _ -> setSensitivities eButtons [True ,True ]
         Conditions  -> setSensitivities eButtons [False,False]
         Root True _ -> setSensitivities eButtons [False,True ]
         Actions     -> setSensitivities eButtons [False,False]
         _           -> setSensitivities eButtons [True ,True ]
     _      -> setSensitivities eButtons [True ,False]

  let editPlayer :: Forest Event
                 -> Either [TreePath] (TreePath,Int,Pointer)
                 -> IO ()
      editPlayer oldEvents j = do
        initGUI
        Just qxml <- xmlNew "Glade Files/Minor Dialogs/Player Query Dialog.glade"
        qWindow <- xmlGetWidget qxml castToWindow "window1"
        qOk     <- xmlGetWidget qxml castToButton "button1"
        qCancel <- xmlGetWidget qxml castToButton "button2"
        qHBox   <- xmlGetWidget qxml castToHBox "hbox2"
        onDestroy qWindow mainQuit
        onClicked qCancel $ widgetDestroy qWindow
        qCombo <- comboBoxNewText
        boxPackEnd qHBox qCombo PackNatural 0
        pls <- readIORef players
        addText qCombo $ map show [1..pls]
        case j of
          Left _          -> comboBoxSetActive qCombo 0
          Right (_,x+1,_) -> comboBoxSetActive qCombo x
        onClicked qOk $ do
          x <- comboBoxGetActive qCombo
          let f = Player (x+1)
              newEvents =
                case j of
                  Left _        ->
                    oldEvents++[Node (f 0) [Node Conditions [leaf (Root True All)]
                                           ,leaf Actions]]
                  Right (p,_,i) -> forestEditNode (f i) oldEvents p
          writeIORef events newEvents
          updateEventView newEvents
          case j of
            Left [] -> return ()
            Left ps -> treeSelectionSelectPath eventSel (concat ps)
            _       -> return ()
          widgetDestroy qWindow
        widgetShowAll qWindow
        mainGUI

  onClicked eAdd $ do
    x   <- comboBoxGetActive eAddCombo
    oldEvents <- readIORef events
    paths <- treeSelectionGetSelectedRows eventSel
    case x of
      0 -> editPlayer oldEvents (Left paths)
      1 -> case paths of
             [p] -> case rootLabel (forestGetSubTree oldEvents p) of
                      Root _ _ -> do
                        let newRoot = Root False All
                            newEvents = forestInsert (leaf newRoot) oldEvents p
                        writeIORef events newEvents
                        updateEventView newEvents
                        treeSelectionSelectPath eventSel p
                      _        -> return ()
             _   -> return ()
      2 -> case paths of
             [p] -> case forestGetValue oldEvents p of
                      Root _ _ -> editCondition p Nothing
                      _        -> return ()
             _   -> return ()
      3 -> case paths of
             [p] -> case forestGetValue oldEvents p of
                      Actions -> editAction p Nothing
                      _       -> return ()
             _   -> return ()
    selectPaths eventSel paths

  onClicked eRemove $ do
    oldEvents <- readIORef events
    paths <- treeSelectionGetSelectedRows eventSel
    let (is,newEvents) = removeConditions oldEvents paths
    let filtered = foldl filterActions newEvents is
    writeIORef events filtered
    updateEventView filtered

  onClicked eEdit $ do
    oldEvents <- readIORef events
    [path] <- treeSelectionGetSelectedRows eventSel
    let Node s sf = forestGetSubTree oldEvents path
    case s of
      Player t i -> editPlayer oldEvents (Right (path,t,i))
      Root b c   -> do
        initGUI
        Just nxml <- xmlNew "Glade Files/Minor Dialogs/Edit Nodes Dialog.glade"
        nWindow <- xmlGetWidget nxml castToWindow "window1"
        nOk     <- xmlGetWidget nxml castToButton "button1"
        nCancel <- xmlGetWidget nxml castToButton "button2"
        nHBox   <- xmlGetWidget nxml castToHBox "hbox1"
        onDestroy nWindow mainQuit
        onClicked nCancel $ widgetDestroy nWindow
        nCombo  <- comboBoxNewText
        comboBoxAppendText nCombo "All"
        comboBoxSetActive nCombo 0
        boxPackEnd nHBox nCombo PackNatural 0
        addText nCombo $ map show [1..(length sf - 1)]
        unless (c == All) $ comboBoxSetActive nCombo (fromChoice $ choiceRT s)
        onClicked nOk $ do
          x <- comboBoxGetActive nCombo
          let newChoice = if x == 0
                           then All
                           else Choice x
              newEvents = forestEditNode (Root b newChoice) oldEvents path
          writeIORef events newEvents
          updateEventView newEvents
          treeSelectionSelectPath eventSel path
          widgetDestroy nWindow
        widgetShowAll nWindow
        mainGUI
      Cond _ c   -> do
        let p = head path
            Player t i = forestGetValue oldEvents [p]
            newEvents = forestEditNode (Player t (i + 1)) oldEvents [p]
            filtered = filterActions newEvents (p,i)
        writeIORef events filtered
        editCondition path (Just (i,c))
      Act a    -> editAction path (Just a)
      _        -> return ()
    treeSelectionSelectPath eventSel path

  widgetShowAll window
  mainGUI

   where encase x = [x]

         string f x = [cellText         := f x]
         pixbuf f x = [cellPixbuf       := f x]
         toggle f x = [cellToggleActive := f x]

         iconDisp = makeColumnIdPixbuf 1

         removal :: [a] -> [Int] -> [a]
         removal = f 0
         f _ [] _ = []
         f n (a:as) xs = if n `elem` xs
                          then    f (n + 1) as xs
                          else a:(f (n + 1) as xs)

         setSensitivities :: [Button] -> [Bool] -> IO ()
         setSensitivities butts bools =
           let pairs = zip butts bools
            in mapM_ (uncurry widgetSetSensitivity) pairs

         checkFile x = do
           b <- doesFileExist x
           return (b,x)

         blend src dest (width,height) alpha = 
           pixbufComposite src dest 0 0 width height
                           0 0 1 1 InterpNearest alpha

         scaleEmpty :: Array Point FullIcon
                    -> Int
                    -> Int
                    -> IO (Array Point FullIcon)
         scaleEmpty arr w h = let (a,b) = unzip (assocs arr) in do
           b' <- mapScale w h b
           return $ array (bounds arr) (zip a b')

         mapScale :: Int -> Int -> [FullIcon] -> IO [FullIcon]
         mapScale _ _ [] = return []
         mapScale w h ((Icon p fp):xs) = do
           p' <- pixbufScaleSimple p w h InterpBilinear
           rt <- mapScale w h xs
           return (Icon {image = p', path = fp}:rt)

         scalePiece :: Array Point [FullIcon]
                    -> Int
                    -> Int
                    -> IO (Array Point [FullIcon])
         scalePiece arr w h =
           let (a,b) = (unzip.assocs) arr
           in do b' <- mapM (mapScale w h) b
                 return $ array (bounds arr) (zip a b')

         col :: (Ix a, Ix b) => Array (a,b) e -> b
         col arr = (snd.snd.bounds) arr

         replaceArrayElem :: Array Point e
                          -> [TreePath]
                          -> e
                          -> Array Point e
         replaceArrayElem arr treePaths x =
           let ps = treePathsToPoints treePaths (col arr)
            in selectiveAMap (const x) arr ps

         selectiveAMap :: Ix i => (e -> e)
                               -> Array i e
                               -> [i]
                               -> Array i e
         selectiveAMap f arr ps =
           array (bounds arr) $ map (selectiveApply f ps) (assocs arr)

         selectiveApply f ps a@(i,e) = if i `elem` ps
                                        then (i, f e)
                                        else a

         funcOnPieceArray :: (e -> e)
                          -> Array Point e
                          -> [TreePath]
                          -> Array Point e
         funcOnPieceArray f arr treePaths =
           selectiveAMap f arr $ treePathsToPoints treePaths (col arr)

         treePathsToPoints :: [TreePath] -> Int -> [Point]
         treePathsToPoints treePaths c = indicesToPoints (concat treePaths) c

         indicesToPoints :: [Int] -> Int -> [Point]
         indicesToPoints tags c =
           map (\k -> let m = k + 1
                          d = m `mod` c
                       in if d == 0
                           then (m `div` c, c)
                           else (m `div` c + 1, d)) tags

         pointsToIndices :: [Point] -> Int -> [Int]
         pointsToIndices points c = map (\(a,b) -> (a - 1) * c + b - 1) points

         color :: [Int]
               -> Pixbuf
               -> Size
               -> [(Int,Pixbuf)]
               -> IO [Pixbuf]
         color tags red size ips = mapM (g tags red size) ips
         g tags red size (i,e) = if i `elem` tags
                                  then do
                                    p <- pixbufCopy e
                                    blend red p size 100
                                    return p
                                  else return e

         errorDialog :: Window -> String -> String -> IO ()
         errorDialog window title message = do
           initGUI
           Just exml <- xmlNew "Glade Files/Minor Dialogs/Error Dialog.glade"
           eWindow <- xmlGetWidget exml castToWindow "window1"
           eOk     <- xmlGetWidget exml castToButton "button1"
           eLabel  <- xmlGetWidget exml castToLabel  "label1"
           windowSetTitle eWindow title
           windowSetTransientFor eWindow window
           onDestroy eWindow mainQuit
           labelSetText eLabel message
           onClicked eOk $ widgetDestroy eWindow
           widgetShowAll eWindow
           mainGUI

         confirmation :: Window -> String -> String -> IO () -> IO ()
         confirmation win title message code = do
           initGUI
           Just cxml <- xmlNew "Glade Files/Minor Dialogs/Confirmation Dialog.glade"
           cWindow <- xmlGetWidget cxml castToWindow "window1"
           cOk     <- xmlGetWidget cxml castToButton "button1"
           cCancel <- xmlGetWidget cxml castToButton "button2"
           cLabel  <- xmlGetWidget cxml castToLabel  "label1"
           onDestroy cWindow mainQuit
           windowSetTitle cWindow title
           windowSetTransientFor cWindow win
           labelSetText cLabel message
           onClicked cCancel $ widgetDestroy cWindow 
           onClicked cOk     $ widgetDestroy cWindow >> code
           widgetShowAll cWindow
           mainGUI

         indexToChoice :: Int -> Choice
         indexToChoice 0 = All
         indexToChoice x = Choice x

         configShowPoints :: Config -> String
         configShowPoints x =
           "occupies "++show (choice x)++" of "++(commas $ map show $ points x)

         removeConditions :: Forest Event
                          -> [TreePath]
                          -> ([(Int,Pointer)],Forest Event)
         removeConditions fr ps = h ([],fr) ps
         h a [] = a
         h (is,fr) (p:ps) =
           case p of
             [_] -> h (is,forestRemove fr p) ps
             _   ->
               let parent = init p
                   fr' = forestRemove fr p
                   Node _ sf = forestGetSubTree fr parent
                   c = rootLabel $ sf !! (last p)
                   is' = case c of
                           Cond i _ -> (head p,i):is
                           _        -> is
                in case forestGetValue fr parent of
                     Root b All -> h (is',fr') ps
                     Root b _   ->
                       let forceAll = forestEditNode (Root b All) fr' parent
                        in h (is',forceAll) ps
                     _          -> h (is',fr') ps

         filterActions :: Forest Event -> (Int,Pointer) -> Forest Event
         filterActions fr (x,ptr) = let Node a [b,Node c d] = fr !! x
                                        d' = filter (diffPointer ptr) d
                                     in replace [Node a [b,Node c d']] fr x
         diffPointer ptr a = case toAction (rootLabel a) of
                               GameEnds -> True
                               x        -> ptr `notElem` (pointers x)

         hasPieces :: Event -> Bool
         hasPieces (Cond _ x) = case x of
                                  Click _ (_,[]) -> False
                                  Hover   (_,[]) -> False
                                  Keys  _        -> False
                                  Conf []        -> False
                                  _              -> True
         hasPieces _          = True

         condGetPieces :: Condition -> [Pixbuf]
         condGetPieces (Click _ (_,x)) = map image x
         condGetPieces (Hover   (_,x)) = map image x
         condGetPieces (Conf xs)       = map (image.icon) xs

         hasPoints :: Event -> Bool
         hasPoints (Cond _ x) = case x of
                                  Click _ ([],_) -> False
                                  Hover   ([],_) -> False
                                  Keys  _        -> False
                                  Conf []        -> False
                                  _              -> True
         hasPoints _          = True

         condGetPoints :: Condition -> [Point]
         condGetPoints (Click _ (x,_)) = x
         condGetPoints (Hover   (x,_)) = x
         condGetPoints (Conf _)        = []

         pairTogg :: [Pointer] -> Event -> (Maybe Bool,Event)
         pairTogg xs e@(Cond i _) = (Just (i `elem` xs),e)
         pairTogg _  e            = (Nothing,e)

         toggFlip :: (Maybe Bool,Event) -> (Maybe Bool,Event)
         toggFlip a@(Nothing,e) = a
         toggFlip   (Just b ,e) = (Just (not b),e)

         toggCheck :: (Maybe Bool,Event) -> Bool
         toggCheck (Just b ,_) = b
         toggCheck _           = False

         addText combo strings =
           mapM_ (comboBoxAppendText combo) strings

         selectPaths treeSel paths =
           mapM_ (treeSelectionSelectPath treeSel) paths