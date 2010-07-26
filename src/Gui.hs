module Main where

-- ANN
import Neuron
import Trainingdata
import Backpropagation
import Utils
import Config
import TopologyParser
import TraindataParser
import GraphicInterface

import Data.IORef
import Control.Concurrent
import Data.List
import Data.Word
import Data.Array.Storable
import Maybe

-- Gui
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Glade
import Control.Monad


-- TODO:
	-- set momentun + lernrate with values set by gui (pass values to backpropagation algo???)
	-- button to reset the trained net
	-- test if pixelbuf array is correct
	-- print result / log
	-- forkIO on work


main :: IO()
main = do

------------------------------------------------------------------------
-- LOAD WIDGETS FROM XML

    initGUI
    Just xml    <- xmlNew "gui.glade"
    window      <- xmlGetWidget xml castToWindow "window1"
    
    -- drawing area
    darea <- xmlGetWidget xml castToDrawingArea "drawingarea1"
    
    -- buttons
    train_button <- xmlGetWidget xml castToButton "button1"
    work_button <- xmlGetWidget xml castToButton "button2"
    clear_button <- xmlGetWidget xml castToButton "button3"
    
    -- spinbuttons
    momentum_spin <- xmlGetWidget xml castToSpinButton "spinbutton1"
    learningRate_spin <- xmlGetWidget xml castToSpinButton "spinbutton2"     
    inputNeurons_spin <- xmlGetWidget xml castToSpinButton "spinbutton3"
    hiddenNeurons_spin <- xmlGetWidget xml castToSpinButton "spinbutton4"
    outputNeurons_spin <- xmlGetWidget xml castToSpinButton "spinbutton5"
    cycles_spin <- xmlGetWidget xml castToSpinButton "spinbutton6"
	
    -- info, labels
    textview <- xmlGetWidget xml castToTextView "textview1"
    statusbar <- xmlGetWidget xml castToStatusbar "statusbar1"
    result_label <- xmlGetWidget xml castToLabel "label10"
     
    -- menuitems
    menuitem1 <- xmlGetWidget xml castToImageMenuItem "imagemenuitem1"
    menuitem2 <- xmlGetWidget xml castToImageMenuItem "imagemenuitem2"
    --menuitem3 <- xmlGetWidget xml castToImageMenuItem "imagemenuitem3"
    --menuitem4 <- xmlGetWidget xml castToImageMenuItem "imagemenuitem4"
    --menuitem5 <- xmlGetWidget xml castToImageMenuItem "imagemenuitem5"

	-- checkbutton
    biasInput_check <- xmlGetWidget xml castToCheckButton "checkbutton1"
    biasHidden_check <- xmlGetWidget xml castToCheckButton "checkbutton2" 
     
------------------------------------------------------------------------
-- ANN, SETTINGS
        
    flip widgetSetSensitivity False inputNeurons_spin    
    flip widgetSetSensitivity False outputNeurons_spin
        
    tdataPathIO <- newIORef (dataPath ++ "traindata/img/10_12_arial/")
    patternPathIO <- newIORef ""
                
    --tdata <- initTraindata (dataPath ++ "traindata/xor/trainingdata")
    tdataPath <- readIORef tdataPathIO
    tdata <- dirToTrainData tdataPath
    tdataIO <- newIORef tdata
    
    biasInput <- toggleButtonGetActive biasInput_check
    biasInputIO <- newIORef biasInput
    
    biasHidden <- toggleButtonGetActive biasHidden_check
    biasHiddenIO <- newIORef biasHidden
    
    hiddenNeurons <- spinButtonGetValueAsInt hiddenNeurons_spin
    hiddenNeuronsIO <- newIORef hiddenNeurons
    
    --net <- initNetwork "4b\n10b\n1"
    net <- initNetwork (genTopologyStr tdata biasInput biasHidden hiddenNeurons)
    netIO <- newIORef net
    updateNetworkSize tdata inputNeurons_spin outputNeurons_spin
    
    --let pattern = [1.0, 0.0]
    pattern <- readPPMFile (dataPath ++ "traindata/img/10_12_great_times/" ++ "1.pgm")
    patternIO <- newIORef pattern

    momentum <- spinButtonGetValueAsInt momentum_spin
    momentumIO <- newIORef momentum
    
    learningRate <- spinButtonGetValueAsInt learningRate_spin
    learningRateIO <- newIORef learningRate
    
    cycles <- spinButtonGetValueAsInt cycles_spin
    cyclesIO <- newIORef cycles
    
    --textViewSetBuffer textview appendText
    resultListIO <- newIORef [0.0]
    
------------------------------------------------------------------------
-- REGISTER EVENTS

    onClicked train_button (trainButtonClicked netIO tdataIO cyclesIO)
    onClicked work_button (workButtonClicked netIO patternIO tdataPathIO resultListIO result_label)
    --onClicked clear_button (clearButtonClicked darea)
    onClicked clear_button (pixelTest darea)
    
         
    onValueSpinned momentum_spin (valueSpinned1 momentum_spin)
    onValueSpinned learningRate_spin (valueSpinned2 learningRate_spin)
    --onValueSpinned inputNeurons_spin (valueSpinned3 inputNeurons_spin)
    onValueSpinned hiddenNeurons_spin (valueSpinned4 hiddenNeurons_spin)
    --onValueSpinned outputNeurons_spin (valueSpinned5 outputNeurons_spin)
    onValueSpinned cycles_spin (valueSpinned6 cycles_spin cyclesIO)
    
    -- SELECT PATTERN FILE
    afterActivateLeaf menuitem1 $do
		openFileDialog window "Please choose the pattern file" patternPathIO
		path <- readIORef patternPathIO
		pattern <- readPPMFile path
		modifyIORef patternIO (\_ -> pattern)
		putStrLn ("pattern file path: " ++ path)
	
	-- SELECT TDATA PATH	
    afterActivateLeaf menuitem2 $do
		openFolderDialog window "Please choose the tdata path" tdataPathIO
		path <- readIORef tdataPathIO
		-- update tdata
		tdata <- dirToTrainData (path ++ "/")
		modifyIORef tdataIO (\_ -> tdata)
		-- updata network
		b1 <- toggleButtonGetActive biasInput_check
		b2 <- toggleButtonGetActive biasHidden_check
		hiddenLen <- spinButtonGetValueAsInt hiddenNeurons_spin
		net <- initNetwork $ genTopologyStr tdata b1 b2 hiddenLen
		modifyIORef netIO (\_ -> net)
		-- update network size
		updateNetworkSize tdata inputNeurons_spin outputNeurons_spin
		-- update result label
		setLabel result_label "--"
		
		putStrLn ("network: " ++ genTopologyStr tdata b1 b2 hiddenLen)
		putStrLn ("tdata path: " ++ path)
    
    --afterActivateLeaf menuitem3 (menuItem3Select window "Please choose..." tdataPathIO)--menuItem3Select
    --afterActivateLeaf menuitem4 (menuItem4Select window "Please choose..." tdataPathIO)--menuItem4Select
    --afterActivateLeaf menuitem5 (menuItem5Select window "Please choose..." tdataPathIO)--menuItem5Select
    
    onToggled biasInput_check (onToggledInputBias biasInput_check biasInputIO)
    onToggled biasHidden_check (onToggledHiddenBias biasHidden_check biasHiddenIO)

    darea `on` sizeRequest $ return (Requisition 120 160)
    darea `on` exposeEvent $ update
    
    --drw <- widgetGetDrawWindow darea
    --drwIO <- newIORef drw
    
    {--
    onRealize darea $do
		--(t1 darea drwIO)
		drw <- widgetGetDrawWindow darea
		pixbuf <- pixbufGetFromDrawable drw (Rectangle 0 0 120 160)
		--px <- pixbufGetPixels (fromJust pixbuf)
		ww <- pixbufGetHasAlpha (fromJust pixbuf)
		putStrLn (show ww)
		--fu <- p1 px
		--setLabel result_label fu -- $do
			--ww <- pixbufGetWidth (fromJust pixbuf)
			--putStrLn (ww)
			--return "asdf"
		return ()
    --}
    
    -- Add mouse listener. 
    --
    -- It seems that Button1MotionMask events (and clicks in general) are not
    -- passed to the modifier, so we work around this (bug?) by forcing the
    -- user to type shift for drawing. Maybe I just don't understand some nifty
    -- detail of the event system of GTK. 
    --
    -- See http://www.haskell.org/gtk2hs/docs/gtk-docs-0.11.0/
    --     Graphics-UI-Gtk-Abstract-Widget.html#7
    -- for more information.
    widgetAddEvents darea [ButtonPressMask,PointerMotionMask]
    (darea `on` motionNotifyEvent) (button darea)
    
    onDestroy window mainQuit
        
------------------------------------------------------------------------
-- SHOW GUI
    widgetShowAll window
    mainGUI

------------------------------------------------------------------------
-- EVENTS
------------------------------------------------------------------------
pixelTest darea = do
	drw <- widgetGetDrawWindow darea
	pixbuf <- pixbufGetFromDrawable drw (Rectangle 0 0 120 160)

	pixels <- p1 (fromJust pixbuf)
	pp1 <- mapM (\x -> readArray pixels x) [1..(120*160)]	-- 120x160 = 19200 pixels
	
	bytes <- pixbufGetRowstride (fromJust pixbuf)
	sp <- pixbufGetBitsPerSample (fromJust pixbuf)
	pixbufSave (fromJust pixbuf) "drawingarea.png" "png" [("","")]
	
	-- create a simple pgm file
	let str = ("P2\n" ++ "#Created with GIMP\n" ++ "120 160\n" ++ "255\n")
	let list = zipWith (\p i -> if i `mod` 20 == 0 then (show p ++"\n") else (show p ++ " ")) pp1 [1..]
	writeFile "drawingarea.pgm" (str ++ (concat list))
	
	putStrLn (show pp1)
	putStrLn ("pixel row stride: " ++ show bytes)
	putStrLn ("bits per sample: " ++ show sp)
	
p1 pb = do
	px <- (pixbufGetPixels pb :: IO (PixbufData Int Word8))
	return px

trainButtonClicked netIO tdataIO cyclesIO = do
	nn <- readIORef netIO
	td <- readIORef tdataIO
	cycles <- readIORef cyclesIO
	
	let trainedNet = trainNet nn td cycles
	modifyIORef netIO (\_ -> trainedNet)
	putStrLn ("network trained!")
	
workButtonClicked netIO patternIO tdataPathIO resultListIO result_label = do --putStrLn "work button clicked"
	trainedNet <- readIORef netIO
	pattern <- readIORef patternIO
	path <- readIORef tdataPathIO
	
	let resultList = work trainedNet pattern
	let bestVal = maximum resultList
	let bestIdx = bestVal `elemIndex` resultList
	
	-- TODO: move this list in SELECT TDATA PATH area as ioref, it is unchanged until the next tdata
	pgmList <- getPgmList path
	let list = map (\x -> fst (break (=='.') x)) pgmList	-- get filenames before dot: 0.ppm => 0
	
	modifyIORef resultListIO (\_ -> resultList)
	--putStrLn ("its a: " ++ list !! (fromJust bestIdx))
	setLabel result_label (list !! (fromJust bestIdx))
	putStrLn ("index: " ++ show (fromJust bestIdx) ++ " bestVal: " ++ show bestVal) -- ++ show resultList)
	putStrLn $ show resultList

setLabel label text = do
    set label [ labelText := text ] --putStrLn "clear button clicked"

valueSpinned1 widget = do
	value <- spinButtonGetValue widget
	putStrLn ("momentum spinned: " ++ show value)
	
valueSpinned2 widget = do
	value <- spinButtonGetValue widget
	putStrLn ("learning rate spinned: " ++ show value)
	
valueSpinned3 widget = do
	value <- spinButtonGetValueAsInt widget
	putStrLn ("input neuron count spinned: " ++ show value)
	
valueSpinned4 widget = do
	value <- spinButtonGetValueAsInt widget
	putStrLn ("hidden neuron count spinned: " ++ show value)

valueSpinned5 widget = do
	value <- spinButtonGetValueAsInt widget
	putStrLn ("output neuron count spinned: " ++ show value)

valueSpinned6 widget cyclesIO = do
	value <- spinButtonGetValueAsInt widget
	modifyIORef cyclesIO (\_ -> value)
	putStrLn ("cycles spinned: " ++ show value)

menuItem3Select window title pathIO = do
	openFileDialog window "Please chose a tdata File or Path" pathIO
	path <- readIORef pathIO
	putStrLn ("MenuItem1 selected, path: " ++ path)
	
menuItem4Select window title pathIO = do
	openFileDialog window "Please chose a tdata File or Path" pathIO
	path <- readIORef pathIO
	putStrLn ("MenuItem1 selected, path: " ++ path)
	
menuItem5Select window title pathIO = do
	openFileDialog window "Please chose a tdata File or Path" pathIO
	path <- readIORef pathIO
	putStrLn ("MenuItem1 selected, path: " ++ path)

onToggledInputBias cb biasInputIO = do
	value <- toggleButtonGetActive cb
	modifyIORef biasInputIO (\_ -> value)
	putStrLn("biasInput toogled: " ++ show value)

onToggledHiddenBias cb biasHiddenIO = do
	value <- toggleButtonGetActive cb
	modifyIORef biasHiddenIO (\_ -> value)
	putStrLn("biasInput toogled: " ++ show value)
	
-- Called when the pointer is moved. See the above comment for the unusual
-- handling of the Shift-Key.
--
-- Minor remarks:
--   * Looses image when changing focus, e.g. to the close button, because
--     the Expose-Event is fired. 
button :: DrawingArea -> EventM EMotion Bool
button draw = do
    m <- eventModifier
    when (m == [Shift]) $ do
        w     <- eventWindow
        (x,y) <- eventCoordinates
        liftIO $ renderWithDrawable w $ do
            arc x y 3.0 0 (2*pi)
            fill
    return True


update :: EventM EExpose Bool
update = do
    win   <- eventWindow
    liftIO $ do
    putStrLn "Update"
    (w,h) <- drawableGetSize win
    let width  = realToFrac w
        height = realToFrac h

    renderWithDrawable win $ do
        setSourceRGB 1 1 1
        rectangle 0 0 width height
        fill

    return True

clearButtonClicked darea = do
    drw <- widgetGetDrawWindow darea
    (w,h) <- drawableGetSize drw
    let width  = realToFrac w
        height = realToFrac h
    
    renderWithDrawable drw $ do
        setSourceRGB 1 1 1
        rectangle 0 0 width height
        fill


openFileDialog parentWindow title filePathIO = do
    dialog <- fileChooserDialogNew 
                    (Just title)
                    (Just parentWindow)
                    FileChooserActionOpen	[("gtk-cancel", ResponseCancel), ("gtk-open", ResponseAccept)]
    widgetShow dialog
    resp <- dialogRun dialog
    case resp of
        ResponseAccept      -> do filePath <- fileChooserGetFilename dialog
                                  let path = case filePath of
                                        (Just s) -> s
                                        Nothing  -> error "Error on ResponseAccept in openFileDialog"
                                  modifyIORef filePathIO (\_ -> path)
        ResponseCancel      -> return ()
        ResponseDeleteEvent -> return ()
        _                   -> return ()
    widgetHide dialog


openFolderDialog parentWindow title filePathIO = do
    dialog <- fileChooserDialogNew 
                    (Just title)
                    (Just parentWindow)
                    FileChooserActionSelectFolder [("gtk-cancel", ResponseCancel), ("gtk-open", ResponseAccept)]
    widgetShow dialog
    resp <- dialogRun dialog
    case resp of
        ResponseAccept      -> do filePath <- fileChooserGetFilename dialog
                                  let path = case filePath of
                                        (Just s) -> s
                                        Nothing  -> error "Error on ResponseAccept in openFolderDialog"
                                  modifyIORef filePathIO (\_ -> path)
        ResponseCancel      -> return ()
        ResponseDeleteEvent -> return ()
        _                   -> return ()
    widgetHide dialog
    
genTopologyStr :: Trainingdata -> Bool -> Bool -> Int -> String
genTopologyStr tdata iBias hBias hiddenLen = do
	let inputLen = length $ head (inputs tdata)		-- get size of one input
	let outputLen = length $ head (outputs tdata)	-- get size of one output
	let b1  | iBias == True = "b"
			| otherwise = ""
	let b2  | hBias == True = "b"
			| otherwise = ""

	(show inputLen ++ b1 ++ "\n" ++ show hiddenLen ++ b2 ++ "\n" ++ show outputLen ++ "\n")

updateNetworkSize tdata inputNeurons_spin outputNeurons_spin = do
	let inputLen = length $ head (inputs tdata)		-- get size of one input
	let outputLen = length $ head (outputs tdata)	-- get size of one output
	spinButtonSetValue inputNeurons_spin (fromIntegral inputLen)
	spinButtonSetValue outputNeurons_spin (fromIntegral outputLen)
		
{--		
appendText = do
	table <- textTagTableNew
    buff <- textBufferNew (Just table)
    --table <- textTagTableNew
    
    --textBufferSetText "hallo"
    return buff
--}
