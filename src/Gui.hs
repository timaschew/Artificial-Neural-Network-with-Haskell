module Main where

-- ANN
import Neuron
import Trainingdata
import Backpropagation
import Utils
import TopologyParser
import TraindataParser
import GraphicInterface

import Control.Parallel.Strategies
import Data.IORef
import Data.List
import Data.Word
import Data.Array.Storable
import Control.Concurrent
import Maybe
import Text.Printf

-- Gui
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Glade
import Control.Monad


{--

NOTE: mouse pressed event does not work. 
      you have to hold the shift button instead, while moving the mouse
--}


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
    scrolledWindow <- xmlGetWidget xml castToScrolledWindow "scrolledwindow1"
    statusbar <- xmlGetWidget xml castToStatusbar "statusbar1"
    result_label <- xmlGetWidget xml castToLabel "label10"
     
    -- menuitems
    menuitem2 <- xmlGetWidget xml castToImageMenuItem "imagemenuitem2"

	-- checkbutton
    biasInput_check <- xmlGetWidget xml castToCheckButton "checkbutton1"
    biasHidden_check <- xmlGetWidget xml castToCheckButton "checkbutton2" 
     
------------------------------------------------------------------------
-- ANN, SETTINGS
             
    flip widgetSetSensitivity False inputNeurons_spin    
    flip widgetSetSensitivity False outputNeurons_spin
    
    -- init traindata
    tdataPathIO <- newIORef (dataPath ++ "traindata/img/10_12_anton/versions/all/")
    tdataPath <- readIORef tdataPathIO
    tdata <- dirToTrainData tdataPath
    tdataIO <- newIORef tdata
    
    -- init pattern
    pattern <- readPPMFile (dataPath ++ "traindata/img/10_12_great_times/" ++ "1.pgm")
    patternIO <- newIORef pattern
    
    biasInput <- toggleButtonGetActive biasInput_check
    biasInputIO <- newIORef biasInput
    
    biasHidden <- toggleButtonGetActive biasHidden_check
    biasHiddenIO <- newIORef biasHidden
    
    hiddenNeurons <- spinButtonGetValueAsInt hiddenNeurons_spin
    hiddenNeuronsIO <- newIORef hiddenNeurons
    
    net <- initNetwork (genTopologyStr tdata biasInput biasHidden hiddenNeurons)
    netIO <- newIORef net
    updateNetworkSize tdata inputNeurons_spin outputNeurons_spin

    momentum <- spinButtonGetValue momentum_spin
    momentumIO <- newIORef momentum
    
    learningRate <- spinButtonGetValue learningRate_spin
    learningRateIO <- newIORef learningRate
    
    cycles <- spinButtonGetValueAsInt cycles_spin
    cyclesIO <- newIORef cycles
    
    trainedCountIO <- newIORef 0
    
    resultListIO <- newIORef [0.0]
    
    pgmNamesIO <- newIORef [""]
    updatePgmNames tdataPathIO pgmNamesIO

    updateStatusBar statusbar trainedCountIO
    appendLog textview ("tdata path: " ++ tdataPath ++ "\n")

------------------------------------------------------------------------
-- REGISTER EVENTS

	-- BUTTONS
    onClicked clear_button (clearButtonClicked darea)
    
    onClicked train_button $do
		nn <- readIORef netIO
		td <- readIORef tdataIO
		cycles <- readIORef cyclesIO
		trainedCount <- readIORef trainedCountIO
		
		-- training
		mvar <- newEmptyMVar
		forkIO (forkTraining mvar nn td cycles momentumIO learningRateIO)
		trainedNet <- takeMVar mvar
		
		-- disable network configs after 1. training
		disableNetConfigs hiddenNeurons_spin biasInput_check biasHidden_check momentum_spin learningRate_spin
		
		-- update network
		modifyIORef netIO (\_ -> trainedNet)
		modifyIORef trainedCountIO (\_ -> trainedCount + cycles)
		updateStatusBar statusbar trainedCountIO
		putStrLn ("network trained!")
		return ()	
    
    onClicked work_button $do
		pValues <- pixelValues darea textview
		
		let inputLen = fromIntegral $ length $ head (inputs tdata)	-- get size of one input
		let scale = 12 -- 120x144 / 12 => 10x12 - has to match the input layer length!
		putStrLn ("scaleDiv: " ++ show scale)
		let width = 120
		let pattern = map (\x -> realToFrac (pValues !! x)) [x | x <- [0 .. (length pValues)-1], x `mod` scale == 0 && (x `mod` (width*scale)) < width]
		modifyIORef patternIO (\_ -> pattern)
		putStrLn (show $ length pattern)
    
		let pgmHeader = ("P2\n" ++ "#Created with ANN\n" ++ "10 12\n" ++ "255\n")
		let pgmData = zipWith (\p i -> if i `mod` 20 == 0 then (show (truncate p) ++"\n") else (show (truncate p) ++ " ")) pattern [1..]
		writeFile "drawingarea_sclaed.pgm" (pgmHeader ++ (concat pgmData))

		pgmNames <- readIORef pgmNamesIO
		(bestIdx, bestVal) <- findBestFit netIO patternIO resultListIO
		-- update label
		setLabel result_label (pgmNames !! bestIdx)
		putStrLn ("index: " ++ show bestIdx ++ " bestVal: " ++ show bestVal) -- ++ show resultList)
		
		resultList <- readIORef resultListIO
		appendLog textview $ concat $ zipWith (\r n -> ("[" ++ n ++ "]" ++ " " ++ (printf "%02.2f%%" (r*100)) ++ "  ")) resultList pgmNames    
		putStrLn $ show resultList
	
	-- SPINBUTTONS
    onValueSpinned momentum_spin (valueSpinned1 momentum_spin)
    onValueSpinned learningRate_spin (valueSpinned2 learningRate_spin)
    --onValueSpinned inputNeurons_spin (valueSpinned3 inputNeurons_spin)
    onValueSpinned hiddenNeurons_spin (valueSpinned4 hiddenNeurons_spin)
    --onValueSpinned outputNeurons_spin (valueSpinned5 outputNeurons_spin)
    onValueSpinned cycles_spin (valueSpinned6 cycles_spin cyclesIO)
    
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
		-- update pgmNames
		updatePgmNames tdataPathIO pgmNamesIO
		-- update statusbar
		modifyIORef trainedCountIO (\_ -> 0)
		updateStatusBar statusbar trainedCountIO
		
		-- enable network configs on new tdata
		enableNetConfigs hiddenNeurons_spin biasInput_check biasHidden_check momentum_spin learningRate_spin
		
		appendLog textview ("tdata path: " ++ path ++ "\n")
		putStrLn ("network: " ++ genTopologyStr tdata b1 b2 hiddenLen)
		putStrLn ("tdata path: " ++ path)

    onToggled biasInput_check (onToggledInputBias biasInput_check biasInputIO)
    onToggled biasHidden_check (onToggledHiddenBias biasHidden_check biasHiddenIO)

    darea `on` sizeRequest $ return (Requisition 120 144)
    darea `on` exposeEvent $ update
    
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
updatePgmNames tdataPathIO pgmNamesIO = do
	path <- readIORef tdataPathIO
	pgmList <- getPgmList path
	-- get filenames before dot: 0.ppm => 0
	let pgmNames = map (\x -> fst (break (=='.') x)) pgmList	
	modifyIORef pgmNamesIO (\_ -> pgmNames)

-- returns a downscaled list of pixel values. 
-- (from 120x144 drawingarea -> to 10x12 pattern, because of 10x12 trainingdata files
pixelValues darea textview = do
	drw <- widgetGetDrawWindow darea
	
	let width = 120
	let height = 144
	
	pixbuf <- pixbufGetFromDrawable drw (Rectangle 0 0 width height)
	pixels <- (pixbufGetPixels (fromJust pixbuf) :: IO (PixbufData Int Word8)) 

	-- pixels is a flat array. 
	-- formula: p = y * rowstride + x * nChannels
	-- pixel Words: w * h * 3 channels
	-- => every third value is a pixel value
	pValues <- mapM (\x -> readArray pixels x) [x | x <- [0..(width * height * 3 - 1)], x `mod` 3 == 0]	
	
	-- save drawing area as png file
	pixbufSave (fromJust pixbuf) "drawingarea.png" "png" [("","")]
	
	-- create a simple pgm file
	let pgmHeader = ("P2\n" ++ "#Created with ANN\n" ++ "120 144\n" ++ "255\n")
	let pgmData = zipWith (\p i -> if i `mod` 20 == 0 then (show p ++"\n") else (show p ++ " ")) pValues [1..]
	writeFile "drawingarea.pgm" (pgmHeader ++ (concat pgmData))
	
	putStrLn ("w: " ++ show width)
	putStrLn ("h: " ++ show height)
	return pValues

forkTraining mvar net train cycles momentumIO learningRateIO = do
    momentum <- readIORef momentumIO
    learningRate <- readIORef learningRateIO
    
    let trainedNet = trainNet net train cycles momentum learningRate
    putMVar mvar trainedNet
    putStrLn $ (show trainedNet)


findBestFit netIO patternIO resultListIO = do
	trainedNet <- readIORef netIO
	pattern <- readIORef patternIO
	
	let resultList = work trainedNet pattern
	let bestVal = maximum resultList
	let bestIdx = bestVal `elemIndex` resultList
	
	modifyIORef resultListIO (\_ -> resultList)
	return ((fromJust bestIdx), bestVal)

setLabel label text = do
    set label [ labelText := text ]
    return ()

valueSpinned1 widget = do
	value <- spinButtonGetValue widget
	return ()
	
valueSpinned2 widget = do
	value <- spinButtonGetValue widget
	return ()
	
valueSpinned3 widget = do
	value <- spinButtonGetValueAsInt widget
	return ()
	
valueSpinned4 widget = do
	value <- spinButtonGetValueAsInt widget
	return ()

valueSpinned5 widget = do
	value <- spinButtonGetValueAsInt widget
	return ()

valueSpinned6 widget cyclesIO = do
	value <- spinButtonGetValueAsInt widget
	modifyIORef cyclesIO (\_ -> value)
	return ()

onToggledInputBias cb biasInputIO = do
	value <- toggleButtonGetActive cb
	modifyIORef biasInputIO (\_ -> value)
	return ()

onToggledHiddenBias cb biasHiddenIO = do
	value <- toggleButtonGetActive cb
	modifyIORef biasHiddenIO (\_ -> value)
	return ()
	
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
            arc x y 10.0 0 (2*pi)
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
    

updateNetworkSize tdata inputNeurons_spin outputNeurons_spin = do
	let inputLen = length $ head (inputs tdata)		-- get size of one input
	let outputLen = length $ head (outputs tdata)	-- get size of one output
	spinButtonSetValue inputNeurons_spin (fromIntegral inputLen)
	spinButtonSetValue outputNeurons_spin (fromIntegral outputLen)
		
appendLog textview newtext = do
    textBuffer <- textViewGetBuffer textview
    charCount <- textBufferGetCharCount textBuffer
    let delimiter | charCount > 0 = "\n" 
				  | otherwise = ""
    endIter <- textBufferGetEndIter textBuffer
    textBufferInsert textBuffer endIter (delimiter ++ newtext)
    
    -- scrolls textview to bottom
    onBufferChanged textBuffer $do
		endIter <- textBufferGetEndIter textBuffer
		mark <- textBufferCreateMark textBuffer Nothing endIter False
		textViewScrollMarkOnscreen textview mark

updateStatusBar statusbar trainedCountIO = do
	trainedCount <- readIORef trainedCountIO
	contextId <- statusbarGetContextId statusbar "status"
	statusbarPush statusbar contextId ("training steps absolved: " ++ show trainedCount)

disableNetConfigs hiddenNeurons_spin biasInput_check biasHidden_check momentum_spin learningRate_spin = do
    flip widgetSetSensitivity False hiddenNeurons_spin    
    flip widgetSetSensitivity False biasInput_check
    flip widgetSetSensitivity False biasHidden_check
    flip widgetSetSensitivity False momentum_spin
    flip widgetSetSensitivity False learningRate_spin

enableNetConfigs hiddenNeurons_spin biasInput_check biasHidden_check momentum_spin learningRate_spin = do
    flip widgetSetSensitivity True hiddenNeurons_spin    
    flip widgetSetSensitivity True biasInput_check
    flip widgetSetSensitivity True biasHidden_check
    flip widgetSetSensitivity True momentum_spin
    flip widgetSetSensitivity True learningRate_spin
