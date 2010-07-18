module Main where
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Glade
import Control.Monad


main = do
    initGUI
    Just xml    <- xmlNew "gui2.glade"
    window      <- xmlGetWidget xml castToWindow "window1"
    
    -- drawing area
    darea <- xmlGetWidget xml castToDrawingArea "drawingarea1"
    darea `on` sizeRequest $ return (Requisition 120 160)
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
    menuitem3 <- xmlGetWidget xml castToImageMenuItem "imagemenuitem3"
    menuitem4 <- xmlGetWidget xml castToImageMenuItem "imagemenuitem4"
    menuitem5 <- xmlGetWidget xml castToImageMenuItem "imagemenuitem5"

	-- checkbutton
    biasInput_check <- xmlGetWidget xml castToCheckButton "checkbutton1"
    biasHidden_check <- xmlGetWidget xml castToCheckButton "checkbutton2" 
     
    -- register events
    onClicked train_button (button1Clicked )
    onClicked work_button button2Clicked
    onClicked clear_button (clear darea)--(button3Clicked result_label)
         
    onValueSpinned momentum_spin (valueSpinned1 momentum_spin)
    onValueSpinned learningRate_spin (valueSpinned2 learningRate_spin)
    onValueSpinned inputNeurons_spin (valueSpinned3 inputNeurons_spin)
    onValueSpinned hiddenNeurons_spin (valueSpinned4 hiddenNeurons_spin)
    onValueSpinned outputNeurons_spin (valueSpinned5 outputNeurons_spin)
    onValueSpinned cycles_spin (valueSpinned6 cycles_spin)
    
    afterActivateLeaf menuitem1 menuItem1Select
    afterActivateLeaf menuitem2 menuItem2Select
    afterActivateLeaf menuitem3 menuItem3Select
    afterActivateLeaf menuitem4 menuItem4Select
    afterActivateLeaf menuitem5 menuItem5Select
    
    onDestroy window mainQuit
        
    -- main GUI
    widgetShowAll window
    mainGUI

------------------------------------------------------------------------
-- EVENTS
------------------------------------------------------------------------
button1Clicked = putStrLn "train button clicked"
button2Clicked = putStrLn "work button clicked"
button3Clicked label = do
    set label [ labelText := getResult ] --putStrLn "clear button clicked"

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

valueSpinned6 widget = do
	value <- spinButtonGetValueAsInt widget
	putStrLn ("cycles spinned: " ++ show value)

menuItem1Select = putStrLn "MenuItem1 selected"
menuItem2Select = putStrLn "MenuItem2 selected"
menuItem3Select = putStrLn "MenuItem3 selected"
menuItem4Select = putStrLn "MenuItem4 selected"
menuItem5Select = putStrLn "MenuItem5 selected"

------------------------------------------------------------------------
-- METHODS
------------------------------------------------------------------------
getResult = "YES!"

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

clear darea = do
    drw <- widgetGetDrawWindow darea
    (w,h) <- drawableGetSize drw
    let width  = realToFrac w
        height = realToFrac h
    
    renderWithDrawable drw $ do
        setSourceRGB 1 1 1
        rectangle 0 0 width height
        fill

{--
updateCanvas :: DrawingArea -> SVG -> Event -> IO Bool
updateCanvas canvas svg (Expose { eventArea=rect }) = do
  drawin <- widgetGetDrawWindow canvas
  let (width, height) = svgGetSize svg
  (width', height') <- widgetGetSize canvas
  renderWithDrawable drawin $ do
    scale (realToFrac width'  / realToFrac width)
          (realToFrac height' / realToFrac height)
    svgRender svg
  return True
--}
