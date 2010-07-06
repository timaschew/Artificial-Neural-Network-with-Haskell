module Main where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

main = do
     initGUI
     Just xml    <- xmlNew "gui2.glade"
     window      <- xmlGetWidget xml castToWindow "window1"
     onDestroy window mainQuit
     
     widgetShowAll window
     mainGUI
