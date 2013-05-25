{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface, CPP #-}

module JavaScript.Canvas ( TextAlign(..)
                         , TextBaseline(..)
                         , LineCap(..)
                         , LineJoin(..)
                         -- , initcvs
                         , save
                         , restore
                         , scale
                         , rotate
                         , translate
                         , transform
                         , setTransform
                         , fill
                         , stroke
                         , beginPath
                         , closePath
                         , clip
                         , moveTo
                         , lineTo
                         , quadraticCurveTo
                         , bezierCurveTo
                         , arc
                         , arcTo
                         , rect
                         , isPointInPath
                         , fillStyle
                         , strokeStyle
                         , globalAlpha
                         , lineJoin
                         , lineCap
                         , lineWidth
                         , setLineDash
                         , lineDashOffset
                         , miterLimit
                         , fillText
                         , strokeText
                         , textAlign
                         , textBaseline
                         , fillRect
                         , strokeRect
                         , clearRect
                         ) where

import GHCJS.Types
import GHCJS.Types.Internal
import GHCJS.Foreign
import Data.Text (Text)

data Canvas_
data Context_
type Canvas = JSRef Canvas_
type Context = JSRef Context_

data TextAlign = Start
               | End
               | Left
               | Right
               | Center
             deriving (Eq, Show, Enum)

data TextBaseline = Top 
                  | Hanging 
                  | Middle
                  | Alphabetic
                  | Ideographic
                  | Bottom
                deriving (Eq, Show, Enum)

data LineJoin = LineJoinBevel
              | LineJoinRound
              | LineJoinMiter
            deriving (Eq, Show, Enum)

data LineCap = LineCapButt
             | LineCapRound
             | LineCapSquare deriving (Eq, Show, Enum)

initcvs :: IO ()
initcvs = js_initcvs
{-# INLINE initcvs #-}

save :: IO ()
save = js_save
{-# INLINE save #-}

restore :: IO ()
restore = js_restore
{-# INLINE restore #-}

transform :: Double -> Double -> Double -> Double -> Double -> Double -> IO ()
transform = transform
{-# INLINE transform #-}

setTransform :: Double -> Double -> Double -> Double -> Double -> Double -> IO ()
setTransform = setTransform
{-# INLINE setTransform #-}

scale :: Double -> Double -> IO ()
scale = js_scale
{-# INLINE scale #-}

translate :: Double -> Double -> IO ()
translate = js_translate
{-# INLINE translate #-}

rotate :: Double -> IO ()
rotate = js_rotate
{-# INLINE rotate #-}

fill :: IO ()
fill = js_fill
{-# INLINE fill #-}

stroke :: IO ()
stroke = js_stroke
{-# INLINE stroke #-}

beginPath :: IO ()
beginPath = js_beginPath
{-# INLINE beginPath #-}

closePath :: IO ()
closePath = js_closePath
{-# INLINE closePath #-}

clip :: IO ()
clip = js_clip
{-# INLINE clip #-}

moveTo :: Double -> Double -> IO ()
moveTo = js_moveTo
{-# INLINE moveTo #-}

lineTo :: Double -> Double -> IO ()
lineTo = js_lineTo
{-# INLINE lineTo #-}

quadraticCurveTo :: Double -> Double -> Double -> Double -> IO ()
quadraticCurveTo = js_quadraticCurveTo
{-# INLINE quadraticCurveTo #-}

bezierCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> IO ()
bezierCurveTo = js_bezierCurveTo
{-# INLINE bezierCurveTo #-}

arc :: Double -> Double -> Double -> Double -> Double -> Bool -> IO ()
arc a b c d e bl = js_arc a b c d e (toJSBool bl) 
{-# INLINE arc #-}

arcTo :: Double -> Double -> Double -> Double -> Double -> IO ()
arcTo = js_arcTo
{-# INLINE arcTo #-}

rect :: Double -> Double -> Double -> Double -> IO ()
rect = js_rect
{-# INLINE rect #-}

isPointInPath :: Double -> Double -> IO ()
isPointInPath = js_isPointInPath
{-# INLINE isPointInPath #-}

fillStyle :: Int -> Int -> Int -> Double -> IO ()
fillStyle = js_fillStyle
{-# INLINE fillStyle #-}

strokeStyle :: Int -> Int -> Int -> Double -> IO ()
strokeStyle = js_strokeStyle
{-# INLINE strokeStyle #-}

globalAlpha :: Double -> IO ()
globalAlpha = js_globalAlpha
{-# INLINE globalAlpha #-}

lineJoin :: LineJoin -> IO ()
lineJoin lj = js_lineJoin (fromEnum lj) 
{-# INLINE lineJoin #-}

lineCap :: LineCap -> IO ()
lineCap lc  = js_lineCap (fromEnum lc) 
{-# INLINE lineCap #-}

miterLimit :: Double -> IO ()
miterLimit = js_miterLimit
{-# INLINE miterLimit #-}

setLineDash :: [Int] -> IO ()
setLineDash = error "setLineDash: not yet implemented"
{-# INLINE setLineDash #-}

lineDashOffset :: Double -> IO ()
lineDashOffset = js_lineDashOffset
{-# INLINE lineDashOffset #-}

textAlign :: TextAlign -> IO ()
textAlign = error "textAlign: not yet implemented"
{-# INLINE textAlign #-}

textBaseline :: TextBaseline -> IO ()
textBaseline = error "textBaseline: not yet implemented"
{-# INLINE textBaseline #-}

lineWidth :: Double -> IO ()
lineWidth = js_lineWidth
{-# INLINE lineWidth #-}

fillText :: Text -> Double -> Double -> IO ()
fillText t x y  = js_fillText (toJSString t) x y 
{-# INLINE fillText #-}

strokeText :: Text -> Double -> Double -> IO ()
strokeText t x y  = js_strokeText (toJSString t) x y 
{-# INLINE strokeText #-}

fillRect :: Double -> Double -> Double -> Double -> IO ()
fillRect = js_fillRect
{-# INLINE fillRect #-}

clearRect :: Double -> Double -> Double -> Double -> IO ()
clearRect = js_clearRect
{-# INLINE clearRect #-}

strokeRect :: Double -> Double -> Double -> Double -> IO ()
strokeRect = js_strokeRect
{-# INLINE strokeRect #-}

#ifdef __GHCJS__
foreign import javascript unsafe "var c = document.getElementById('mycanvas'); window.ctxt = c.getContext('2d')" js_initcvs  :: IO ()

foreign import javascript unsafe "window.ctxt.save()" js_save :: IO ()
foreign import javascript unsafe "window.ctxt.restore()" js_restore :: IO ()
foreign import javascript unsafe "window.ctxt.transform($1,$2,$3,$4,$5,$6)"
  js_transform :: Double -> Double -> Double -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "window.ctxt.setTransform($1,$2,$3,$4,$5,$6)"
  js_setTransform :: Double -> Double -> Double -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "window.ctxt.scale($1,$2)"     
  js_scale       :: Double -> Double -> IO ()
foreign import javascript unsafe "window.ctxt.translate($1,$2)" 
  js_translate   :: Double -> Double -> IO ()
foreign import javascript unsafe "window.ctxt.rotate($1)"       
  js_rotate      :: Double -> IO ()
foreign import javascript unsafe "window.ctxt.fill()" 
  js_fill        :: IO ()
foreign import javascript unsafe "window.ctxt.stroke()"        
  js_stroke      :: IO ()
foreign import javascript unsafe "window.ctxt.beginPath()" 
  js_beginPath   :: IO ()
foreign import javascript unsafe "window.ctxt.closePath()" 
  js_closePath   :: IO ()
foreign import javascript unsafe "window.ctxt.clip()" 
  js_clip        :: IO ()
foreign import javascript unsafe "window.ctxt.moveTo($1,$2)" 
  js_moveTo      :: Double -> Double -> IO ()
foreign import javascript unsafe "window.ctxt.lineTo($1,$2)" 
  js_lineTo      :: Double -> Double -> IO ()
foreign import javascript unsafe "window.ctxt.quadraticCurveTo($1,$2,$3,$4)"
  js_quadraticCurveTo :: Double -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "window.ctxt.bezierCurveTo($1,$2,$3,$4,$5,$6)"
  js_bezierCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "window.ctxt.arc($1,$2,$3,$4,$5,$6)"
  js_arc :: Double -> Double -> Double -> Double -> Double -> JSBool -> IO ()
foreign import javascript unsafe "window.ctxt.arcTo($1,$2,$3,$4,$5)"
  js_arcTo :: Double -> Double -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "window.ctxt.rect($1,$2,$3,$4)"
  js_rect :: Double -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "window.ctxt.isPointInPath($1,$2)"
  js_isPointInPath :: Double -> Double -> IO ()
foreign import javascript unsafe "window.ctxt.fillStyle = 'rgba(' + $1 + ',' + $2 + ',' + $3 + ',' + $4 + ')'"
  js_fillStyle :: Int -> Int -> Int -> Double -> IO ()
foreign import javascript unsafe "window.ctxtstrokeStyle = 'rgba(' + $1 + ',' + $2 + ',' + $3 + ',' + $4 + ')'"
  js_strokeStyle :: Int -> Int -> Int -> Double -> IO ()
foreign import javascript unsafe "window.ctxt.globalAlpha = $1" js_globalAlpha :: Double           -> IO ()
foreign import javascript unsafe "window.ctxt.lineJoin = ($1===0)?'bevel':(($1===1)?'round':'miter')"
                                                          js_lineJoin :: Int              -> IO ()
foreign import javascript unsafe "window.ctxt.lineCap = ($1===0)?'butt':(($1===1)?'round':'square')"
                                                           js_lineCap :: Int              -> IO ()
foreign import javascript unsafe "window.ctxt.miterLimit = $1"             js_miterLimit :: Double -> IO ()
foreign import javascript unsafe "window.ctxt.setLineDash($1)"  js_setLineDash :: JSArray JSNumber -> IO ()
foreign import javascript unsafe "window.ctxt.lineDashOffset = $1"     js_lineDashOffset :: Double -> IO ()
foreign import javascript unsafe "window.ctxt.font = $1"                                  js_font :: JSString -> IO ()
foreign import javascript unsafe "window.ctxt.textAlign = $1"                  js_textAlign :: Int -> IO ()
foreign import javascript unsafe "window.ctxt.textBaseline = $1"            js_textBaseline :: Int -> IO ()


foreign import javascript unsafe "window.ctxt.lineWidth = $1"     js_lineWidth :: Double           -> IO ()
foreign import javascript unsafe "window.ctxt.fillText($1,$2,$3)"
                                              js_fillText :: JSString -> Double -> Double -> IO ()
foreign import javascript unsafe "window.ctxt.strokeText($1,$2,$3)"
                                            js_strokeText :: JSString -> Double -> Double -> IO ()
foreign import javascript unsafe "window.ctxt.fillRect($1,$2,$3,$4)"
                                      js_fillRect :: Double -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "window.ctxt.clearRect($1,$2,$3,$4)"
                                     js_clearRect :: Double -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "window.ctxt.strokeRect($1,$2,$3,$4)"
                                    js_strokeRect :: Double -> Double -> Double -> Double -> IO ()

#else

#include "nonGhcjsStubs.txt"

#endif
