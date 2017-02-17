{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Codec.Picture
import Data.Aeson.Types
import Data.Aeson.Lens
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toStrict)
import Data.Conduit
import Data.Conduit.Binary (sinkFile, sinkLbs)
import qualified Data.Conduit.Combinators as CC
import Data.Either
import Data.List.Split (splitOn)
import Data.String (fromString)
import Data.Text (Text, unpack)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Control.Applicative (liftA2, liftA3)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Graphics.Rendering.Cairo
import Network.HTTP.Simple

data ProgramData = ProgramData Int Int Text

someFunc :: IO ()
someFunc = do
  makeSurface ("new.png", ProgramData 10 20 "Good show")
  loadInfo >>= mapM ((>>= (traverse makeSurface)) . loadPng)
           >>= (\surfacesE -> do
             let err = lefts surfacesE
             mapM_ print err
             concatVSurfaces (rights surfacesE) >>= (`surfaceWriteToPNG` "res.png")
               )
                    
    where loadPng info = runExceptT $ do
            (url, programData) <- ExceptT $ return info
            filename <- ExceptT $ loadFileAsPng url
            return (filename, programData)

concatVSurfaces surfaces = do
  heights <- mapM imageSurfaceGetHeight surfaces
  widths <- mapM imageSurfaceGetWidth surfaces
  let totalHeight = sum heights
      maxWidth    = maximum widths
      [widthsD, heightsD] = fmap (fmap fromIntegral) [widths, heights]
  canvas <- createImageSurface FormatARGB32 maxWidth totalHeight
  renderWith canvas . mapM_ drawSurface $ zip3 surfaces widthsD heightsD
  return canvas
  where drawSurface (surface, width, height) = do
          setSourceSurface surface 0 0
          moveTo 0 0
          lineTo width 0
          lineTo width height
          lineTo 0 height
          fill
          translate 0 height

makeSurface (filename, ProgramData tb te title) = do
  img <- imageSurfaceCreateFromPNG filename
  [width, height] <- mapM ($ img) [imageSurfaceGetWidth, imageSurfaceGetHeight]
  canvas <- createImageSurface FormatARGB32 width (20 + height)
  renderWith canvas $ do
    let [widthD, heightD] = fmap fromIntegral [width, height]
    translate 0 18
    setFontSize 20
    let [tb', te'] = fmap showTime [tb, te]
    textPath $ tb' ++ " - " ++ te' ++ ": " ++ unpack title
    setLineWidth 1
    stroke
    translate 0 2
    setSourceSurface img 0 0
    moveTo 0 0
    lineTo widthD 0
    lineTo widthD heightD
    lineTo 0 heightD
    fill
  return canvas

showTime x =  showCh h ++ ":" ++ showCh m
  where d = mod x (24 * 3600)
        m = div (mod d 3600) 60
        h = div d 3600
        showCh x
          | x < 10    = '0': show x
          | otherwise = show x
  

loadInfo :: IO [Either String (Text, ProgramData)]
loadInfo = fmap getInfo
         . (^.. (key "data" . key "programs". values)) . getResponseBody
        <$> (httpJSON url :: IO (Response Value))
  where url = "https://api.ovva.tv/v2/ru/tvguide/1plus1/2017-02-16"
        getInfo obj = maybe (Left "Wrong info format") Right
          $ liftA2 (,) (obj ^? (key "image" . key "preview" . _String))
          $ liftA3 ProgramData (obj ^? (key "realtime_begin" . _Integral))
                               (obj ^? (key "realtime_end" . _Integral))
                               (obj ^? (key "title" . _String))

loadFile :: Text -> ResourceT IO ()
loadFile urlT = httpSink (fromString url) . const $ sinkFile filename'
  where url = unpack urlT
        filename' = last $ splitOn "/" url

jpegToPng filename = B.readFile filename >>= traverse (savePngImage (toPngExt filename)) . decodeImage

loadFileAsPng :: Text -> IO (Either String String)
loadFileAsPng urlT = runResourceT (httpSource urlB getResponseBody $$ sinkLbs)
                 >>= traverse saveFile . decodeImage . toStrict
  where url  = unpack urlT
        urlB = fromString url
        filename' = toPngExt . last $ splitOn "/" url
        saveFile = (>> (return filename')) . savePngImage filename'

toPngExt = fst . foldr addChar ("", False)
  where addChar '.' (res, False)  = (".png", True)
        addChar x   (res, wasDot) = (x : res, wasDot)

