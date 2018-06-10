{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Sound.Pulse.Simple as Pulse
import Control.Exception (bracket)
import qualified Math.FFT as FFT
import Data.Array.IArray (amap, listArray)
import qualified Data.Array.IArray as IArray
import Data.Complex (Complex(..))
import Pipes
import qualified Pipes.Prelude as Pipes
import Graphics.Rendering.OpenGL (GLfloat)
import Graphics.DynamicGraph.FillLine
import Graphics.DynamicGraph.Line
import Graphics.DynamicGraph.Window
import Control.Monad (unless, guard, forever, when)
import Control.Error.Util (exceptT)
import Foreign.C.Types (CInt (..))
import Data.Foldable (for_)
import Data.Maybe (isJust)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson.TH
import qualified Data.Yaml as Yaml
import System.Environment (getArgs)
import Debug.Trace

-- key presses
-- --------------------------------------------------------------------

foreign import ccall "fakekey_init"    fakeKeyInit    :: IO ()
foreign import ccall "fakekey_press"   fakeKeyPress   :: CInt -> IO ()
foreign import ccall "fakekey_release" fakeKeyRelease :: CInt -> IO ()

keyCodeToX11KeySym :: String -> Maybe CInt
keyCodeToX11KeySym code = case code of
  "KeyA" -> Just 0x0061
  "KeyB" -> Just 0x0062
  "KeyC" -> Just 0x0063
  "KeyD" -> Just 0x0064
  "KeyE" -> Just 0x0065
  "KeyF" -> Just 0x0066
  "KeyG" -> Just 0x0067
  "KeyH" -> Just 0x0068
  "KeyI" -> Just 0x0069
  "KeyJ" -> Just 0x006a
  "KeyK" -> Just 0x006b
  "KeyL" -> Just 0x006c
  "KeyM" -> Just 0x006d
  "KeyN" -> Just 0x006e
  "KeyO" -> Just 0x006f
  "KeyP" -> Just 0x0070
  "KeyQ" -> Just 0x0071
  "KeyR" -> Just 0x0072
  "KeyS" -> Just 0x0073
  "KeyT" -> Just 0x0074
  "KeyU" -> Just 0x0075
  "KeyV" -> Just 0x0076
  "KeyW" -> Just 0x0077
  "KeyX" -> Just 0x0078
  "KeyY" -> Just 0x0079
  "KeyZ" -> Just 0x007a
  "ArrowLeft" -> Just 0xff51
  "ArrowUp" -> Just 0xff52
  "ArrowRight" -> Just 0xff53
  "ArrowDown" -> Just 0xff54
  "Space" -> Just 0x0020
  _ -> Nothing

-- fft
-- --------------------------------------------------------------------

sampleRate :: Int
sampleRate = 16000

numSamples :: Int
numSamples = 512

fftFreqs :: [Float]
fftFreqs = 0 : (do
  let binWidth :: Float = fromIntegral sampleRate / fromIntegral numSamples
  traceM ("bin width: " ++ show binWidth)
  ix <- fromIntegral <$> [1..numSamples]
  let freq = binWidth * ix
  -- filter frequencies above 2000, they're not very useful for what
  -- we're doing
  guard (freq < 2000)
  return freq)

fft :: [Float] -> [Float]
fft signal = IArray.elems $ amap
  (\(real :+ imag) -> sqrt (real ** 2 + imag ** 2))
  (FFT.dftRC (listArray (0, numSamples-1) signal))

-- reading audio
-- --------------------------------------------------------------------

data Graph = Audio | Frequency
  deriving (Show, Eq)
instance Aeson.FromJSON Graph where
  parseJSON = Aeson.withText "Graph" $ \case
    "Audio" -> return Audio
    "Frequency" -> return Frequency
    txt -> fail ("Invalid graph " ++ show txt)

data Params = Params
  { paramsAmplitudeCoefficient :: GLfloat
  , paramsControls :: Maybe (String, String) -- ^ what to press when low and high
  , paramsPrintBestFreq :: Bool -- ^ wether to print what the chosen frequency
  , paramsAmplitudeTreshold :: GLfloat -- ^ what the minimum scaled peak frequency should be to trigger high / low
  , paramsGraph :: Graph
  } deriving (Eq, Show)
Aeson.TH.deriveFromJSON
  Aeson.defaultOptions{ Aeson.fieldLabelModifier = drop (length ("params" :: String))}
  ''Params

data Frame = Frame
  { frameAudio :: [GLfloat]
  , frameFrequencies :: [GLfloat] -- ^ the frequencies amplitudes are scaled by paramsAmplitudeCoefficient
  }

audioFreqs :: Pulse.Simple -> Params -> Producer Frame IO void
audioFreqs spl Params{..} = forever $ do
  frameAudio <- lift (Pulse.simpleRead spl numSamples)
  let frameFrequencies = take (length fftFreqs) (map (* paramsAmplitudeCoefficient) (fft frameAudio))
  yield Frame{..}

data LowHigh = Low | High
  deriving (Eq, Show)

computeLowHigh :: Params -> [GLfloat] -> IO (Maybe LowHigh)
computeLowHigh Params{..} freqs = do
  let weightedAvg = sum (zipWith (*) fftFreqs freqs)
  let mbLowHigh = do
        guard (maximum freqs > paramsAmplitudeTreshold)
        return (if weightedAvg < 650 then Low else High)
  when (paramsPrintBestFreq && isJust mbLowHigh) $ do
    putStrLn ("best freq: " ++ show weightedAvg)
  return mbLowHigh

control :: Params -> Pipe Frame Frame IO ()
control params@Params{..} = go Nothing
  where
    go mbLowHigh = do
      frame@(Frame _ freqs) <- await
      newMbLowHigh <- lift (computeLowHigh params freqs)
      lift $ case (mbLowHigh, newMbLowHigh) of
        (Nothing, Nothing) -> return ()
        (Nothing, Just newLowHigh) -> do
          putStrLn ("Going from idle to " ++ show newLowHigh)
          keyPress newLowHigh
        (Just lowHigh, Nothing) -> do
          putStrLn ("Going from " ++ show lowHigh ++ " to idle")
          keyRelease lowHigh
        (Just lowHigh, Just newLowHigh) | lowHigh == newLowHigh -> return ()
        (Just lowHigh, Just newLowHigh) -> do
          putStrLn ("Going from " ++ show lowHigh ++ " to " ++ show lowHigh)
          keyRelease lowHigh
          keyPress newLowHigh
      yield frame
      go newMbLowHigh

    keyPress lowHigh = for_ paramsControls $ \(lowKey, highKey) -> case lowHigh of
      Low -> do
        Just keyCode <- return (keyCodeToX11KeySym lowKey)
        fakeKeyPress keyCode
      High -> do
        Just keyCode <- return (keyCodeToX11KeySym highKey)
        fakeKeyPress keyCode

    keyRelease lowHigh = for_ paramsControls $ \(lowKey, highKey) -> case lowHigh of
      Low -> do
        Just keyCode <- return (keyCodeToX11KeySym lowKey)
        fakeKeyRelease keyCode
      High -> do
        Just keyCode <- return (keyCodeToX11KeySym highKey)
        fakeKeyRelease keyCode

    {-
    {- ARKANOID -}
    keyPress = \case
      KSLeft -> do
        Just keyCode <- return (keyCodeToX11KeySym "ArrowLeft")
        fakeKeyPress keyCode
      KSRight -> do
        Just keyCode <- return (keyCodeToX11KeySym "ArrowRight")
        fakeKeyPress keyCode
    keyRelease = \case
      KSLeft -> do
        Just keyCode <- return (keyCodeToX11KeySym "ArrowLeft")
        fakeKeyRelease keyCode
      KSRight -> do
        Just keyCode <- return (keyCodeToX11KeySym "ArrowRight")
        fakeKeyRelease keyCode
    -}

    {- ROBOT UNICORN -}
    {-
    keyPress = \case
      KSLeft -> do
        Just keyCode <- return (keyCodeToX11KeySym "KeyZ")
        fakeKeyPress keyCode
      KSRight -> do
        Just keyCode <- return (keyCodeToX11KeySym "KeyX")
        fakeKeyPress keyCode
    keyRelease = \case
      KSLeft -> do
        Just keyCode <- return (keyCodeToX11KeySym "KeyZ")
        fakeKeyRelease keyCode
      KSRight -> do
        Just keyCode <- return (keyCodeToX11KeySym "KeyX")
        fakeKeyRelease keyCode
    -}

main :: IO ()
main = do
  [cfg] <- getArgs
  params <- either (error . Yaml.prettyPrintParseException) id <$> Yaml.decodeFileEither cfg
  fakeKeyInit
  bracket
    (Pulse.simpleNew
      Nothing "pitch-control" Pulse.Record Nothing "Pitch controls for your keyboard"
      (Pulse.SampleSpec (Pulse.F32 Pulse.LittleEndian) sampleRate 1) Nothing Nothing)
    Pulse.simpleFree
    (\spl -> exceptT error return $ do
        res <- lift setupGLFW
        unless res (error "Unable to initilize GLFW")
        render <- window 1024 480 $ fmap pipeify $ case paramsGraph params of
          Frequency -> renderFilledLine (length fftFreqs) jet_mod
          Audio -> renderLine numSamples 1024
        let data_ = Pipes.map $ case paramsGraph params of
              Frequency -> frameFrequencies
              Audio -> map ((+ 0.5) . (/ 2)) . frameAudio
        lift (runEffect (audioFreqs spl params >-> control params >-> data_ >-> render)))

{-
main :: IO ()
main = do
  fakeKeyInit
  bracket
    (simpleNew Nothing "pitch-control" Record Nothing "Pitch controls for your keyboard"
      (SampleSpec (F32 LittleEndian) sampleRate 1) Nothing Nothing)
    simpleFree
    (\spl -> exceptT error return $ do
        res <- lift setupGLFW
        unless res (error "Unable to initilize GLFW")
        pangoCtx <- lift (cairoCreateContext Nothing)
        render <- window 1024 480 (pipeify <$> renderFilledLine (length fftFreqs) jet_mod)
        lift (runEffect (audioFreqs spl >-> control >-> render)))
-}
