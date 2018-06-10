{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Sound.Pulse.Simple as Pulse
import Control.Exception (bracket, finally)
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
import qualified Data.Yaml.Pretty as Yaml.Pretty
import System.Environment (getArgs)
import Data.IORef
import qualified Data.ByteString.Char8 as BSC8

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
  , paramsAmplitudeLowTreshold :: GLfloat
  , paramsAmplitudeHighTreshold :: GLfloat
  -- ^ what the minimum scaled peak frequency should be to trigger high / low
  , paramsFrequncyTreshold :: GLfloat
  } deriving (Eq, Show)
Aeson.TH.deriveJSON
  Aeson.defaultOptions{ Aeson.fieldLabelModifier = drop (length ("params" :: String))}
  ''Params

data Options = Options
  { optionsPrintBestFreq :: Bool -- ^ wether to print what the chosen frequency
  , optionsControls :: Maybe (String, String) -- ^ what to press when low and high
  , optionsGraph :: Graph
  } deriving (Eq, Show)
Aeson.TH.deriveFromJSON
  Aeson.defaultOptions{ Aeson.fieldLabelModifier = drop (length ("options" :: String))}
  ''Options

data Frame = Frame
  { frameAudio :: [GLfloat]
  , frameFrequencies :: [GLfloat]
  -- ^ the frequencies amplitudes are scaled by paramsAmplitudeCoefficient
  }

-- returns the unscaled frame
readFrame :: Pulse.Simple -> IO Frame
readFrame spl = do
  frameAudio <- Pulse.simpleRead spl numSamples
  let frameFrequencies = take (length fftFreqs) (fft frameAudio)
  return Frame{..}

audioFreqs :: Pulse.Simple -> Params -> Producer Frame IO void
audioFreqs spl Params{..} = forever $ do
  frame <- lift (readFrame spl)
  yield frame{ frameFrequencies = map (* paramsAmplitudeCoefficient) (frameFrequencies frame) }

data LowHigh = Low | High
  deriving (Eq, Show)

computeLowHigh :: Options -> Params -> [GLfloat] -> IO (Maybe LowHigh)
computeLowHigh Options{..} Params{..} freqs = do
  let weightedAvg = sum (zipWith (*) fftFreqs freqs) / sum freqs
  let mbLowHigh = do
        let lowHigh = if weightedAvg < paramsFrequncyTreshold then Low else High
        guard $ case lowHigh of
          Low -> maximum freqs > paramsAmplitudeLowTreshold
          High -> maximum freqs > paramsAmplitudeHighTreshold
        return lowHigh
  when (optionsPrintBestFreq && isJust mbLowHigh) $ do
    putStrLn ("best freq: " ++ show weightedAvg)
  return mbLowHigh

control :: Options -> Params -> Pipe Frame Frame IO ()
control options@Options{..} params@Params{..} = go Nothing
  where
    go mbLowHigh = do
      frame@(Frame _ freqs) <- await
      newMbLowHigh <- lift (computeLowHigh options params freqs)
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

    keyPress lowHigh = for_ optionsControls $ \(lowKey, highKey) -> case lowHigh of
      Low -> do
        Just keyCode <- return (keyCodeToX11KeySym lowKey)
        fakeKeyPress keyCode
      High -> do
        Just keyCode <- return (keyCodeToX11KeySym highKey)
        fakeKeyPress keyCode

    keyRelease lowHigh = for_ optionsControls $ \(lowKey, highKey) -> case lowHigh of
      Low -> do
        Just keyCode <- return (keyCodeToX11KeySym lowKey)
        fakeKeyRelease keyCode
      High -> do
        Just keyCode <- return (keyCodeToX11KeySym highKey)
        fakeKeyRelease keyCode

calibrate :: Pulse.Simple -> IORef GLfloat -> Producer Frame IO void
calibrate spl maxFreqRef = forever $ do
  maxFreq <- lift (readIORef maxFreqRef)
  frame <- lift (readFrame spl)
  lift (writeIORef maxFreqRef (max maxFreq (maximum (frameFrequencies frame))))
  yield frame

main :: IO ()
main = do
  bracket
    (Pulse.simpleNew
      Nothing "pitch-control" Pulse.Record Nothing "Pitch controls for your keyboard"
      (Pulse.SampleSpec (Pulse.F32 Pulse.LittleEndian) sampleRate 1) Nothing Nothing)
    Pulse.simpleFree
    (\spl -> exceptT error return $ do
      res <- lift setupGLFW
      unless res (error "Unable to initilize GLFW")
      args <- lift getArgs
      case args of
        ["calibrate"] -> do
          maxFreqRef <- lift (newIORef 0)
          render <- window 1024 480 (fmap pipeify (renderFilledLine (length fftFreqs) jet_mod))
          let tranformFreqs = forever $ do
                frame <- await
                maxFreq <- lift (readIORef maxFreqRef)
                yield (map (/ maxFreq) (frameFrequencies frame))
          lift $ finally (runEffect (calibrate spl maxFreqRef >-> tranformFreqs >-> render)) $ do
            maxFreq <- readIORef maxFreqRef
            BSC8.putStrLn $ Yaml.Pretty.encodePretty Yaml.Pretty.defConfig Params
              { paramsAmplitudeCoefficient = 1 / maxFreq
              , paramsAmplitudeLowTreshold = 0.1
              , paramsAmplitudeHighTreshold = 0.2
              , paramsFrequncyTreshold = 700
              }
        ["run", paramsFile, optionsFile] -> do
          params <- lift (either (error . Yaml.prettyPrintParseException) id <$> Yaml.decodeFileEither paramsFile)
          options <- lift (either (error . Yaml.prettyPrintParseException) id <$> Yaml.decodeFileEither optionsFile)
          lift fakeKeyInit
          render <- window 1024 480 $ fmap pipeify $ case optionsGraph options of
            Frequency -> renderFilledLine (length fftFreqs) jet_mod
            Audio -> renderLine numSamples 1024
          let data_ = Pipes.map $ case optionsGraph options of
                Frequency -> frameFrequencies
                Audio -> map ((+ 0.5) . (/ 2)) . frameAudio
          lift (runEffect (audioFreqs spl params >-> control options params >-> data_ >-> render)))

