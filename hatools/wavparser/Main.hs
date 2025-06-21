module Main where

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Binary.Get
import Data.Word
import Data.Int
import Data.Char (chr)
import System.Environment (getArgs)
import Text.Printf (printf)

data WavHeader = WavHeader
  { chunkId       :: String
  , chunkSize     :: Word32
  , format        :: String
  , subchunk1Id   :: String
  , subchunk1Size :: Word32
  , audioFormat   :: Word16
  , numChannels   :: Word16
  , sampleRate    :: Word32
  , byteRate      :: Word32
  , blockAlign    :: Word16
  , bitsPerSample :: Word16
  , subchunk2Id   :: String
  , subchunk2Size :: Word32
  } deriving Show

getFourCC :: Get String
getFourCC = map (chr . fromIntegral) <$> BS.unpack <$> getByteString 4

parseWavHeader :: Get WavHeader
parseWavHeader = do
  chunkId       <- getFourCC
  chunkSize     <- getWord32le
  format        <- getFourCC
  subchunk1Id   <- getFourCC
  subchunk1Size <- getWord32le
  audioFormat   <- getWord16le
  numChannels   <- getWord16le
  sampleRate    <- getWord32le
  byteRate      <- getWord32le
  blockAlign    <- getWord16le
  bitsPerSample <- getWord16le

  -- skip any remaining bytes in fmt chunk if needed
  let fmtExtraSize = fromIntegral subchunk1Size - 16
  if fmtExtraSize > 0 then skip fmtExtraSize else pure ()

  -- read until "data" chunk
  -- some WAVs have additional chunks like "LIST" or "fact"
  let findDataChunk = do
        cid <- getFourCC
        size <- getWord32le
        if cid == "data"
          then return (cid, size)
          else skip (fromIntegral size) >> findDataChunk

  (subchunk2Id, subchunk2Size) <- findDataChunk

  return WavHeader
    { chunkId       = chunkId
    , chunkSize     = chunkSize
    , format        = format
    , subchunk1Id   = subchunk1Id
    , subchunk1Size = subchunk1Size
    , audioFormat   = audioFormat
    , numChannels   = numChannels
    , sampleRate    = sampleRate
    , byteRate      = byteRate
    , blockAlign    = blockAlign
    , bitsPerSample = bitsPerSample
    , subchunk2Id   = subchunk2Id
    , subchunk2Size = subchunk2Size
    }


audioFormatName :: Word16 -> String
audioFormatName 0x0001 = "PCM (Uncompressed)"
audioFormatName 0x0003 = "IEEE Float"
audioFormatName 0x0006 = "ALaw"
audioFormatName 0x0007 = "MuLaw"
audioFormatName 0x0010 = "OKI ADPCM"
audioFormatName 0x0011 = "IMA ADPCM (DVI)"
audioFormatName 0x0050 = "MPEG (e.g. MP3)"
audioFormatName 0x0161 = "Windows Media Audio (WMA)"
audioFormatName 0xFFFE = "WAVE_FORMAT_EXTENSIBLE"
audioFormatName code   = "Unknown format (" ++ show code ++ ")"

printWavInfo :: WavHeader -> IO ()
printWavInfo header = do
  putStrLn $ "Audio Format:     " ++ show (audioFormat header) ++ " (" ++ (audioFormatName $ audioFormat header) ++ ")"
  putStrLn $ "Channels:         " ++ show (numChannels header)
  putStrLn $ "Sample Rate:      " ++ show (sampleRate header) ++ " Hz"
  putStrLn $ "Byte Rate:        " ++ show (byteRate header) ++ " B/s"
  putStrLn $ "Block Align:      " ++ show (blockAlign header) ++ " bytes"
  putStrLn $ "Bits per Sample:  " ++ show (bitsPerSample header)
  putStrLn $ "Data Size:        " ++ show (subchunk2Size header) ++ " bytes"
  let duration = fromIntegral (subchunk2Size header) / fromIntegral (byteRate header) :: Double
  printf "Duration:         %.3f seconds\n" duration

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- BL.readFile filename
      let header = runGet parseWavHeader contents
      printWavInfo header
    _ -> putStrLn "Usage: wavparser <file.wav>"
