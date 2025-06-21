module Main where

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Get as BG
import Data.Word
import Data.Int
import Data.Char (chr)
import System.Environment (getArgs)

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

getFourCC :: BG.Get String
getFourCC = map (chr . fromIntegral) <$> BL.unpack <$> BL.fromStrict <$> BG.getByteString 4

parseWavHeader :: BG.Get WavHeader
parseWavHeader = do
  chunkId       <- getFourCC
  chunkSize     <- BG.getWord32le
  format        <- getFourCC
  subchunk1Id   <- getFourCC
  subchunk1Size <- BG.getWord32le
  audioFormat   <- BG.getWord16le
  numChannels   <- BG.getWord16le
  sampleRate    <- BG.getWord32le
  byteRate      <- BG.getWord32le
  blockAlign    <- BG.getWord16le
  bitsPerSample <- BG.getWord16le

  -- skip any remaining bytes in fmt chunk if needed
  let fmtExtraSize = fromIntegral subchunk1Size - 16
  if fmtExtraSize > 0 then BG.skip fmtExtraSize else pure ()

  -- read until "data" chunk
  -- some WAVs have additional chunks like "LIST" or "fact"
  let findDataChunk = do
        cid <- getFourCC
        size <- BG.getWord32le
        if cid == "data"
          then return (cid, size)
          else BG.skip (fromIntegral size) >> findDataChunk

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

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- BL.readFile filename
      let header = BG.runGet parseWavHeader contents
      print header
    _ -> putStrLn "Usage: wavparser <file.wav>"
