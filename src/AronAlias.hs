
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module AronAlias where
import Control.Monad
import qualified Control.Monad.IO.Class as CM
import Control.Concurrent
import Data.Array.IO
import Data.Char 
import qualified Data.List as L
import Data.List.Split
import Data.Time
import Data.Ratio
import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.Time.Clock.POSIX
import Data.Foldable (foldrM)
import Data.Typeable (typeOf) -- runtime type checker, typeOf "k"
import Data.Typeable 
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Posix.Files
import System.Posix.Unistd
import System.Posix.Types
import System.Process
import System.Random
import Text.Read (Read)
import Text.Regex
import Text.Regex.Base
import Text.Regex.Base.RegexLike
import Text.Regex.Posix
import Text.Printf
import Debug.Trace (trace)
import Text.Pretty.Simple (pPrint)

import qualified Data.Vector         as V
import qualified Data.HashMap.Strict as M
import qualified Text.Regex.TDFA     as TD
import qualified Text.Regex          as TR
import qualified Data.Set            as DS
import qualified Data.Word           as DW 

import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Lazy.Char8 as LC8 
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Lazy.Internal as IN (ByteString)
import qualified Data.ByteString.UTF8      as BSU
import qualified Data.ByteString           as BS
import qualified Data.Text.Lazy            as TL
import qualified Data.Text                 as TS
import qualified Data.Text.IO              as TSO
import qualified Data.Text.Encoding        as TSE
import qualified Data.Text.Lazy.Encoding   as TLE
import qualified Data.ByteString.Char8     as S8 (putStrLn, putStr, lines)   -- strict ?
import qualified Data.ByteString.Internal  as BSI (c2w) 
import qualified Data.Array as DR 
import Data.Array.IO

import Text.RawString.QQ (r)         -- Need QuasiQuotes too 

import AronModule
       
{-|
    === shorthand Write [String] to file
-}
wfs::FilePath->[String]->IO()
wfs p list = writeToFile p list

wf::FilePath->String->IO()
wf = writeFile

sw :: Show a => a -> String
sw = show

rev :: [a] -> [a]
rev = reverse
  
wfl = writeFileList

rfl = readFileLatin1ToList

rep = replicate

tr = trim

lent = len . trim

ft = filter

env = getEnv
ev = getEnv

cdEnv::String -> IO()
cdEnv s = getEnv s >>= cd

cde = cdEnv





