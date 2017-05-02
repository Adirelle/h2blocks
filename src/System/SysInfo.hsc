{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module System.SysInfo
    ( SysInfo (..)
    , sysInfo
    ) where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal

#include <sys/sysinfo.h>

data SysInfo = SysInfo
    { uptime    :: CULong
    , loads     :: (CULong, CULong, CULong)
    , totalRam  :: CULong
    , freeRam   :: CULong
    , sharedRam :: CULong
    , bufferRam :: CULong
    , totalSwap :: CULong
    , freeSwap  :: CULong
    , procs     :: CShort
    , totalHigh :: CULong
    , freeHigh  :: CULong
    , memUnit   :: CInt
    }
    deriving (Show, Eq)

instance Storable SysInfo where
    alignment _ = #{alignment struct sysinfo}
    sizeOf _ = #{size struct sysinfo}

    peek ptr = do
        u   <- #{peek struct sysinfo, uptime   } ptr
        l1  <- #{peek struct sysinfo, loads[0] } ptr
        l5  <- #{peek struct sysinfo, loads[1] } ptr
        l15 <- #{peek struct sysinfo, loads[2] } ptr
        tr  <- #{peek struct sysinfo, totalram } ptr
        fr  <- #{peek struct sysinfo, freeram  } ptr
        sr  <- #{peek struct sysinfo, sharedram} ptr
        br  <- #{peek struct sysinfo, bufferram} ptr
        ts  <- #{peek struct sysinfo, totalswap} ptr
        fs  <- #{peek struct sysinfo, freeswap } ptr
        p   <- #{peek struct sysinfo, procs    } ptr
        th  <- #{peek struct sysinfo, totalhigh} ptr
        fh  <- #{peek struct sysinfo, freehigh } ptr
        mu  <- #{peek struct sysinfo, mem_unit } ptr
        return $ SysInfo u (l1, l5, l15) tr fr sr br ts fs p th fh mu

    poke ptr (SysInfo u (l1, l5, l15) tr fr sr br ts fs p th fh mu) = do
        #{poke struct sysinfo, uptime   } ptr u
        #{poke struct sysinfo, loads[0] } ptr l1
        #{poke struct sysinfo, loads[1] } ptr l5
        #{poke struct sysinfo, loads[2] } ptr l15
        #{poke struct sysinfo, totalram } ptr tr
        #{poke struct sysinfo, freeram  } ptr fr
        #{poke struct sysinfo, sharedram} ptr sr
        #{poke struct sysinfo, bufferram} ptr br
        #{poke struct sysinfo, totalswap} ptr ts
        #{poke struct sysinfo, freeswap } ptr fs
        #{poke struct sysinfo, procs    } ptr p
        #{poke struct sysinfo, totalhigh} ptr th
        #{poke struct sysinfo, freehigh } ptr fh
        #{poke struct sysinfo, mem_unit } ptr mu

foreign import ccall unsafe "sys/sysinfo.h sysinfo"
    c_sysinfo :: Ptr SysInfo -> IO Int

sysInfo :: IO SysInfo
sysInfo = alloca $ \ptr -> do
    throwErrnoIfMinus1_ "sysinfo" $ c_sysinfo ptr
    peek ptr
