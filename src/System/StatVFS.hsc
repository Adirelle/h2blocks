{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module System.StatVFS
    ( StatVFS (..)
    , statVFS
    ) where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal

#include <sys/statvfs.h>

data StatVFS = StatVFS
    { blockSize :: CULong
    , total :: CULong
    , free :: CULong
    , available :: CULong
    }
    deriving (Show, Eq)

instance Storable StatVFS where
    alignment _ = #{alignment struct statvfs}
    sizeOf _ = #{size struct statvfs}

    peek ptr = do
        s <- #{peek struct statvfs, f_bsize  } ptr
        t <- #{peek struct statvfs, f_blocks } ptr
        f <- #{peek struct statvfs, f_bfree  } ptr
        a <- #{peek struct statvfs, f_bavail } ptr
        return $ StatVFS s t f a

    poke ptr (StatVFS s t f a) = do
        #{poke struct statvfs, f_bsize  } ptr s 
        #{poke struct statvfs, f_blocks } ptr t 
        #{poke struct statvfs, f_bfree  } ptr f 
        #{poke struct statvfs, f_bavail } ptr a 

foreign import ccall unsafe "sys/statvfs.h statvfs"
    c_statvfs :: CString -> Ptr StatVFS -> IO Int

statVFS :: FilePath -> IO StatVFS
statVFS fp = 
    withCString fp $ \fp' ->
        alloca $ \ptr -> do
            throwErrnoPathIfMinus1_ "statvfs" fp $ c_statvfs fp' ptr
            peek ptr
