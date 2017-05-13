module H2Blocks.Data.Container
    ( Container(sysInfo, statVFS)
    , newContainer
    ) where

import           H2Blocks.Data.Cache
import qualified H2Blocks.System.StatVFS as VFS
import qualified H2Blocks.System.SysInfo as SI

data Container = MkContainer
    { sysInfo :: TimeSpec -> IO SI.SysInfo
    , statVFS :: FilePath -> TimeSpec -> IO VFS.StatVFS
    }

newContainer :: IO Container
newContainer = do
    si <- newMemo0 SI.sysInfo
    scv <- newMemo1 VFS.statVFS
    return $ MkContainer si scv
