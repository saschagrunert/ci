module Docker
  ( bla
  ) where

import Prelude hiding (FilePath)
import Turtle  (ExitCode, empty, shell)

bla :: IO ExitCode
bla = shell "ls -lah /usr; echo $SHELL" empty
