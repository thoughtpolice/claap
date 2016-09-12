module Options
  ( myShakeOpts
  ) where

import Development.Shake

-- | Shake build options.
myShakeOpts :: ShakeOptions
myShakeOpts = shakeOptions
  { shakeFiles    = ".shake"
  , shakeProgress = progress
  , shakeVersion  = "1"
  , shakeTimings  = False
  }
  where
    -- | A progress bar that polls once every second, as opposed to once every
    -- five seconds.
    progress p = do
      program <- progressProgram
      progressDisplay 1 (\s -> progressTitlebar s >> program s) p
