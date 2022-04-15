import XMonad
import XMonad.Actions.SpawnOn ( spawnOn )
import XMonad.Hooks.SetWMName ( setWMName )
import XMonad.Layout.NoBorders ( noBorders )
import XMonad.Hooks.EwmhDesktops ( ewmh )

main = xmonad
  $ ewmh def
  { workspaces = ["1"],
    layoutHook = noBorders Full,
    startupHook = do
      setWMName "LG3D"
      spawnOn "1" "sleep 5; steamLoginWrapper"
  }
