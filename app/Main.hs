import Core (AppEvent (..), AppModel (..), buildUI, handleEvent)
import Monomer (appFontDef, appInitEvent, appTheme, appWindowIcon, appWindowTitle, darkTheme, startApp)

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config =
      [ appWindowTitle "Haskell Snake Game",
        appWindowIcon "./assets/images/icon.png",
        appTheme darkTheme,
        appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
        appInitEvent AppInit
      ]
    model = AppModel 0
