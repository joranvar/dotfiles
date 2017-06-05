import System.Environment (lookupEnv)

import System.Taffybar

import System.Taffybar.TaffyPager
import System.Taffybar.Pager

import System.Taffybar.Battery
import System.Taffybar.SimpleClock
import System.Taffybar.Systray
import System.Taffybar.MPRIS2
import System.Taffybar.Weather

import System.Taffybar.Widgets.PollingGraph

import System.Information.Memory
import System.Information.CPU


import Control.Applicative ((<$>))

pagerCfg :: PagerConfig
pagerCfg = defaultPagerConfig
     { emptyWorkspace = colorize "#6b6b6b" "" . escape
     , visibleWorkspace = colorize "#429942" "" . escape . wrap "(" ")"
-- --    { emptyWorkspace = \xs -> ""
     , activeWorkspace  = colorize "#429942" "" . escape . wrap "[" "]"
     }


main :: IO ()
main = do
    scr <- maybe 0 read <$> lookupEnv "TAFFY_SCREEN"

    let -- wss = wspaceSwitcherNew pager
        -- wnd = windowSwitcherNew pager

        pager = taffyPagerNew pagerCfg
        clock = textClockNew Nothing "<span fgcolor='orange'>%Y %m %d %H:%M:%S</span>" 1
        mpris = mpris2New
        battery = textBatteryNew "$percentage$%/$time$" 60
        tray = systrayNew
        wea = weatherNew (defaultWeatherConfig "EHBK") 10
--        vol = volumeW

        mem = pollingGraphNew memCfg 1 memCallback
            where
                memCallback = do
                    mi <- parseMeminfo
                    return [memoryUsedRatio mi]
                memCfg = defaultGraphConfig
                    { graphDataColors = [(1, 0, 0, 1)]
                    , graphLabel = Nothing
                    , graphDirection = RIGHT_TO_LEFT
                    }

        cpu = pollingGraphNew cpuCfg 1 cpuCallback
            where
                cpuCallback = do
                    (_, _, totalLoad) <- cpuLoad
                    return [totalLoad]
                cpuCfg = defaultGraphConfig
                    { graphDataColors = [(0, 1, 0, 1)]
                    , graphLabel = Nothing
                    , graphDirection = RIGHT_TO_LEFT
                    }

    defaultTaffybar defaultTaffybarConfig
        { barHeight = 20
        , monitorNumber = scr
        , startWidgets = [pager]
        -- , startWidgets = [wss, wnd]
        -- , startWidgets = [wss]
        , endWidgets = reverse $ if scr == 0
            then [wea, mpris, cpu, mem, clock, battery, tray]
            else [clock]
        }
