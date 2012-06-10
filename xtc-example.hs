import Graphics.UI.WX
import Graphics.UI.XTC

main :: IO ()
main = start $
 do {
    ; f <- frame []
    ; counterV <- mkValueEntry f [ typedValue := Just 1 ]

    ; listV <- mkListView f [ typedItems := ["abc", "def"]
                            , enabled := True
                            ]

    ; radioV <- mkRadioView f Vertical [ "Een", "Twee" ] []
    ; choiceV <- mkChoiceView f [ typedItems := ["un", "deux"]
                               , enabled := True
                               ]
--    ; comboV <- mkComboView f [ typedItems := ["sdfsdf", "fdssd"]
--                              , enabled := True
--                              ]
    ; t  <- textCtrl f []
    ; ve <- mkValueEntry f [ typedValue := Just True ]
--    ; set t [ on (change counterV) := \i -> set t [ text := show i ] ]

    ; bUp   <- button f [ text := "increase"
                        , on command := set counterV [ typedValue :~ fmap (+1) ] ]
    ; bDown <- button f [ text := "decrease"
                        , on command := set counterV [ typedValue :~ fmap (+ (-1::Int)) ]]

    ; let refreshT =
           do { -- s1 <- get comboV typedSelection
              ; s1 <- get radioV typedSelection
              ; s2 <- get listV typedMaybeSelection
              ; s3 <- get counterV typedValue
              ; s4 <- get choiceV typedMaybeSelection
              ; set t [ text := unlines [ "radio: " ++ s1
                      , "list:  " ++ show s2
                      , "counter: " ++ show s3
                      , "choice: " ++ show s4 ] ]
              }

    ; set bUp     [ on command :~ (>> refreshT) ]
    ; set bDown   [ on command :~ (>> refreshT) ]
    ; set listV   [ on select  :~ (>> refreshT) ]
    ; set radioV  [ on select  :~ (>> refreshT) ]
    ; set choiceV [ on select  :~ (>> refreshT) ]

--    ; bChangeHandler <- button f [ text := "change handler"
--                                 , on command := set t [ on (change counterV) := \i -> set t [text := "<<"++show i++">>"] ]]
    ; set f [ layout := column 5 [ row 5 [ Graphics.UI.WX.label "Counter value:", widget counterV ]
                                 , hfloatCenter $ row 5 [ widget bUp, widget bDown ]
                                 --, hfloatCenter $ widget bChangeHandler
                                 , widget listV
                                 , widget radioV
                                 , widget choiceV
--                                 , widget comboV
                                 , widget ve
                                 , glue
                                 , hfill $ widget t
                                 ]
                                 ]

    }
