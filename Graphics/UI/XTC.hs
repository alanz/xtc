--------------------------------------------------------------------------------
{-| 
    Module      :  XTC
    Copyright   :  (c) Martijn Schrage 2005

    Maintainer  :  martijn@cs.uu.nl
    Stability   :  experimental
    Portability :  portable


    XTC: eXtended & Typed Controls for wxHaskell
    
    The XTC library provides a typed interface to several wxHaskell controls.

      - radio view (typed radio box)

      - single-selection list view (typed single-selection list box)

      - muliple-selection list view (typed multiple-selection list box)

      - choice view (typed choice box)

      - value entry (typed text entry)

    XTC controls keep track of typed values and items, rather than
    being string based. Selections in XTC controls consist of actual values
    instead of indices.
-}
--------------------------------------------------------------------------------
module Graphics.UI.XTC (
              -- * Classes
             Labeled( toLabel )
           , TypedValued( typedValue )
           , TypedItems( typedItems )
           , TypedSelection( typedSelection )
           , TypedMaybeSelection( typedMaybeSelection )
           , TypedSelections( typedSelections )
           , Observable( change )
             -- * Controls
             -- ** Radio view
           , RadioView, mkRadioView, mkRadioViewEx
             -- ** Single-selection list view
           , ListView, mkListView, mkListViewEx
             -- ** Multiple-selection list view
           , MultiListView, mkMultiListView, mkMultiListViewEx
             -- ** Choice view
           , ChoiceView, mkChoiceView, mkChoiceViewEx
             -- ** Value entry
           , ValueEntry, mkValueEntry, mkValueEntryEx
           ) where

import Graphics.UI.WX hiding (window, label)
import Graphics.UI.WXCore hiding (label, Event)
import Data.List
import Data.Maybe

-- | The labeled class is used by 'mkRadioView', 'mkListView', 'mkMultiListView', and
--   'mkChoiceView' for conveniently passing the function that maps an item onto its label.
class Labeled x where
  toLabel :: x -> String

instance Labeled String where
  toLabel str = str

-- | Widgets that have a typed selection. The selection can be accessed via the attribute 'typedSelection', and has type @x@.
class Selection w => TypedSelection x w | w -> x where
  typedSelection :: Attr w x

-- | Widgets that have a typed selection that may be empty. The selection can be accessed via the attribute 'typedMaybeSelection', and has type @Maybe x@.
class Selection w => TypedMaybeSelection x w | w -> x where
  typedMaybeSelection :: Attr w (Maybe x)

-- | Widgets that have a typed list of selections. The selection list can be accessed via the attribute 'typedSelections', and has type @[x]@.
class Selections w => TypedSelections x w | w -> x where
  typedSelections :: Attr w [x]

-- | Widgets that have a typed list of items. The item list can be accessed via the attribute 'typedItems', and has type @[x]@.
class Items w String => TypedItems x w | w -> x where
  typedItems :: Attr w [x]

-- | Widgets that have a typed value. The value can be accessed via the attribute 'typedValue', and has type @x@.
class TypedValued  x w | w -> x where
  typedValue :: Attr w (Maybe x)


{--------------------------------------------------------------------------------
  Radio view
--------------------------------------------------------------------------------}

data CRadioView x b

-- | Pointer to a radio view, deriving from 'RadioBox'.
type RadioView x b = RadioBox (CRadioView x b)

instance TypedSelection x (RadioView x ()) where
  typedSelection
    = newAttr "typedSelection" radioViewGetTypedSelection radioViewSetTypedSelection

instance TypedItems x (RadioView x ()) where
  typedItems = newAttr "typedItems" viewGetTypedItems viewSetTypedItems

-- | Create a new radio view with an initial orientation and a list of
-- typed items. The item type (@x@) must be an instance of 'Labeled' to show each item's
-- label. Use attribute 'typedSelection' to access the currently selected item, and 'typedItems' to access the list of items. Note:
-- for a radio view (or radio box) the items may not be modified dynamically.
--
-- * Instances: 'TypedSelection', 'TypedItems', 'Selecting','Selection','Items' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
mkRadioView :: Labeled x => Window a -> Orientation -> [x] -> [Prop (RadioView x ())] -> IO (RadioView x ())
mkRadioView window orientation viewItems props =
  mkRadioViewEx window toLabel orientation viewItems props

-- | Create a new radio view with an initial orientation and a list of
-- typed items. A function of type @(x -> String)@ maps items onto labels. 
-- Use attribute 'typedSelection' to access the currently selected item, and 'typedItems' to access the list of items. Note:
-- for a radio view (or radio box) the items may not be modified dynamically.
--
-- * Instances: 'TypedSelection', 'Selecting','Selection','Items' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
mkRadioViewEx :: Window a -> (x -> String) -> Orientation -> [x] -> [Prop (RadioView x ())] -> IO (RadioView x ())
mkRadioViewEx window present orientation viewItems props =
 do { model <- varCreate viewItems
    ; radioView <- fmap objectCast $ radioBox window orientation (map present viewItems) []
    ; objectSetClientData radioView (return ()) (model, present)
    ; set radioView props
    ; return radioView
    } -- cannot use mkViewEx because items must be set at creation (items is not writeable)


radioViewSetTypedSelection :: RadioView x () -> x -> IO ()
radioViewSetTypedSelection radioView x = viewSetTypedMaybeSelection radioView (Just x)

radioViewGetTypedSelection :: RadioView x () -> IO x
radioViewGetTypedSelection radioView = 
 do { mSel <- viewGetTypedMaybeSelection radioView
    ; case mSel of
        Just item -> return item
        Nothing   -> internalError "XTC" "radioViewGetTypedSelection" "Radio view has empty selection"
    }
     
{--------------------------------------------------------------------------------
  Single-selection list view
--------------------------------------------------------------------------------}

data CListView a b

-- | Pointer to a single-selection list view, deriving from 'SingleListBox'.
type ListView a b = SingleListBox (CListView a b)

instance TypedMaybeSelection x (ListView x ()) where
  typedMaybeSelection = newAttr "typedMaybeSelection" viewGetTypedMaybeSelection viewSetTypedMaybeSelection

instance TypedItems x (ListView x ()) where
  typedItems = newAttr "typedItems" viewGetTypedItems viewSetTypedItems

-- | Create a single-selection list view. The item type (@x@) must be an instance of 'Labeled' to show each item's
-- label. Use attribute 'typedMaybeSelection' to access the currently selected item, and 'typedItems' to access the list of items.
--
-- * Instances: 'TypedMaybeSelection', 'TypedItems', 'Sorted','Selecting','Selection','Items' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
mkListView :: Labeled x => Window a -> [Prop (ListView x ())] -> IO (ListView x ())
mkListView window props = mkListViewEx window toLabel props

-- | Create a single-selection list view. A function of type @(x -> String)@ maps items onto labels. 
-- Use attribute 'typedMaybeSelection' to access the currently selected item, and 'typedItems' to access the list of items.
--
-- * Instances: 'TypedMaybeSelection', 'TypedItems', 'Sorted','Selecting','Selection','Items' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
mkListViewEx :: Window a -> (x -> String) -> [Prop (ListView x ())] -> IO (ListView x ())
mkListViewEx window present props = mkViewEx singleListBox window present props


{--------------------------------------------------------------------------------
  Multiple-selection list view
--------------------------------------------------------------------------------}

data CMultiListView a b

-- | Pointer to a multiple-selection list view, deriving from 'MultiListBox'.
type MultiListView a b = MultiListBox (CMultiListView a b)

instance TypedSelections x (MultiListView x ()) where
  typedSelections = newAttr "typedSelections" multiListViewGetTypedSelections multiListViewSetTypedSelections

instance TypedItems x (MultiListView x ()) where
  typedItems = newAttr "typedItems" viewGetTypedItems viewSetTypedItems

-- | Create a multiple-selection list view. The item type (@x@) must be an instance of 'Labeled' to show each item's
-- label.
-- Use attribute 'typedSelections' to access the currently selected items, and 'typedItems' to access the list of items.
--
-- * Instances: 'TypedSelections', 'TypedItems', 'Sorted', 'Selecting','Selections','Items' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
mkMultiListView :: Labeled x => Window a -> [Prop (MultiListView x ())] -> IO (MultiListView x ())
mkMultiListView window props = mkMultiListViewEx window toLabel props

-- | Create a multiple-selection list view. A function of type @(x -> String)@ maps items onto labels. 
-- Use attribute 'typedSelections' to access the currently selected items, and 'typedItems' to access the list of items.
--
-- * Instances: 'TypedSelections', 'TypedItems', 'Sorted', 'Selecting','Selections','Items' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
mkMultiListViewEx :: Window a -> (x -> String) -> [Prop (MultiListView x ())] -> IO (MultiListView x ())
mkMultiListViewEx window present props = mkViewEx multiListBox window present props

multiListViewSetTypedSelections :: MultiListView x () -> [x] -> IO ()
multiListViewSetTypedSelections (multiListView :: MultiListView x ()) selectionItems =
 do { Just ((model, present) :: (Var [x], x -> String)) <-
        unsafeObjectGetClientData multiListView
    ; viewItems <- get model value
    ; let labels = map present selectionItems
    ; let indices = catMaybes [ findIndex (\it -> present it == label) viewItems
                              | label <- labels ]
    ; set multiListView [ selections := indices ]
    }

multiListViewGetTypedSelections :: forall x . MultiListView x () -> IO [x]
multiListViewGetTypedSelections multiListView =
 do { Just ((model, _) :: (Var [x], x -> String)) <-
        unsafeObjectGetClientData multiListView
    ; selectedIndices <- get multiListView selections
    ; viewItems <- get model value
    ; return (map (safeIndex "XTC.multiListViewGetTypedSelections" viewItems)
                    selectedIndices)
    }


{--------------------------------------------------------------------------------
  Choice view
--------------------------------------------------------------------------------}

data CChoiceView a b

-- | Pointer to a choice view, deriving from 'Choice'.
type ChoiceView a b = Choice (CChoiceView a b)

instance TypedMaybeSelection x (ChoiceView x ()) where
  typedMaybeSelection = newAttr "typedMaybeSelection" viewGetTypedMaybeSelection viewSetTypedMaybeSelection

instance TypedItems x (ChoiceView x ()) where
  typedItems = newAttr "typedItems" viewGetTypedItems viewSetTypedItems

-- | Create a choice view to select one item from a list of typed items. The item type (@x@) must be an instance of 'Labeled' to show each item's
-- label.
-- Use attribute 'typedMaybeSelection' to access the currently selected item, and 'typedItems' to access the list of items.
--
-- * Instances: 'TypedMaybeSelection', 'TypedItems', 'Sorted', 'Selecting','Selection','Items' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
mkChoiceView :: Labeled x => Window a -> [Prop (ChoiceView x ())] -> IO (ChoiceView x ())
mkChoiceView window (props :: [Prop (ChoiceView x ())]) =
  mkViewEx choice window (toLabel :: x -> String) props

-- | Create a choice view to select one item from a list of typed items. A function of type @(x -> String)@ maps items onto labels. 
-- Use attribute 'typedMaybeSelection' to access the currently selected item, and 'typedItems' to access the list of items.
--
-- * Instances: 'TypedMaybeSelection', 'TypedItems', 'Sorted', 'Selecting','Selection','Items' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
mkChoiceViewEx :: Window a -> (x -> String) -> Style -> [Prop (ChoiceView x ())] -> IO (ChoiceView x ())
mkChoiceViewEx window present stl props =
  mkViewEx (\win -> choiceEx win stl) window present props


-- Generic constructors, getters, and setters

-- Generic mk function that puts a model and a present function in the client data.
-- Used for ListView, MultiListView, and ChoiceView.
mkViewEx :: (parent -> [p] -> IO (Object a)) -> parent -> (x -> String) -> [Prop (WxObject b)] ->
            IO (WxObject b)
mkViewEx mkView window present props =
 do { model <- varCreate []
    ; view <- fmap objectCast $ mkView window []
    ; objectSetClientData view (return ()) (model, present)
    ; set view props
    ; return view
    }

-- Generic getTypedMaybeSelection for RadioView, ListView, and ChoiceView.
viewGetTypedMaybeSelection :: forall x a . Selection (WxObject a) => WxObject a -> IO (Maybe x)
viewGetTypedMaybeSelection view =
 do { Just ((model, _) :: (Var [x], x -> String)) <-
        unsafeObjectGetClientData view
    ; selectedIndex <- get view selection
    ; if selectedIndex == -1
      then return Nothing
      else do { viewItems <- get model value
              ; return $ Just (safeIndex "XTC.viewGetTypedMaybeSelection" viewItems selectedIndex)
              }
    }

-- Generic setTypedMaybeSelection for RadioView, ListView, and ChoiceView.
viewSetTypedMaybeSelection :: forall x a . Selection (WxObject a) => WxObject a -> Maybe x -> IO ()
viewSetTypedMaybeSelection view mSelectionItem =
 do { Just ((model, present) :: (Var [x], x -> String)) <-
        unsafeObjectGetClientData view
    ; viewItems <- get model value
    ; let index = case mSelectionItem of
                    Nothing            -> -1
                    Just selectionItem -> let label = present selectionItem
                                          in  findLabelIndex present label viewItems
    ; set view [ selection := index ]
    }
 where findLabelIndex :: (x -> String) -> String -> [x] -> Int
       findLabelIndex present label theItems =
         case findIndex (\it -> present it == label) theItems of
           Just ix -> ix
           Nothing -> -1
           
-- Generic getTypedItems for ListView, MultiListView, and ChoiceView.
viewGetTypedItems :: forall x a . TypedItems x (WxObject a) => WxObject a -> IO [x]
viewGetTypedItems view =
 do { Just ((model, _) :: (Var [x], x -> String)) <-
        unsafeObjectGetClientData view
    ; viewItems <- get model value
    ; return viewItems
    }

-- Generic setTypedItems for ListView, MultiListView, and ChoiceView.
viewSetTypedItems :: forall x a . TypedItems x (WxObject a) => WxObject a -> [x] -> IO ()
viewSetTypedItems view viewItems =
 do { Just ((model, present) :: (Var [x], x -> String)) <-
        unsafeObjectGetClientData view
    ; set model [ value := viewItems ]
    ; set view [ items := map present viewItems ]
    }


{--------------------------------------------------------------------------------
  Value entry
--------------------------------------------------------------------------------}

data CValueEntry x b

-- | Pointer to a choice view, deriving from 'TextCtrl'.
type ValueEntry x b = TextCtrl (CValueEntry x b)

instance TypedValued x (ValueEntry x ()) where
  typedValue
    = newAttr "typedValue" valueEntryGetTypedValue valueEntrySetTypedValue

-- | Create a single-line value entry control. The value type (@x@) must be an instance of 'Show' and 'Read'
-- to present a value as a string in the entry and parse the string from the entry back to (maybe) a value.
-- Use 'typedValue' to access the value.
-- Note: 'alignment' has to
-- be set at creation time (or the entry has default alignment (=left) ).
--
-- * Instances: 'TypedValued', 'Wrap', 'Aligned', 'Commanding' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
mkValueEntry :: (Show x, Read x) => Window b -> [ Prop (ValueEntry x ()) ] -> IO (ValueEntry x ())
mkValueEntry window props = mkValueEntryEx window show readParse props

-- | Create a single-line value entry control. The two functions of type @(x -> String)@ and @(String -> Maybe x)@ are used
-- to present a value as a string in the entry and parse the string from the entry back to (maybe) a value.
-- Use 'typedValue' to access the value.
-- Note: 'alignment' has to
-- be set at creation time (or the entry has default alignment (=left) ).
--
-- * Instances: 'TypedValued', 'Wrap', 'Aligned', 'Commanding' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
mkValueEntryEx :: Window b -> (x -> String) -> (String -> Maybe x) -> [ Prop (ValueEntry x ()) ] -> IO (ValueEntry x ())
mkValueEntryEx window present parse props =
 do { valueEntry <- fmap objectCast $ textEntry window []
    ; objectSetClientData valueEntry (return ()) (present, parse)
    ; set valueEntry $ props ++ [ on change :~ \handler -> do {validate valueEntry; handler} ]
    ; validate valueEntry
    ; return valueEntry
    }
 where validate :: ValueEntry x () -> IO ()
       validate valueEntry =
        do { mVal <- get valueEntry typedValue
           ; set valueEntry [ bgcolor := case mVal of
                                           Nothing -> rgb 255 100 100
                                           _       -> white
                            ]
           ; repaint valueEntry
           } -- drawing a squiggly is not possible because font metrics are not available

valueEntryGetTypedValue :: forall x . ValueEntry x () -> IO (Maybe x)
valueEntryGetTypedValue valueEntry =
 do { Just ((_, parse) :: (x -> String, String -> Maybe x)) <- unsafeObjectGetClientData valueEntry
    ; valueStr <- get valueEntry text
    ; return $ parse valueStr
    }

valueEntrySetTypedValue :: forall x . ValueEntry x () -> Maybe x -> IO ()
valueEntrySetTypedValue valueEntry mValue =
 do { Just ((present, _) :: (x -> String, String -> Maybe x)) <- unsafeObjectGetClientData valueEntry
    ; case mValue of
        Nothing    -> return ()
        Just theValue -> set valueEntry [ text := present theValue ]
    }


-- Utility functions

-- A variation of 'read' that returns Nothing if the string cannot be parsed.
readParse :: Read x => String -> Maybe x
readParse str = case reads str of
                  [(x, "")] -> Just x
                  _         -> Nothing

safeIndex :: String -> [a] -> Int -> a
safeIndex msg xs i
    | i >= 0 && i < length xs = xs !! i
    | otherwise = internalError "XTC" "safeIndex" msg

internalError :: String -> String -> String -> a
internalError moduleName functionName errorString =
    error (moduleName ++ "." ++ functionName ++ ": " ++ errorString)


-- Some bits that should be part of wxHaskell

instance Selecting (ChoiceView x ()) where
  select = newEvent "select" choiceGetOnCommand choiceOnCommand
-- Necessary because wxHaskell declares "instance Selecting (Choice ())" instead of
-- "Selecting (Choice a)".

instance Selection (ChoiceView x ()) where
  selection = newAttr "selection" choiceGetSelection choiceSetSelection
-- Necessary because wxHaskell declares "instance Selection (Choice ())" instead of
-- "Selection (Choice a)".


-- The Observable class is missing from wxHaskell, even though the components are there.
class Observable w where
  change :: Event w (IO ())

instance Observable (TextCtrl a) where
  change = newEvent "change" (controlGetOnText) (controlOnText)
