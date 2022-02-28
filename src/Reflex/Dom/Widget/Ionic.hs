{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Dom.Widget.Ionic
  (ionInputElement)
where

import Control.Lens (Identity(..), imapM_, iforM_, (^.), makeLenses)
import Control.Monad.Ref
import Data.Maybe
import Data.IORef
import Control.Monad
import Data.Functor.Misc
import Control.Monad.Fix
import GHCJS.DOM.Document (Document, createDocumentFragment, createElement, createElementNS, createTextNode, createComment)
import GHCJS.DOM.Types (liftJSM, askJSM, runJSM, JSM, MonadJSM, FocusEvent, IsElement, IsEvent, IsNode, KeyboardEvent, Node, TouchEvent, WheelEvent, uncheckedCastTo, ClipboardEvent)
import Reflex.Class as Reflex
import Reflex.Host.Class

import qualified GHCJS.DOM as DOM

import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.FileList as FileList
import qualified GHCJS.DOM.GlobalEventHandlers as Events
import qualified GHCJS.DOM.HTMLInputElement as Input

import qualified GHCJS.DOM.Text as DOM
import qualified GHCJS.DOM.Types as DOM

import Reflex.Dom.Widget.Basic
import Reflex.Dom.Widget.Input

import Reflex.Dom.Builder.Class
import Reflex.Dom.Builder.Immediate
import Reflex.Dom.Builder.Class.Events
import Reflex.Dom.Class
import Reflex.Dom.Old
import Reflex.Dynamic
import Data.Text(Text)
import qualified Data.Map as Map
import Data.Map (Map)

ionInputElement
  :: (MonadWidget t m, DomRenderHook t m)
  => Text -> InputElementConfig EventResult t GhcjsDomSpace -> m (InputElement EventResult GhcjsDomSpace t)
ionInputElement tag cfg  = do
  (e, ()) <- element tag  (cfg ^. inputElementConfig_elementConfig)  $ return ()
  let domInputElement = uncheckedCastTo DOM.HTMLInputElement $ _element_raw e
      eventSelector = _element_events e
  Input.setValue domInputElement $ cfg ^. inputElementConfig_initialValue
  v0 <- Input.getValue domInputElement
  let getMyValue = Input.getValue domInputElement
  valueChangedByUI <- requestDomAction $ liftJSM getMyValue <$ Reflex.select eventSelector (WrapArg Input)
  valueChangedBySetValue <- case _inputElementConfig_setValue cfg of
    Nothing -> return never
    Just eSetValue -> requestDomAction $ ffor eSetValue $ \v' -> do
      Input.setValue domInputElement v'
      getMyValue 
  v <- holdDyn v0 $ leftmost
    [ valueChangedBySetValue
    , valueChangedByUI
    ]
  Input.setChecked domInputElement $ _inputElementConfig_initialChecked cfg
  checkedChangedByUI <- wrapDomEvent domInputElement (`DOM.on` Events.click) $ do
    Input.getChecked domInputElement
  checkedChangedBySetChecked <- case _inputElementConfig_setChecked cfg of
    Nothing -> return never
    Just eNewchecked -> requestDomAction $ ffor eNewchecked $ \newChecked -> do
      oldChecked <- Input.getChecked domInputElement
      Input.setChecked domInputElement newChecked
      return $ if newChecked /= oldChecked
                  then Just newChecked
                  else Nothing
  c <- holdDyn (_inputElementConfig_initialChecked cfg) $ leftmost
    [ fmapMaybe id checkedChangedBySetChecked
    , checkedChangedByUI
    ]
  hasFocus <- mkHasFocus e
  files <- holdDyn mempty <=< wrapDomEvent domInputElement (`DOM.on` Events.change) $ do
    mfiles <- Input.getFiles domInputElement
    let getMyFiles xs = fmap catMaybes . mapM (FileList.item xs) . flip take [0..] . fromIntegral =<< FileList.getLength xs
    maybe (return []) getMyFiles mfiles
  checked <- holdUniqDyn c
  return $ InputElement
    { _inputElement_value = v
    , _inputElement_checked = checked
    , _inputElement_checkedChange =  checkedChangedByUI
    , _inputElement_input = valueChangedByUI
    , _inputElement_hasFocus = hasFocus
    , _inputElement_element = e
    , _inputElement_raw = domInputElement
    , _inputElement_files = files
    }


