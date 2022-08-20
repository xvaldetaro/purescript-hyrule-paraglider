module Util.VarUpstream where

import Prelude

import Bolson.Core as Bolson
import Control.Alt ((<|>))
import Data.Eq.Generic (genericEq)
import Data.Filterable (filter, filterMap)
import Data.Foldable (for_, oneOf)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Deku.Attribute (cb, (:=))
import Deku.Control (dyn_, text_)
import Deku.Core (class Korok, Domable, bussedUncurried, insert_, remove)
import Deku.DOM as D
import Deku.Do (useMemoized)
import Deku.Do as DekuDo
import Deku.Do as Doku
import Deku.Listeners (click)
import Effect (Effect)
import FRP.Event (AnEvent, count, fold)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, value)

type SourceId = Int
type Emission = String
type State m =
  -- The current Upstream Sources
  { sourcesEv :: AnEvent m (Set SourceId)
  -- The bus through which all Sources' emissions go through
  , busEv :: AnEvent m (SourceId /\ Emission)
  }

-- | A widget where the user can add/remove Upstream events and Emit values from those events.
-- | Exposes the current inner State through returned (via closure parameter) Event
crackedNut
  :: ∀ s m lg o l p
   . Korok s m
  => (State m /\ Domable m l p -> Bolson.Entity lg o m l)
  -> Bolson.Entity lg o m l
crackedNut cont = Doku.do
  mailPush /\ mailEv <- useMemoized \e -> e <|> (pure $ stamp 0 AddMe) <|> (pure $ stamp 1 AddMe)

  let
    serializedIdsEv :: AnEvent m SourceId
    serializedIdsEv = filterMap
      (\{ address, payload } -> if payload == AddMe then Just address else Nothing)
      mailEv

    currentIdEv :: AnEvent m Int
    currentIdEv = count $ filter (\{ payload } -> payload == AddMe) mailEv

    mkRowEv :: Int -> AnEvent m _ -- dyn cmds
    mkRowEv id = do
      let
        myDeletionEv = filterMap (\a -> if a == DeleteMe then Just remove else Nothing)
          $ watchAddress id mailEv
      pure (insert_ $ rowUi mailPush id) <|> myDeletionEv

    ui =
      D.div_
        [ dyn_ D.div $ mkRowEv <$> serializedIdsEv
        , D.div_
            [ D.button
                (click ((\id -> mailPush $ stamp id AddMe) <$> currentIdEv))
                [ text_ "+ Add Source" ]
            ]
        ]

  cont $ Tuple (aggregateMailIntoState mailEv) ui

data Action = EmitMe String | DeleteMe | AddMe

derive instance genericAction :: Generic Action _
instance eqAction :: Eq Action where
  eq = genericEq

-- | The nut for each individual Upstream Source row that can be added to the main UI
rowUi :: ∀ s m l p. Korok s m => (MailPush SourceId Action) -> Int -> Domable m l p
rowUi mailPush id = DekuDo.do
  let pushAddressed = mailPush <<< stamp id
  setText /\ text <- bussedUncurried
  D.div_
    [ D.span_ [ text_ $ "Model id: " <> show id ]
    , D.button
        (click $ pure $ pushAddressed DeleteMe)
        [ text_ "-" ]
    , D.input
        ( oneOf
            [ pure $ D.OnInput := cb \e -> for_
                (target e >>= fromEventTarget)
                (value >=> setText)
            ]
        )
        []
    , D.button
        ( click $ text <#> \t -> do
            pushAddressed (EmitMe t)
        )
        [ text_ "Emit->" ]
    ]

------------------------------------
-- | Helpers to convert internal Actions to State, which is exposed upwards

aggregateMailIntoState :: ∀ s m. Korok s m => MailEvent m SourceId Action -> State m
aggregateMailIntoState mailEv =
  let
      filterEmitMe {address, payload} = case payload of
        EmitMe value -> Just $ address /\ value
        _ -> Nothing
      upwardsEmissionEvent = filterMap filterEmitMe mailEv

      mutationsEv = filterMap reduceSources mailEv
      sourcesEv = fold ($) mutationsEv Set.empty
  in
  { sourcesEv, busEv: upwardsEmissionEvent }

  where
  reduceSources :: Mail SourceId Action -> Maybe (Set SourceId -> Set SourceId)
  reduceSources {address, payload} = case payload of
    AddMe -> Just $ Set.insert address
    DeleteMe -> Just $ Set.delete address
    _ -> Nothing

------------------------------------
-- | Mail utility stuff

type Mail a b = { address :: a, payload :: b }
type MailPush a b = Mail a b -> Effect Unit
type MailEvent m a b = AnEvent m (Mail a b)
type MailHub m a b = MailPush a b /\ MailEvent m a b

stamp :: ∀ a b. a -> b -> Mail a b
stamp address payload = { address, payload }

watchAddress :: ∀ m a b. Applicative m => Eq a => a -> MailEvent m a b -> AnEvent m b
watchAddress address' = filterMap \{ address, payload } ->
  if address' == address then Just payload else Nothing
