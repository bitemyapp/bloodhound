module Database.Bloodhound.Internal.Utils.Requests where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import Database.Bloodhound.Internal.Client.BHRequest
import qualified Network.HTTP.Types.Method as NHTM
import Prelude hiding (filter, head)

delete ::
  (ParseBHResponse contextualized, FromJSON body) =>
  Endpoint ->
  BHRequest contextualized body
delete = mkSimpleRequest NHTM.methodDelete

deleteWithBody ::
  (ParseBHResponse contextualized, FromJSON body) =>
  Endpoint ->
  L.ByteString ->
  BHRequest contextualized body
deleteWithBody = mkFullRequest NHTM.methodDelete

get ::
  (ParseBHResponse contextualized, FromJSON body) =>
  Endpoint ->
  BHRequest contextualized body
get = mkSimpleRequest NHTM.methodGet

head' ::
  (ParseBHResponse contextualized, FromJSON body) =>
  Endpoint ->
  BHRequest contextualized body
head' = mkSimpleRequest NHTM.methodHead

put ::
  (ParseBHResponse contextualized, FromJSON body) =>
  Endpoint ->
  L.ByteString ->
  BHRequest contextualized body
put = mkFullRequest NHTM.methodPut

post ::
  (ParseBHResponse contextualized, FromJSON body) =>
  Endpoint ->
  L.ByteString ->
  BHRequest contextualized body
post = mkFullRequest NHTM.methodPost
