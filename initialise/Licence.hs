module Licence (text) where

import Configuration (MetaData (name))
import Control.Monad.Extra (pureIf)
import Data.ByteString.Lazy (ByteString, empty)
import Data.Maybe (fromJust)
import Network.HTTP.Client (responseBody)
import Network.HTTP.Simple (httpLBS, parseRequest, setRequestResponseTimeout)
import Network.URI (escapeURIString, isUnescapedInURI)
import Text.HTML.TagSoup (fromTagText, isTagText, parseTags, (~/=))

text :: String -> IO String
text name = fromJust . extract <$> download name

download :: String -> IO ByteString
download name = do
  request <- setRequestResponseTimeout 60 <$> parseRequest $ "https://opensource.org/licenses/" ++ escapeURIString isUnescapedInURI name
  responseBody <$> httpLBS request

extract :: ByteString -> Maybe ByteString
extract html = pureIf (not empty result) $ head result
  where
    tags = parseTags html
    selector t = (t ~/= "<div id=LicenseText>") || (t ~/= "<article>")
    result = map fromTagText $ filter isTagText $ filter selector tags
