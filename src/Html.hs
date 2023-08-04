{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Html
    ( PrettyHTML
    ) where

import           Data.ByteString.Lazy
import           Network.HTTP.Media.MediaType
import           Servant
import           Text.Blaze.Html.Renderer.Pretty
import qualified Text.Blaze.Html                 as H
import qualified Data.ByteString.Lazy.UTF8       as BLU

data PrettyHTML

instance Accept PrettyHTML where
    contentType :: Proxy PrettyHTML -> MediaType
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance H.ToMarkup a => MimeRender PrettyHTML a where
    mimeRender :: Proxy PrettyHTML -> a -> ByteString
    mimeRender _ = BLU.fromString . renderHtml . H.toHtml

instance MimeRender PrettyHTML H.Html where
    mimeRender :: Proxy PrettyHTML -> H.Html -> ByteString
    mimeRender _ = BLU.fromString . renderHtml