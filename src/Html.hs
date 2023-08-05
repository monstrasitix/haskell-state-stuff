{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Html
    ( PrettyHTML
    , RawHTML
    ) where

import           Data.ByteString.Lazy
import           Network.HTTP.Media.MediaType
import           Servant
import           Text.Blaze.Html
import qualified Data.ByteString.Lazy.UTF8       as BLU
import qualified Text.Blaze.Html.Renderer.Pretty as HP
import qualified Text.Blaze.Html.Renderer.Utf8   as HU

defaultContentType :: MediaType
defaultContentType = "text" // "html" /: ("charset", "utf-8")

data PrettyHTML

instance Accept PrettyHTML where
    contentType :: Proxy PrettyHTML -> MediaType
    contentType _ = defaultContentType

instance ToMarkup a => MimeRender PrettyHTML a where
    mimeRender :: Proxy PrettyHTML -> a -> ByteString
    mimeRender _ = BLU.fromString . HP.renderHtml . toHtml

instance MimeRender PrettyHTML Html where
    mimeRender :: Proxy PrettyHTML -> Html -> ByteString
    mimeRender _ = BLU.fromString . HP.renderHtml

data RawHTML

instance Accept RawHTML where
    contentType :: Proxy RawHTML -> MediaType
    contentType _ = defaultContentType

instance ToMarkup a => MimeRender RawHTML a where
    mimeRender :: Proxy RawHTML -> a -> ByteString
    mimeRender _ = HU.renderHtml . toHtml

instance MimeRender RawHTML Html where
    mimeRender _ = HU.renderHtml