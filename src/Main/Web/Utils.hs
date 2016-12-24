{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main.Web.Utils
    ( CfgBaseUri
    , GetFound
    , redirect
    , showAbsoluteUri
    , showRelativeUri
    , withBaseUri
    )
  where

import Data.Monoid ((<>))

import Network.URI (URI, parseAbsoluteURI, relativeTo)
import Servant
    ( Header
    , Headers
    , PlainText
    , Server
    , StdMethod(GET)
    , Verb
    , addHeader
    )


type family Append (x :: a) (xs :: [a]) :: [a] where
    Append a '[] = '[a]
    Append a (b ': bs) = b ': Append a bs

type family AppendHeader h v a :: * where
    AppendHeader h v (Headers ls a) = Headers (Append (Header h v) ls) a
    AppendHeader h v a = Headers '[Header h v] a

type GetFound ctypes a
    = Verb 'GET 302 ctypes (AppendHeader "Location" String a)

redirect :: String -> Server (GetFound '[PlainText] String)
redirect uri = pure $ addHeader uri ("redirect to " <> uri)

showRelativeUri :: URI -> String
showRelativeUri = ("/" <>) . show

type CfgBaseUri = ?baseUri :: URI

withBaseUri :: String -> (CfgBaseUri => IO a) -> IO a
withBaseUri uri f = do
    let ?baseUri = let Just u = parseAbsoluteURI uri in u
    f

showAbsoluteUri :: CfgBaseUri => URI -> String
showAbsoluteUri = show . (`relativeTo` ?baseUri)
