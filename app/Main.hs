{-# LANGUAGE OverloadedStrings #-}


module Main where
import           Control.Exception          as E
import           Control.Lens
import           Control.Monad
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy       as BSL (ByteString, toStrict)
import           Data.ByteString.Lazy.Char8 (pack, unpack)
import           Data.Char
import           Data.List
import           Data.Map                   (Map, singleton)
import           Data.Maybe
import           Data.Text.Encoding
import           Lib
import           Network.Wreq
import qualified Network.Wreq.Session       as S
import           Safe
import           System.Environment
import           Text.HTML.TagSoup


opts = defaults & header "User-Agent" .~ ["User-Agent:Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"]
                        & header "Accept" .~ ["text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"]
                        & header "Accept-Encoding" .~ ["gzip, deflate, sdch"]

main :: IO ()
main = do
    urls <- getArgs
    let url = headMay(urls) `getOrElse` def
	{-putStrLn "Username"
    username <- getLine
    putStrLn "Password"
    passwd <- getLine -}
    r <- S.withSession(\s -> S.get s url)
    let tags = parseTags(r ^.responseBody)
    let f = (fromAttrib "action") `fmap` (filter (isTagOpenName "form") $ tags )
    let i = (filter (isTagOpenName "input") $ tags )
    let (hidden, is) = partition (\x -> fromAttrib "type" x == "hidden") i
    let [a,b] = fmap (unpack . encodePretty .extract) [is, hidden]
    putStrLn a
    putStrLn ""
    putStrLn b
  where
    def = "https://chaseonline.chase.com/"


extract:: [Tag ByteString] -> [Map String String]
extract tags = do
      t <-  tags
      let r = fmap unpack t
      let k = fromAttrib "name" r
      let v = fromAttrib "value" r
      guard ("" /= k)
      return $ singleton k v
