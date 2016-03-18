{-# LANGUAGE OverloadedStrings #-}


module Main where
import Safe
import Lib
import Network.Wreq
import qualified Network.Wreq.Session as S
import Control.Lens
import Text.HTML.Scalpel
import Text.HTML.TagSoup 
import Data.Maybe
import Data.List
import Data.Char
import Data.ByteString.Lazy (ByteString)
import Data.Aeson (toJSON)
import System.Environment


opts = defaults & header "User-Agent" .~ ["User-Agent:Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"]
                        & header "Accept" .~ ["text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"]
                        & header "Accept-Encoding" .~ ["gzip, deflate, sdch"]

main :: IO ()
main = do 
    urls <- getArgs                            
    let url = headMay(urls) `getOrElse` def
    r <- S.withSession $ \session -> S.getWith opts session url
    let tags = parseTags(r ^.responseBody)

    --putStrLn "Username"
    --username <- getLine
    --putStrLn "Password"
    --passwd <- getLine
    let f = (fromAttrib "action") `fmap` (filter (isTagOpenName "form") $ tags )
    let i = (filter (isTagOpenName "input") $ tags )
    let (hidden, is) = partition (isTagOpenName "hidden") i 
    bs <- chase tags

    print hidden
  where 
    def :: String
    def = "https://chaseonline.chase.com/"            
 



chase :: [Tag ByteString] -> IO (Maybe [ByteString])
chase tags =  do
  let f = scrape forms tags
  let it = scrape inputs tags
  let ifr = scrape iframes tags
  --S.withSession $ \session -> S.postWith opts session   
  return f
  where     
    forms =   htmls("form"::String)    
    inputs  = htmls(("form"::String) \\ ("input"::String))
    iframes  = htmls("iframe"::String)      