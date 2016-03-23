{-# LANGUAGE OverloadedStrings #-}


module Main where
import Safe
import Lib
import Network.Wreq
import qualified Network.Wreq.Session as S
import Control.Lens
import Control.Exception as E
import Text.HTML.Scalpel
import Text.HTML.TagSoup 
import Data.Maybe
import Data.List
import Data.Char
import Data.ByteString.Char8 (pack)
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
    let header = unwords $ intersperse "\n" ofxheader 
    let request = header ++ "\n\n" ++ xml
    putStrLn request
    r <- E.try(postWith opts url (pack request))
    --let tags = parseTags(r ^.responseBody)

    --putStrLn "Username"
    --username <- getLine
    --putStrLn "Password"
    --passwd <- getLine
    --let f = (fromAttrib "action") `fmap` (filter (isTagOpenName "form") $ tags )
    --let i = (filter (isTagOpenName "input") $ tags )
    --let (hidden, is) = partition (isTagOpenName "hidden") i 
    --bs <- chase r
    print request
    --print (r ^. responseBody)
  where 
    def :: String
    def = "https://chaseonline.chase.com/"            
    
    ofxheader = ["OFXHEADER:100","DATA:OFXSGML","VERSION:103","SECURITY:NONE","ENCODING:USASCII","CHARSET:1252","COMPRESSION:NONE","OLDFILEUID:NONE","NEWFILEUID:NONE"] 
    xml = "<OFX><SIGNONMSGSRQV1><SONRQ><DTCLIENT>20160418021529.000[-8:PST]<USERID>test<USERPASS>test<LANGUAGE>ENG<FI><ORG>HAN<FID>5959</FI><APPID>QWIN<APPVER>0900</SONRQ></SIGNONMSGSRQV1></OFX>"
    -- <BANKMSGSRQV1>    <STMTTRNRQ><TRNUID>23382938      <STMTRQ>        <BANKACCTFROM>          <BANKID>987654321          <ACCTID>23517          <ACCTTYPE>SAVINGS        </BANKACCTFROM>        <INCTRAN>          <INCLUDE>Y        </INCTRAN>      </STMTRQ>    </STMTTRNRQ>  </BANKMSGSRQV1></OFX>"



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

