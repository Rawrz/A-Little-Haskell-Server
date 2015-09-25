{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Monoid
import Control.Monad
import Control.Concurrent
import Control.Exception
import Data.Char
import System.IO

--I'm not happy about this... didn't know how to get around it...
import System.IO.Unsafe
import System.FilePath
import System.Directory
import Network
import Data.Time.LocalTime
import Lucid

data RequestCmd = GET 
				| POST

instance Show RequestCmd where
	show GET = "GET"
	show POST = "POST"

data ReqURI = ReqURI String

instance Show ReqURI where
	show (ReqURI str) = show str 

data HTTPVersion = HTTPVersion String

instance Show HTTPVersion where
	show (HTTPVersion s) = show s

data RequestHeader = RequestHeader String

instance Show RequestHeader where
	show (RequestHeader str) = show str

data Request = Request { 
	reqCmd :: RequestCmd, 
	reqURI :: String,
	reqHTTPVer :: HTTPVersion,
	reqHeaders :: [(String,String)]
}

data ResponseHeader = ResponseHeader String

instance Show ResponseHeader where
	show (ResponseHeader str) = show str

data Response = Response {
	respCode :: Int,
	respHeaders :: [ResponseHeader],
	respCoding :: [String], --TransferCoding
	respBody :: ResponseBody ,
	respSendBody :: Bool,
	version :: String
}

data ResponseBody = NoBody
				  | FileBody Int FilePath
				  | HtmlBody (Html ())
 
instance Show Request where
	show r = (show (reqCmd r)) ++ " /" ++ (reqURI r)  ++ " " ++ (show (reqHTTPVer r)) ++ (foldl (\acc (k,v) -> acc ++ "\n  " ++ k ++ ": " ++ v)) "" (reqHeaders r)
 
instance Show (Response ) where
	show r = version(r) ++ " " ++ show(respCode(r)) ++ " " ++ (case respCode(r) of
		100 -> "Continue"
		200 -> "OK"
		400 -> "Bad Request"
		404 -> "Not Found") ++ "\r\n\r\n" ++ show (respBody(r))
 
instance Show ResponseBody where
	show NoBody = ""
	show (HtmlBody a) = show a
	show (FileBody _ path) = unsafePerformIO (getFileContents path) --im sorry

getFileContents :: FilePath -> IO String
getFileContents path = do
	hfile <- openFile path ReadMode
	contents <- hGetContents hfile
	return contents
 
respond :: Request -> Handle -> IO ()
respond request handle = do
	putStrLn $ show request
	if (reqURI request) == "/"
		then do
		let response = Response {version = (show (reqHTTPVer request)), respCode = 200, respBody =  HtmlBody defaultWelcomeMsg}
		hPutStr handle $ show(response)
		time <- getZonedTime
		--I did this because for some reason I cannot add concat Lucid's HTML strings with variables...it's weird
		hPutStr handle $ "<p>Here is the time -> " ++ show(time) ++ "</p><p>Here is the request you sent:</p><div>" ++ show(request) ++ "</div>"
	else do
		path <- doesFileExist $ tail $ reqURI request
		if path
			then do 
				--Made the filesize 0 for now until we ficure out how to send the io content through the show function?
				let response = Response {version = (show (reqHTTPVer request)), respCode = 200, respBody =  FileBody 0 (tail (reqURI request))}
				hPutStr handle $ show(response)
			else do 
				let response = Response {version = (show (reqHTTPVer request)), respCode = 404, respBody = HtmlBody fileNotFound}
				hPutStr handle $ show(response)
		return ()

defaultWelcomeMsg :: Html ()
defaultWelcomeMsg = html_ $ do
	head_ $ do
		title_ "WELCOME!"
		body_ $ do
			h1_ "Roz says hi from afar!"
        	p_ "Welcome to Roz's Server!"
        	p_ "I wish I could include other things. This is from my defaultWelcomeMsg function!"
        	p_ "Lucid wont let me concatinate variables with strings which makes me sad so I'll send later."
        	p_ "Go to the index.html page! Also go to /lol/dir.html page!"


fileNotFound :: Html ()
fileNotFound = html_ $ do
	head_ $ do
		title_ "Uh Oh!"
		body_ $ do
			h1_ "404 Not Found"
        	p_ "The file cannot be found on this server."

 
 --Parses the request... doesn't really do any error checking
parseRequest :: [String] -> Request
parseRequest lns = case (words (head lns)) of
	[t,p,_] -> Request {reqCmd=(parseString t), reqURI=p, reqHTTPVer=(HTTPVersion "HTTP/1.1"), reqHeaders=parseRequest'((tail lns),[])}
 

parseString :: String -> RequestCmd
parseString t = case t of
	"GET" -> GET
	"POST" -> POST

 --helper function for parse request. Doesn't handle errors so a bad request is never really returned
parseRequest' :: ([String], [(String,String)]) -> [(String,String)]
parseRequest' ([], str) = str
parseRequest' ((x:xs), str) 
	| (length (words x)) < 2 = str
	| otherwise = parseRequest'(xs, str ++ [((reverse (tail (reverse ( head (words x))))), unwords (tail (words x)))] )


--Gets the request		   
getRequest :: Handle -> String -> IO ()
getRequest handle hostname = do 
	putStrLn $ "Getting the request from: " ++ hostname
	request <- fmap (parseRequest . lines) (hGetContents handle)
	respond request handle
	return ()

--Main top level functions from the doc
acceptConnections :: Socket -> IO ()
acceptConnections sock = do
	(handle, hostAddr, portNum) <- accept sock
	forkIO (catch
				(talk handle hostAddr `finally` hClose handle)
				(\e -> ioError e) --Fix This
			)
	acceptConnections sock

talk :: Handle -> HostName -> IO()
talk handle addr = do
	strs <- getRequest handle addr
	return () --The rest of the function is not implemented

main = withSocketsDo $ do
	sock <- listenOn (PortNumber 9000)
	putStrLn "Listening on port 9000"
	acceptConnections sock