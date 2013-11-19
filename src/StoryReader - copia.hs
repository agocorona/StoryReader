{-# OPTIONS -XDeriveDataTypeable
            -XTypeSynonymInstances
            -XScopedTypeVariables
            -XFlexibleInstances
            -XRecordWildCards

            #-}
{-

getCookie
falla backreturn cuando acaba el tiempo
ofrecer envio por correo cuando acaba el tiempo
Login elegir recordar o no y logout
se pierde un caracter al final de el papel de Rey

-}

module  Main where
import Prelude hiding (writeFile)
import MFlow.Hack.XHtml.All hiding (many)
import Data.Typeable
import Control.Monad(when)
import Control.Monad.Trans
import qualified Data.Map as M
import Data.Maybe
import System.IO hiding(openFile,writeFile,openBinaryFile, hGetLine, hPutStr)
import System.IO.UTF8(openBinaryFile,hGetLine, hPutStr,writeFile)
import System.IO.Unsafe
import System.IO.Error(isEOFError)
import Data.Char
import System.Time
import Control.Concurrent

import Data.ByteString.Lazy.Char8 as B(ByteString,take,pack,append, unpack,snoc,empty)
import Util.Mail
import Data.Monoid

import Data.TCache
import Data.TCache.Defs
import Data.TCache.Memoization
import Data.TCache.IndexQuery

import Text.Parsec  hiding ((<|>))
import qualified Text.Parsec as P (try,(<|>))
import Text.Parsec.Token

import System.Directory
import Data.List((\\))
import Data.Char(isSpace)
import qualified Control.Exception as E
import qualified Data.Vector as V
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import Control.Monad
import System.Environment

import Debug.Trace

(!>)= flip trace

justify=  flip fromMaybe

adminUser= "admin"



wait f= do
    mv <- newEmptyMVar
    forkIO (f >> putMVar mv True)
    takeMVar mv
   `E.catch` (\(e:: E.SomeException) ->do
                  putStr "sync.."
                  syncCache
                  putStr "done\n"
                  error $ "Signal: "++ show e)


--main= do
--     Story _ chunks <- getResource (Story "ETER" undefined) `onNothing` error ("getSize: story not found")
--     return . fromIntegral $ V.length chunks
--
main= do
   args <- getArgs
   let port= case args of [] -> 8080 ; [x] -> read x
   syncWrite SyncManual
   index userName
   setAdminUser adminUser adminUser
   maybeColdRebuildStories
   addAdminWF
   addFileServerWF
   addMessageFlows messageFlows
   wait $ run port  hackMessageFlow

   where
   messageFlows=  [("" , transient $ runFlow showStories)
                  ,("admin"    , transient $ runFlow admin)
                  ,("mail"     , transient $ runFlow mail)]


type Name = String
type Size= Int


type Stories= [(Name,Size)]



instance Indexable Stories where
  key= const keyStories
  defPath _= storiesPath

storiesPath= "Stories/"

keyStories= "Stories"

rstories :: DBRef Stories
rstories= getDBRef keyStories

chunkSize= 1000 :: Int

appheader :: String -> ByteString -> ByteString
appheader title c =
         bhtml []
             $ (toByteString (header
                   << (thetitle << title
                   +++ meta ! [name "Keywords", content "parpendicular, sci-fi"]
                   +++ meta ! [name "viewport", content "width=device-width"]
                   +++ style <<
                           ("@-webkit-keyframes slide \
                            \        { \
                            \       from {right:0;} \
                            \       to {right:80%;} \
                            \       } " ++
                            "\n@-webkit-keyframes slideback \
                            \        { \
                            \       from {right:0;} \
                            \       to {right:-80%;} \
                            \       } ")

                            ))

                 <>
--                       <link rel="shortcut icon" href="/favicon.ico" />

                  (bbody [("style", "margin-left:5%;margin-right:5%")]
                     $ (btag "div" []
                       $ (c <> bottom))))


bottom =
 btag "div" [("style", "float:right")]
    $ (monitor <> powered)

monitor=  btag "a" [("href", "http://www.monitor.us")]
     $ btag "img"
         [("width", "50"), ("height", "25")
         ,("src", "http://images.monitor.us/monbadges120-40.png")
         ,("title","Monitor.Us - Free website, server & network monitoring tool")
         ,("border","0")] $ B.empty


powered= btag "a" [("href", "http://haskell.org")]
   $ btag "img"
         [("width", "50"), ("height", "25")
         ,("src", linkFile "haskell-logo-revolution.png")] $ B.empty


maybeColdRebuildStories=   do
   strs <- atomically $ readDBRef rstories
   case strs of
     Just _ -> return()
     Nothing -> do
       l<- getDirectoryContents storiesPath   -- UTF8.decodeString
       let l'= l \\ [keyObjDBRef rstories,"..","."]
       l'' <- mapM (\s -> getSize s >>= \n -> return (s,n)) l'
       atomically $  writeDBRef rstories  l''

getSize s= do
     Story _ chunks <- getResource (Story s undefined) `onNothing` error ("getSize: story not found:"++s)
     return . fromIntegral $ V.length chunks

userForm= anchor ![name "login"] << bold << "Login/register" ++>
        ((,) <$> br ++> getString Nothing      <! [("size","9")]
             <*> br ++> toHtml "Password: " ++> br  ++> getPassword            <! [("size","9")]
             <** br ++> submitButton "login")
             <+>(br ++> toHtml "Repeat Pass." ++> br ++> getPassword            <! [("size","9")]
             <*  br ++> submitButton "Register" <++ br )

userwidget=  userWidget Nothing userForm

userFormOrName= userwidget `wmodify` f
  where
  f _ justu@(Just u)  =  return ([fromString u], justu) -- !> "input"
  f felem Nothing = do
     us <-  getCurrentUser -- getEnv cookieuser
     if us == anonymous
           then return (felem, Nothing)
           else return([fromString us],  Just us)


linkOrUser user= do
 case  user== anonymous of
     False -> toHtml user
     True  -> anchor ! [href "#login"] << "login"


data Story= Story{stname :: String, blocks :: V.Vector T.Text} deriving (Typeable, Show)

instance Indexable Story where
  key (Story n _)= n
  defPath _= storiesPath


instance IResource Story where
  keyResource = key
  writeResource s=return()
  readResourceByKey = defReadResourceByKey
  delResource = defDelResource

--getSeparator= try(string "&gt;&gt;&gt;") P.<|> string ">>>"
--symbol1 s= mapM char s

instance Serializable Story where
  serialize=  error "serialize Story not implemented"
  deserialize str = case parse pstory  "" str  of
                     Left err  -> error $ "deserialize Story error: " ++ show err
                     Right story  ->   story

pstory = P.try separatorp -- <|> nseparatorp

scan= scan1 0 B.empty >>= return .  E.decodeUtf8With (\_ _ -> return  '-')
scan1 2000 _= fail ""
scan1 n s=
    try(string "&gt;&gt;&gt;" >> return s) P.<|> try( string ">>>" >> return s) P.<|>
--    (getSeparator >> return s)     P.<|>
    (anyChar >>= \c -> scan1 (n+1) (B.snoc s c) P.<|>  return s)

separatorp= do
       n <- getName
       xs <- many  scan
       return . Story n $ V.fromList xs

getName=   getLine1 >>= return . filter isAscii
nseparatorp= do
       n  <- getName
       ch <- many $ takep chunkSize
       return . Story n $ V.fromList ch  -- !> (n)

takep n= takep1 n []

getLine1= manyTill anyChar ((eof >> return '\n'  ) <|> char '\n')

takep1 0 cs=do
    s <- getLine1
    return . E.decodeUtf8 . B.pack $ reverse cs ++ s

takep1 n cs= do
       c <-   anyChar
       takep1 (n-1) (c:cs) P.<|> return ( E.decodeUtf8 . B.pack  $ reverse cs)




data GeneralConf=
     G{ gblockreceive :: Int
      , gTimeBetweenBlocks :: Integer}
      deriving (Read, Show, Typeable)

config0= G 4 1
rconf= getDBRef $ key config0

instance Indexable GeneralConf where
   key _ = "config"

data UserStoryContext= USC
                      { sname :: String
                      , byMail :: Maybe String
                      , confirmed :: Bool       -- XXX mover a userContext
                      , seek :: Int
--                      , ssize :: Int
                      , lastAccess :: Integer
                      , readToday  :: Int
                      , ref  :: DBRef Story}
                      deriving (Read, Show, Typeable)

data UserContext=UC{ ucName :: String
                   , currentStory :: Maybe String
                   , ucStories :: M.Map String UserStoryContext} deriving (Read, Show, Typeable)



userStorycontext0= USC (error "name nof defined in story") Nothing False 0 0   0 (error "ref nof defined in story")

instance Indexable UserContext where
  key (UC n _ _)= "UC"++ n




data Navigation= Seek Int | Menu | ByMail deriving (Typeable, Read, Show)


daySecs= 24*60*60 :: Integer

readStories= atomic $ readDBRef rstories `onNothing` error "showStories: stories not found"
--showStories ::  FlowM Html (Workflow IO) ()
showStories   = do
  setHeader $ appheader parpendicular
  setTimeouts 0 0
  stories <- readStories

  showStories1 stories
  where

  showStories1  stories =  do
     user <- getCurrentUser                            -- !> "getCurrentUser"
     rUContext<- ( atomic $  newDBRef $ UC user Nothing . M.fromList . map (\(n,s) -> (n,userStorycontext0{sname=n, ref= getDBRef n})) $ stories)
     uc@UC{..} <- atomic $ readDBRef rUContext `onNothing` error "nor found user context "
     back <- goingBack
     story <- case (currentStory, back) of
          (Just s, False) -> return s
          _ -> do
            s <- ask $   linkOrUser user .++>. cachedWidget "menu" 0 (listStories1 stories) .<**.  userFormOrName
            atomic $ writeDBRef rUContext  $ uc{currentStory=Just s}
            return s

     navigate rUContext  story                      -- !> "exit the menu. going to navigate"
     showStories1 stories

  navigate rUContext  story= do
     uc@(UC n _ mapcontext) <- atomic $ readDBRef rUContext `onNothing` error "nor found user context "
     G ntimes t <- atomic $ readDBRef rconf `onNothing` return config0
     TOD tnow _ <- liftIO $ getClockTime
     let context@USC{..}= M.lookup story mapcontext `justify` userStorycontext0{sname=story, ref= getDBRef story}
     user <- getCurrentUser
--     if  user /= adminUser && readToday >= ntimes -- && lastAccess + t * daySecs < tnow
--      then do
--        ask $   (p << "Sorry, you have completed the Story for today. Try tomorrow if you like or read other stories" +++
--                 p << "You can also can receive it by mail.  If you are registered, Click \"receive by mail\" in the next page")
--           .++>. wlink () (bold << "press here")
--        atomic $ writeDBRef rUContext  uc{currentStory= Nothing}
--        breturn ()
--      else do
     msize <- readStories >>= return . lookup story
     case msize of
       Nothing -> do
               atomic $ writeDBRef rUContext uc{currentStory=Nothing}
               return ()
       Just size -> do
--     error ( "story no in stories: " ++ story)
         r <- ask $       topForm sname  seek  size
                     .**>.   cachedWidget (sname  ++ show seek ) 0
                                          (showBuffer context)
                     .<**.   td <<< userFormOrName

         case r of
                Menu       -> do
                           atomic $ writeDBRef rUContext  uc{currentStory= Nothing}
                           breturn ()
                ByMail     -> setMail  rUContext context >> navigate rUContext  story
                Seek seek  -> do
                    let newc = context{seek= seek, lastAccess= tnow, readToday= readToday + 1}
                    atomic $ writeDBRef rUContext  uc{ucStories= M.insert story newc mapcontext}
                    navigate rUContext story

  topForm title seek size= do
         user <- getCurrentUser
         let mailw =
               case  user== anonymous of
                        False -> byMail
                        True  -> noWidget

         table ! [thestyle "width:100%"]
          <<< tr
            <<< td <<  linkOrUser user
            ++> td  ! [align "center"] << toHtml (if size==0 then "0" else show (seek * 100 `div` size) ++ "%")
            ++> mailw
            <++ td  ! [align "right"] << title
       where
       byMail=  td  <<< wlink ByMail (thespan << "By mail")

getChunk  context  = do
       Story _ chunks <- atomically $ readDBRef (ref context) `onNothing` error ("Story not found: " ++ keyObjDBRef (ref context))
       let seekit= seek context
           chunk= chunks V.! seekit
       return (chunk , seekit, fromIntegral $ V.length chunks)



parpendicular= "Parpendicular Universes"
--listStories :: [(String,Int)] -> View Html IO  String

listStories1 stories=
   h3 << parpendicular ++>
   h4 << "Infotainment for a galaxy in expansion" ++>
   h4 << "Be an explorer!" ++>
   listStories  stories

listStories  stories=
   h4 << "Choose a story"  ++>
   firstOf [p <<< wlink s (bold << s) | (s, _) <- stories]






disableAttrs = [("style","visibility:hidden")]

showBuffer  context =
   let
     (buf,seekit,size)  = execute $ getChunk context


     seekbn   = let x= seekit - 1
                in if x  >= 0 then x else seekit

     seekfw   = let x= seekit + 1
                in if x < size then x else seekit


     fwlink= let link= (wlink (Seek seekfw) $ bold << ">>>>") -- <! [("data-ftrans", "slide" )]
             in if seekfw== seekit
                           then link <! disableAttrs
                           else link

     bwlink= let link= (wlink (Seek seekbn) $ bold << "<<<<") -- <! [("data-ftrans", "slide reverse") ]
             in if seekbn == seekit
                           then link <! disableAttrs
                           else link

     otherLink = wlink Menu ( bold <<"Other stories")
     centered  = [align "center"]

     links     = table   ! [thestyle "width:100%"]
                 <<< tr  <<<(td            <<<  bwlink
                         <|> td ! centered <<< otherLink
--                         <|> td ! centered <<< byMail
                         <|> td ! centered <<< fwlink)

     stylet="position:relative;word-wrap:break-word,-webkit-animation-duration: .5s;"

     wrap buf ret= thediv ! [strAttr "id" "textdiv", thestyle stylet]
                    << ((thespan <<  (map (\l-> primHtml $ l++"<br/>") . lines $ T.unpack buf))
                    +++ divs ret)

     divs ret=    -- divs for navigation up-down
          let locationb= ret $ Seek seekbn
              locationf= ret $ Seek seekfw
          in (thediv ![thestyle "position:absolute;left:0;top:0;width:50%;height:100%;zindex:1"
                  ,strAttr "onclick" ("var e=document.getElementById('textdiv');e.style.webkitAnimationName ='slideback';e.style.webkitAnimationDuration='.5s';window.location='"++locationb++"'")] << noHtml)
             +++
             (thediv ![thestyle "position:absolute;left:50%;top:0%;width:50%;height:100%;zindex:1"
                  ,strAttr "onclick" ("var e=document.getElementById('textdiv');e.style.webkitAnimationName ='slide';e.style.webkitAnimationDuration='.5s';window.location='"++locationf++"'")]  << noHtml)

     in normalize $ links <|> (returning $ wrap buf )  <|> links

myURL="parpendicularuniverses.com"

showMailBuff context chunk user=
 if confirmed context then conf  else unconf

 where
 conf =
     hotlink (myURL++ "/story="++sname context++"mail?"++user) << thespan << "get next page" +++
     br +++
     thespan << T.unpack chunk +++
     br +++
     hotlink (myURL++ "/story="++sname context++"mail?"++user) << thespan << "get next page"


 unconf=
  let msg= "This pail was sent to you because  you have requested"
            ++" parpendicuarunierses.com to receive futher content of"

  in bold << msg +++
     br +++
     hotlink (myURL++ "/mail?"++user) << thespan << "confirm your mail" +++
     br +++
     thespan << T.unpack chunk +++
     br +++
     hotlink (myURL++ "/mail?"++user) << (thespan << "confirm your mail")

setMail ref context=  do
  mail <- ask . normalize $ p << "You will receive just five blocks unless you ask for more"
              ++> getString (Just "Enter your mail")
  (chunk,seekit,size)  <- liftIO $ getChunk context
  UC{..} <- atomic $ readDBRef ref     `onNothing` error "setMail: user context not found"
  let xhtml = showMailBuff context chunk ucName
  let message = showHtml xhtml
  liftIO $ sendMail "ptueba" "agocorona@gmail.com" "title" message
  ask $ thespan << "The mail has been sent to you" .++>. wlink "" ( thespan << "click here")
  let newc = context{byMail= Just mail, confirmed= False, seek= seek context + size}
  atomic $ do
    uc@UC{..} <- readDBRef ref `onNothing` error "showStories1: not found user context"
    writeDBRef ref uc{ucStories =M.insert (sname newc) newc ucStories}



mail= do
     user <-  getUser Nothing   userFormLine
     let ref= getDBRef . key $ UC user undefined undefined
     uc@UC{..} <- atomic $ readDBRef ref        `onNothing` error "showStories1: not found user context"
     let story = currentStory `justify` error "no current story"
     let context =  M.lookup story ucStories `justify` userStorycontext0

     (chunk,seekit,size) <- liftIO $ getChunk context
     atomic $ do
         let newc= context{confirmed= True, seek= seek context + size}
         writeDBRef ref $ uc{ ucStories= M.insert (sname newc) newc ucStories}

     let xhtml = showMailBuff context chunk user
     let message = showHtml xhtml
     liftIO $ sendMail "ptueba" "agocorona@gmail.com" "title" message
     ask . normalize $ thespan << "The mail has been sent to you" ++> wlink "" ( thespan << "click here")
     showStories



atomic= liftIO . atomically
--admin :: FlowM Html  IO ()
admin= do
     let admintext= "Administration"
         adcontstr= "Edit content to a story"
         addrelstr= "Add a new story"
         delrelstr= "Delete a Story"
         gnconfstr= "Configuration"

     setHeader $ \c -> appheader admintext
        $  btag "h1" [("align", "center")] (pack admintext)
        <> btag "script" [("src","http://js.nicedit.com/nicEdit-latest.js")
                          ,("type", "text/javascript")] B.empty
        <> btag "script" [("type","text/javascript")]  (pack  "bkLib.onDomLoaded(nicEditors.allTextAreas)")
        <> c

     setTimeouts 0 0

     u <- getUser (Just "admin") . normalize $ p << bold << "Please login as Administrator" ++> userLogin


     op <- ask . normalize $   p <<< wlink "edit" (p << adcontstr)
                <|> p <<< wlink "add"  (p << addrelstr)
                <|> p <<< wlink "del"  (p << delrelstr)
                <|> p <<< wlink "conf" (p << gnconfstr)


     case op of
      "edit" -> do
         stories <- readStories
         r <- ask $ homelink .|+|. h3 ! [align "center"] << adcontstr .++>.  listStories stories
         case r of
          (Just _,_)  -> admin
          (_,Just hist)-> do
           let ref = getDBRef hist
           Story _ content <- atomic (readDBRef ref) `onNothing` (error $ "not found: "++ hist)

           mr <- ask $  homelink
                    .|+|. wform ( h3 ! [align "center"] << adcontstr
                               ++> getMultilineText (concatMap T.unpack $ V.toList content) ![rows "30",thestyle "width:80%"]
                               <* br ++> submitButton "submit")
                      -- <+> br +> homelink
           case mr of
            (_,Just ncontent) -> do

               liftIO $ writeFile (storiesPath ++ hist) ncontent
               atomic $ flushDBRef ref
            (Just _,_) -> admin

      "add" -> do
         r <- ask $ homelink .|+|. wform ( h3 << addrelstr ++> br ++>
                          ((,) <$> bold << "Name of the Story" ++> (p <<< getString Nothing)
                               <*> p <<< getMultilineText  "Enter the content" ![rows "30",  thestyle "width:80%"]
                               <*  p <<< submitButton "submit"))

         case r of
          (Just _,_) -> admin
          (_,Just(hist,ncontent)) -> do
             liftIO $  writeFile (storiesPath ++ hist) $ hist++ "\r\n" ++ ncontent
             s <- liftIO $ getSize hist
             atomic $ do
                 mstories <- readDBRef rstories            `onNothing` error "no found Stories"
                 writeDBRef rstories $ (hist,s) : mstories


      "del" -> do
           stories <- readStories
           hist  <- ask $ normalize $ listStories  stories
           liftIO $ do
             atomically . writeDBRef rstories $ filter (\r-> hist /= fst r ) stories
             removeFile $ storiesPath ++ hist
      "conf" -> do
           G{..} <- atomic $  readDBRef rconf `onNothing` (writeDBRef rconf config0 >> return config0)
           r <- ask $ homelink
                   .|+|. (G <$> br ++> fromString "Number of blocks to receive " ++> getInt (Just gblockreceive)
                          <*> br ++> fromString "Time between receives" ++> getInteger (Just gTimeBetweenBlocks))
                          <*  p <<< submitButton "submit"
           case r of
             (Just _,_) -> admin
             (_, Just conf)  -> atomic $ writeDBRef rconf conf

     liftIO $ syncCache
     admin
  where
  homelink= br ++> wlink "h" ( bold << "Admin Home")
  del k   stories= M.delete  k stories

  add k v (Just stories)= M.insert k (strip k,v) stories
  add k v Nothing =  M.singleton k (strip k,v)
  strip k= k \\ "\\/:*?\"<>|"

