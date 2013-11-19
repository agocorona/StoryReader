{-# OPTIONS -XDeriveDataTypeable
            -XTypeSynonymInstances
            -XScopedTypeVariables
            -XFlexibleInstances
            -XRecordWildCards
            -XOverloadedStrings
            #-}
{-

getCookie
falla backreturn cuando acaba el tiempo
ofrecer envio por correo cuando acaba el tiempo
Login elegir recordar o no y logout
se pierde un caracter al final de el papel de Rey

-}

--module  Main where
import Prelude hiding (writeFile)
import MFlow.Wai.Blaze.Html.All
import Text.Blaze.Html5 as El hiding (map)
import Text.Blaze.Html5.Attributes as At hiding (step)

import Data.Typeable
import Control.Monad(when)
import Control.Monad.Trans
import Control.Monad.Identity
import qualified Data.Map as M
import Data.Maybe
import System.IO hiding(openFile,writeFile,openBinaryFile, hGetLine, hPutStr)
import System.IO.UTF8(openBinaryFile,hGetLine, hPutStr,writeFile)
import System.IO.Unsafe
import System.IO.Error(isEOFError)
import Data.Char
import System.Time
import Control.Concurrent

import qualified Data.ByteString.Lazy.Char8 as B
--import Util.Mail
import Network.Mail.SMTP
import Network.Mail.Mime hiding (simpleMail)
import Data.Monoid

--import Data.TCache
import Data.TCache.Defs
import Data.TCache.Memoization(execute)
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
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.Encoding as E
import Control.Monad
import System.Environment
import Data.String
import GHC.Conc
--import Debug.Trace
--
--(!>)= flip trace

justify=  flip fromMaybe

adminUser= "admin"

main= do
   args <- getArgs
   let port= case args of [] -> 8080 ; [x] -> read x
   syncWrite SyncManual
   index userName
   setAdminUser adminUser adminUser
   maybeColdRebuildStories
   addAdminWF
   addMessageFlows messageFlows
   runNavigation "storyreader" $ step frontpage


   where
   messageFlows=  [("admin" , transient $ runFlow admin)
                  ,("mail"  , transient $ runFlow mail)]


type Name = String
type Size= Int


type Stories= [(Name,Size)]



instance Indexable Stories where
  key= const keyStories
  defPath _= storiesPath

instance Serializable Stories where
  serialize = B.pack . show
  deserialize= read . B.unpack

storiesPath= "Stories/"

keyStories= "Stories"

rstories :: DBRef Stories
rstories= getDBRef keyStories

chunkSize= 1000 :: Int

appheader :: String -> Html -> Html
appheader titl c =
         html $ do
             El.head $ do
                   El.title << titl
                   meta ! name  "Keywords" ! content "parpendicular, sci-fi"
                   meta ! name "viewport" ! content "width=device-width,initial-scale=1"
                   meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
                   link ! rel "stylesheet" ! type_ "text/css" ! href   "http://code.jquery.com/mobile/1.2.0/jquery.mobile-1.2.0.min.css"
--                   <> link ! rel "stylesheet" ! type_ "text/css" ! href  (fromString . linkFile $ "jquery.mobile.pagination.css")
                   script ! src "http://code.jquery.com/jquery-1.8.2.min.js"  $ mempty
                   script ! src "http://code.jquery.com/mobile/1.2.0/jquery.mobile-1.2.0.min.js" $ mempty
--                 script ! src (fromString . linkFile $ "jquery.mobile.pagination.js") $ mempty



--                       <link rel="shortcut icon" href="favicon.ico" />

             body ! At.style "margin-left:5%;margin-right:5%" $ do
                     El.div $ do
                              c
                              bottom


bottom = El.div ! At.style "float:right" $ do
                 br
                 monitor
                 powered

monitor=  a ! href "http://www.monitor.us" $
     img
         ! width" 50" ! height "25"
         ! src "http://images.monitor.us/monbadges120-40.png"
         ! At.title "Monitor.Us - Free website, server & network monitoring tool"
         ! At.style "border:0"



powered=  a ! href "http://haskell.org" $
       img ! width "50" ! height "25"
           ! src "haskell-logo-revolution.png"



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

userForm= a ! name  "login" << b "Login/register" ++> br ++>
        fromStr "user" ++>
        ((,) <$> br ++> getString Nothing                             <! [("size","9")]
             <*> br ++> fromStr "Password: " ++> br  ++> getPassword  <! [("size","9")]
             <** br ++> submitButton "login")
        <+> br ++> (fromStr "Repeat Pass." ++> br ++> getPassword <! [("size","9")]
        <*  br ++> submitButton "Register")



userFormOrName1= (wform $ userFormOrName Nothing userForm)   <! [("data-ajax","false")]

linkOrUser user= do
 case  user== anonymous of
     False -> fromStr user
     True  -> ftag "a" (fromStr "login")  `attrs` [("href","#login")]


data Story= Story{stname :: String, blocks :: V.Vector T.Text} deriving Typeable

instance Indexable Story where
  key (Story n _)= n
  defPath _= storiesPath

instance IResource Story where
  keyResource = key
  writeResource s=return()
  readResourceByKey = defReadResourceByKey
  delResource = defDelResource

instance Serializable Story where
  serialize=  error "serialize Story not implemented"
  deserialize str =
            case parse pstory  "" (B.snoc str ' ')  of
                     Left  err   -> error $ "deserialize Story error: " ++ show err
                     Right story -> story

pstory = P.try separatorp P.<|> nseparatorp

separatorp = do
       n <- getName
       xs <- Text.Parsec.many scan
       return . Story n $ V.fromList
         $ Prelude.map(\(i,x) -> decorate i  x  )  xs

scan= do
     (has,s) <- scan1 False 0 B.empty
     return (has,  E.decodeUtf8With (\_ _ -> return  '-') s)

scan1 has 5000 _ = fail ""
scan1 has n s=
    ( try hasSeparator >> return (has,s)) P.<|>
    ( try hasHtmlBreaks >>= \t-> scan1 True n (s `B.append` B.pack t)) P.<|>
    ( anyChar >>= \c -> scan1 has (n+1) (B.snoc s c) P.<|> return (has,s))


getName=   getLine1 >>= return . filter isAscii

hasSeparator=  try (string "&gt;&gt;&gt;")  P.<|>
               try (string ">>>")

hasHtmlBreaks=
               try (string "</p>" >> return  "</p>" ) P.<|>
               try (string "<br/>" >> return  "<br/>") P.<|>
               try (string "<br>" >> return "<br>")


nseparatorp= do
       n  <- getName
       xs <- Text.Parsec.many (takep chunkSize)
       return . Story n $ V.fromList $ map(\(i,x) -> decorate i  x)  xs


takep n= takep1 False n []

getLine1= manyTill anyChar ((eof >> return '\n'  ) <|> char '\n')

takep1 has 0 cs=do
    s <- getLine1
    return (has,  E.decodeUtf8With (\_ _ -> return  '-') $ B.pack . reverse $s ++cs)


takep1 has n cs=
       (hasHtmlBreaks >>= \t -> takep1 True n (reverse t ++ cs)) P.<|>
       do c <- anyChar
          takep1 has (n-1) (c:cs) -- P.<|> return ( E.decodeUtf8 . B.pack  $ reverse cs)

data GeneralConf=
     G{ gblockreceive :: Int
      , gTimeBetweenBlocks :: Integer}
      deriving (Read, Show, Typeable)

instance Serializable GeneralConf where
  serialize = B.pack . show
  deserialize= read . B.unpack

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

instance Serializable UserContext where
  serialize = B.pack . show
  deserialize= read . B.unpack

data Navigation= Seek Int | Menu | ByMail deriving (Typeable, Read, Show)


daySecs= 24*60*60 :: Integer


loginAsAdmin=    getUser (Just adminUser)
                      $    El.div ! customAttribute "data-role" "page"
                      <<<  p << b  "Please login as Administrator"
                      ++>  wform userLogin  <! [("data-ajax","false")]

readStories= atomic $ readDBRef rstories `onNothing` error "showStories: stories not found"

showStories ::  FlowM Html IO ()
showStories   = do
  setHeader $ appheader parpendicular
  setTimeouts 200 0
  loginAsAdmin

  showStories1
  where
  showStories1 :: FlowM Html IO ()
  showStories1   =  do

     stories <- readStories
     user <- getCurrentUser                            -- !> "getCurrentUser"
     rUContext<- ( atomic $  newDBRef $ UC user Nothing . M.fromList . map (\(n,s) -> (n,userStorycontext0{sname=n, ref= getDBRef n})) $ stories)
     uc@UC{..} <- atomic $ readDBRef rUContext `onNothing` error "nor found user context "
     back <- goingBack
     story <- case (currentStory, back) of
          (Just s, False) -> return s
          _ -> do
            s <- ask $   linkOrUser user ++> maybeLogout **> listStories1 stories <**  userFormOrName1
            atomic $ writeDBRef rUContext  $ uc{currentStory=Just s}
            return s

     navigate rUContext  story                      -- !> "exit the menu. going to navigate"
     showStories1

  navigate rUContext  story= do
     uc@(UC n _ mapcontext) <- atomic $ readDBRef rUContext `onNothing` error "nor found user context "
     G ntimes t <- atomic $ readDBRef rconf `onNothing` return config0
     TOD tnow _ <- liftIO $ getClockTime
     let context@USC{..}= M.lookup story mapcontext `justify` userStorycontext0{sname=story, ref= getDBRef story}
     user <- getCurrentUser
--     if  user /= adminUser && readToday >= ntimes -- && lastAccess + t * daySecs < tnow
--      then do
--        ask $   (p << "Sorry, you have completed the Story for today. Try tomorrow if you like or read other stories" +++
--                 p << "You can also receive it by mail.  If you are registered, Click \"receive by mail\" in the next page")
--           .++>. wlink () (b << "press here")
--        atomic $ writeDBRef rUContext  uc{currentStory= Nothing}
--        breturn ()
--      else do
     msize <- readStories >>= return . lookup story
     case msize of
       Nothing -> do
               atomic $ writeDBRef rUContext uc{currentStory = Nothing}
               return ()
       Just size -> do

         let key1= sname  ++ show seek

         r <- ask $     requirements
                  >>   (topForm sname  seek  size
                  <|> wcached key1 0 (showBuffer context)
                  <** td ! customAttribute "data-role" "bottom" <<< userFormOrName1)

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
         let mailw = case  user== anonymous of
                        False -> byMail
                        True  -> noWidget
             byMail= td <<< wlink ByMail (El.span "By mail")
         table ! At.style "width:100%" !  customAttribute "data-role" "header"
          <<< tr
            <<< (td <<<(  linkOrUser user ++> maybeLogout)
            **> (td  ! At.style "text-align:center" << toHtml (if size==0 then "0" else show (seek * 100 `Prelude.div` size) ++ "%")
            ++> mailw)
            <++ td  ! At.style "text-align:right" << title)

requirements= return ()


getChunk  context  = do
       Story _ chunks <- atomically $ readDBRef (ref context) `onNothing` error ("Story not found: " ++ keyObjDBRef (ref context))
       let seekit= seek context
           chunk= chunks V.! seekit
       return (chunk , seekit, fromIntegral $ V.length chunks)

parpendicular= "Parpendicular Universes"
--listStories :: [(String,Int)] -> View Html IO  String

frontpage :: FlowM Html IO ()
frontpage= do
   setHeader $ appheader parpendicular
   ask  $ (El.div ! At.style "word-wrap:break-word;text-align:center"
      << (p << b  "==/////=="
      <> h3 <<  b  "ParpendicularUniverses.com"
      <> h4  "The best infotainment for commuters, non crionized hibernants and in-transit cargo ship crews"
      <> p  << b   "(Don't read this while maneoeuvering!)"
      <> h4  "Zoom in your electronic advices"
      <> p  << b  "|\\|"
      <> p  << b  "=================       #       ================"
      <> br)

      <> footer "Content of this service is under approval process. The software of this service is currently being designed and tested in collaboration with the parasite sofware eradication task force"

      )
      ++> wlink () (p  "Enter. Only authorized personnel")
      <++ a ! href "/admin" << El.span "Admin"
   showStories
   where
   footer= p! At.style "font-family:courier, \"courier new\", monospace;font-size:100%;"

listStories1  stories=
   h4  "Choose a story"  ++>
   firstOf [p <<< wlink s (b << s) | (s, _) <- stories]






disableAttrs = [("style","visibility:hidden")]
decorate isHtml buf =  case isHtml of
         False -> T.concat . map (\l->  l <> T.pack "<br/>") $  T.lines buf
         True  ->  buf

showBuffer  context =
   let
     (formatbuf,seekit,size)  = execute $ getChunk context

     seekbn   = let x= seekit - 1
                in if x  >= 0 then x else seekit

     seekfw   = let x= seekit + 1
                in if x < size then x else seekit

     fwlink= let link= (wlink (Seek seekfw) $ b ">>>>")  <! [("data-transtion", "slide" )]
             in if seekfw== seekit
                           then link <! disableAttrs
                           else link

     bwlink= let link= (wlink (Seek seekbn) $ b "<<<<")  <! [("data-transtion", "reverse slide") ]
             in if seekbn == seekit
                           then link <! disableAttrs
                           else link

     otherLink = wlink Menu ( b  "Other stories")
     centered  = At.style "text-align:center"

     links = table ! At.style "width:100%"
                 <<< tr  <<<(td            <<<  bwlink
                         <|> td ! centered <<< otherLink
--                         <|> td ! centered <<< byMail
                         <|> td ! centered <<< fwlink)

--     stylet="position:relative;word-wrap:break-word,-webkit-animation-duration: .5s;"


--     jqueryMobilePagination ret=
--       ul ! customAttribute "data-role" "pagination"
--         <<( (li ! class_ "ui-pagination-prev" $ a ! href(fromString . ret $ Seek seekbn) $ "Prev")
--         <>  (li ! class_ "ui-pagination-next" $ a ! href(fromString . ret $ Seek seekfw) $ "Next")
--           )
     wrap ret=  El.span
                 ! customAttribute "data-role"  "content"
                 ! customAttribute "swipeleft"  ("$.mobile.changePage( '" <> (fromString $ ret (Seek seekbn)) <> "', { transition: 'slide' } )")
                 ! customAttribute "swiperight" ("$.mobile.changePage( '" <> (fromString $ ret (Seek seekfw)) <> "', { transition: 'slide', reverse: true } )")

                 << formatbuf



   in  links  <|> (returning  wrap )  <|> links


myURL="parpendicularuniverses.com"

showMailBuff context chunk user=
 if confirmed context then conf  else unconf

 where
 conf =
     a ! href( fromString $ myURL++ "/story="++sname context++"mail?"++user) << El.span  "get next page" <>
     br <>
     El.span << B.unpack chunk <>
     br <>
     a ! href( fromString $ myURL++ "/story="++sname context++"mail?"++user) << El.span  "get next page"


 unconf=
  let msg= "This mail was sent to you because  you have requested"
            <>" parpendicuarunierses.com to receive futher content of"

  in b  msg <>
     br <>
     a ! href(fromString $ myURL++ "/mail?"++user) << El.span "confirm your mail" <>
     br <>
     El.span << B.unpack chunk <>
     br <>
     a ! href(fromString $ myURL++ "/mail?"++user) << (El.span "confirm your mail")

setMail ref context=  do
  mail <- ask $ wform( p "You will receive just five blocks unless you ask for more"
              ++> getString (Just "Enter your mail") <** submitButton "submit")  <! [("data-ajax","false")]
  (chunk,seekit,size)  <- liftIO $ getChunk context
  UC{..} <- atomic $ readDBRef ref     `onNothing` error "setMail: user context not found"
  let xhtml =  showMailBuff context (E.encodeUtf8 chunk) ucName
  let message = toByteString xhtml
  liftIO $ ssendmail "ptueba" "agocorona@gmail.com" "title" message
  ask $ El.span "The mail has been sent to you" ++> wlink () ( El.span "click here")
  let newc = context{byMail= Just mail, confirmed= False, seek= seek context + size}
  atomic $ do
    uc@UC{..} <- readDBRef ref `onNothing` error "showStories1: not found user context"
    writeDBRef ref uc{ucStories =M.insert (sname newc) newc ucStories}


ssendmail username usermail title content=
   let wmail= "webmaster@parpendicularuniverses.com"
   in sendMailWithLogin'
               "smtp.strato.com"
               25
               "webmaster@parpendicularuniverses.com"
               "59afr0"
               $ simpleMail
                    (Address (Just "webmaster") $ fromString wmail)
                    [Address (Just "webmaster") $ fromString usermail]

                    []
                    []
                    title
                    [Part "text/html" None Nothing [] content]


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

     let xhtml = showMailBuff context (E.encodeUtf8 chunk) user
     let message = toByteString xhtml
     liftIO $ ssendmail "ptueba" "agocorona@gmail.com" "title"  message
     ask $ El.span "The mail has been sent to you" ++> wlink () ( El.span  "click here")
     showStories



atomic= liftIO . atomically

admin :: FlowM Html  IO ()
admin= do
     let admintext= "Administration"
         adcontstr= "Edit the content of an story"
         addrelstr= "Add a new story"
         delrelstr= "Delete a Story"
         gnconfstr= "Configuration"

     setHeader $ \c -> appheader admintext $ do
        h1 ! At.style "align:center" << admintext
        script ! src "http://js.nicedit.com/nicEdit-latest.js"
               ! type_  "text/javascript" $ mempty
        script ! At.type_ "text/javascript" $  "bkLib.onDomLoaded(nicEditors.allTextAreas)"
        c

     setTimeouts 0 0

     u <- loginAsAdmin


     op <- ask  $   p <<< wlink ("edit" :: String) (p adcontstr)
                    <|> p <<< wlink "add"  (p addrelstr)
                    <|> p <<< wlink "del"  (p delrelstr)
                    <|> p <<< wlink "conf" (p gnconfstr)
                    <++ toHtml (a ! href "/" << (El.span "exit from Administration"))


     case op of

      "edit" -> do
         stories <- readStories
         r <- ask $ homelink |+| h3 ! At.style "text-align:center" << adcontstr ++>  listStories1 stories
         case r of
          (Just _,_)  -> admin
          (_,Just hist)-> do
           let ref = getDBRef hist
           Story _ content <- atomic (readDBRef ref) `onNothing` (error $ "not found: "++ hist)

           mr <- ask $  homelink
                    |+| wform ( h3 ! At.style "text-align:center" << adcontstr
                               ++> getMultilineText ( T.toStrict . T.concat . map (\s ->  s <> ">>>") $ V.toList  content) <! [("rows", "30"), ("style", "width:80%")]
                               <* br ++> submitButton "submit")
                      -- <+> br +> homelink
           case mr of
            (_,Just ncontent) -> do

               liftIO $ TIO.writeFile (storiesPath ++  hist) $  fromString hist <> "\r\n" <> ncontent
               atomic $ flushDBRef ref
            (Just _,_) -> admin

      "add" -> do
         let atleast8 s= if length s <4 || or (map ( (flip elem) s) "$/,")
                then return $ Just "Sorry. The title must have at least 4 characters, with no '$' ',' or '/'"
                else return Nothing
         r <- ask $ homelink |+| wform ( h3 << addrelstr ++> br ++>
                          ((,) <$> b "Name of the Story" ++> (p <<< getString Nothing `validate` atleast8)
                               <*> p <<< getMultilineText  "Enter the content" <!  [("rows", "30"),("style", "width:80%")]
                               <*  p <<< submitButton "submit"))

         case r of
          (Just _,_) -> admin
          (_,Just(hist,ncontent)) -> do
             liftIO $  TIO.writeFile (storiesPath ++  hist) $ fromString hist <> "\r\n" <> ncontent
             s <- liftIO $ getSize hist
             atomic $ do
                 mstories <- readDBRef rstories `onNothing` error "admin: stories nor found"
                 writeDBRef rstories $ (hist,s) : mstories

      "del" -> do
          stories <- readStories
          hist  <- ask $ wlink "$home" << p << b "Admin home" <|> listStories1  stories
          if hist== "$home" then return () else do
           liftIO $ do
             atomically . writeDBRef rstories $ filter (\r-> hist /= fst r ) stories
             removeFile $ storiesPath ++  hist

      "conf" -> do
           G{..} <- atomic $  readDBRef rconf `onNothing` (writeDBRef rconf config0 >> return config0)
           r <- ask $    homelink
                    |+| (G <$> br ++> fromStr "Number of blocks to receive " ++> getInt (Just gblockreceive)
                          <*> br ++> fromStr "Time between receives" ++> getInteger (Just gTimeBetweenBlocks))
                          <*  p <<< submitButton "submit"
           case r of
             (Just _,_) -> admin
             (_, Just conf)  -> atomic $ writeDBRef rconf conf

     liftIO $ syncCache
     admin
  where
  homelink= br ++> wlink ("h" :: String) ( b  "Admin Home")
  del k   stories= M.delete  k stories

  add k v (Just stories)= M.insert k (strip k,v) stories
  add k v Nothing =  M.singleton k (strip k,v)
  strip k= k \\ "\\/:*?\"<>|"

