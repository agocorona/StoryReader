{-# OPTIONS -XDeriveDataTypeable
            -XTypeSynonymInstances
            -XScopedTypeVariables
            -XFlexibleInstances
            -XRecordWildCards
            #-}
{-


limitar la navegación hacia adelante por tiempo (probar)
cambiar cookies a Max_Time x
login de admin con letrero y sin registro x
si se ha borrado la defaultStory, se para
se pierde un caracter al final de el papel de Rey
byteString
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

import Data.ByteString.Lazy.Char8 as B(take,pack, unpack,snoc,empty)

import Util.Mail


import Data.Monoid

import Data.TCache
import Data.TCache.DefaultPersistence
import Data.TCache.Defs
import Data.TCache.Memoization
import Data.TCache.IndexQuery
--import Data.TCache.Memoization
--import Data.RefSerialize hiding((<|>),empty)
--import qualified Data.RefSerialize  as R ((<|>),empty,try)
import Text.Parsec  hiding ((<|>))
import qualified Text.Parsec as P (try,(<|>))

import System.Directory
import Data.List((\\))
import Data.Char(isSpace)
import Control.Exception as E
import qualified Data.Vector as V
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import Control.Monad

import Debug.Trace
(!>)= flip trace



justify=  flip fromMaybe

adminUser= "admin"




main= do
   index userName
   userRegister adminUser adminUser
   syncWrite SyncManual
   maybeColdRebuildStories

   putStrLn $ "in the browser go to: http://localhost"

   run 80 $ hackMessageFlow  messageFlows
   loop
  `E.catch` (\(e:: E.SomeException) ->do

                      ssyncCache
                      error $ "\nException: "++ show e)
   where
   ssyncCache= putStr "sync..." >> syncCache >> putStrLn "done"
   loop= do
       op <- getLine
       case op of
        "sync" -> ssyncCache >>  loop
        "flush" -> atomically flushAll >> putStrLn "flushed cache" >> loop
        "end"  -> ssyncCache >> putStrLn "bye" >> return()
        "abrt" -> return()
        _ -> loop

   messageFlows=  [("noscript" , transient $ runFlow showStories)
                  ,("admin"    , transient $ runFlow admin)
                  ,adminWF
                  ,("mail"     , transient $ runFlow mail)]


type Name = String
type Size= Int


type Stories= [(Name,Size)]



--
--instance Serialize Stories where
-- showp  stories = showp . map (\(n,(f,_)) ->  (n,f) )$ M.assocs stories
-- readp  = do
--    stories :: [(String,String)] <- readp
--    return $ M.fromList $ map (\(s,n)-> (s(n,getDBRef n))) stories
--    where
--
--    storiesList stories = unsafePerformIO $ do
--       hs <- mapM ( \f -> openBinaryFile f ReadWriteMode) $ map (storiesPath++) stories
--       return . M.fromList $ zip stories $ zip stories hs

instance Indexable Stories where
  key= const keyStories
  defPath _= storiesPath

storiesPath= "Stories/"

keyStories= "Stories"

rstories :: DBRef Stories
rstories= getDBRef keyStories

chunkSize= 1000 :: Int

appheader title c =  thehtml
               << ((header
                   << (thetitle << title +++
                       meta ! [name "Keywords",content "parpendicular, sci-fi"])) +++
                  body ! [thestyle "margin-left:5%;margin-right:5%"]<<  c)


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
     Story _ chunks <- getResource (Story s undefined) `onNothing` error ("coldRebuild: story not found:"++s)
     return . fromIntegral $ V.length chunks

userForm=
       (User <$> getString (Just "name")           <! [("size","4")]
             <*> getPassword                       <! [("size","4")]
             <+> submitButton "login")
             <+> getPassword                       <! [("size","4")]
             <*  submitButton "Reg"


userwidget=  userWidget Nothing userForm

userFormOrName= View $ do
  felem@(FormElm f mu) <- runView userwidget
  case mu of
    Just u -> return $ FormElm [fromString u] mu
    Nothing -> return felem

data Story= Story{stname :: String, blocks :: V.Vector T.Text} deriving Typeable

instance Indexable Story where
  key (Story n _)= n
  defPath _= storiesPath

separator=  ">>>"
instance IResource Story where
  keyResource = key
  writeResource s=return()
  readResourceByKey = defReadResourceByKey
  delResource = defDelResource

instance Serializable Story where
  serialize=  error "serialize Story not implemented"
  deserialize str = case parse pstory  "" str  of
                     Left err  -> error $ "deserialize Story error: " ++ show err
                     Right story  ->   story
   where

    pstory = P.try(separatorp)  <|> nseparatorp


    scan= scan1 0 B.empty >>= return .  E.decodeUtf8
    scan1 2000 _= fail ""
    scan1 n s=
        (symbol1 separator >> return s)     P.<|>
        (anyChar >>= \c -> scan1 (n+1) (B.snoc s c) P.<|>  return s)


    separatorp= do
           n <- getName
           xs <- many  scan
           return . Story n $ V.fromList xs

    getName=   manyTill anyChar (char '\n') >>= return  . filter isAscii
    nseparatorp= do
           n  <- getName
           ch <- many $ takep chunkSize

           return . Story n $ V.fromList ch  -- !> (n)

    takep n= takep1 n []

    takep1 0 cs= return . E.decodeUtf8 . B.pack $ reverse cs
    takep1 n cs= do
           c <-   anyChar
           takep1 (n-1) (c:cs) P.<|> return ( E.decodeUtf8 . B.pack  $ reverse cs)

    symbol1 s= mapM char s

--
--instance Serialize Story where
--  showp= error " showp [Story] not implemented"
--  readp = do
--    sep <- detectSeparator
--    if sep then separatorp !> "separator"
--            else nseparatorp !> "nonserparator"
--    where
--    detectSeparator= R.try (detectSeparator1 0) R.<|> return False
--
--    detectSeparator1 200= return False
--    detectSeparator1 n=
--              (symbol1 separator >> return True) R.<|>
--              (anyChar >> detectSeparator1 (n+1))
--    separatorp= return Story `ap` getName
--                            `ap`  ( return V.fromList
--                               `ap` sepBy (return T.pack `ap` many anyChar) (symbol1 separator))
--    getName=  manyTill anyChar (char '\n')
--    nseparatorp= do
--       n <- getName
--       ch <-  many $ takep chunkSize
--
--       return . Story n $ V.fromList ch
--
--    takep n= takep1 n []
--
--    takep1 0 cs= return . T.pack $ reverse cs
--    takep1 n cs= do
--       c <- anyChar
--       takep1 (n-1) (c:cs) R.<|> return ( T.pack $ reverse (c:cs))
--
--    symbol1 s= mapM char s
--
--
--    --takep1 n= replicateM n anyChar >>= return . T.pack
--
--
--     return Story `ap` getName `ap` ( return V.fromList `ap` many( return E.decodeUtf8 `ap` takep chunkSize))




data GeneralConf=
     G{ gblockreceive :: Int
      , gtimenetweenblocks :: Integer}
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
                      , ssize :: Int
                      , lastAccess :: Integer
                      , readToday  :: Int
                      , ref  :: DBRef Story}
                      deriving (Read, Show, Typeable)

data UserContext=UC{ ucName :: String
                   , currentStory :: Maybe String
                   , ucStories :: M.Map String UserStoryContext} deriving (Read, Show, Typeable)



userStorycontext0= USC (error "name nof defiend in story") Nothing False 0 0 0  0 (error "ref nof defined in story")

instance Indexable UserContext where
  key (UC n _ _)= "UC"++ n




data Navigation= Seek Int | Menu | ByMail deriving (Typeable, Read, Show)

--getUserContext= atomically $  do
--  stor <- readDBRef rstories  `onNothing` error "not found stories"
--  let cont = UC "" $ M.fromList [(n,userStorycontext0{sname=n}) | n <- M.keys stor]
--  newDBRef cont
daySecs= 24*60*60 :: Integer

newDBRef1 ::   (IResource a, Typeable a) => a -> STM  (DBRef a)
newDBRef1 x = do
  let ref= getDBRef $! keyResource x                -- !> "getDBRef"

  mr <- readDBRef  ref                              -- !> "readDBRef"
  case mr of
    Nothing -> writeDBRef ref x >> return ref       -- !> " write"
    Just r -> return ref                            -- !> " non write"


--showStories ::  FlowM Html (Workflow IO) ()
showStories   = do
  setHeader $ appheader parpendicular
  setTimeouts 0 0
  stories <- atomic $ readDBRef rstories `onNothing` error "showStories: stories not found"

  showStories1 stories
  where

  showStories1  stories =  do
     user <- getCurrentUser                            -- !> "getCurrentUser"
     rUContext<- ( atomic $  newDBRef1  $ UC user Nothing . M.fromList . map (\(n,s) -> (n,userStorycontext0{sname=n,ssize=s, ref= getDBRef n})) $ stories)
     uc@UC{..} <- atomic $ readDBRef rUContext `onNothing` error "nor found user context "
     back <- goingBack
     story <- case (currentStory, back) of
          (Just s, False) -> return s
          _ -> do
            s <- ask $  userFormOrName **> {- cachedWidget "menu" 0-} (listStories1 stories)
            atomic $ writeDBRef rUContext  $ uc{currentStory=Just s}
            return s

     navigate rUContext  story                      -- !> "exit the menu. going to navigate"
     showStories1 stories

  navigate rUContext  story= do
     uc@(UC n _ mapcontext) <- atomic $ readDBRef rUContext `onNothing` error "nor found user context "
     G ntimes t <- atomic $ readDBRef rconf `onNothing` return config0
     TOD tnow _ <- liftIO $ getClockTime
     let context@USC{..}= M.lookup story mapcontext `justify` error ("context not found for "++ story)
     if lastAccess + t * daySecs  < tnow && readToday >= ntimes
      then do
       ask $ p << "sorry, you have completed the Story for today. Try tomorrow if you like or read other stories"
           ++> wlink () (bold << "press here")
--       when (readToday >= ntimes) $
       backPointReturn ()
      else do
        logged <- isLogged

--        (chunk,seekit,size)  <- getChunk context
--        key <- addrStr chunk
        r <- ask $   topForm sname  seek  ssize
                 **>  -- cachedWidget (sname  ++ show seek ) 0
                                   (showBuffer logged context)

        case r of
            Menu       -> do
                       atomic $ writeDBRef rUContext  uc{currentStory= Nothing}
                       backPointReturn ()
            ByMail     -> setMail  rUContext context >> navigate rUContext  story
            Seek seek  -> do

                let newc = context{seek= seek, lastAccess= tnow, readToday= readToday + 1}
                atomic $ writeDBRef rUContext  uc{ucStories= M.insert story newc mapcontext}
                navigate rUContext story

  topForm title seek size=
         table ! [thestyle "width:100%"]
          <<< tr
            <<< (td <<< userFormOrName
                <++ concatHtml
                  [td  ! [align "center"] << (if size==0 then "0" else show (seek * 100 `div` size) ++ "%")
                  ,td  ! [align "right"] << title])


getChunk  context  = do
       Story _ chunks <- atomically $ readDBRef (ref context) `onNothing` error ("Story not found: " ++ keyObjDBRef (ref context))
       let seekit= seek context
           chunk= chunks V.! seekit
       return (chunk , seekit, fromIntegral $ V.length chunks)

-- do
--  Just stories <- atomically $ readDBRef rstories
--  let mh =  M.lookup (sname context) stories
--  case mh of
--    Nothing -> return (["This Story not longer exist"],0,0)
--    Just (_,h) -> do
--      hSeek h  AbsoluteSeek $ seek context
--      readLines h (seek context)

parpendicular= "Parpendicular Universes"
--listStories :: [(String,Int)] -> View Html IO  String
listStories  stories=
   h2 << "Choose a story"  ++>
   msum [p <<< wlink s (bold << s) | (s, _) <- stories]

listStories1 stories=
   h1 << parpendicular ++>
   h3 << "Infotainment for a galaxy in crisis" ++>
   listStories  stories

readLines h seek= readLines1     [] 0
  where
  readLines1 buf len =do
    mr <- hGetLineExc h
    size<- hFileSize h
    case mr of
       Nothing -> return (buf,seek,size)
       Just line  -> do
        let len'= len + length line
            buf'= buf++ [line]
        if fromIntegral len' >= chunkSize + 80
           then do
             let buf''= if seek >0 then dropWhile(not . isSpace) (head buf'):tail buf'
                                   else buf'
             return (buf'',seek, size)
           else readLines1 buf' len'

hGetLineExc h= (do
     x <- hGetLine h
     return $ Just x)
    `E.catch` (\(e :: IOError) -> do
        when( not $ isEOFError e) $ print e
        return Nothing)



--showBuffer :: (MonadIO m, Functor m)
--           =>  Integer ->  Int
--           -> [String] -> View Html m Integer
showBuffer logged context =

   let
     (buf,seekit,size)  = execute $ getChunk context
     disableAttrs = [("style","visibility:hidden")]

     seekbn   = let x= seekit - 1
                in if x  >= 0 then x else seekit

     seekfw   = let x= seekit + 1
                in if x < size then x else seekit


     fwlink= let link= wlink (Seek seekfw) $ bold << ">>>>"
             in if seekfw== seekit
                           then link <! disableAttrs
                           else link


     bwlink= let link= wlink (Seek seekbn) $ bold << "<<<<"
             in if seekbn == seekit
                           then link <! disableAttrs
                           else link

     otherLink = wlink Menu ( bold <<"Other stories")
     mailattrs = if not logged then disableAttrs else []
     byMail    = wlink ByMail (thespan << "receive it by mail") <! mailattrs
     centered  = [align "center"]

     links     = table   ! [thestyle "width:100%"]
                 <<< tr  <<<(td            <<<  bwlink
                         <|> td ! centered <<< otherLink
                         <|> td ! centered <<< byMail
                         <|> td ! centered <<< fwlink)

     lenbuf2    = T.length buf `div` 2
     (buf1,buf2)= T.splitAt lenbuf2  buf
     disablemail= if logged then [("style","text-decoration: none;color:black")] else []
     wrap buf= thespan ! [thestyle "word-wrap:break-word"]
                    << (thespan << (linesToHtml . lines $ T.unpack buf))

--    cwrap buff i= cachedp  wrap buff

     bufLink1   = wlink  (Seek seekbn) (wrap buf1 ) <! [("style","text-decoration: none;color:black")]
     bufLink2   = wlink  (Seek seekfw) (wrap buf2 ) <! [("style","text-decoration: none;color:black")]

   in links <|> bufLink1 <|> bufLink2 <|> links

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
  mail <- ask $ p << "You will receive just five blocks unless you ask for more"
              ++> getString (Just "Enter your mail")
  (chunk,seekit,size)  <- liftIO $ getChunk context
  UC{..} <- atomic $ readDBRef ref     `onNothing` error "setMail: user context not found"
  let xhtml = showMailBuff context chunk ucName
  let message = showHtml xhtml
  liftIO $ sendMail "ptueba" "agocorona@gmail.com" "title" message
  ask $ thespan << "The mail has been sent to you" ++> wlink "" ( thespan << "click here")
  let newc = context{byMail= Just mail, confirmed= False, seek= seek context + size}
  atomic $ do
    uc@UC{..} <- readDBRef ref `onNothing` error "showStories1: not found user context"
    writeDBRef ref uc{ucStories =M.insert (sname newc) newc ucStories}



mail= do
     story <- getEnv  "story" `onNothing`  error "mail: story not found"
     user <-  getUser Nothing   userFormLine
     let ref= getDBRef . key $ UC user undefined undefined
     uc@UC{..} <- atomic $ readDBRef ref        `onNothing` error "showStories1: not found user context"
     let context =  M.lookup story ucStories    `justify` userStorycontext0

     (chunk,seekit,size) <- liftIO $ getChunk context
     atomic $ do
         let newc= context{confirmed= True, seek= seek context + size}
         writeDBRef ref $ uc{ ucStories= M.insert (sname newc) newc ucStories}

     let xhtml = showMailBuff context chunk user
     let message = showHtml xhtml
     liftIO $ sendMail "ptueba" "agocorona@gmail.com" "title" message
     ask $ thespan << "The mail has been sent to you" ++> wlink "" ( thespan << "click here")
     showStories



atomic= liftIO . atomically
--admin :: FlowM Html  IO ()
admin= do
     let admintext= "Administration"
         adcontstr= "Edit content to a story"
         addrelstr= "Add a new story"
         delrelstr= "Delete a Story"
         gnconfstr= "Configuration"

     setHeader $ \c -> appheader admintext  ( h1 ![align "center"] << admintext +++ c)
     setTimeouts 0 0
     clearEnv
     u <- getUser (Just "admin") $ p << bold << "Please login as Administrator" ++> userLogin


     op <- ask   $  p <<< wlink "edit" (p << adcontstr)
                <|> p <<< wlink "add" (p << addrelstr)
                <|> p <<< wlink "del" (p << delrelstr)
                <|> p <<< wlink "conf" (p << gnconfstr)


     case op of
      "edit" -> do
         stories <- atomic ( readDBRef rstories) `onNothing` error "not found stories"
         r <- ask $ homelink |+| h3 ! [align "center"] << adcontstr ++>  listStories stories
         case r of
          (Just _,_)  -> admin
          (_,Just hist)-> do
           let ref = getDBRef hist
           Story _ content <- atomic (readDBRef ref) `onNothing` (error $ "not found: "++ hist)

           mr <- ask $  homelink
                    |+| wform ( h3 ! [align "center"] << adcontstr
                               ++> getMultilineText (Just $ concatMap T.unpack $ V.toList content) ![rows "30",thestyle "width:80%"]
                               <* br ++> submitButton "submit")
                      -- <+> br +> homelink
           case mr of
            (_,Just ncontent) -> do

               liftIO $ writeFile (storiesPath ++ hist) ncontent
               atomic $ flushDBRef ref
            (Just _,_) -> admin

      "add" -> do
         r <- ask $ homelink |+| wform ( h3 << addrelstr ++> br ++>
                          ((,) <$> p <<< getString (Just "Name of the Story")
                               <*> p <<< getMultilineText (Just "Enter the content") ![rows "30",  thestyle "width:80%"]
                               <*  p <<< submitButton "submit"))

         case r of
          (Just _,_) -> admin
          (_,Just(hist,ncontent)) -> do
             liftIO $  writeFile (storiesPath ++ hist) ncontent
             s <- liftIO $ getSize hist
             atomic $ do
                 mstories <- readDBRef rstories            `onNothing` error "no found Stories"
                 writeDBRef rstories  $ (hist,s) : mstories


      "del" -> do
           stories <- atomic $ readDBRef rstories `onNothing` error "not foun stories"
           hist  <- ask $ listStories  stories
           liftIO $ do
             atomically . writeDBRef rstories $ filter (\r-> hist /= fst r ) stories
             removeFile $ storiesPath ++ hist
      "conf" -> do
           G{..} <- atomic $  readDBRef rconf `onNothing` (writeDBRef rconf config0 >> return config0)
           r <- ask $ homelink
                   |+| (G <$> br ++> fromString "Number of blocks to receive " ++> getInt (Just gblockreceive)
                          <*> br ++> fromString "Time between receives" ++> getInteger (Just gtimenetweenblocks))
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
--  hGetContents1 h = hGetContents2 []
--   where
--   hGetContents2 str  = do
--      hSeek h  AbsoluteSeek 0
--      ml <- hGetLineExc h
--      case ml of
--       Nothing -> return str
--       Just l  -> hGetContents2 $ str++l


