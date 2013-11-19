-----------------------------------------------------------------------------
--
-- Module      :  Util.Mail
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Util.Mail ( sendMail

) where

import Network.SMTP.ClientSession
import Network.SMTP.Client
import Network.Socket
import System.Time
import System.IO
import Data.Bits
import Data.IORef



myDomain = "parpendicularuniverses.com"
smtpHost = "smtp.strato.com"    -- <-- Your SMTP server here

 -- This will send the author an email.  I don't mind!
sendMail username usermail title content= withSocketsDo $  do
     now <- getClockTime
     nowCT <- toCalendarTime now
     let message = Message [
                 From [NameAddr (Just "Parmendicular Universes") "noreply@parpendicularuniverses.com"],
                 To   [NameAddr (Just username) usermail],
                 Subject title,
                 Date nowCT
             ]
             content
     addrs <-  getAddrInfo Nothing (Just smtpHost) Nothing
     let addr = addrAddress (addrs !! 0)
     let sockAddr= case addr of
                       SockAddrInet _ hostAddr -> SockAddrInet (fromIntegral 587) hostAddr
                       SockAddrInet6 pn finfo hostAddr scope -> SockAddrInet6 (fromIntegral 25) finfo hostAddr scope
                       other -> error $ show other

     putStrLn $ "connecting to "++show sockAddr
     sentRef <- newIORef []
     sendSMTP' (hPutStrLn stderr) (Just sentRef) myDomain
         sockAddr [message]
     statuses <- readIORef sentRef
     -- If no exception was caught, statuses is guaranteed to be
     -- the same length as the list of input messages, therefore head won't fail here.
     case head statuses of
         Nothing     -> putStrLn "Message successfully sent"
         Just status -> putStrLn $ "Message send failed with status "++show status


testmail= sendMail "ptueba" "agocorona@gmail.com" "title" "message"

