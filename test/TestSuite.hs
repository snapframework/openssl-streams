{-# LANGUAGE OverloadedStrings #-}
module Main where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Data.ByteString.Char8          ()
import qualified Network.Socket                 as N
import qualified OpenSSL                        as SSL
import qualified OpenSSL.Session                as SSL
import qualified System.IO.Streams              as Streams
import qualified System.IO.Streams.SSL          as SSLStreams
import           System.Timeout                 (timeout)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)
------------------------------------------------------------------------------


main :: IO ()
main = defaultMain [sanityCheck]


------------------------------------------------------------------------------
sanityCheck :: Test
sanityCheck = testCase "sanitycheck" $ N.withSocketsDo $
              SSL.withOpenSSL $ do
    ctx1 <- setupContext
    ctx2 <- setupContext
    x <- timeout (10 * 10^(6::Int)) $ go ctx1 ctx2
    assertEqual "ok" (Just ()) x

  where
    setupContext = do
        ctx <- SSL.context
        SSL.contextSetDefaultCiphers ctx
        SSL.contextSetCertificateFile ctx "test/cert.pem"
        SSL.contextSetPrivateKeyFile ctx "test/key.pem"
        SSL.contextSetVerificationMode ctx SSL.VerifyNone

        certOK <- SSL.contextCheckPrivateKey ctx
        assertBool "private key is bad" certOK
        return ctx

    go ctx1 ctx2 = do
        portMVar   <- newEmptyMVar
        resultMVar <- newEmptyMVar
        forkIO $ client ctx1 portMVar resultMVar
        server ctx2 portMVar
        l <- takeMVar resultMVar
        assertEqual "testSocket" l ["ok"]

    client ctx mvar resultMVar = do
        port <- takeMVar mvar
        (is, os, ssl) <- SSLStreams.connect ctx "127.0.0.1" port
        Streams.fromList ["ok"] >>= Streams.connectTo os
        SSL.shutdown ssl SSL.Unidirectional
        Streams.toList is >>= putMVar resultMVar
        maybe (return ()) N.sClose $ SSL.sslSocket ssl

    server ctx mvar = do
        let hint = N.defaultHints {
                        N.addrFamily     = N.AF_INET,
                        N.addrSocketType = N.Stream,
                        N.addrFlags      = [N.AI_NUMERICHOST]
                      }
        xs <- N.getAddrInfo (Just hint) (Just "127.0.0.1") Nothing
        let [addr] = xs
        sock  <- N.socket N.AF_INET N.Stream N.defaultProtocol
        let saddr = N.addrAddress addr
        N.bind sock saddr
        N.listen sock 5
        port  <- N.socketPort sock
        putMVar mvar port
        (csock, _) <- N.accept sock
        ssl <- SSL.connection ctx csock
        SSL.accept ssl
        (is, os) <- SSLStreams.sslToStreams ssl
        Streams.toList is >>= flip Streams.writeList os
        SSL.shutdown ssl SSL.Unidirectional
        maybe (return ()) N.close $ SSL.sslSocket ssl
        N.close sock
