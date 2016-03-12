{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad
import           Control.Monad.Trans              (liftIO)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Maybe
import           Data.Angle
import           Data.Maybe
import           Data.String                      (fromString)
import           Data.Text                        (Text)
import           Data.Text.Lazy                   (pack, unpack)
import           Data.Time
import           Data.Time.RFC3339                (formatTimeRFC3339,
                                                   parseTimeRFC3339)
import           Happstack.Lite
import           Happstack.Server.Internal.Monads (ServerPartT)
import           HEphem.Data
import           HEphem.HEphem
import           Text.Blaze.Html5                 (Html, a, form, input, label,
                                                   p, toHtml, (!))
import qualified Text.Blaze.Html5                 as H
import           Text.Blaze.Html5.Attributes      (action, enctype, href, name,
                                                   type_, value)
import qualified Text.Blaze.Html5.Attributes      as A
import           Text.Printf
import           Text.Read.HT

main :: IO ()
main = serve Nothing myApp

myApp :: ServerPart Response
myApp = msum
 [
   dir "tour" tourPage
 ]

template :: Text -> Html -> Response
template title body = toResponse $
 H.html $ do
   H.head $
     H.title (toHtml title)
   H.body $ do
     body
     p $ a ! href "/tour" $ "back home"
     p $ H.h3 $ a ! href "http://github.com/slspeek/hephem" $ "HEphem source code"

prettyH :: TimeZone -> Report -> Html
prettyH lz (Report so t  (HorPos az h) score tb ta) =
   H.html $
   H.tr $ do
     H.td (toHtml tf)
     H.td (toHtml (description so))
     H.td (toHtml (printDeg az))
     H.td (toHtml (printDeg h))
     H.td (toHtml $ pack (printf "%.2f" score))
     H.td (toHtml (printDegAsTime tb))
     H.td (toHtml (printDegAsTime ta))
  where
    tf = formatTime defaultTimeLocale "%X" lt
    lt = utcToLocalTime lz t

parseDeg :: forall x. Read x => String -> MaybeT (ServerPartT IO) (Degrees x)
parseDeg s = Degrees <$> parse s

parse :: forall x. Read x => String -> MaybeT (ServerPartT IO) x
parse s = lift  (lookText s) >>= f
  where
    f t = MaybeT . pure $ maybeRead (unpack t)

parseRect ::  MaybeT (ServerPartT IO) Rectangle
parseRect  =
  do
     minAz <- parseDeg "min_az"
     maxAz <- parseDeg "max_az";
     minAl <- parseDeg "min_al";
     maxAl <- parseDeg "max_al";
     return $ Rectangle (minAz, maxAz) (minAl, maxAl)

parseGeo :: MaybeT (ServerPartT IO) GeoLoc
parseGeo =
  do
    lat <- parseDeg "lat"
    long <- parseDeg "long"
    return $ GeoLoc lat long

parseStart :: MaybeT (ServerPartT IO) ZonedTime
parseStart =
  do
    startT <- lift $ lookText "start"
    MaybeT . pure $ parseTimeRFC3339 startT

parseViewOps :: MaybeT (ServerPartT IO) ViewOps
parseViewOps =
  do
    minMag <- parse "max_mag"
    minScore <- parse "min_score"
    hours <- parse "hours"
    minObsTime <- parse "minObsTime"
    r <- parseRect
    geo <- parseGeo
    t <- parseStart
    return $ ViewOps geo minMag r (zonedTimeToUTC t) (3600 * hours) (60 * minObsTime) minScore

numberInput :: Html -> H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> Html
numberInput l i mi ma dV = H.p $ H.tr $ do
                   H.td $ label ! A.for i $ l
                   H.td $ input ! type_ "number"
                                ! A.step "0.001"
                                ! A.min mi
                                ! A.max ma
                                ! A.id i
                                ! name i
                                ! A.value dV
                                ! A.required "true"

dateTimeInput :: Html -> H.AttributeValue -> ZonedTime -> Html
dateTimeInput l i dV = H.p $ H.tr $ do
                   H.td $ label ! A.for i $ l
                   H.td $ input ! type_ "datetime"
                                ! name i
                                ! A.value (fromString (formatTimeRFC3339 dV::String))
                                ! A.required "true"

tourPage :: ServerPart Response
tourPage = msum [ viewForm, processForm ]
 where
   viewForm :: ServerPart Response
   viewForm =
       do method GET
          t <- liftIO (getCurrentTime >>= utcToLocalZonedTime)
          ok $ template "tour" $ do
             H.h1 (toHtml (pack "HEphem Sky Tour Form"))
             form ! action "/tour" ! enctype "multipart/form-data" ! A.method "POST" $
               H.table $ do
                 numberInput "Latitude" "lat" "-90" "90" "52"
                 numberInput "Longitude" "long" "-180" "180" "4"
                 numberInput "Maximum magnitude" "max_mag" "-5" "30" "9"
                 numberInput "Minimum azimuth" "min_az" "0" "360" "250"
                 numberInput "Maximum azimuth" "max_az" "0" "360" "20"
                 numberInput "Minimum height" "min_al" "0" "90" "30"
                 numberInput "Maximum height" "max_al" "0" "90" "60"
                 dateTimeInput "Observation start" "start" t
                 numberInput "For hours" "hours" "0" "24" "3"
                 numberInput "Minimum Observation time (minutes)" "minObsTime" "0" "120" "15"
                 numberInput "Minimum score" "min_score" "0" "100" "80"
                 H.p $ H.tr $
                   H.td $ input ! type_ "submit" ! value "Give a tour"

   processForm :: ServerPart Response
   processForm =
       do method POST
          mOps <- runMaybeT parseViewOps
          guard $ isJust mOps
          tz <- liftIO getCurrentTimeZone
          let skyobjects = tour $ fromJust mOps
          ok $ template "HEphem Sky Tour" $ do
            H.h1 "HEphem Sky Tour"
            H.table $ do
              H.tr $ do
                H.th (toHtml (pack "Server Local time "))
                H.th (toHtml (pack "Object"))
                H.th (toHtml (pack "Azimuth"))
                H.th (toHtml (pack "Height"))
                H.th (toHtml (pack "Relative height %"))
                H.th (toHtml (pack "Visible before"))
                H.th (toHtml (pack "Visible after"))
              forM_ skyobjects (prettyH tz)
