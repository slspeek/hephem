{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad
import           Control.Monad.Trans         (liftIO)
import           Data.Angle
import           Data.Maybe
import           Data.Text                   (Text)
import           Data.Text.Lazy              (pack, unpack)
import qualified Data.Text.Lazy              as L
import           Data.Time
import           Happstack.Lite
import           HEphem.Data
import           HEphem.HEphem
import           Text.Blaze.Html5            (Html, a, form, input, label, p,
                                              toHtml, (!))
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes (action, enctype, href, name,
                                              type_, value)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Printf
import           Text.Read.HT

main :: IO ()
main = serve Nothing myApp

myApp :: ServerPart Response
myApp = msum
 [
   dir "tour" formPage
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

numberInput :: Html -> H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> Html
numberInput l i mi ma dV = H.p $ H.tr $ do
                   H.td $ label ! A.for i $ l
                   H.td $ input ! type_ "number" ! A.min mi ! A.max ma ! A.id i ! name i ! A.value dV ! A.required "true"

parseRect :: L.Text -> L.Text -> L.Text -> L.Text -> Maybe Rectangle
parseRect s0 s1 s2 s3  =
  do
     d0 <- f s0
     d1 <- f s1
     d2 <- f s2
     d3 <- f s3
     return $ Rectangle (d0, d1) (d2, d3)
  where
    f s = Degrees <$> maybeRead (unpack s)

parseGeo :: L.Text -> L.Text -> Maybe GeoLoc
parseGeo s0 s1 =
  do
     d0 <- f s0
     d1 <- f s1
     return $ GeoLoc d0 d1
  where
    f s = Degrees <$> maybeRead (unpack s)

formPage :: ServerPart Response
formPage = msum [ viewForm, processForm ]
 where
   viewForm :: ServerPart Response
   viewForm =
       do method GET
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
                 numberInput "For hours" "hours" "0" "24" "3"
                 numberInput "Minimum score" "min_score" "0" "100" "80"
                 H.p $ H.tr $
                   H.td $ input ! type_ "submit" ! value "Give a tour"

   processForm :: ServerPart Response
   processForm =
       do method POST
          minMag <- lookText "max_mag"
          minAz <- lookText "min_az"
          maxAz <- lookText "max_az"
          minAl <- lookText "min_al"
          maxAl <- lookText "max_al"
          minScore <- lookText "min_score"
          lat <- lookText "lat"
          long <- lookText "long"
          hours <- lookText "hours"

          let mr = parseRect minAz maxAz minAl maxAl
          guard $ isJust mr

          let mgeo = parseGeo lat long
          guard $ isJust mgeo

          let mh = maybeRead $ unpack hours
          guard $ isJust mh

          let mScore = maybeRead $ unpack minScore
          guard $ isJust mScore

          let mMinMag = maybeRead $ unpack minMag
          guard $ isJust mMinMag

          tz <- liftIO getCurrentTimeZone
          t <- liftIO getCurrentTime

          let skyobjects = tour (ViewOps (fromJust mgeo)
                                  (fromJust mMinMag)
                                    (fromJust mr)
                                      t
                                        (fromJust mh * 3600)
                                          600
                                          (fromJust mScore))
          ok $ template "HEphem Sky Tour" $ do
            H.h1 "HEphem Sky Tour"
            H.table $ do
              H.tr $ do
                H.th (toHtml (pack "Local time (dutch)"))
                H.th (toHtml (pack "Object"))
                H.th (toHtml (pack "Azimuth"))
                H.th (toHtml (pack "Height"))
                H.th (toHtml (pack "Relative height %"))
                H.th (toHtml (pack "Visible before"))
                H.th (toHtml (pack "Visible after"))
              forM_ skyobjects (prettyH tz)
