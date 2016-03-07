{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Angle

import           Control.Monad
import           Control.Monad.Trans         (liftIO)
import           Data.Text                   (Text)
import           Data.Text.Lazy              (pack, unpack)
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

prettyH :: TimeZone -> (UTCTime, SkyObject, HorPos, Deg, Deg, Deg) -> Html
prettyH lz (t, so, HorPos az h, score, tb, ta) =
   H.html $
   H.tr $ do
     H.td (toHtml tf)
     H.td (toHtml (description so))
     H.td (toHtml (printDeg az))
     H.td (toHtml (printDeg h))
     H.td (toHtml $ pack (printf "%.2f" (undeg score)))
     H.td (toHtml (printDegAsTime tb))
     H.td (toHtml (printDegAsTime ta))
  where
    tf = formatTime defaultTimeLocale "%X" lt
    lt = utcToLocalTime lz t

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
                 H.p $ H.tr $ do
                   H.td $ label ! A.for "min_mag" $ "Minimum magnitude"
                   H.td $ input ! type_ "text" ! A.id "min_mag" ! name "min_mag"
                 H.p $ H.tr $ do
                   H.td $ label ! A.for "min_az" $ "Minimum azimuth"
                   H.td $ input ! type_ "text" ! A.id "min_az" ! name "min_az"
                 H.p $ H.tr $ do
                   H.td $ label ! A.for "max_az" $ "Maximum azimuth"
                   H.td $ input ! type_ "text" ! A.id "max_az" ! name "max_az"
                 H.p $ H.tr $ do
                   H.td $ label ! A.for "min_al" $ "Minimum height"
                   H.td $ input ! type_ "text" ! A.id "min_al" ! name "min_al"
                 H.p $ H.tr $ do
                   H.td $ label ! A.for "max_al" $ "Maximum height"
                   H.td $ input ! type_ "text" ! A.id "max_al" ! name "max_al"
                 H.p $ H.tr $ do
                   H.td $ label ! A.for "hours" $ "For hours"
                   H.td $ input ! type_ "text" ! A.id "hours" ! name "hours"
                 H.p $ H.tr $
                   H.td $ input ! type_ "submit" ! value "Give a tour"

   processForm :: ServerPart Response
   processForm =
       do method POST
          minMag <- lookText "min_mag"
          minAz <- lookText "min_az"
          maxAz <- lookText "max_az"
          minAl <- lookText "min_al"
          maxAl <- lookText "max_al"
          let r = Rectangle (Degrees $ read (unpack minAz), Degrees $ read (unpack maxAz))
                    (Degrees $ read (unpack minAl), Degrees $ read (unpack maxAl))
          hours <- lookText "hours"
          let h = read $ unpack hours

          tz <- liftIO getCurrentTimeZone
          t <- liftIO getCurrentTime
          let skyobjects = tour geoAms (read (unpack minMag)) r t (h * 3600) 80
          ok $ template "HEphem Sky Tour" $ do
            H.h1 "HEphem Sky Tour"
            H.table $ do
              H.tr $ do
                H.th (toHtml (pack "Local time (dutch)"))
                H.th (toHtml (pack "Object"))
                H.th (toHtml (pack "Azimuth"))
                H.th (toHtml (pack "Height"))
                H.th (toHtml (pack "Score (% of maxHeight)"))
                H.th (toHtml (pack "Time visible before"))
                H.th (toHtml (pack "Time visible after"))
              forM_ skyobjects (prettyH tz)
