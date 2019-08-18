{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Lib ( someFunc) where 
import GHC.Generics
-- import qualified Data.Text (Text())
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import Lens.Micro.TH (makeLenses)
import Lucid (toHtml, body_, charset_, meta_, head_, renderText, doctypehtml_)
import Graphics.Plotly.Histogram (histogram)
import Graphics.Plotly.Lucid (plotlyCDN)
import Graphics.Plotly (aes, bars, orientation, Orientation(Vertical,Horizontal))
import Graphics.Plotly.Simple (hbarChart, fanPlot)
import Graphics.Plotly(Barmode(Stack,Group), barmode)
import Data.Aeson (toJSON)
-- import Data.Text (Text)
import Data.Aeson
import Lens.Micro ((.~), (&), (?~))
import Lens.Micro 
-- import Graphics.Plotly (plotly, line, aes, x, y, hbars, z, Trace())
import Graphics.Plotly.Base hiding (layout)
import qualified Graphics.Plotly.Base as PB
import Graphics.Plotly.Utils (jsonOptions, dropInitial, unLens)

import qualified Data.Text.Lazy.IO as I
import qualified Data.Aeson.Text as AT -- (encodeToLazyText)

data RankSummary = RankSummary { rankValue :: T.Text
                 , readCount :: Int
                 , rank :: Rank
                 , n50   :: Double
                 , contigCount :: Int
                 , assemblyLength :: Int
                 , speciesCount :: Int
                 , staxids :: [T.Text]
                 , qseqid :: T.Text }

data Rank = Species | Genus
 -- preceding underscores are necessary for makeLenses tow work properly
 -- otherwise it will copmlain about Functor.Identity with the use of barmode
data Figure = Figure { _figdata :: [Trace], _figlayout :: Layout } deriving (Generic)

--instance ToJSON Figure where
--  toJSON = genericToJSON jsonOptions {fieldLabelModifier = renamer}
--    where renamer = dropInitial "fig" . unLens
instance ToJSON Figure where
  toJSON = genericToJSON jsonOptions {fieldLabelModifier = renamer}
    where renamer = dropInitial "fig" . unLens

makeLenses ''Figure
figure tcs = Figure tcs defLayout

someFunc :: IO ()
someFunc = main
main = do
    putStrLn "hi!"
    T.writeFile "blue." "red!"
    let traces = [subPlot bars taxon sampY "Sample" summs & marker ?~ (defMarker & opacity ?~ 0.2)
                , subPlot bars taxon contY "Control" summs]
    let plot' =  plotly "anHtmlId" traces & PB.layout . barmode ?~ Group
    -- let fig' = plotly "bleh" traces & layout . barmode ?~ Group
    -- let fig' = figure traces & figlayout . barmode ?~ Group
    I.writeFile "finplot.json" $ AT.encodeToLazyText $ figure traces & figlayout . barmode ?~ Group
--    T.writeFile "mygroup.html" $ renderText $ doctypehtml_ $ do
--    head_ $ do meta_ [charset_ "utf-8"]
--               plotlyCDN
--    body_ $ toHtml $  plotly "myDiv" 
--      [subPlot bars taxon sampY "Sample" summs & marker ?~ (defMarker & opacity ?~ 0.2) 
--      , subPlot bars taxon contY "Control" summs] & layout . barmode ?~ Group

pointsData :: [(Double, Double)]
pointsData = zip [1,2,3,4] [500,3000,700,200]


hbarData :: [(T.Text, Double, Double)]
hbarData = [("Simon", 14.5, 63), ("Joe", 18.9, 9), ("Dorothy", 16.2, 2)]
-- any extractor will work after .~; it needs to return a json value.

data Summary = Summary { sampY :: Double, contY :: Double, taxon :: T.Text }

summs = [Summary 1 10 "giraffe", Summary 3 10 "elefant", Summary 5 32 "baz"]
fst' (a,_,_) = a
snd' (_,b,_) = b
thd' (_,_,c) = c

subPlot typ fx fy label vs = 
         typ  & x ?~ fmap (toJSON . fx) vs
              & y ?~ fmap (toJSON . fy) vs
              & name ?~ label
              & orientation ?~ Vertical
myTrace2 :: Trace
myTrace2
  = bars     & x ?~ fmap (toJSON . taxon) summs
             & y ?~ fmap (toJSON . sampY) summs
             & name ?~ "Sample"
             & orientation ?~ Vertical

myTrace :: Trace
myTrace
  = bars     & x ?~ fmap (toJSON . taxon) summs
             & y ?~ fmap (toJSON . contY) summs
             & name ?~ "Control"
             & orientation ?~ Vertical

hbarData' :: [(T.Text, Double)]
hbarData' = [("Simon", 14.5), ("Joe", 18.9), ("Dorothy", 16.2)]
