module Site.Html.Base where

import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A



pageSkeleton :: H.Html5 -> H.Html5
pageSkeleton content = 
    H.docTypeHtml $
        H.Html $ do
            siteWideHead
            pageContentWrapper content


pageContentWrapper :: H.Html5 -> H.Html5
pageContentWrapper content = 
    H.body $ do
        H.div ! A.class_ "site-wide-wrapper" $ do
            siteWideContentHead

            H.div ! A.class_ "home-main" $
                content

        siteWideFooter
            
siteWideNav :: Html5
siteWideNav = H.nav ! A.class_ "site-head-nav" $
    H.ul $ do
        H.li ! A.class_ "nav-item" $
            H.a ! A.href="/posts" $ "Blog"
        H.li ! A.class_ "nav-item" $
            H.a ! A.href="/projects" $ "Projects"
        H.li ! A.class_ "nav-item" $
            H.a ! A.href="/about" $ "About"
        H.li ! A.class_ "nav-item" $
            H.a ! A.href="/contact" $ "Contact"


siteWideContentHead :: H.Html
siteWideContentHead = H.header ! A.class_ "site-wide-head" $ do
    H.div ! A.class_ "site-head-logo" $ do
        H.div ! A.class_ "logo-left" $ "ekadanta"
        H.div ! A.class_ "logo-right" $ ".co"
    siteWideNav


siteWideHead :: H.Html5
siteWideHead = H.head $ do
    H.title "Ekadanta.co / erik aker"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    H.meta ! A.name "Content-Type" ! A.content "text/html; charset=UTF-8"
    H.link ! A.href "/static/images/favicon.ico" ! A.rel "icon"
    H.link ! A.href "//cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.link ! A.href "/static/css/normalize.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.link ! A.href "/static/css/skeleton.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.link ! A.href "/static/highlight/styles/default.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.link ! A.href "/static/css/styles.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.script ! A.type_ "text/javascript" ! A.src "static/highlight/highlight.pack.js"


siteWideFooter :: Int -> H.Html5
siteWideFooter year = H.footer $ do
    H.p ! A.class_ "footer-copyright" $ "Ekadanta.co Copyright (c) " <> show year <> " Erik Aker"
    H.p ! A.class_ "footer-attribution" $ "Site designed by " $
        H.a ! A.href "" $ "Jonathan Whitmire"
    H.p ! A.class_ "footer-footenote" $ "This site is" $
        H.a ! A.href "" $ "open source"
    googAnalytics
    

googAnalytics :: T.Text
googAnalytics = T.unlines ["(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){"
                , "(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),"
                , "m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)"
                , "})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');"
                , "ga('create', 'UA-89190801-1', 'auto');"
                , "ga('send', 'pageview');"]