module Site.Html.Base (
  pageSkeleton
) where


import qualified Data.Text                   as T
import           RIO
import           Text.Blaze.Html             ( Html )
import           Text.Blaze.Html5            ( (!) )
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A


pageSkeleton :: Html -> Html
pageSkeleton content = 
  H.docTypeHtml $ do
      siteWideHead
      pageContentWrapper content


pageContentWrapper :: Html -> Html
pageContentWrapper content = 
  H.body $ do
    H.div ! A.class_ "site-wide-wrapper" $ do
      siteWideContentHead

      H.div ! A.class_ "home-main" $
        content

    siteWideFooter
            
siteWideNav :: Html
siteWideNav = 
  H.nav ! A.class_ "site-head-nav" $
    H.ul $ do
      H.li ! A.class_ "nav-item" $
        H.a ! A.href "/posts" $ "Blog"
      H.li ! A.class_ "nav-item" $
        H.a ! A.href "/projects" $ "Projects"
      H.li ! A.class_ "nav-item" $
        H.a ! A.href "/about" $ "About"
      H.li ! A.class_ "nav-item" $
        H.a ! A.href "/contact" $ "Contact"


siteWideContentHead :: Html
siteWideContentHead = 
  H.header ! A.class_ "site-wide-head" $ do
    H.div ! A.class_ "site-head-logo" $ do
      H.div ! A.class_ "logo-left" $ H.a ! A.href "/" $ "ekadanta"
      H.div ! A.class_ "logo-right" $ H.a ! A.href "/" $ ".co"
    siteWideNav


siteWideHead :: Html
siteWideHead = 
  H.head $ do
    H.title "Ekadanta.co / erik aker"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    H.meta ! A.name "Content-Type" ! A.content "text/html; charset=UTF-8"
    H.link ! A.href "/static/images/favicon.ico" ! A.rel "icon"
    H.link ! A.href "//cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.link ! A.href "/static/css/normalize.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.link ! A.href "/static/css/skeleton.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.link ! A.href "/static/highlight/styles/default.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.link ! A.href "/static/css/styles.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.script ! A.type_ "text/javascript" ! A.src "static/highlight/highlight.pack.js" $ ""


siteWideFooter :: Html
siteWideFooter = 
  H.footer ! A.id "site-footer" $ do
    H.p ! A.class_ "footer-copyright" $ "Ekadanta.co Copyright (c) 2018 Erik Aker"
    H.p ! A.class_ "footer-attribution" $ do
      void "Site designed by "
      H.a ! A.href "https://www.linkedin.com/in/jonwhitmire" ! A.target "_blank" $ "Jon Whitmire"
    H.p ! A.class_ "footer-footenote" $ do
      void "This site is "
      H.a ! A.href "https://gitlab.com/erewok/ekadanta-co-servant" ! A.target "_blank" $ "open source"
    H.script $ H.text googAnalytics
    

googAnalytics :: T.Text
googAnalytics = T.unlines ["(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){"
                , "(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),"
                , "m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)"
                , "})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');"
                , "ga('create', 'UA-89190801-1', 'auto');"
                , "ga('send', 'pageview');"]