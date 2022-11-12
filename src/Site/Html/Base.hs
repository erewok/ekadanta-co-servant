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
    H.link ! A.href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.2.0/css/all.min.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.link ! A.href "/static/images/favicon.ico" ! A.rel "icon"
    H.link ! A.href "/static/css/normalize.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.link ! A.href "/static/css/skeleton.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.link ! A.href "/static/highlight/styles/default.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.link ! A.href "/static/css/styles.min.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.script ! A.type_ "text/javascript" ! A.src "/static/highlight/highlight.pack.js" $ ""

siteWideFooter :: Html
siteWideFooter =
  H.footer ! A.id "site-footer" $ do
    H.p ! A.class_ "footer-copyright" $ "Ekadanta.co Copyright (c) 2022 Erik Aker"
    H.p ! A.class_ "footer-attribution" $ do
      void "Site designed by "
      H.a ! A.href "https://www.linkedin.com/in/jonwhitmire" ! A.target "_blank" $ "Jon Whitmire"
    H.p ! A.class_ "footer-footenote" $ do
      void "This site is "
      H.a ! A.href "https://gitlab.com/erewok/ekadanta-co-servant" ! A.target "_blank" $ "open source"
    H.script ! A.type_ "text/javascript" $ "hljs.initHighlightingOnLoad();"
