module Site.Html.Home (
  homePage
  , redirectPage
) where

import qualified Data.Text                   as T
import           RIO
import           Text.Blaze.Html             ( Html )
import           Text.Blaze.Html5            ( (!), Attribute )
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Site.Html.Base              as Base
import qualified Site.Types                  as Types


homePage :: [Types.Resource] -> Types.Resource -> Html
homePage projects latestPost = 
  Base.pageSkeleton $ homePageBuilder projects latestPost

homePageBuilder :: [Types.Resource] -> Types.Resource -> Html
homePageBuilder projects latestPost = do
  homeAboutSection
  homeProjectsSection projects
  homeLatestPostSection latestPost


homeAboutSection :: Html
homeAboutSection = 
  H.section ! A.class_ "home-about home-section" $
    homeAboutContent

homeAboutContent :: Html
homeAboutContent = do
  H.h1 "👋 Hi, I'm Erik."
  H.p . H.toMarkup . T.unlines $ ["I'm a former English teacher and freelance writer. " 
                                , "Now I work as a software developer in San Diego, California." 
                                , "My areas of expertise are Python, Javascript, ETL, DevOps, cloud infrastructure, "
                                , " and application design."]
  H.p . H.toMarkup . T.unlines $ ["My interests include functional programming in Haskell," 
                                 ," natural language processing, surfing, and playing with Milo (my son)."]

homeProjectsSection :: [Types.Resource] -> Html
homeProjectsSection projects = 
  H.section ! A.class_ "home-section" $ do
    H.h2 ! A.class_ "section-head" $ "Projects"
    H.section ! A.class_ "home-projects" $ do
      mconcat $ fmap (uncurry renderHomeProject) (zip [1..] projects)

homeLatestPostSection :: Types.Resource -> Html
homeLatestPostSection post =  
  H.section ! A.class_ "home-section home-post-latest" $ do
    H.h2 ! A.class_ "section-head" $ "Latest Post"
    H.div ! A.class_ "home-latest-post-bg-img" ! latestFeaturedImg post $
      H.div ! A.class_ "home-latest-post-content-left" $
        H.div ! A.class_ "home-article-lede" $ do
          H.span ! A.class_ "home-article-date" $ H.toMarkup (post ^. Types.pubdate)
          H.h3 $ H.toMarkup (post ^. Types.title)
          H.p $ H.toMarkup (post ^. Types.lede)
          H.div ! A.class_ "home-latest-post-read-more" $
            H.a ! A.href (H.toValue $ "/posts/" <> post ^. Types.pid) $
              "Read More..."


-- | Project rendering helpers

renderHomeProject :: Int -> Types.Resource -> Html
renderHomeProject projectNum project = 
  H.div ! A.class_ (H.toValue (T.unpack "home-project project" <> show projectNum)) $ do

    homeProjectTagList project

    H.div ! A.class_ "home-project-title" $
      H.h3 $ H.toMarkup $ project ^. Types.title
    H.div ! A.class_ "home-project-text" $
      H.h3 $ H.toMarkup $ project ^. Types.body
    H.div ! A.class_ "home-project-button" $
      H.a ! A.class_ "button" ! A.href (H.toValue ("/projects/" <> project ^. Types.pid)) $
        "View Project"

homeProjectTagList :: Types.Resource -> Html
homeProjectTagList project = 
  H.div ! A.class_ "home-project-tag-list" $
    H.ul ! A.class_ "home-project-tags" $ do
      mconcat $ fmap (H.li . H.toMarkup) (project ^. Types.tags)


-- | Latest Post rendering helpers

latestFeaturedImg :: Types.Resource -> Attribute
latestFeaturedImg post = case post ^. Types.featuredImage of
  Nothing -> A.style "" 
  Just featuredImg ->
    A.style $ H.toValue $ "background: url('"<> featuredImg <> "');"


redirectPage :: String -> H.Html
redirectPage uri = 
  Base.pageSkeleton $ do
    H.head $ do
      H.title "redirecting..."
      H.meta ! A.httpEquiv "refresh" ! A.content (H.toValue $ "1; url=" ++ uri)
    H.body $ do
      H.p "You are being redirected."
      H.p $ do
        void "If your browser does not refresh the page click "
        H.a ! A.href (H.toValue uri) $ "here"