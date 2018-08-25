module Site.Html.Home where

import qualified Data.Text                   as T
import           RIO
import           Text.Blaze.Html             ( Html )
import           Text.Blaze.Html5            ( (!) )
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Site.Types as Types


homeAboutSection :: Html -> Html
homeAboutSection homeAboutContent = 
    H.section ! A.class_ "home-about home-section" $
        homeAboutContent

homeProjectsSection :: [Types.Resource] -> Html
homeProjectsSection projects = 
    H.section ! A.class_ "home-section" $ do
        H.h2 ! A.class_ "section-head" $ "Projects"
        H.section ! A.class_ "home-projects" $ do
            mconcat $ fmap (uncurry renderHomeProject) (zip [1..] projects)


renderHomeProject :: Int -> Types.Resource -> Html
renderHomeProject projectNum project = 
    H.div ! A.class_ (H.toValue (T.unpack "home-project" <> show projectNum)) $ do

        homeProjectTagList project

        H.div ! A.class_ "home-project-title" $
            H.h3 (H.toMarkup $ project ^. Types.title)
        H.div ! A.class_ "home-project-text" $
            H.h3 (H.toMarkup $ project ^. Types.body)
        H.div ! A.class_ "home-project-button" $
            H.a ! A.class_ "button" ! A.href (H.toValue ("/projects/" <> project ^. Types.pid)) $
                "View Project"

homeProjectTagList :: Types.Resource -> Html
homeProjectTagList project = 
    H.div ! A.class_ "home-project-tag-list" $
        H.ul ! A.class_ "home-project-tags" $ do
            mconcat $ fmap (H.li . H.toMarkup) (project ^. Types.tags)
