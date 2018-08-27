module Site.Html.ContentList where

import qualified Data.Text                   as T
import           RIO
import           Text.Blaze.Html             ( Html )
import           Text.Blaze.Html5            ( (!) )
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Site.Html.Base              as Base
import qualified Site.Types                  as Types
  

contentListPage ::  Types.ResourceType -> [Types.Resource] -> Html
contentListPage rtype content = Base.pageSkeleton $ contentListPageBuilder rtype content

contentListPageBuilder :: Types.ResourceType -> [Types.Resource] -> Html
contentListPageBuilder rtype content = do
  makeSectionHead rtype



makeSectionHead :: Types.ResourceType -> Html
makeSectionHead rtype = do
  H.h2 ! A.class_ "section-head" $
    case rtype of
      Types.About -> H.a ! A.href "/about" $ "About"
      Types.BlogPost -> H.a ! A.href "/posts" $ "📖 Blog"
      Types.Project -> H.a ! A.href "/projects" $ "📡 Projects"
