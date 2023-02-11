module Site.Html.ContentDetail (
  contentDetailPage
) where

import qualified CMarkGFM                    as Mark
import qualified Data.Text                   as T
import           RIO
import           Text.Blaze.Html             ( Html )
import           Text.Blaze.Html5            ( (!), Attribute )
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Site.Html.Base              as Base
import qualified Site.Types                  as Types


contentDetailPage :: Types.Resource -> Html
contentDetailPage post = Base.pageSkeleton $ contentDetailPageBuilder post

contentDetailPageBuilder :: Types.Resource -> Html
contentDetailPageBuilder post = do
  makeBreadCrumbs (post ^. Types.resourceType)
  renderDetailHead (post ^. Types.pubdate)
  renderDetailContent post

makeBreadCrumbs :: Types.ResourceType -> Html
makeBreadCrumbs Types.BlogPost = do
  breadCrumber $ H.a ! A.href "/posts" $ "📖 Blog"
  breadCrumber ">"
  breadCrumber "Blog Post"
makeBreadCrumbs Types.About = breadCrumber "About"
makeBreadCrumbs Types.Project = do
  breadCrumber $ H.a ! A.href "/projects" $ "📡 Projects"
  breadCrumber ">"
  breadCrumber "Project"

breadCrumber :: Html -> Html
breadCrumber = H.span ! A.class_ "breadcrumbs"


-- | Detail Page Head with Date and Contact
type PubDate = Text

renderDetailHead :: PubDate -> Html
renderDetailHead pubDate =
  H.div ! A.class_ "content-detail-head" $ do
    H.span ! A.class_ "content-detail-date-head" $ H.toMarkup pubDate

-- | Detail Page Content
renderDetailContent :: Types.Resource -> Html
renderDetailContent post = do
  maybeInsertFeaturedImg (post ^. Types.featuredImage)
  H.div ! A.class_ "content-detail-content" $ do

    H.div ! A.class_ "content-detail-body-head" $ do
      H.h3 ! A.class_ "content-detail-title" $ H.toMarkup (post ^. Types.title)
      H.span ! A.class_ "content-detail-lede" $ H.toMarkup (post ^. Types.lede)

    H.div ! A.class_ "content-detail-body-content" $ renderBodyAsHtmlOrMarkdown post

renderBodyAsHtmlOrMarkdown :: Types.Resource -> Html
renderBodyAsHtmlOrMarkdown post =
  if (post ^. Types.contentEncoding) == Types.ContentMarkdown
    then  H.preEscapedToHtml (Mark.commonmarkToHtml [Mark.optHardBreaks] [Mark.extStrikethrough] $ post ^. Types.body)
    else H.preEscapedToHtml (post ^. Types.body)


type FeaturedImg = Maybe Text
maybeInsertFeaturedImg ::  FeaturedImg -> Html
maybeInsertFeaturedImg Nothing = pure ()
maybeInsertFeaturedImg (Just img) = H.img ! A.class_ "content-detail-featured-img" ! A.src (H.toValue img)
