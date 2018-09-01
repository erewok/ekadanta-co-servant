module Site.Html.Admin where

import qualified Data.Text                   as T
import           RIO
import           Text.Blaze.Html             ( Html )
import           Text.Blaze.Html5            ( (!), Attribute )
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Site.Html.Base              as Base
import           Site.Html.ContentList       ( renderPaginator )
import           Site.Types


adminEditListPage :: PageNum -> [Resource] -> Html
adminEditListPage pgNum content = Base.pageSkeleton $ contentEditList pgNum content

adminEditDetailPage :: Resource -> Html
adminEditDetailPage item = Base.pageSkeleton $ contentEditDetail item




-- | Content Edit List: Show all Resources for editing
contentEditList :: PageNum -> [Resource] -> Html
contentEditList pgNum content = do
  H.div ! A.class_ "edit-head" $
    H.h1 "Admin"
  renderPaginator pgNum
  H.table ! A.class_ "u-full-width" $ do
    renderTableHead
    H.tbody $ mconcat (map renderTableRow content)

-- | Edit List helpers
renderTableHead :: Html
renderTableHead =
  H.thead $
      H.tr $ do
        H.td "Id"
        H.td "Type"
        H.td "Published"
        H.td "Publish Date"
        H.td "Title"
        H.td "Lede"
        H.td "Tags"
        H.td "Featured Image"

renderTableRow :: Resource -> Html
renderTableRow item = 
  H.tr $ do
    H.td $ H.toMarkup (item ^. pid)
    H.td $ H.toMarkup $ show (item ^. resourceType)
    H.td $ renderPublishedCheck (item ^. published)
    H.td $ H.toMarkup (item ^. pubdate)
    H.td $ H.toMarkup (item ^. title)
    H.td $ H.toMarkup (item ^. lede)
    H.td $ renderTagList (item ^. tags)
    H.td $ renderFeaturedImg (item ^. featuredImage)

renderPublishedCheck :: Bool -> Html
renderPublishedCheck False = "❌"
renderPublishedCheck True = "✅"

renderTagList :: [Text] -> Html
renderTagList = H.toMarkup . (T.intercalate ", ")

renderFeaturedImg :: Maybe Text -> Html
renderFeaturedImg Nothing = ""
renderFeaturedImg (Just img) = H.img ! A.src (H.toValue img)


-- | Content Edit Detail: Edit a particular item
contentEditDetail :: Resource -> Html
contentEditDetail = undefined

-- | Edit Detail helpers
