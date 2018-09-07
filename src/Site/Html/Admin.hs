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

adminEditDetailPage :: Maybe Resource -> Html
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
        H.td "Content"
        H.td "Published"
        H.td "Publish Date"
        H.td "Title"
        H.td "Lede"
        H.td "Tags"
        H.td "Featured Image"

renderTableRow :: Resource -> Html
renderTableRow item = 
  H.tr $ do
    H.td $ H.a ! A.href (H.toValue (item ^. pid)) $ H.toMarkup (item ^. pid)
    H.td $ H.toMarkup $ show (item ^. resourceType)
    H.td $ H.toMarkup $ show (item ^. contentEncoding)
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
contentEditDetail :: Maybe Resource -> Html
contentEditDetail Nothing = newContentItemForm
contentEditDetail (Just item) = updateContentItem item

-- | Edit Detail helpers
newContentItemForm :: Html
newContentItemForm = undefined


updateContentItem :: Resource -> Html
updateContentItem item = do
  H.div ! A.class_ "edit-head" $
    H.h1 $ (H.toMarkup $ item ^. pid) <> (H.toMarkup . show $ item ^. resourceType)

  H.form ! A.method "post" ! A.action "/admin/item" $
    H.div ! A.class_ "row" $ do
      H.div ! A.class_ "two columns" $ do
        H.label ! A.for "pid" $ "Post ID"
        H.input ! A.type_ "text" ! A.id "pid" ! A.name "_pid" ! A.readonly "true" ! A.value (H.toValue $ item ^. pid)
      
      H.div ! A.class_ "two columns" $
        H.label ! A.for "published" $ do
          "Post ID"
          H.input ! A.type_ "checkbox" ! A.id "published" ! A.name "_published" ! A.value (H.toValue $ item ^. published)
          H.span ! A.class_ "label-body" $ "Publish?"

      H.div ! A.class_ "four columns" $ do
        H.label ! A.for "resourceType" $ "Resource Type"
        H.select ! A.class_ "u-fill-width" ! A.class_ "resourceType" $ do
          H.select ! A.class_ "u-full-width" ! A.id "resourceType" ! A.name "_resourceType" ! A.value (H.toValue . show $ item ^. resourceType) $ do
            H.option ! A.value "BlogPost" $ "BlogPost"
            H.option ! A.value "BlogPost" $ "Project"
            H.option ! A.value "BlogPost" $ "About"

      H.div ! A.class_ "four columns" $ do
        H.label ! A.for "pubdate" $ "Publication Date"
        H.input ! A.type_ "text" ! A.id "pubdate" ! A.name "_pubdate" ! A.value (H.toValue $ item ^. pubdate)
      


  -- H.section ! A.id "contact" ! A.class_ "container contact-us u-full-width u-max-full-width" $

  --   H.div ! A.class_ "eight columns edit-content-form" $
  --     H.form ! A.method "post" ! A.action "/admin/item" $ do
  --       -- _featuredImage
  --       -- _published
  --       H.input ! A.class_ "u-full-width" ! A.type_ "text" ! A.name "_title" ! A.id "titleInput" $ H.toMarkup $ item ^. title
  --       H.input ! A.class_ "u-full-width" ! A.type_ "text" ! A.name "_pubdate" ! A.id "pudateInput" $ H.toMarkup $ item ^. pubdate
  --       H.input ! A.class_ "u-full-width" ! A.type_ "text" !  A.name "_resourceType" ! A.placeholder "Email" ! A.id "resourceTypeInput"
  --       H.textarea ! A.class_ "u-full-width" ! A.name "_body" ! A.id "body" $ H.toMarkup $ item ^. body
  --       H.input ! A.class_ "button u-pull-right" ! A.type_ "submit" !  A.value "Send"
