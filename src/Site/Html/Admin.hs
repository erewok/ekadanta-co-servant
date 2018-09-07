module Site.Html.Admin (
  adminEditListPage
  , adminEditDetailPage
) where

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
        H.td "Encoding"
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
newContentItemForm = do
  H.div ! A.class_ "edit-head" $
    H.h1 "New Content Item"

  H.form ! A.method "post" ! A.action "/admin/item" $ do
    H.div ! A.class_ "row" $ do
      H.div ! A.class_ "two columns" $ do
        H.label ! A.for "pid" $ "Post ID"
        H.input ! A.type_ "text" ! A.id "pid" ! A.name "_pid" ! A.readonly "true"
      
      H.div ! A.class_ "two columns" $
        H.label ! A.for "published" $ do
          "Post ID"
          H.input ! A.type_ "checkbox" ! A.id "published" ! A.name "_published"
          H.span ! A.class_ "label-body" $ "Publish?"

      H.div ! A.class_ "two columns" $ do
        H.label ! A.for "resourceType" $ "Resource Type"
        H.select ! A.class_ "u-fill-width" ! A.class_ "resourceType" $ do
          H.select ! A.class_ "u-full-width" ! A.id "resourceType" ! A.name "_resourceType" $ do
            H.option ! A.value "blogpost" $ "BlogPost"
            H.option ! A.value "project" $ "Project"
            H.option ! A.value "about" $ "About"

      H.div ! A.class_ "two columns" $ do
        H.label ! A.for "contentEncoding" $ "Resource Type"
        H.select ! A.class_ "u-fill-width" ! A.class_ "contentEncoding" $ do
          H.select ! A.class_ "u-full-width" 
            ! A.id "contentEncoding" ! A.name "_contentEncoding" $ do
              H.option ! A.value "html" $ "Html"
              H.option ! A.value "markdown" $ "Markdown"

      H.div ! A.class_ "two columns" $ do
        H.label ! A.for "pubdate" $ "Publication Date"
        H.input ! A.type_ "text" 
          ! A.id "pubdate" ! A.name "_pubdate"

    H.label ! A.for "featuredImage" $ "Featured Image"
    H.input ! A.type_ "text" 
      ! A.id "featuredImage" ! A.name "_featuredImage" 
      ! A.placeholder "Featured Image URL" ! A.class_ "u-full-width"

    H.label ! A.for "title" $ "Title"
    H.input ! A.type_ "text" 
      ! A.id "title" ! A.name "_title" 
      ! A.placeholder "Sample post title" ! A.class_ "u-full-width" 

    H.label ! A.for "lede" $ "Lede"
    H.textarea 
      ! A.id "lede" ! A.name "_lede" 
      ! A.placeholder "Lede" ! A.class_ "u-full-width" 
      $ ""

    H.label ! A.for "body" $ "Body"
    H.textarea 
      ! A.id "body" ! A.name "_body" 
      ! A.placeholder "body" ! A.class_ "u-full-width"
      $ ""
      
    H.label ! A.for "tags" $ "Tags"
    H.input ! A.type_ "text" ! A.id "tags" 
      ! A.name "_tags" ! A.placeholder "Haskell, Rust, etc." 
      ! A.class_ "u-full-width" 


    H.input ! A.class_ "button-primary" ! A.type_ "submit" !  A.value "Send"


updateContentItem :: Resource -> Html
updateContentItem item = do
  H.div ! A.class_ "edit-head" $
    H.h1 $ (H.toMarkup $ item ^. pid) <> (H.toMarkup . show $ item ^. resourceType)

  H.form ! A.method "post" ! A.action "/admin/item" $ do
    H.div ! A.class_ "row" $ do
      H.div ! A.class_ "two columns" $ do
        H.label ! A.for "pid" $ "Post ID"
        H.input ! A.type_ "text" ! A.id "pid" ! A.name "_pid" ! A.readonly "true" ! A.value (H.toValue $ item ^. pid)
      
      H.div ! A.class_ "two columns" $
        H.label ! A.for "published" $ do
          "Post ID"
          H.input ! A.type_ "checkbox" ! A.id "published" ! A.name "_published" ! A.value (H.toValue $ item ^. published)
          H.span ! A.class_ "label-body" $ "Publish?"

      H.div ! A.class_ "two columns" $ do
        H.label ! A.for "resourceType" $ "Resource Type"
        H.select ! A.class_ "u-fill-width" ! A.class_ "resourceType" $ do
          H.select ! A.class_ "u-full-width" ! A.id "resourceType" ! A.name "_resourceType" ! A.value (H.toValue . show $ item ^. resourceType) $ do
            H.option ! A.value "blogpost" $ "BlogPost"
            H.option ! A.value "project" $ "Project"
            H.option ! A.value "about" $ "About"

      H.div ! A.class_ "two columns" $ do
        H.label ! A.for "contentEncoding" $ "Resource Type"
        H.select ! A.class_ "u-fill-width" ! A.class_ "contentEncoding" $ do
          H.select ! A.class_ "u-full-width" 
            ! A.id "contentEncoding" ! A.name "_contentEncoding" 
            ! A.value (H.toValue . show $ item ^. contentEncoding) $ do
              H.option ! A.value "html" $ "Html"
              H.option ! A.value "markdown" $ "Markdown"

      H.div ! A.class_ "two columns" $ do
        H.label ! A.for "pubdate" $ "Publication Date"
        H.input ! A.type_ "text" 
          ! A.id "pubdate" ! A.name "_pubdate" 
          ! A.value (H.toValue $ item ^. pubdate)

    H.label ! A.for "featuredImage" $ "Featured Image"
    H.input ! A.type_ "text" 
      ! A.id "featuredImage" ! A.name "_featuredImage" 
      ! A.placeholder "Featured Image URL" ! A.class_ "u-full-width" 
      ! A.value (H.toValue $ maybe "" id (item ^. featuredImage))

    H.label ! A.for "title" $ "Title"
    H.input ! A.type_ "text" 
      ! A.id "title" ! A.name "_title" 
      ! A.placeholder "Sample post title" ! A.class_ "u-full-width" 
      ! A.value (H.toValue $ item ^. title)

    H.label ! A.for "lede" $ "Lede"
    H.textarea 
      ! A.id "lede" ! A.name "_lede" 
      ! A.placeholder "Lede" ! A.class_ "u-full-width" $ 
        H.toMarkup $ item ^. lede

    H.label ! A.for "body" $ "Body"
    H.textarea 
      ! A.id "body" ! A.name "_body" 
      ! A.placeholder "body" ! A.class_ "u-full-width" $ 
        H.toMarkup $ item ^. body

    H.label ! A.for "tags" $ "Tags"
    H.input ! A.type_ "text" ! A.id "tags" 
      ! A.name "_tags" ! A.placeholder "Haskell, Rust, etc." 
      ! A.class_ "u-full-width" 
      ! A.value (H.toValue . T.intercalate ", " $ item ^. tags)


    H.input ! A.class_ "button-primary" ! A.type_ "submit" !  A.value "Send"
