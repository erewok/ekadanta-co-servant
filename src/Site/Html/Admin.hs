module Site.Html.Admin (
  adminEditListPage
  , adminEditDetailPage
  , adminLoginPage
  , redirectPage
  , LoginForm(..)
) where

import qualified Data.Text                   as T
import qualified Data.UUID                   as UUID
import           RIO
import           Text.Blaze.Html                ( Html )
import           Text.Blaze.Html5               ( (!), Attribute )
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Web.FormUrlEncoded             (Form(..)
                                                , FromForm(..))

import qualified Site.Html.Base              as Base
import           Site.Html.ContentList          ( renderPaginator )
import           Site.Types


adminEditListPage :: PageNum -> [Resource] -> Html
adminEditListPage pgNum content = Base.pageSkeleton $ contentEditList pgNum content

adminEditDetailPage :: Maybe Resource -> Html
adminEditDetailPage item = Base.pageSkeleton $ contentEditDetail item

adminLoginPage :: Html
adminLoginPage = Base.pageSkeleton $ loginForm

redirectPage :: String -> H.Html
redirectPage uri = H.docTypeHtml $ do
  H.head $ do
    H.title "redirecting..."
    H.meta ! A.httpEquiv "refresh" ! A.content (H.toValue $ "1; url=" ++ uri)
  H.body $ do
    H.p "You are being redirected."
    H.p $
      void "If your browser does not refresh the page click "
  H.a ! A.href (H.toValue uri) $ "here"

-- | Content Edit List: Show all Resources for editing
contentEditList :: PageNum -> [Resource] -> Html
contentEditList pgNum content = do
  H.div ! A.class_ "edit-head" $ do
    H.div ! A.class_ "edit-head-header" $
      H.h1 "Admin"
    H.div ! A.class_ "new-post-link" $ H.a ! A.href "/admin/item" $ "Create Item"
    renderPaginator pgNum makeAdminPaginatorButton
  H.table ! A.class_ "u-full-width" $ do
    renderTableHead
    H.tbody $ mconcat (map renderTableRow content)

makeAdminPaginatorButton :: Int -> Html
makeAdminPaginatorButton num = H.div ! A.class_ "paginator" $
  H.a ! A.href (H.toValue $ "/admin/" <> show num) $ H.toMarkup num
    
            

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
    H.td $ H.a ! A.href (H.toValue ("/admin/item/" <> (item ^. pid))) $ H.toMarkup (item ^. pid)
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
renderFeaturedImg (Just img) = H.img ! A.src (H.toValue img) ! A.width "100px"


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
        H.input ! A.type_ "text" ! A.id "pid" ! A.name "_pid" ! A.readonly "true" ! A.value (H.toValue . UUID.toText $ UUID.nil)
      
      H.div ! A.class_ "two columns" ! A.class_ "publishedCheck" $ do
        H.label ! A.for "published" $ "Published"
        H.select ! A.class_ "u-fill-width" ! A.class_ "published" ! A.name "_published" $ do
          H.option ! A.value "True" $ "Yes"
          H.option ! A.value "False" $ "No"

      H.div ! A.class_ "two columns" $ do
        H.label ! A.for "resourceType" $ "Resource Type"
        H.select ! A.class_ "u-fill-width" ! A.class_ "resourceType" ! A.name "_resourceType" $ do
          H.option ! A.value "blogpost" $ "BlogPost"
          H.option ! A.value "project" $ "Project"
          H.option ! A.value "about" $ "About"

      H.div ! A.class_ "two columns" $ do
        H.label ! A.for "contentEncoding" $ "Resource Type"
        H.select ! A.class_ "u-fill-width" ! A.class_ "contentEncoding" ! A.name "_contentEncoding" $ do
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
      ! A.id "lede" ! A.name "_lede" ! A.rows "30"
      ! A.placeholder "Lede" ! A.class_ "u-full-width" 
      $ ""

    H.label ! A.for "body" $ "Body"
    H.textarea 
      ! A.id "body" ! A.name "_body" ! A.rows "60"
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

  H.form ! A.method "post" ! A.action (H.toValue ("/admin/item/" <> (item ^. pid))) $ do
    H.div ! A.class_ "row" $ do
      H.div ! A.class_ "two columns" $ do
        H.label ! A.for "pid" $ "Post ID"
        H.input ! A.type_ "text" ! A.id "pid" ! A.name "_pid" ! A.readonly "true" ! A.value (H.toValue $ item ^. pid)
      
      H.div ! A.class_ "two columns" ! A.class_ "publishedCheck" $ do
        H.label ! A.for "published" $ "Published"
        H.select ! A.class_ "u-fill-width" ! A.class_ "published" ! A.name "_published" $ do
          H.option ! A.value "True" $ "Yes"
          H.option ! A.value "False" $ "No"

      H.div ! A.class_ "two columns" $ do
        H.label ! A.for "resourceType" $ "Resource Type"
        H.select ! A.class_ "u-fill-width" ! A.class_ "resourceType" ! A.name "_resourceType" $ do
          H.option ! A.value "blogpost" $ "BlogPost"
          H.option ! A.value "project" $ "Project"
          H.option ! A.value "about" $ "About"

      H.div ! A.class_ "two columns" $ do
        H.label ! A.for "contentEncoding" $ "Resource Type"
        H.select ! A.class_ "u-fill-width" ! A.class_ "contentEncoding" ! A.name "_contentEncoding" $ do
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
      ! A.id "lede" ! A.name "_lede" ! A.rows "30"
      ! A.placeholder "Lede" ! A.class_ "u-full-width" $ 
        H.toMarkup $ item ^. lede

    H.label ! A.for "body" $ "Body"
    H.textarea 
      ! A.id "body" ! A.name "_body" ! A.rows "60"
      ! A.placeholder "body" ! A.class_ "u-full-width" $ 
        H.toMarkup $ item ^. body

    H.label ! A.for "tags" $ "Tags"
    H.input ! A.type_ "text" ! A.id "tags" 
      ! A.name "_tags" ! A.placeholder "Haskell, Rust, etc." 
      ! A.class_ "u-full-width" 
      ! A.value (H.toValue . T.intercalate ", " $ item ^. tags)

    H.input ! A.class_ "button-primary" ! A.type_ "submit" !  A.value "Send"


-- | Login Form and Renderings

data LoginForm = LoginForm { username :: Text, password :: Text }
   deriving (Eq, Show, Read, Generic)

instance FromForm LoginForm


loginForm :: Html
loginForm = 
  H.section ! A.id "login" ! A.class_ "container login u-full-width u-max-full-width" $
    H.form ! A.method "post" ! A.action "/login" $ do
      H.input ! A.class_ "u-full-width" ! A.type_ "text" ! A.name "username" ! A.placeholder "Name" ! A.id "nameInput"
      H.input ! A.class_ "u-full-width" ! A.type_ "password" !  A.name "password" ! A.placeholder "Password" ! A.id "emailInput"
      H.input ! A.class_ "button u-pull-right" ! A.type_ "submit" ! A.value "Submit"
