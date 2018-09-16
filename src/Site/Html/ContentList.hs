module Site.Html.ContentList (
  contentListPage
  , renderPaginator
) where

import           Control.Lens
import qualified Data.Text                   as T
import           RIO                         hiding ((^.))
import qualified RIO.HashSet                 as HS
import           Text.Blaze.Html             ( Html )
import           Text.Blaze.Html5            ( (!) )
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Site.Html.Base              as Base
import           Site.Types


contentListPage ::  PageNum -> ResourceType -> AllSiteTags -> [Resource] -> Html
contentListPage pgNum rtype allTags content = 
  Base.pageSkeleton $ contentListPageBuilder pgNum rtype allTags content

contentListPageBuilder :: PageNum -> ResourceType -> AllSiteTags -> [Resource] -> Html
contentListPageBuilder pgNum rtype allTags content = do
  makeSectionHead rtype
  H.div ! A.class_ "row" $ do
    H.div ! A.class_ "eight columns content-list" $ do
      renderContentList content
      renderPaginator pgNum
    renderSearchSection content allTags


makeSectionHead :: ResourceType -> Html
makeSectionHead rtype = do
  H.h2 ! A.class_ "section-head" $
    case rtype of
      About -> H.a ! A.href "/about" $ "About"
      BlogPost -> H.a ! A.href "/posts" $ "📖 Blog"
      Project -> H.a ! A.href "/projects" $ "📡 Projects"


-- | Functions for rendering content
renderContentList :: [Resource] -> Html
renderContentList content = mconcat $ map renderContentListItem content

renderContentListItem :: Resource -> Html
renderContentListItem item =
  H.div ! A.class_ "content-list-item" $ do
    renderContentListItemFeaturedImg item
    renderContentListItemLede item
    renderContentListItemTagList (item ^. tags)
   

renderContentListItemFeaturedImg :: Resource -> Html
renderContentListItemFeaturedImg item = 
  case (item ^. featuredImage) of
    Nothing -> pure ()
    Just img -> H.div ! A.class_ "content-list-item-featured-img" $
      H.img ! A.src (H.toValue img)

renderContentListItemLede :: Resource -> Html
renderContentListItemLede item = 
  H.div ! A.class_ "content-list-item-lede" $ do
    H.span ! A.class_ "content-list-item-date" $ H.toMarkup (item ^. pubdate)
    H.h3 $ H.toMarkup (item ^. title)
    H.p $ H.toMarkup (item ^. lede)
    H.div ! A.class_ "content-list-item-read-more" $
      readMoreLink item


readMoreLink :: Resource -> Html
readMoreLink item = 
  if (item ^. resourceType) == BlogPost
    then H.a ! A.href (H.toValue $ "/posts/" <> item ^. pid) $ "Read More..."
    else H.a ! A.href (H.toValue $ "/projects/" <> item ^. pid) $ "Read More..."

renderContentListItemTagList :: [Text] -> Html
renderContentListItemTagList tags =
  H.div ! A.class_ "content-list-item-tags" $ do
    H.span $ H.i ! A.class_ "fa fa-tag" $ ""
    H.ul $ mconcat (map renderContentListItemTag tags)

renderContentListItemTag :: Text -> Html
renderContentListItemTag tag = H.li ! A.class_ "content-list-item-tag" $ H.toMarkup tag


-- | Auxiliary page content: paginator, search column, etc.
renderPaginator :: PageNum -> Html
renderPaginator (totalPages, currentPageNum) = 
  H.div ! A.class_ "content-list-paginator" $ do
    makeMaybeValidLeftArrow currentPageNum
    mconcat $ map makePaginatorButton [1..currentPageNum - 1]
    H.div ! A.class_ "paginator active" $ H.toMarkup currentPageNum
    mconcat $ map makePaginatorButton [currentPageNum + 1..totalPages]
    makeMaybeValidRightArrow totalPages currentPageNum

makePaginatorButton :: Int -> Html
makePaginatorButton num = H.div ! A.class_ "paginator" $ H.toMarkup num


makeMaybeValidLeftArrow :: Int -> Html
makeMaybeValidLeftArrow currentPageNum = 
  if currentPageNum == 1
    then H.div ! A.class_ "paginator invalid" $ "<"
    else H.div ! A.class_ "paginator valid" $ "<"

makeMaybeValidRightArrow :: PageTotal -> CurrentPage -> Html
makeMaybeValidRightArrow totalPages currentPageNum = 
  if currentPageNum == totalPages
    then H.div ! A.class_ "paginator invalid" $ ">"
    else H.div ! A.class_ "paginator valid" $ ">"


renderSearchSection :: [Resource] -> AllSiteTags -> Html
renderSearchSection content allTags =
  H.div ! A.class_ "four columns" $ do
    H.form ! A.method "post" ! A.action "/search" $
      H.input ! A.class_ "u-full-width" ! A.placeholder "search ekadanta.co" ! A.name "query" ! A.type_ "text"
    H.h3 "Tags"
    H.ul ! A.class_ "content-search-tags" $ renderSearchTags (foldMap _tags content) allTags

renderSearchTags :: [Text] -> AllSiteTags -> Html
renderSearchTags currentTags allTags = do
  let hashTags = HS.fromList allTags
      activeHashTags = HS.fromList currentTags
      inactiveTags = HS.difference hashTags activeHashTags
      makeActiveTag tg = H.li ! A.class_ "active" $ H.toMarkup tg
      makeInactiveTag = H.li . H.toMarkup

  foldMap makeActiveTag (HS.toList activeHashTags)
  foldMap makeInactiveTag (HS.toList inactiveTags)
  
