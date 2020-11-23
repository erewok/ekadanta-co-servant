module Site.Html.Contact where

import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as L
import           GHC.Generics
import           Network.Mail.SMTP
import           Network.Mail.Mime           (Address(..))
import           RIO
import           Text.Blaze.Html             ( Html )
import           Text.Blaze.Html5            ( (!), Attribute )
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Web.FormUrlEncoded                (Form(..)
                                                  , FromForm(..)
                                                  , ToForm(..))
import           Web.Internal.HttpApiData          (FromHttpApiData(..)
                                                  , toQueryParam)
import           Web.Internal.FormUrlEncoded       (parseUnique)

import qualified Site.Html.Base              as Base
import           Site.Config


contactPage  :: Bool -> Html
contactPage firstTime = 
  Base.pageSkeleton $
    H.div ! A.class_ "row contact-content" $ do
      renderFindMeLeftCol
      renderGetInTouchRightCol firstTime


contactForm :: Html
contactForm = 
  H.section ! A.id "contact" ! A.class_ "container contact-us u-full-width u-max-full-width" $
    H.form ! A.method "post" ! A.action "/contact" $ do
      H.input ! A.class_ "u-full-width" ! A.type_ "text" ! A.name "cname" ! A.placeholder "Name" ! A.id "nameInput"
      H.input ! A.class_ "u-full-width" ! A.type_ "text" !  A.name "cemail" ! A.placeholder "Email" ! A.id "emailInput"
      H.textarea ! A.class_ "u-full-width" ! A.name "cmessage" ! A.placeholder "Message" ! A.id "messageInput" $ ""
      H.input ! A.class_ "button u-pull-right" ! A.type_ "submit" ! A.value "Send"


renderFindMeLeftCol :: Html
renderFindMeLeftCol = 
  H.ul ! A.class_ "content-detail-body-contact-links" $ do
    H.li $ H.i ! A.class_ "fa fa-github" $ "github.com/erewok"
    H.li $ H.i ! A.class_ "fa fa-linkedin" $ "github.com/erewok"
    H.li $ H.i ! A.class_ "fa fa-twitter" $ "github.com/erewok"

renderGetInTouchRightCol :: Bool -> Html
renderGetInTouchRightCol firstTime = 
  H.div ! A.id "contact-page" $
      H.div ! A.class_ "contact-page-box" $ do
        let formOrError = if firstTime then contactForm else H.p "Please fill in all fields"
        formOrError


data ContactForm = ContactForm
  { cname    :: !T.Text
  , cemail   :: !T.Text
  , cmessage :: !T.Text
  } deriving (Eq, Show, Generic)

instance FromForm ContactForm


thanksPage :: Html
thanksPage = Base.pageSkeleton $
      H.div ! A.class_ "row" $
        H.div ! A.id "thanks-page" ! A.class_ "container" $
          H.div ! A.class_ "contact-page-box" $
            H.div ! A.class_ "u-full-width" $ do
              H.h2 "Thanks"
              H.p "We'll get back to you soon."

constructSubject :: ContactForm -> T.Text
constructSubject form = "New message from " `mappend` cname form

constructBody :: ContactForm -> L.Text
constructBody form = L.fromStrict . T.unlines $ [
  "From: " `mappend` cname form
  , "\nEmail: " `mappend` cemail form
  ,  "\n\n"
  , cmessage form]

sendContact :: SiteConfig -> ContactForm -> IO ()
sendContact config contact = do
  let 
    addressTrunk = Address (Just "Erik Aker")
    from = addressTrunk $ emailUsername config
    to = [addressTrunk $ emailUsername config]
    cc = []
    bcc = []
    timeout = 6000000
    subject = constructSubject contact
    body = plainTextPart $ constructBody contact
    html = htmlPart ""
    mail = simpleMail from to cc bcc subject [body, html]    
  sendMailWithLogin 
    "smtp.gmail.com"
        (T.unpack $ emailUsername config)
          (T.unpack $ emailPasswd config)
            mail
