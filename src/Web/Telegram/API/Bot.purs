module Web.Telegram.API.Bot where

import Prelude

import Control.Monad.Aff (Aff, Milliseconds(Milliseconds), delay)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Control.Monad.Rec.Class (Step(Loop), tailRecM)
import Data.Argonaut (JBoolean, Json, encodeJson)
import Data.Default (class Default, class PartialDefault, default, withRequired)
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.Foreign (ForeignError)
import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (decodeJSON, encodeJSON, genericDecode)
import Data.Foreign.Generic as FG
import Data.FormURLEncoded (encode, fromArray)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (Method(..))
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Debug.Trace (spy)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest, get)
import Type.Data.Boolean (kind Boolean)

type ParsedUpdates = Either (NonEmptyList ForeignError) GetUpdates

newtype ApiOptions = ApiOptions
	{ token ∷ String
	, baseUrl ∷ String
	}

instance defaultApiOptions ∷ PartialDefault ApiOptions { token ∷ String } where
  withRequired {token} = ApiOptions {token, baseUrl}
	where
		baseUrl = "https://api.telegram.org/bot"

pollUpdates :: ∀acc. ApiOptions → (acc → ParsedUpdates → Aff _ acc) → acc → Aff _ Unit
pollUpdates options onUpdate i = 
	tailRecM f (i /\ 0)
	where
		f (acc /\ updateId) = do
			updates <- getUpdates options updateId
			logShow updates
			delay (Milliseconds 5000.0)
			ret <- onUpdate acc updates
			let newUpdateId = map (_ + 1) $ maybeGetUpdateId updates
			pure $ Loop (ret /\ fromMaybe updateId newUpdateId)

		maybeGetUpdateId :: ParsedUpdates → Maybe Int
		maybeGetUpdateId (Right (GetUpdates updates)) = maximum $ map (unwrap >>> _.update_id) updates.result
		maybeGetUpdateId (Left _) = Nothing

getUpdates :: ∀e. ApiOptions → Int → Aff (ajax :: AJAX, console :: CONSOLE | e) ParsedUpdates
getUpdates (ApiOptions opt) offset = do
	let options = encode $ fromArray ["offset" /\ Just (show offset)]
	res <- affjax $ defaultRequest { url = spy $ opt.baseUrl <> opt.token <> "/getUpdates?" <> options, method = Left GET }
	let parsed = parseMessage res.response
	liftEff $ log res.response
	pure parsed

data ParseMode = Html | Markdown

urlEncodeParseMode :: ParseMode → String
urlEncodeParseMode Html = "HTML"
urlEncodeParseMode Markdown = "Markdown"

data CustomKeyboard 
	= InlineKeyboardMarkup
	| ReplyKeyboardMarkup
	| ReplyKeyboardRemove
	| ForceReply

newtype SendMessageOptions = SendMessageOptions 
	{ parseMode :: Maybe ParseMode
	, disable_web_page_preview :: Maybe Boolean
	, disable_notifications :: Maybe Boolean
	, reply_to_message_id :: Maybe Int
	, reply_markup :: Maybe CustomKeyboard
	}

instance sendMessageOptions :: Default SendMessageOptions where
	default = SendMessageOptions 
		{ parseMode : Nothing
		, disable_web_page_preview : Nothing
		, disable_notifications : Nothing
		, reply_to_message_id : Nothing
		, reply_markup : Nothing
		}

urlEncodeSendMessageOptions :: SendMessageOptions → Array (String /\ Maybe String)
urlEncodeSendMessageOptions (SendMessageOptions {parseMode, disable_web_page_preview, disable_notifications, reply_to_message_id, reply_markup}) = 
	[ "parseMode" /\ map urlEncodeParseMode parseMode
	, "disable_web_page_preview" /\ map show disable_web_page_preview
	, "disable_notifications" /\ map show disable_notifications
	, "reply_to_message_id" /\ map show reply_to_message_id
	, "reply_markup" /\ Nothing -- TODO: correct value
	]

sendMessage :: ∀e. ApiOptions → (SendMessageOptions → SendMessageOptions) → ChatId → String → Aff (ajax :: AJAX | e) Json
sendMessage (ApiOptions apiOpts) optionate chatId message = do
	let options = encode $ fromArray $ urlEncodedOpts <> ["chat_id" /\ Just (show chatId), "text" /\ Just message]
	res <- get (apiOpts.baseUrl <> apiOpts.token <> "/sendMessage?" <> options)
	pure res.response
	where 
		sendOpts = optionate default
		urlEncodedOpts = urlEncodeSendMessageOptions sendOpts

parseMessage :: String → Either (NonEmptyList ForeignError) GetUpdates
parseMessage msg = runExcept $ decodeJSON msg

type ChatId = Int

-- | [User Object](https://core.telegram.org/bots/api#user)
newtype User = User
	{ id :: Int
	, is_bot :: Boolean
	, first_name :: Maybe String
	, last_name :: Maybe String
	, username :: Maybe String
	, language_code :: Maybe String
	}

derive instance genericUser :: Generic User _

instance decodeUser ∷ Decode User where decode = genericDecode $ FG.defaultOptions {unwrapSingleConstructors = true} 

instance showUser ∷ Show User where show = genericShow

newtype MessageEntity = MessageEntity
	{ offset :: Int
	, length :: Int
	, type :: String
	, url :: Maybe String
	, user :: Maybe User
	}

derive instance genericMessageEntity :: Generic MessageEntity _

instance showMessageEntity ∷ Show MessageEntity where show = genericShow

instance decodeMessageEntity :: Decode MessageEntity where decode = genericDecode $ FG.defaultOptions {unwrapSingleConstructors = true}

-- | Chat for [Chat Object](https://core.telegram.org/bots/api#chat)
newtype Chat = Chat
	{ id :: Int
	, type :: String
	, title :: Maybe String
	, username :: Maybe String
	, first_name :: Maybe String
	, last_name :: Maybe String
	, all_members_are_administrators :: Maybe Boolean
	, description :: Maybe String
	, invite_link :: Maybe String
	, pinned_message :: Maybe Message
	, sticker_set_name :: Maybe String
	, can_set_sticker_set :: Maybe Boolean
	}

derive instance genericChat :: Generic Chat _

instance showChat ∷ Show Chat where show = genericShow

instance decodeChat ∷ Decode Chat where decode = genericDecode $ FG.defaultOptions {unwrapSingleConstructors = true} 

-- | Message for [Message Object](https://core.telegram.org/bots/api#message)
newtype Message = Message
	{ message_id :: Int
	, from :: Maybe User
	, date :: Int
	, chat :: Chat
	, text :: Maybe String	
	, forward_from :: Maybe User
	, forward_from_chat :: Maybe Chat
	, forward_from_message_id :: Maybe Int
	, forward_signature :: Maybe String
	, forward_date :: Maybe Int
	, reply_to_message :: Maybe Message
	, edit_date :: Maybe Int
	, media_group_id :: Maybe String
	, author_signature :: Maybe String
	, entities :: Maybe (Array MessageEntity)
	, caption_entities :: Maybe (Array MessageEntity)
	, caption :: Maybe String
	, new_chat_members :: Maybe (Array User)
	, left_chat_member :: Maybe User
	, new_chat_title :: Maybe String
	, delete_chat_photo :: Maybe Boolean
	, group_chat_created :: Maybe Boolean
	, supergroup_chat_created :: Maybe Boolean
	, channel_chat_created :: Maybe Boolean
	, migrate_to_chat_id :: Maybe Int
	, migrate_from_chat_id :: Maybe Int
	, pinned_message :: Maybe Message
	, connected_website :: Maybe String
	}

derive instance genericMessage :: Generic Message _
derive instance newtypeMessage ∷ Newtype Message _

instance showMessage ∷ Show Message where show x = genericShow x

instance decodeMessage ∷ Decode Message where decode x = genericDecode (FG.defaultOptions {unwrapSingleConstructors = true}) x

-- | Update for [Update Object](https://core.telegram.org/bots/api#update)
newtype Update = Update 
	{ update_id :: Int
	, message :: Maybe Message
	, edited_message :: Maybe Message
	, channel_post :: Maybe Message
	, edited_channel_post :: Maybe Message
	}

derive instance genericUpdate :: Generic Update _
derive instance newtypeUpdate :: Newtype Update _

instance showUpdate ∷ Show Update where show = genericShow

instance decodeUpdate ∷ Decode Update where decode = genericDecode $ FG.defaultOptions {unwrapSingleConstructors = true} 

newtype GetUpdates = GetUpdates
	{ ok :: Boolean
	, result :: Array Update
	}

derive instance genericGetUpdates :: Generic GetUpdates _

instance showGetUpdates ∷ Show GetUpdates where show = genericShow

instance decodeGetUpdates ∷ Decode GetUpdates where decode = genericDecode $ FG.defaultOptions {unwrapSingleConstructors = true}