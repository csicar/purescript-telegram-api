module Web.Telegram.API.Bot where

import Prelude

import Control.Monad.Aff (Aff, Milliseconds(Milliseconds), delay)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Control.Monad.Rec.Class (Step(Loop), tailRecM)
import Data.Argonaut (class EncodeJson, JAssoc, JBoolean, Json, encodeJson, jsonEmptyObject, jsonNull, jsonSingletonObject, toString, (:=), (~>))
import Data.Argonaut.Encode ((:=?), (~>?))
import Data.Default (class Default, class PartialDefault, default, withRequired)
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.Foreign (ForeignError, toForeign)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)
import Data.Foreign.Generic as FG
import Data.Foreign.Generic.Types (SumEncoding(..))
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
import Global.Unsafe (unsafeStringify)
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

newtype InlineKeyboardButton = InlineKeyboardButton 
	{ text :: String
	, url :: Maybe String
	, callback_data :: Maybe String
	, switch_inline_query :: Maybe String
	, switch_inline_query_current_chat :: Maybe String
	-- , callback_game :: CallbackGame
	, pay :: Maybe Boolean
	}

derive instance genericInlineKeyboardButton :: Generic InlineKeyboardButton _
derive instance newtypeInlineKeyboardButton :: Newtype InlineKeyboardButton _

instance showInlineKeyboardButton :: Show InlineKeyboardButton where show = genericShow

instance encodeJsonInlineKeyboardButton ∷ EncodeJson InlineKeyboardButton where
  encodeJson (InlineKeyboardButton 
  	{text
	, url
	, callback_data
	, switch_inline_query
	, switch_inline_query_current_chat
	, pay
	}) = 
			"text" := encodeJson text 
		~> "pay" :=? pay
		~>? "url" :=? map encodeJson url
		~>? "callback_data" :=? map encodeJson callback_data
		~>? "switch_inline_query" :=? map encodeJson switch_inline_query
		~>? "switch_inline_query_current_chat" :=? map encodeJson switch_inline_query_current_chat

instance particalDefaultInlineKeyboardButton ∷ PartialDefault InlineKeyboardButton {text :: String} where 
	withRequired {text} = InlineKeyboardButton 
		{ text
		, url : Nothing
		, callback_data : Nothing
		, switch_inline_query : Nothing
		, switch_inline_query_current_chat : Nothing
		, pay : Nothing
		}

data Request = RequestContact | RequestLocation

data KeyboardButton 
	= KeyboardStringButton String
	| KeyboardRequestButton String Request

instance encodeJsonKeyboardButton :: EncodeJson KeyboardButton where
	encodeJson (KeyboardStringButton s) = "text" := s ~> jsonEmptyObject
	encodeJson (KeyboardRequestButton s req) = spy $ "text" := s ~> (attrName req) := true ~> jsonEmptyObject
		where
			attrName RequestContact = "request_contact"
			attrName RequestLocation = "request_location"

data CustomKeyboard 
	= InlineKeyboardMarkup (Array (Array InlineKeyboardButton))
	| ReplyKeyboardMarkup 
		{ keyboard :: Array (Array KeyboardButton)
		, resize_keyboard :: Maybe Boolean		
		, one_time_keyboard :: Maybe Boolean
		, selective :: Maybe Boolean
		}
	| ReplyKeyboardRemove
	| ForceReply

instance encodeJsonCustomKeyboard ∷ EncodeJson CustomKeyboard where
	encodeJson (InlineKeyboardMarkup a) = jsonSingletonObject "inline_keyboard" (encodeJson a)
	encodeJson (ReplyKeyboardMarkup a) =
		"keyboard" :=  a.keyboard
		~> "resize_keyboard" :=? a.resize_keyboard
		~>? "one_time_keyboard" :=? a.one_time_keyboard
		~>? "selective" :=? a.selective
	encodeJson _ = jsonNull -- TODO : complete

newtype SendMessageOptions = SendMessageOptions 
	{ parseMode :: Maybe ParseMode
	, disable_web_page_preview :: Maybe Boolean
	, disable_notifications :: Maybe Boolean
	, reply_to_message_id :: Maybe Int
	, reply_markup :: Maybe CustomKeyboard
	}

derive instance newtypeSendMessageOptions ∷ Newtype SendMessageOptions _

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
	, "reply_markup" /\  map (encodeJson >>> show >>> spy) reply_markup
	]

sendMessage :: ∀e. ApiOptions → SendMessageOptions → ChatId → String → Aff (ajax :: AJAX | e) Json
sendMessage (ApiOptions apiOpts) options chatId message = do
	let options = spy $ encode $ fromArray $ urlEncodedOpts <> ["chat_id" /\ Just (show chatId), "text" /\ Just message]
	res <- get (apiOpts.baseUrl <> apiOpts.token <> "/sendMessage?" <> options)
	pure res.response
	where 
		urlEncodedOpts = urlEncodeSendMessageOptions options

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
derive instance newtypeGetUpdates :: Newtype GetUpdates _

instance showGetUpdates ∷ Show GetUpdates where show = genericShow

instance decodeGetUpdates ∷ Decode GetUpdates where decode = genericDecode $ FG.defaultOptions {unwrapSingleConstructors = true}