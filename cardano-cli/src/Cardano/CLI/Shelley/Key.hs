{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Shelley CLI option data types and functions for cryptographic keys.
module Cardano.CLI.Shelley.Key
  ( KeyFormat (..)
  , KeyDecodeError (..)
  , deserialiseKey
  , deserialiseKeyAnyOf
  , readKeyFile
  , readKeyFileAnyOf
  , readKeyFileTextEnvelope
  , renderKeyDecodeError

  , readSigningKeyFile
  , readSigningKeyFileAnyOf

  , VerificationKeyOrFile (..)
  , readVerificationKeyOrFile
  , readVerificationKeyOrTextEnvFile

  , VerificationKeyTextOrFile (..)
  , VerificationKeyTextOrFileError (..)
  , readVerificationKeyTextOrFileAnyOf
  , renderVerificationKeyTextOrFileError

  , VerificationKeyOrHashOrFile (..)
  , readVerificationKeyOrHashOrFile
  , readVerificationKeyOrHashOrTextEnvFile
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.Api.TextView (TextViewError (..))
import           Cardano.Api.Typed

import           Cardano.CLI.Types

------------------------------------------------------------------------------
-- Cryptographic key deserialisation
------------------------------------------------------------------------------

-- | Cryptographic key format/encoding.
data KeyFormat
  = KeyFormatBech32
  -- ^ Bech32 encoding.
  | KeyFormatHex
  -- ^ Hex/Base16 encoding.
  | KeyFormatTextEnvelope
  -- ^ Text envelope format.
  deriving (Eq, Show)

-- | Cryptographic key decoding error.
data KeyDecodeError
  = KeyTextEnvelopeError !TextEnvelopeError
  -- ^ The provided data seems to be a valid text envelope, but some error
  -- occurred in extracting a valid cryptographic key from it.
  | KeyBech32DecodeError !Bech32DecodeError
  -- ^ The provided data is valid Bech32, but some error occurred in
  -- deserializing it to a cryptographic key.
  | KeyInvalidError
  -- ^ The provided data does not represent a valid cryptographic key.
  deriving (Eq, Show)

instance Error KeyDecodeError where
  displayError = Text.unpack . renderKeyDecodeError

-- | Render an error message for a 'KeyDecodeError'.
renderKeyDecodeError :: KeyDecodeError -> Text
renderKeyDecodeError err =
  case err of
    KeyTextEnvelopeError textEnvErr ->
      Text.pack (displayError textEnvErr)
    KeyBech32DecodeError decodeErr ->
      Text.pack (displayError decodeErr)
    KeyInvalidError -> "Invalid key."

-- | The result of a key deserialisation function.
--
-- Note that this type isn't intended to be exported, but only used as a
-- helper within the 'deserialiseKey' function.
data DeserialiseKeyResult a
  = DeserialiseKeySuccess !a
  -- ^ Key successfully deserialised.
  | DeserialiseKeyError !KeyDecodeError
  -- ^ The provided data is of the expected format/encoding, but an error
  -- occurred in deserializing it to a cryptographic key.
  | DeserialiseKeyErrorFormatMismatch
  -- ^ The provided data's formatting/encoding does not match that which was
  -- expected. This error is an indication that one could attempt to
  -- deserialise the key again, but instead expecting a different format.

-- | Deserialise a cryptographic key of some type that is formatted in some
-- way.
deserialiseKey
  :: forall a. (HasTextEnvelope a, SerialiseAsBech32 a)
  => AsType a
  -> NonEmpty KeyFormat
  -> ByteString
  -> Either KeyDecodeError a
deserialiseKey asType acceptedFormats keyBs =
    go (NE.toList acceptedFormats)
  where
    keyText :: Text
    keyText = Text.decodeUtf8 keyBs

    go :: [KeyFormat] -> Either KeyDecodeError a
    go [] = Left KeyInvalidError
    go (kf:kfs) =
      let res =
            case kf of
              KeyFormatBech32 -> deserialiseBech32
              KeyFormatHex -> deserialiseHex
              KeyFormatTextEnvelope -> deserialiseTextEnvelope
      in case res of
        DeserialiseKeySuccess a -> Right a
        DeserialiseKeyError err -> Left err
        DeserialiseKeyErrorFormatMismatch -> go kfs

    deserialiseTextEnvelope :: DeserialiseKeyResult a
    deserialiseTextEnvelope = do
      let textEnvRes :: Either TextEnvelopeError a
          textEnvRes =
            deserialiseFromTextEnvelope asType
              =<< first TextViewAesonDecodeError (Aeson.eitherDecodeStrict' keyBs)
      case textEnvRes of
        Right res -> DeserialiseKeySuccess res

        -- The input was valid a text envelope, but there was a type mismatch
        -- error.
        Left err@(TextViewTypeError _ _) ->
          DeserialiseKeyError (KeyTextEnvelopeError err)

        -- The input was not valid a text envelope.
        Left _ -> DeserialiseKeyErrorFormatMismatch

    deserialiseBech32 :: DeserialiseKeyResult a
    deserialiseBech32 =
      case deserialiseFromBech32 asType keyText of
        Right res -> DeserialiseKeySuccess res

        -- The input was not valid Bech32.
        Left (Bech32DecodingError _) -> DeserialiseKeyErrorFormatMismatch

        -- The input was valid Bech32, but some other error occurred.
        Left err -> DeserialiseKeyError $ KeyBech32DecodeError err

    deserialiseHex :: DeserialiseKeyResult a
    deserialiseHex
      | isValidHex keyBs =
          maybe
            (DeserialiseKeyError KeyInvalidError)
            DeserialiseKeySuccess
            (deserialiseFromRawBytesHex asType keyBs)
      | otherwise = DeserialiseKeyErrorFormatMismatch

    isValidHex :: ByteString -> Bool
    isValidHex x =
      all (`elem` hexAlpha) (toLower <$> BSC.unpack x)
        && even (BSC.length x)
      where
        hexAlpha :: [Char]
        hexAlpha = "0123456789abcdef"

-- | Deserialise a cryptographic key of some type that is formatted in some
-- way.
--
-- The provided 'ByteString' can either be Bech32-encoded or in the text
-- envelope format.
deserialiseKeyAnyOf
  :: forall b.
     [FromSomeType SerialiseAsBech32 b]
  -> [FromSomeType HasTextEnvelope b]
  -> ByteString
  -> Either KeyDecodeError b
deserialiseKeyAnyOf bech32Types textEnvTypes keyBs =
    case deserialiseBech32 `orTry` deserialiseTextEnvelope of
      DeserialiseKeySuccess res -> Right res
      DeserialiseKeyError err -> Left err
      DeserialiseKeyErrorFormatMismatch -> Left KeyInvalidError
  where
    keyText :: Text
    keyText = Text.decodeUtf8 keyBs

    orTry
      :: DeserialiseKeyResult b
      -> DeserialiseKeyResult b
      -> DeserialiseKeyResult b
    orTry x y =
      case x of
        DeserialiseKeySuccess _ -> x
        DeserialiseKeyError _ -> x
        DeserialiseKeyErrorFormatMismatch -> y

    deserialiseTextEnvelope :: DeserialiseKeyResult b
    deserialiseTextEnvelope = do
      let textEnvRes :: Either TextEnvelopeError b
          textEnvRes =
            deserialiseFromTextEnvelopeAnyOf textEnvTypes
              =<< first TextViewAesonDecodeError (Aeson.eitherDecodeStrict' keyBs)
      case textEnvRes of
        Right res -> DeserialiseKeySuccess res

        -- The input was valid a text envelope, but there was a type mismatch
        -- error.
        Left err@(TextViewTypeError _ _) ->
          DeserialiseKeyError (KeyTextEnvelopeError err)

        -- The input was not valid a text envelope.
        Left _ -> DeserialiseKeyErrorFormatMismatch

    deserialiseBech32 :: DeserialiseKeyResult b
    deserialiseBech32 =
      case deserialiseAnyOfFromBech32 bech32Types keyText of
        Right res -> DeserialiseKeySuccess res

        -- The input was not valid Bech32.
        Left (Bech32DecodingError _) -> DeserialiseKeyErrorFormatMismatch

        -- The input was valid Bech32, but some other error occurred.
        Left err -> DeserialiseKeyError $ KeyBech32DecodeError err

-- | Read a cryptographic key from a file.
--
-- The contents of the file can either be Bech32-encoded, hex-encoded, or in
-- the text envelope format.
readKeyFile
  :: forall a. (HasTextEnvelope a, SerialiseAsBech32 a)
  => AsType a
  -> NonEmpty KeyFormat
  -> FilePath
  -> IO (Either (FileError KeyDecodeError) a)
readKeyFile asType acceptedFormats path =
  runExceptT $ do
    content <- handleIOExceptT (FileIOError path) $ BS.readFile path
    firstExceptT (FileError path) $ hoistEither $
      deserialiseKey asType acceptedFormats content

-- | Read a cryptographic key from a file.
--
-- The contents of the file must be in the text envelope format.
readKeyFileTextEnvelope
  :: HasTextEnvelope a
  => AsType a
  -> FilePath
  -> IO (Either (FileError KeyDecodeError) a)
readKeyFileTextEnvelope asType fp =
    first toKeyDecodeError <$> readFileTextEnvelope asType fp
  where
    toKeyDecodeError
      :: FileError TextEnvelopeError
      -> FileError KeyDecodeError
    toKeyDecodeError err =
      case err of
        FileIOError path ex -> FileIOError path ex
        FileError path textEnvErr ->
          FileError path (KeyTextEnvelopeError textEnvErr)

-- | Read a cryptographic key from a file given that it is one of the provided
-- types.
--
-- The contents of the file can either be Bech32-encoded or in the text
-- envelope format.
readKeyFileAnyOf
  :: forall b.
     [FromSomeType SerialiseAsBech32 b]
  -> [FromSomeType HasTextEnvelope b]
  -> FilePath
  -> IO (Either (FileError KeyDecodeError) b)
readKeyFileAnyOf bech32Types textEnvTypes path =
  runExceptT $ do
    content <- handleIOExceptT (FileIOError path) $ BS.readFile path
    firstExceptT (FileError path) $ hoistEither $
      deserialiseKeyAnyOf bech32Types textEnvTypes content

------------------------------------------------------------------------------
-- Signing key deserialisation
------------------------------------------------------------------------------

-- | Read a signing key from a file.
--
-- The contents of the file can either be Bech32-encoded, hex-encoded, or in
-- the text envelope format.
readSigningKeyFile
  :: forall keyrole.
     ( HasTextEnvelope (SigningKey keyrole)
     , SerialiseAsBech32 (SigningKey keyrole)
     )
  => AsType keyrole
  -> SigningKeyFile
  -> IO (Either (FileError KeyDecodeError) (SigningKey keyrole))
readSigningKeyFile asType (SigningKeyFile fp) =
  readKeyFile
    (AsSigningKey asType)
    (NE.fromList [KeyFormatBech32, KeyFormatHex, KeyFormatTextEnvelope])
    fp

-- | Read a signing key from a file given that it is one of the provided types
-- of signing key.
--
-- The contents of the file can either be Bech32-encoded or in the text
-- envelope format.
readSigningKeyFileAnyOf
  :: forall b.
     [FromSomeType SerialiseAsBech32 b]
  -> [FromSomeType HasTextEnvelope b]
  -> SigningKeyFile
  -> IO (Either (FileError KeyDecodeError) b)
readSigningKeyFileAnyOf bech32Types textEnvTypes (SigningKeyFile fp) =
  readKeyFileAnyOf bech32Types textEnvTypes fp

------------------------------------------------------------------------------
-- Verification key deserialisation
------------------------------------------------------------------------------

-- | Either a verification key or path to a verification key file.
data VerificationKeyOrFile keyrole
  = VerificationKeyValue !(VerificationKey keyrole)
  -- ^ A verification key.
  | VerificationKeyFilePath !VerificationKeyFile
  -- ^ A path to a verification key file.
  -- Note that this file hasn't been validated at all (whether it exists,
  -- contains a key of the correct type, etc.)

deriving instance Show (VerificationKey keyrole)
  => Show (VerificationKeyOrFile keyrole)

deriving instance Eq (VerificationKey keyrole)
  => Eq (VerificationKeyOrFile keyrole)

-- | Read a verification key or verification key file and return a
-- verification key.
--
-- If a filepath is provided, the file can either be formatted as Bech32, hex,
-- or text envelope.
readVerificationKeyOrFile
  :: ( HasTextEnvelope (VerificationKey keyrole)
     , SerialiseAsBech32 (VerificationKey keyrole)
     )
  => AsType keyrole
  -> VerificationKeyOrFile keyrole
  -> IO (Either (FileError KeyDecodeError) (VerificationKey keyrole))
readVerificationKeyOrFile asType verKeyOrFile =
  case verKeyOrFile of
    VerificationKeyValue vk -> pure (Right vk)
    VerificationKeyFilePath (VerificationKeyFile fp) ->
      readKeyFile
        (AsVerificationKey asType)
        (NE.fromList [KeyFormatBech32, KeyFormatHex, KeyFormatTextEnvelope])
        fp

-- | Read a verification key or verification key file and return a
-- verification key.
--
-- If a filepath is provided, it will be interpreted as a text envelope
-- formatted file.
readVerificationKeyOrTextEnvFile
  :: HasTextEnvelope (VerificationKey keyrole)
  => AsType keyrole
  -> VerificationKeyOrFile keyrole
  -> IO (Either (FileError KeyDecodeError) (VerificationKey keyrole))
readVerificationKeyOrTextEnvFile asType verKeyOrFile =
  case verKeyOrFile of
    VerificationKeyValue vk -> pure (Right vk)
    VerificationKeyFilePath (VerificationKeyFile fp) ->
      readKeyFileTextEnvelope (AsVerificationKey asType) fp

-- | Either an unvalidated text representation of a verification key or a path
-- to a verification key file.
data VerificationKeyTextOrFile
  = VktofVerificationKeyText !Text
  | VktofVerificationKeyFile !VerificationKeyFile
  deriving (Eq, Show)

-- | An error in deserializing a 'VerificationKeyTextOrFile' to a
-- 'VerificationKey'.
data VerificationKeyTextOrFileError
  = VerificationKeyTextError !KeyDecodeError
  | VerificationKeyFileError !(FileError KeyDecodeError)
  deriving Show

-- | Render an error message for a 'VerificationKeyTextOrFileError'.
renderVerificationKeyTextOrFileError :: VerificationKeyTextOrFileError -> Text
renderVerificationKeyTextOrFileError vkTextOrFileErr =
  case vkTextOrFileErr of
    VerificationKeyTextError err -> renderKeyDecodeError err
    VerificationKeyFileError err -> Text.pack (displayError err)

-- | Deserialise a verification key from text or a verification key file given
-- that it is one of the provided types.
--
-- If a filepath is provided, the file can either be formatted as Bech32, hex,
-- or text envelope.
readVerificationKeyTextOrFileAnyOf
  :: forall b.
     [FromSomeType SerialiseAsBech32 b]
  -> [FromSomeType HasTextEnvelope b]
  -> VerificationKeyTextOrFile
  -> IO (Either VerificationKeyTextOrFileError b)
readVerificationKeyTextOrFileAnyOf bech32Types textEnvTypes verKeyTextOrFile =
  case verKeyTextOrFile of
    VktofVerificationKeyText vkText ->
      pure $ first VerificationKeyTextError $
        deserialiseKeyAnyOf bech32Types textEnvTypes (Text.encodeUtf8 vkText)
    VktofVerificationKeyFile (VerificationKeyFile fp) ->
      first VerificationKeyFileError
        <$> readKeyFileAnyOf bech32Types textEnvTypes fp

-- | Verification key, verification key hash, or path to a verification key
-- file.
data VerificationKeyOrHashOrFile keyrole
  = VerificationKeyOrFile !(VerificationKeyOrFile keyrole)
  -- ^ Either a verification key or path to a verification key file.
  | VerificationKeyHash !(Hash keyrole)
  -- ^ A verification key hash.

deriving instance (Show (VerificationKeyOrFile keyrole), Show (Hash keyrole))
  => Show (VerificationKeyOrHashOrFile keyrole)

deriving instance (Eq (VerificationKeyOrFile keyrole), Eq (Hash keyrole))
  => Eq (VerificationKeyOrHashOrFile keyrole)

-- | Read a verification key or verification key hash or verification key file
-- and return a verification key hash.
--
-- If a filepath is provided, the file can either be formatted as Bech32, hex,
-- or text envelope.
readVerificationKeyOrHashOrFile
  :: (Key keyrole, SerialiseAsBech32 (VerificationKey keyrole))
  => AsType keyrole
  -> VerificationKeyOrHashOrFile keyrole
  -> IO (Either (FileError KeyDecodeError) (Hash keyrole))
readVerificationKeyOrHashOrFile asType verKeyOrHashOrFile =
  case verKeyOrHashOrFile of
    VerificationKeyOrFile vkOrFile -> do
      eitherVk <- readVerificationKeyOrFile asType vkOrFile
      pure (verificationKeyHash <$> eitherVk)
    VerificationKeyHash vkHash -> pure (Right vkHash)

-- | Read a verification key or verification key hash or verification key file
-- and return a verification key hash.
--
-- If a filepath is provided, it will be interpreted as a text envelope
-- formatted file.
readVerificationKeyOrHashOrTextEnvFile
  :: Key keyrole
  => AsType keyrole
  -> VerificationKeyOrHashOrFile keyrole
  -> IO (Either (FileError KeyDecodeError) (Hash keyrole))
readVerificationKeyOrHashOrTextEnvFile asType verKeyOrHashOrFile =
  case verKeyOrHashOrFile of
    VerificationKeyOrFile vkOrFile -> do
      eitherVk <- readVerificationKeyOrTextEnvFile asType vkOrFile
      pure (verificationKeyHash <$> eitherVk)
    VerificationKeyHash vkHash -> pure (Right vkHash)
