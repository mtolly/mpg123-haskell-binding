{-# LANGUAGE ForeignFunctionInterface #-}
module Codec.Mpg123.Raw where

import Control.Applicative (pure)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import System.Posix.Types

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

#include <mpg123.h>

foreign import ccall unsafe "mpg123_init"
  c_mpg123_init :: IO CInt

foreign import ccall unsafe "mpg123_exit"
  c_mpg123_exit :: IO ()

data Mpg123_handle = Mpg123_handle
type Mpg123_handle_ptr = Ptr Mpg123_handle

foreign import ccall unsafe "mpg123_new"
  c_mpg123_new :: CString -> Ptr CInt -> IO Mpg123_handle_ptr

foreign import ccall unsafe "mpg123_delete"
  c_mpg123_delete :: Mpg123_handle_ptr -> IO ()

newtype Mpg123_parms = Mpg123_parms { mpg123_parms :: CInt }
#{enum Mpg123_parms, Mpg123_parms
 , mpg123_verbose = MPG123_VERBOSE
 , mpg123_flags = MPG123_FLAGS
 , mpg123_add_flags = MPG123_ADD_FLAGS
 , mpg123_force_rate = MPG123_FORCE_RATE
 , mpg123_down_sample = MPG123_DOWN_SAMPLE
 , mpg123_rva = MPG123_RVA
 , mpg123_downspeed = MPG123_DOWNSPEED
 , mpg123_upspeed = MPG123_UPSPEED
 , mpg123_start_frame = MPG123_START_FRAME
 , mpg123_decode_frames = MPG123_DECODE_FRAMES
 , mpg123_icy_interval = MPG123_ICY_INTERVAL
 , mpg123_outscale = MPG123_OUTSCALE
 , mpg123_timeout = MPG123_TIMEOUT
 , mpg123_remove_flags = MPG123_REMOVE_FLAGS
 , mpg123_resync_limit = MPG123_RESYNC_LIMIT
 , mpg123_index_size = MPG123_INDEX_SIZE
 , mpg123_preframes = MPG123_PREFRAMES
 , mpg123_feedpool = MPG123_FEEDPOOL
 , mpg123_feedbuffer = MPG123_FEEDBUFFER }

newtype Mpg123_param_flags = Mpg123_param_flags { mpg123_param_flags :: CInt }
#{enum Mpg123_param_flags, Mpg123_param_flags
 , mpg123_force_mono = MPG123_FORCE_MONO
 , mpg123_mono_left = MPG123_MONO_LEFT
 , mpg123_mono_right = MPG123_MONO_RIGHT
 , mpg123_mono_mix = MPG123_MONO_MIX
 , mpg123_force_stereo = MPG123_FORCE_STEREO
 , mpg123_force_8bit = MPG123_FORCE_8BIT
 , mpg123_quiet = MPG123_QUIET
 , mpg123_gapless = MPG123_GAPLESS
 , mpg123_no_resync = MPG123_NO_RESYNC
 , mpg123_seekbuffer = MPG123_SEEKBUFFER
 , mpg123_fuzzy = MPG123_FUZZY
 , mpg123_force_float = MPG123_FORCE_FLOAT
 , mpg123_plain_id3text = MPG123_PLAIN_ID3TEXT
 , mpg123_ignore_streamlength = MPG123_IGNORE_STREAMLENGTH
 , mpg123_skip_id3v2 = MPG123_SKIP_ID3V2
 , mpg123_ignore_infoframe = MPG123_IGNORE_INFOFRAME
 , mpg123_auto_resample = MPG123_AUTO_RESAMPLE
 , mpg123_picture = MPG123_PICTURE }

newtype Mpg123_param_rva = Mpg123_param_rva { mpg123_param_rva :: CInt }
#{enum Mpg123_param_rva, Mpg123_param_rva
 , mpg123_rva_off = MPG123_RVA_OFF
 , mpg123_rva_mix = MPG123_RVA_MIX
 , mpg123_rva_album = MPG123_RVA_ALBUM
 , mpg123_rva_max = MPG123_RVA_MAX }


foreign import ccall unsafe "mpg123_param"
  c_mpg123_param :: Mpg123_handle_ptr
                 -> CInt -- ^ enum mpg123_parms
                 -> CLong
                 -> CDouble
                 -> IO CInt

foreign import ccall unsafe "mpg123_getparam"
  c_mpg123_getparam :: Mpg123_handle_ptr
                    -> CInt -- ^ enum mpg123_parms
                    -> Ptr CLong
                    -> Ptr CDouble
                    -> IO CInt

newtype Mpg123_feature_set = Mpg123_feature_set { mpg123_feature_set :: CInt }
#{enum Mpg123_feature_set, Mpg123_feature_set
 , mpg123_feature_abi_utf8open = MPG123_FEATURE_ABI_UTF8OPEN
 , mpg123_feature_output_8bit = MPG123_FEATURE_OUTPUT_8BIT
 , mpg123_feature_output_16bit = MPG123_FEATURE_OUTPUT_16BIT
 , mpg123_feature_output_32bit = MPG123_FEATURE_OUTPUT_32BIT
 , mpg123_feature_index = MPG123_FEATURE_INDEX
 , mpg123_feature_parse_id3v2 = MPG123_FEATURE_PARSE_ID3V2
 , mpg123_feature_decode_layer1 = MPG123_FEATURE_DECODE_LAYER1
 , mpg123_feature_decode_layer2 = MPG123_FEATURE_DECODE_LAYER2
 , mpg123_feature_decode_layer3 = MPG123_FEATURE_DECODE_LAYER3
 , mpg123_feature_decode_accurate = MPG123_FEATURE_DECODE_ACCURATE
 , mpg123_feature_decode_downsample = MPG123_FEATURE_DECODE_DOWNSAMPLE
 , mpg123_feature_decode_ntom = MPG123_FEATURE_DECODE_NTOM
 , mpg123_feature_parse_icy = MPG123_FEATURE_PARSE_ICY
 , mpg123_feature_timeout_read = MPG123_FEATURE_TIMEOUT_READ }

foreign import ccall unsafe "mpg123_feature"
  c_mpg123_feature :: CInt -- ^ enum mpg123_feature_set
                   -> IO CInt

newtype Mpg123_errors = Mpg123_errors { mpg123_errors :: CInt }
#{enum Mpg123_errors, Mpg123_errors
 , mpg123_done = MPG123_DONE
 , mpg123_new_format = MPG123_NEW_FORMAT
 , mpg123_need_more = MPG123_NEED_MORE
 , mpg123_err = MPG123_ERR
 , mpg123_ok = MPG123_OK
 , mpg123_bad_outformat = MPG123_BAD_OUTFORMAT
 , mpg123_bad_channel = MPG123_BAD_CHANNEL
 , mpg123_bad_rate = MPG123_BAD_RATE
 , mpg123_err_16to8table = MPG123_ERR_16TO8TABLE
 , mpg123_bad_param = MPG123_BAD_PARAM
 , mpg123_bad_buffer = MPG123_BAD_BUFFER
 , mpg123_out_of_mem = MPG123_OUT_OF_MEM
 , mpg123_not_initialized = MPG123_NOT_INITIALIZED
 , mpg123_bad_decoder = MPG123_BAD_DECODER
 , mpg123_bad_handle = MPG123_BAD_HANDLE
 , mpg123_no_buffers = MPG123_NO_BUFFERS
 , mpg123_bad_rva = MPG123_BAD_RVA
 , mpg123_no_gapless = MPG123_NO_GAPLESS
 , mpg123_no_space = MPG123_NO_SPACE
 , mpg123_bad_types = MPG123_BAD_TYPES
 , mpg123_bad_band = MPG123_BAD_BAND
 , mpg123_err_null = MPG123_ERR_NULL
 , mpg123_err_reader = MPG123_ERR_READER
 , mpg123_no_seek_from_end = MPG123_NO_SEEK_FROM_END
 , mpg123_bad_whence = MPG123_BAD_WHENCE
 , mpg123_no_timeout = MPG123_NO_TIMEOUT
 , mpg123_bad_file = MPG123_BAD_FILE
 , mpg123_no_seek = MPG123_NO_SEEK
 , mpg123_no_reader = MPG123_NO_READER
 , mpg123_bad_pars = MPG123_BAD_PARS
 , mpg123_bad_index_par = MPG123_BAD_INDEX_PAR
 , mpg123_out_of_sync = MPG123_OUT_OF_SYNC
 , mpg123_resync_fail = MPG123_RESYNC_FAIL
 , mpg123_no_8bit = MPG123_NO_8BIT
 , mpg123_bad_align = MPG123_BAD_ALIGN
 , mpg123_null_buffer = MPG123_NULL_BUFFER
 , mpg123_no_relseek = MPG123_NO_RELSEEK
 , mpg123_null_pointer = MPG123_NULL_POINTER
 , mpg123_bad_key = MPG123_BAD_KEY
 , mpg123_no_index = MPG123_NO_INDEX
 , mpg123_index_fail = MPG123_INDEX_FAIL
 , mpg123_bad_decoder_setup = MPG123_BAD_DECODER_SETUP
 , mpg123_missing_feature = MPG123_MISSING_FEATURE
 , mpg123_bad_value = MPG123_BAD_VALUE
 , mpg123_lseek_failed = MPG123_LSEEK_FAILED
 , mpg123_bad_custom_io = MPG123_BAD_CUSTOM_IO
 , mpg123_lfs_overflow = MPG123_LFS_OVERFLOW
 , mpg123_int_overflow = MPG123_INT_OVERFLOW }

foreign import ccall unsafe "mpg123_plain_strerror"
  c_mpg123_plain_strerror :: CInt -- ^ enum mpg123_errors
                          -> IO CString

foreign import ccall unsafe "mpg123_strerror"
  c_mpg123_strerror :: Mpg123_handle_ptr -> IO CString

foreign import ccall unsafe "mpg123_errcode"
  c_mpg123_errcode :: Mpg123_handle_ptr -> IO CInt

foreign import ccall unsafe "mpg123_decoders"
  c_mpg123_decoders :: IO (Ptr CString)

foreign import ccall unsafe "mpg123_supported_decoders"
  c_mpg123_supported_decoders :: IO (Ptr CString)

foreign import ccall unsafe "mpg123_decoder"
  c_mpg123_decoder :: Mpg123_handle_ptr -> CString -> IO CInt

foreign import ccall unsafe "mpg123_current_decoder"
  c_mpg123_current_decoder :: Mpg123_handle_ptr -> IO CString

newtype Mpg123_enc_enum = Mpg123_enc_enum { mpg123_enc_enum :: CInt }
#{enum Mpg123_enc_enum, Mpg123_enc_enum
 , mpg123_enc_8 = MPG123_ENC_8
 , mpg123_enc_16 = MPG123_ENC_16
 , mpg123_enc_24 = MPG123_ENC_24
 , mpg123_enc_32 = MPG123_ENC_32
 , mpg123_enc_signed = MPG123_ENC_SIGNED
 , mpg123_enc_float = MPG123_ENC_FLOAT
 , mpg123_enc_signed_16 = MPG123_ENC_SIGNED_16
 , mpg123_enc_unsigned_16 = MPG123_ENC_UNSIGNED_16
 , mpg123_enc_unsigned_8 = MPG123_ENC_UNSIGNED_8
 , mpg123_enc_signed_8 = MPG123_ENC_SIGNED_8
 , mpg123_enc_ulaw_8 = MPG123_ENC_ULAW_8
 , mpg123_enc_alaw_8 = MPG123_ENC_ALAW_8
 , mpg123_enc_signed_32 = MPG123_ENC_SIGNED_32
 , mpg123_enc_unsigned_32 = MPG123_ENC_UNSIGNED_32
 , mpg123_enc_signed_24 = MPG123_ENC_SIGNED_24
 , mpg123_enc_unsigned_24 = MPG123_ENC_UNSIGNED_24
 , mpg123_enc_float_32 = MPG123_ENC_FLOAT_32
 , mpg123_enc_float_64 = MPG123_ENC_FLOAT_64
 , mpg123_enc_any = MPG123_ENC_ANY }

newtype Mpg123_channelcount = Mpg123_channelcount { mpg123_channelcount :: CInt }
#{enum Mpg123_channelcount, Mpg123_channelcount
 , mpg123_mono = MPG123_MONO
 , mpg123_stereo = MPG123_STEREO }

foreign import ccall unsafe "mpg123_rates"
  c_mpg123_rates :: Ptr (Ptr CLong) -> Ptr CSize -> IO ()

foreign import ccall unsafe "mpg123_encodings"
  c_mpg123_encodings :: Ptr (Ptr CInt) -> Ptr CSize -> IO ()

foreign import ccall unsafe "mpg123_encsize"
  c_mpg123_encsize :: CInt -> IO CInt

foreign import ccall unsafe "mpg123_format_none"
  c_mpg123_format_none :: Mpg123_handle_ptr -> IO CInt

foreign import ccall unsafe "mpg123_format_all"
  c_mpg123_format_all :: Mpg123_handle_ptr -> IO CInt

foreign import ccall unsafe "mpg123_format"
  c_mpg123_format :: Mpg123_handle_ptr -> CLong -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "mpg123_format_support"
  c_mpg123_format_support :: Mpg123_handle_ptr -> CLong -> CInt -> IO CInt

foreign import ccall unsafe "mpg123_getformat"
  c_mpg123_getformat :: Mpg123_handle_ptr -> Ptr CLong -> Ptr CInt -> Ptr CInt -> IO CInt


foreign import ccall unsafe "mpg123_open"
  c_mpg123_open :: Mpg123_handle_ptr -> CString -> IO CInt

foreign import ccall unsafe "mpg123_open_fd"
  c_mpg123_open_fd :: Mpg123_handle_ptr -> CInt -> IO CInt

foreign import ccall unsafe "mpg123_open_handle"
  c_mpg123_open_handle :: Mpg123_handle_ptr -> Ptr () -> IO CInt

foreign import ccall unsafe "mpg123_open_feed"
  c_mpg123_open_feed :: Mpg123_handle_ptr -> IO CInt

foreign import ccall unsafe "mpg123_close"
  c_mpg123_close :: Mpg123_handle_ptr -> IO CInt

foreign import ccall unsafe "mpg123_read"
  c_mpg123_read :: Mpg123_handle_ptr -> CString -> CSize -> Ptr CSize -> IO CInt

foreign import ccall unsafe "mpg123_feed"
  c_mpg123_feed :: Mpg123_handle_ptr -> CString -> CSize -> IO CInt

foreign import ccall unsafe "mpg123_decode"
  c_mpg123_decode :: Mpg123_handle_ptr -> CString -> CSize -> CString -> CSize -> Ptr CSize -> IO CInt

foreign import ccall unsafe "mpg123_decode_frame"
  c_mpg123_decode_frame :: Mpg123_handle_ptr -> Ptr COff -> Ptr CString -> Ptr CSize -> IO CInt

foreign import ccall unsafe "mpg123_framebyframe_decode"
  c_mpg123_framebyframe_decode :: Mpg123_handle_ptr -> Ptr COff -> Ptr CString -> Ptr CSize -> IO CInt

foreign import ccall unsafe "mpg123_framebyframe_next"
  c_mpg123_framebyframe_next :: Mpg123_handle_ptr -> IO CInt

foreign import ccall unsafe "mpg123_framedata"
  c_mpg123_framedata :: Mpg123_handle_ptr -> Ptr CULong -> Ptr CString -> Ptr CSize -> IO CInt

foreign import ccall unsafe "mpg123_framepos"
  c_mpg123_framepos :: Mpg123_handle_ptr -> IO COff

foreign import ccall unsafe "mpg123_tell"
  c_mpg123_tell :: Mpg123_handle_ptr -> IO COff

foreign import ccall unsafe "mpg123_tellframe"
  c_mpg123_tellframe :: Mpg123_handle_ptr -> IO COff

foreign import ccall unsafe "mpg123_tell_stream"
  c_mpg123_tell_stream :: Mpg123_handle_ptr -> IO COff

foreign import ccall unsafe "mpg123_seek"
  c_mpg123_seek :: Mpg123_handle_ptr -> COff -> CInt -> IO COff

foreign import ccall unsafe "mpg123_feedseek"
  c_mpg123_feedseek :: Mpg123_handle_ptr -> COff -> CInt -> Ptr COff -> IO COff

foreign import ccall unsafe "mpg123_seek_frame"
  c_mpg123_seek_frame :: Mpg123_handle_ptr -> COff -> CInt -> IO COff

foreign import ccall unsafe "mpg123_timeframe"
  c_mpg123_timeframe :: Mpg123_handle_ptr -> CDouble -> IO COff

foreign import ccall unsafe "mpg123_index"
  c_mpg123_index :: Mpg123_handle_ptr -> Ptr (Ptr COff) -> Ptr COff -> Ptr CSize -> IO CInt

foreign import ccall unsafe "mpg123_set_index"
  c_mpg123_set_index :: Mpg123_handle_ptr -> Ptr COff -> COff -> CSize -> IO CInt

foreign import ccall unsafe "mpg123_position"
  c_mpg123_position ::  Mpg123_handle_ptr -> COff -> COff -> Ptr COff -> Ptr COff -> Ptr CDouble -> Ptr CDouble -> IO CInt

newtype Mpg123_channels = Mpg123_channels { mpg123_channels :: CInt }
#{enum Mpg123_channels, Mpg123_channels
 , mpg123_left = MPG123_LEFT
 , mpg123_right = MPG123_RIGHT
 , mpg123_lr = MPG123_LR }

foreign import ccall unsafe "mpg123_eq"
  c_mpg123_eq :: Mpg123_handle_ptr
              -> CInt -- ^ enum mpg123_channels
              -> CInt
              -> CDouble
              -> IO CInt

foreign import ccall unsafe "mpg123_geteq"
  c_mpg123_geteq :: Mpg123_handle_ptr
                 -> CInt -- ^ enum mpg123_channels
                 -> IO CDouble

foreign import ccall unsafe "mpg123_reset_eq"
  c_mpg123_reset_eq :: Mpg123_handle_ptr -> IO CInt

foreign import ccall unsafe "mpg123_volume"
  c_mpg123_volume :: Mpg123_handle_ptr -> CDouble -> IO CInt

foreign import ccall unsafe "mpg123_volume_change"
  c_mpg123_volume_change :: Mpg123_handle_ptr -> CDouble -> IO CInt

foreign import ccall unsafe "mpg123_getvolume"
  c_mpg123_getvolume :: Mpg123_handle_ptr -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt

newtype Mpg123_vbr = Mpg123_vbr { mpg123_vbr' :: CInt }
#{enum Mpg123_vbr, Mpg123_vbr
 , mpg123_cbr = MPG123_CBR
 , mpg123_vbr = MPG123_VBR
 , mpg123_abr = MPG123_ABR }

newtype Mpg123_version = Mpg123_version { mpg123_version :: CInt }
#{enum Mpg123_version, Mpg123_version
 , mpg123_1_0 = MPG123_1_0
 , mpg123_2_0 = MPG123_2_0
 , mpg123_2_5 = MPG123_2_5 }

newtype Mpg123_mode = Mpg123_mode { mpg123_mode :: CInt }
#{enum Mpg123_mode, Mpg123_mode
 , mpg123_m_stereo = MPG123_M_STEREO
 , mpg123_m_joint = MPG123_M_JOINT
 , mpg123_m_dual = MPG123_M_DUAL
 , mpg123_m_mono = MPG123_M_MONO }

newtype Mpg123_flags = Mpg123_flags { mpg123_flags' :: CInt }
#{enum Mpg123_flags, Mpg123_flags
 , mpg123_crc = MPG123_CRC
 , mpg123_copyright = MPG123_COPYRIGHT
 , mpg123_private = MPG123_PRIVATE
 , mpg123_original = MPG123_ORIGINAL }

data Mpg123_frameinfo =
  Mpg123_frameinfo { version   :: CInt -- ^ enum mpg123_version
                   , layer     :: CInt
                   , rate      :: CLong
                   , mode      :: CInt -- ^ enum mpg123_mode
                   , mode_ext  :: CInt
                   , framesize :: CInt
                   , flags     :: CInt -- ^ enum mpg123_flags
                   , emphasis  :: CInt
                   , bitrate   :: CInt
                   , abr_rate  :: CInt
                   , vbr       :: CInt -- ^ enum mpg123_vbr
                   }
instance Storable Mpg123_frameinfo where
  sizeOf _ = (#size struct mpg123_frameinfo)
  alignment _ = (#alignment struct mpg123_frameinfo)
  peek ptr = do
    version   <- (#peek struct mpg123_frameinfo,   version) ptr
    layer     <- (#peek struct mpg123_frameinfo,     layer) ptr
    rate      <- (#peek struct mpg123_frameinfo,      rate) ptr
    mode      <- (#peek struct mpg123_frameinfo,      mode) ptr
    mode_ext  <- (#peek struct mpg123_frameinfo,  mode_ext) ptr
    framesize <- (#peek struct mpg123_frameinfo, framesize) ptr
    flags     <- (#peek struct mpg123_frameinfo,     flags) ptr
    emphasis  <- (#peek struct mpg123_frameinfo,  emphasis) ptr
    bitrate   <- (#peek struct mpg123_frameinfo,   bitrate) ptr
    abr_rate  <- (#peek struct mpg123_frameinfo,  abr_rate) ptr
    vbr       <- (#peek struct mpg123_frameinfo,       vbr) ptr
    pure $! Mpg123_frameinfo version layer rate mode mode_ext framesize flags emphasis bitrate abr_rate vbr
  poke ptr (Mpg123_frameinfo version layer rate mode mode_ext framesize flags emphasis bitrate abr_rate vbr) = do
    (#poke struct mpg123_frameinfo,   version) ptr version
    (#poke struct mpg123_frameinfo,     layer) ptr layer
    (#poke struct mpg123_frameinfo,      rate) ptr rate
    (#poke struct mpg123_frameinfo,      mode) ptr mode
    (#poke struct mpg123_frameinfo,  mode_ext) ptr mode_ext
    (#poke struct mpg123_frameinfo, framesize) ptr framesize
    (#poke struct mpg123_frameinfo,     flags) ptr flags
    (#poke struct mpg123_frameinfo,  emphasis) ptr emphasis
    (#poke struct mpg123_frameinfo,   bitrate) ptr bitrate
    (#poke struct mpg123_frameinfo,  abr_rate) ptr abr_rate
    (#poke struct mpg123_frameinfo,       vbr) ptr vbr

type Mpg123_frameinfo_ptr = Ptr Mpg123_frameinfo

foreign import ccall unsafe "mpg123_info"
  c_mpg123_info :: Mpg123_handle_ptr -> Mpg123_frameinfo_ptr -> IO CInt
foreign import ccall unsafe "mpg123_safe_buffer"
  c_mpg123_safe_buffer :: IO CSize
foreign import ccall unsafe "mpg123_scan"
  c_mpg123_scan :: Mpg123_handle_ptr -> IO CInt
foreign import ccall unsafe "mpg123_length"
  c_mpg123_length :: Mpg123_handle_ptr -> IO COff
foreign import ccall unsafe "mpg123_set_filesize"
  c_mpg123_set_filesize :: Mpg123_handle_ptr -> COff -> IO CInt
foreign import ccall unsafe "mpg123_tpf"
  c_mpg123_tpf :: Mpg123_handle_ptr -> IO CDouble
foreign import ccall unsafe "mpg123_spf"
  c_mpg123_spf :: Mpg123_handle_ptr -> IO CInt
foreign import ccall unsafe "mpg123_clip"
  c_mpg123_clip :: Mpg123_handle_ptr -> IO CDouble

newtype Mpg123_state = Mpg123_state { mpg123_state :: CInt }
#{enum Mpg123_state, Mpg123_state
 , mpg123_accurate = MPG123_ACCURATE
 , mpg123_bufferfill = MPG123_BUFFERFILL
 , mpg123_frankenstein = MPG123_FRANKENSTEIN
 , mpg123_fresh_decoder = MPG123_FRESH_DECODER }

foreign import ccall unsafe "mpg123_getstate"
  c_mpg123_getstate :: Mpg123_handle_ptr
                    -> CInt -- ^ enum mpg123_state
                    -> Ptr CLong
                    -> Ptr CDouble
                    -> IO CInt

data Mpg123_string = Mpg123_string { p    :: CString
                                   , size :: CSize
                                   , fill :: CSize }

instance Storable Mpg123_string where
  sizeOf _ = (#size mpg123_string)
  alignment _ = (#alignment mpg123_string)
  peek ptr = do
    p    <- (#peek mpg123_string,    p) ptr
    size <- (#peek mpg123_string, size) ptr
    fill <- (#peek mpg123_string, fill) ptr
    pure $! Mpg123_string p size fill
  poke ptr (Mpg123_string p size fill) = do
    (#poke mpg123_string,    p) ptr p
    (#poke mpg123_string, size) ptr size
    (#poke mpg123_string, fill) ptr fill

type Mpg123_string_ptr = Ptr Mpg123_string


foreign import ccall unsafe "mpg123_init_string"
  c_mpg123_init_string :: Mpg123_string_ptr -> IO ()
foreign import ccall unsafe "mpg123_free_string"
  c_mpg123_free_string :: Mpg123_string_ptr -> IO ()
foreign import ccall unsafe "mpg123_resize_string"
  c_mpg123_resize_string :: Mpg123_string_ptr -> CSize -> IO CInt
foreign import ccall unsafe "mpg123_grow_string"
  c_mpg123_grow_string :: Mpg123_string_ptr -> CSize -> IO CInt
foreign import ccall unsafe "mpg123_copy_string"
  c_mpg123_copy_string :: Mpg123_string_ptr -> Mpg123_string_ptr -> IO CInt
foreign import ccall unsafe "mpg123_add_string"
  c_mpg123_add_string :: Mpg123_string_ptr -> CString -> IO CInt
foreign import ccall unsafe "mpg123_add_substring"
  c_mpg123_add_substring :: Mpg123_string_ptr -> CString -> CSize -> CSize -> IO CInt
foreign import ccall unsafe "mpg123_set_string"
  c_mpg123_set_string :: Mpg123_string_ptr -> CString -> IO CInt
foreign import ccall unsafe "mpg123_set_substring"
  c_mpg123_set_substring :: Mpg123_string_ptr -> CString -> CSize -> CSize -> IO CInt
foreign import ccall unsafe "mpg123_strlen"
  c_mpg123_strlen :: Mpg123_string_ptr -> CInt -> IO CSize
foreign import ccall unsafe "mpg123_chomp_string"
  c_mpg123_chomp_string :: Mpg123_string_ptr -> IO CInt

newtype Mpg123_text_encoding = Mpg123_text_encoding { mpg123_text_encoding :: CInt }
#{enum Mpg123_text_encoding, Mpg123_text_encoding
 , mpg123_text_unknown = mpg123_text_unknown
 , mpg123_text_utf8 = mpg123_text_utf8
 , mpg123_text_latin1 = mpg123_text_latin1
 , mpg123_text_icy = mpg123_text_icy
 , mpg123_text_cp1252 = mpg123_text_cp1252
 , mpg123_text_utf16 = mpg123_text_utf16
 , mpg123_text_utf16bom = mpg123_text_utf16bom
 , mpg123_text_utf16be = mpg123_text_utf16be
 , mpg123_text_max = mpg123_text_max }

newtype Mpg123_id3_enc = Mpg123_id3_enc { mpg123_id3_enc :: CInt }
#{enum Mpg123_id3_enc, Mpg123_id3_enc
 , mpg123_id3_latin1 = mpg123_id3_latin1
 , mpg123_id3_utf16bom = mpg123_id3_utf16bom
 , mpg123_id3_utf16be = mpg123_id3_utf16be
 , mpg123_id3_utf8 = mpg123_id3_utf8
 , mpg123_id3_enc_max = mpg123_id3_enc_max }

foreign import ccall unsafe "mpg123_enc_from_id3"
  c_mpg123_enc_from_id3 :: CChar
                        -> CInt -- ^ enum mpg123_text_encoding
foreign import ccall unsafe "mpg123_store_utf8"
  c_mpg123_store_utf8 :: Mpg123_string_ptr
                      -> CInt -- ^ enum mpg123_text_encoding
                      -> Ptr CChar
                      -> CSize
                      -> IO CInt

data Mpg123_text = Mpg123_text { lang  :: CString -- ^ length 3 (not terminated by NUL)
                               , id3v2 :: CString -- ^ length 4 (not terminated by NUL)
                               , description :: Mpg123_string
                               , text        :: Mpg123_string }
instance Storable Mpg123_text where
  sizeOf _ =    (#size      mpg123_text)
  alignment _ = (#alignment mpg123_text)
  peek ptr = do
    lang        <- (#peek mpg123_text,        lang) ptr
    id3v2       <- (#peek mpg123_text,          id) ptr
    description <- (#peek mpg123_text, description) ptr
    text        <- (#peek mpg123_text,        text) ptr
    pure $! Mpg123_text lang id3v2 description text
  poke ptr (Mpg123_text lang id3v2 description text) = do
    (#poke mpg123_text,        lang) ptr lang
    (#poke mpg123_text,          id) ptr id3v2
    (#poke mpg123_text, description) ptr description
    (#poke mpg123_text,        text) ptr text
type Mpg123_text_ptr = Ptr Mpg123_text

newtype Mpg123_id3_pic_type = Mpg123_id3_pic_type { mpg123_id3_pic_type :: CInt }
#{enum Mpg123_id3_pic_type, Mpg123_id3_pic_type
 , mpg123_id3_pic_other = mpg123_id3_pic_other
 , mpg123_id3_pic_icon = mpg123_id3_pic_icon
 , mpg123_id3_pic_other_icon = mpg123_id3_pic_other_icon
 , mpg123_id3_pic_front_cover = mpg123_id3_pic_front_cover
 , mpg123_id3_pic_back_cover = mpg123_id3_pic_back_cover
 , mpg123_id3_pic_leaflet = mpg123_id3_pic_leaflet
 , mpg123_id3_pic_media = mpg123_id3_pic_media
 , mpg123_id3_pic_lead = mpg123_id3_pic_lead
 , mpg123_id3_pic_artist = mpg123_id3_pic_artist
 , mpg123_id3_pic_conductor = mpg123_id3_pic_conductor
 , mpg123_id3_pic_orchestra = mpg123_id3_pic_orchestra
 , mpg123_id3_pic_composer = mpg123_id3_pic_composer
 , mpg123_id3_pic_lyricist = mpg123_id3_pic_lyricist
 , mpg123_id3_pic_location = mpg123_id3_pic_location
 , mpg123_id3_pic_recording = mpg123_id3_pic_recording
 , mpg123_id3_pic_performance = mpg123_id3_pic_performance
 , mpg123_id3_pic_video = mpg123_id3_pic_video
 , mpg123_id3_pic_fish = mpg123_id3_pic_fish
 , mpg123_id3_pic_illustration = mpg123_id3_pic_illustration
 , mpg123_id3_pic_artist_logo = mpg123_id3_pic_artist_logo
 , mpg123_id3_pic_publisher_logo = mpg123_id3_pic_publisher_logo }


data Mpg123_picture = Mpg123_picture { pType :: CChar
                                     , pDescription :: Mpg123_string
                                     , pMime_type   :: Mpg123_string
                                     , pSize :: CSize
                                     , pData :: CString }
instance Storable Mpg123_picture where
  sizeOf    _ = (#size      mpg123_picture)
  alignment _ = (#alignment mpg123_picture)
  peek ptr = do
    pType        <- (#peek mpg123_picture,        type) ptr
    pDescription <- (#peek mpg123_picture, description) ptr
    pMime_type   <- (#peek mpg123_picture,   mime_type) ptr
    pSize        <- (#peek mpg123_picture,        size) ptr
    pData        <- (#peek mpg123_picture,        data) ptr
    pure $! (Mpg123_picture pType pDescription pMime_type pSize pData)
  poke ptr (Mpg123_picture pType pDescription pMime_type pSize pData) = do
    (#poke mpg123_picture,        type) ptr pType
    (#poke mpg123_picture, description) ptr pDescription
    (#poke mpg123_picture,   mime_type) ptr pMime_type
    (#poke mpg123_picture,        size) ptr pSize
    (#poke mpg123_picture,        data) ptr pData
type Mpg123_picture_ptr = Ptr Mpg123_picture

data Mpg123_id3v2 = Mpg123_id3v2
                    { tVersion :: CChar
                    , tTitle   :: Mpg123_string_ptr
                    , tArtist  :: Mpg123_string_ptr
                    , tAlbum   :: Mpg123_string_ptr
                    , tYear    :: Mpg123_string_ptr
                    , tGenre   :: Mpg123_string_ptr
                    , tComment :: Mpg123_string_ptr
                    , tComment_list :: Mpg123_text_ptr
                    , tComments :: CSize
                    , tText    :: Mpg123_text_ptr
                    , tTexts   :: CSize
                    , tExtra   :: Mpg123_text_ptr
                    , tExtras  :: CSize
                    , tPicture :: Mpg123_picture_ptr
                    , tPictures :: CSize }
instance Storable Mpg123_id3v2 where
  sizeOf    _ = (#size      mpg123_id3v2)
  alignment _ = (#alignment mpg123_id3v2)
  peek ptr = do
    tVersion      <- (#peek mpg123_id3v2,      version) ptr
    tTitle        <- (#peek mpg123_id3v2,        title) ptr
    tArtist       <- (#peek mpg123_id3v2,       artist) ptr
    tAlbum        <- (#peek mpg123_id3v2,        album) ptr
    tYear         <- (#peek mpg123_id3v2,         year) ptr
    tGenre        <- (#peek mpg123_id3v2,        genre) ptr
    tComment      <- (#peek mpg123_id3v2,      comment) ptr
    tComment_list <- (#peek mpg123_id3v2, comment_list) ptr
    tComments     <- (#peek mpg123_id3v2,     comments) ptr
    tText         <- (#peek mpg123_id3v2,         text) ptr
    tTexts        <- (#peek mpg123_id3v2,        texts) ptr
    tExtra        <- (#peek mpg123_id3v2,        extra) ptr
    tExtras       <- (#peek mpg123_id3v2,       extras) ptr
    tPicture      <- (#peek mpg123_id3v2,      picture) ptr
    tPictures     <- (#peek mpg123_id3v2,     pictures) ptr
    pure $! (Mpg123_id3v2 tVersion tTitle tArtist tAlbum tYear tGenre tComment tComment_list tComments tText tTexts tExtra tExtras tPicture tPictures)
  poke ptr (Mpg123_id3v2 tVersion tTitle tArtist tAlbum tYear tGenre tComment tComment_list tComments tText tTexts tExtra tExtras tPicture tPictures) = do
    (#poke mpg123_id3v2,      version) ptr tVersion
    (#poke mpg123_id3v2,        title) ptr tTitle
    (#poke mpg123_id3v2,       artist) ptr tArtist
    (#poke mpg123_id3v2,        album) ptr tAlbum
    (#poke mpg123_id3v2,         year) ptr tYear
    (#poke mpg123_id3v2,        genre) ptr tGenre
    (#poke mpg123_id3v2,      comment) ptr tComment
    (#poke mpg123_id3v2, comment_list) ptr tComment_list
    (#poke mpg123_id3v2,     comments) ptr tComments
    (#poke mpg123_id3v2,         text) ptr tText
    (#poke mpg123_id3v2,        texts) ptr tTexts
    (#poke mpg123_id3v2,        extra) ptr tExtra
    (#poke mpg123_id3v2,       extras) ptr tExtras
    (#poke mpg123_id3v2,      picture) ptr tPicture
    (#poke mpg123_id3v2,     pictures) ptr tPictures
type Mpg123_id3v2_ptr = Ptr Mpg123_id3v2

data Mpg123_id3v1 = Mpg123_id3v1 { t1Tag     :: CString -- ^ length 3
                                 , t1Title   :: CString -- ^ length 30
                                 , t1Artist  :: CString -- ^ length 30
                                 , t1Album   :: CString -- ^ length 30
                                 , t1Year    :: CString -- ^ length 4
                                 , t1Comment :: CString -- ^ length 30
                                 , t1Genre   :: CChar }
-- } mpg123_id3v1;
instance Storable Mpg123_id3v1 where
  sizeOf    _ = (#size      mpg123_id3v1)
  alignment _ = (#alignment mpg123_id3v1)
  peek ptr = do
    t1Tag     <- (#peek mpg123_id3v1,     tag) ptr
    t1Title   <- (#peek mpg123_id3v1,   title) ptr
    t1Artist  <- (#peek mpg123_id3v1,  artist) ptr
    t1Album   <- (#peek mpg123_id3v1,   album) ptr
    t1Year    <- (#peek mpg123_id3v1,    year) ptr
    t1Comment <- (#peek mpg123_id3v1, comment) ptr
    t1Genre   <- (#peek mpg123_id3v1,   genre) ptr
    pure $! (Mpg123_id3v1 t1Tag t1Title t1Artist t1Album t1Year t1Comment t1Genre)
  poke ptr (Mpg123_id3v1 t1Tag t1Title t1Artist t1Album t1Year t1Comment t1Genre) = do
    (#poke mpg123_id3v1,     tag) ptr t1Tag
    (#poke mpg123_id3v1,   title) ptr t1Title
    (#poke mpg123_id3v1,  artist) ptr t1Artist
    (#poke mpg123_id3v1,   album) ptr t1Album
    (#poke mpg123_id3v1,    year) ptr t1Year
    (#poke mpg123_id3v1, comment) ptr t1Comment
    (#poke mpg123_id3v1,   genre) ptr t1Genre
type Mpg123_id3v1_ptr = Ptr Mpg123_id3v1

mpg123_id3     = (#enum MPG123_ID3    , CInt)
mpg123_new_id3 = (#enum MPG123_NEW_ID3, CInt)
mpg123_icy     = (#enum MPG123_ICY    , CInt)
mpg123_new_icy = (#enum MPG123_NEW_ICY, CInt)

foreign import ccall unsafe "mpg123_meta_check"
  c_mpg123_meta_check :: Mpg123_handle_ptr -> IO CInt
foreign import ccall unsafe "mpg123_meta_free"
  c_mpg123_meta_free :: Mpg123_handle_ptr -> IO ()
foreign import ccall unsafe "mpg123_id3"
  c_mpg123_id3 :: Mpg123_handle_ptr -> Ptr Mpg123_id3v1_ptr -> Ptr Mpg123_id3v2_ptr -> IO CInt
foreign import ccall unsafe "mpg123_icy"
  c_mpg123_icy :: Mpg123_handle_ptr -> Ptr CString -> IO CInt
foreign import ccall unsafe "mpg123_icy2utf8"
  c_mpg123_icy2utf8 :: Ptr CString -> IO CString

data Mpg123_pars = Mpg123_pars
type Mpg123_pars_ptr = Ptr Mpg123_pars

foreign import ccall unsafe "mpg123_parnew"
  c_mpg123_parnew :: Mpg123_pars_ptr -> CString -> Ptr CInt -> IO Mpg123_handle_ptr
foreign import ccall unsafe "mpg123_new_pars"
  c_mpg123_new_pars :: Ptr CInt -> IO Mpg123_pars_ptr
foreign import ccall unsafe "mpg123_delete_pars"
  c_mpg123_delete_pars :: Mpg123_pars_ptr -> IO ()
foreign import ccall unsafe "mpg123_fmt_none"
  c_mpg123_fmt_none :: Mpg123_pars_ptr -> IO CInt
foreign import ccall unsafe "mpg123_fmt_all"
  c_mpg123_fmt_all :: Mpg123_pars_ptr -> IO CInt
foreign import ccall unsafe "mpg123_fmt"
  c_mpg123_fmt :: Mpg123_pars_ptr -> CLong -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "mpg123_fmt_support"
  c_mpg123_fmt_support :: Mpg123_pars_ptr -> CLong -> CInt -> IO CInt
foreign import ccall unsafe "mpg123_par"
  c_mpg123_par :: Mpg123_pars_ptr
               -> CInt -- ^ enum mpg123_parms
               -> CLong
               -> CDouble
               -> IO CInt
foreign import ccall unsafe "mpg123_getpar"
  c_mpg123_getpar :: Mpg123_pars_ptr -> CInt -- ^ enum mpg123_parms
                  -> Ptr CLong
                  -> Ptr CDouble
                  -> IO CInt
foreign import ccall unsafe "mpg123_replace_buffer"
  c_mpg123_replace_buffer :: Mpg123_handle_ptr -> CString -> CSize -> IO CInt
foreign import ccall unsafe "mpg123_outblock"
  c_mpg123_outblock :: Mpg123_handle_ptr -> IO CSize

type R_read  = CInt -> Ptr () -> CSize -> IO CSsize
type R_lseek = CInt -> COff -> CInt -> IO COff

type R_read_h  = Ptr () -> Ptr () -> CSize -> IO CSsize
type R_lseek_h = Ptr () -> COff -> CInt -> IO COff
type R_cleanup = Ptr () -> IO ()

foreign import ccall unsafe "mpg123_replace_reader"
  c_mpg123_replace_reader :: Mpg123_handle_ptr -> FunPtr R_read -> FunPtr R_lseek -> IO CInt

foreign import ccall unsafe "mpg123_replace_reader_handle"
  c_mpg123_replace_reader_handle :: Mpg123_handle_ptr -> FunPtr R_read_h -> FunPtr R_lseek_h -> FunPtr R_cleanup -> IO CInt
