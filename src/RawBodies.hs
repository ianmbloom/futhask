{-# LANGUAGE QuasiQuotes #-}


module RawBodies where
import Text.RawString.QQ
import Type

commonRawBody = [r|
data Futhark_context_config

foreign import ccall unsafe "futhark_context_config_new"
  context_config_new
    :: IO (Ptr Futhark_context_config)
foreign import ccall unsafe "futhark_context_config_free"
  context_config_free
    :: Ptr Futhark_context_config
    -> IO ()
foreign import ccall unsafe "futhark_context_config_set_debugging"
  context_config_set_debugging
    :: Ptr Futhark_context_config
    -> Int
    -> IO ()
foreign import ccall unsafe "futhark_context_config_set_profiling"
  context_config_set_profiling
    :: Ptr Futhark_context_config
    -> Int
    -> IO ()
foreign import ccall unsafe "futhark_context_config_set_logging"
  context_config_set_logging
    :: Ptr Futhark_context_config
    -> Int
    -> IO ()
foreign import ccall unsafe "futhark_context_config_set_tuning_param"
  context_config_set_tuning_param
    :: Ptr Futhark_context_config
    -> Ptr CChar
    -> CSize
    -> IO Int

data Futhark_context
foreign import ccall unsafe "futhark_context_new"
  context_new
    :: Ptr Futhark_context_config
    -> IO (Ptr Futhark_context)
foreign import ccall unsafe "futhark_context_free"
  context_free
    :: Ptr Futhark_context
    -> IO ()
foreign import ccall unsafe "futhark_get_tuning_param_count"
  get_tuning_param_count
    :: IO Int
foreign import ccall unsafe "futhark_get_tuning_param_name"
  get_tuning_param_name
    :: Int
    -> IO (Ptr CChar)
foreign import ccall unsafe "futhark_get_tuning_param_class"
  get_tuning_param_class
    :: Int
    -> IO (Ptr CChar)

--Miscellaneous
foreign import ccall unsafe "futhark_context_sync"
  context_sync
    :: Ptr Futhark_context
    -> IO Int
foreign import ccall unsafe "futhark_context_config_set_cache_file"
  futhark_context_config_set_cache_file
    :: Ptr Futhark_context_config
    -> Ptr CChar
    -> IO ()
foreign import ccall unsafe "futhark_context_report"
  context_report
    :: Ptr Futhark_context
    -> IO (Ptr CChar)
foreign import ccall unsafe "futhark_context_get_error"
  context_get_error
    :: Ptr Futhark_context
    -> IO (Ptr CChar)
foreign import ccall unsafe "futhark_context_set_logging_file"
  context_set_logging_file
    :: Ptr Futhark_context
    -> Ptr CFile
    -> IO ()
foreign import ccall unsafe "futhark_context_pause_profiling"
  context_pause_profiling
    :: Ptr Futhark_context
    -> IO ()
foreign import ccall unsafe "futhark_context_unpause_profiling"
  context_unpause_profiling
    :: Ptr Futhark_context
    -> IO ()
foreign import ccall unsafe "futhark_context_clear_caches"
  context_clear_caches
    :: Ptr Futhark_context
    -> IO Int

|]

rawBody Cuda = [r|
foreign import ccall unsafe "futhark_context_config_add_nvrtc_option"
  context_config_add_nvrtc_option
    :: Ptr Futhark_context_config
    -> Ptr CChar
    -> IO ()
foreign import ccall unsafe "futhark_context_config_set_device"
  context_config_set_device
    :: Ptr Futhark_context_config
    -> Ptr CChar
    -> IO ()
foreign import ccall unsafe "futhark_context_config_dump_program_to"
  context_config_dump_program_to
    :: Ptr Futhark_context_config
    -> Ptr CChar
    -> IO ()
foreign import ccall unsafe "futhark_context_config_load_program_from"
  context_config_load_program_from
    :: Ptr Futhark_context_config
    -> Ptr CChar
    -> IO ()
foreign import ccall unsafe "futhark_context_config_dump_ptx_to"
  context_config_dump_ptx_to
    :: Ptr Futhark_context_config
    -> Ptr CChar
    -> IO ()
foreign import ccall unsafe "futhark_context_config_load_ptx_from"
  context_config_load_ptx_from
    :: Ptr Futhark_context_config
    -> Ptr CChar
    -> IO ()
foreign import ccall unsafe "futhark_context_config_set_default_group_size"
  context_config_set_default_group_size
    :: Ptr Futhark_context_config
    -> Int
    -> IO ()
foreign import ccall unsafe "futhark_context_config_set_default_num_groups"
  context_config_set_default_num_groups
    :: Ptr Futhark_context_config
    -> Int
    -> IO ()
foreign import ccall unsafe "futhark_context_config_set_default_tile_size"
  context_config_set_default_tile_size
    :: Ptr Futhark_context_config
    -> Int
    -> IO ()
foreign import ccall unsafe "futhark_context_config_set_default_reg_tile_size"
  context_config_set_default_reg_tile_size
    :: Ptr Futhark_context_config
    -> Int
    -> IO ()
foreign import ccall unsafe "futhark_context_config_set_default_threshold"
  context_config_set_default_threshold
    :: Ptr Futhark_context_config
    -> Int
    -> IO ()
|]
rawBody OpenCL = [r|
foreign import ccall unsafe "futhark_context_config_add_build_option"
  context_config_add_build_option
    :: Ptr Futhark_context_config
    -> Ptr CChar
    -> IO ()
foreign import ccall unsafe "futhark_context_config_set_platform"
  context_config_set_platform
    :: Ptr Futhark_context_config
    -> Ptr CChar
    -> IO ()
foreign import ccall unsafe "futhark_context_config_load_binary_from"
  context_config_load_binary_from
    :: Ptr Futhark_context_config
    -> Ptr CChar
    -> IO ()
foreign import ccall unsafe "futhark_context_config_dump_binary_to"
  context_config_dump_binary_to
    :: Ptr Futhark_context_config
    -> Ptr CChar
    -> IO ()
foreign import ccall unsafe "futhark_context_config_set_device"
  context_config_set_device
    :: Ptr Futhark_context_config
    -> Ptr CChar
    -> IO ()
foreign import ccall unsafe "futhark_context_config_load_program_from"
  context_config_load_program_from
    :: Ptr Futhark_context_config
    -> Ptr CChar
    -> IO ()
foreign import ccall unsafe "futhark_context_config_set_default_group_size"
  context_config_set_default_group_size
    :: Ptr Futhark_context_config
    -> Int
    -> IO ()
foreign import ccall unsafe "futhark_context_config_set_default_num_groups"
  context_config_set_default_num_groups
    :: Ptr Futhark_context_config
    -> Int
    -> IO ()
foreign import ccall unsafe "futhark_context_config_set_default_tile_size"
  context_config_set_default_tile_size
    :: Ptr Futhark_context_config
    -> Int
    -> IO ()
foreign import ccall unsafe "futhark_context_config_set_default_threshold"
  context_config_set_default_threshold
    :: Ptr Futhark_context_config
    -> Int
    -> IO ()
|]
rawBody C = [r|
|]
