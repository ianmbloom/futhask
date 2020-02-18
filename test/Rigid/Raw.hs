{-# LANGUAGE ForeignFunctionInterface #-}
module Rigid.Raw where
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32 Word64)
import Foreign.CTypes (CBool)
import Foreign.Ptr (Ptr)

-- Headers










-- Initialisation
foreign import ccall unsafe "futhark_get_num_sizes"
  get_num_sizes
    :: IO Int

foreign import ccall unsafe "futhark_get_size_name"
  get_size_name
    :: Int
    -> IO (Ptr Word8)

foreign import ccall unsafe "futhark_get_size_class"
  get_size_class
    :: Int
    -> IO (Ptr Word8)

data Futhark_context_config
foreign import ccall unsafe "futhark_context_config_new"
  context_config_new
    :: IO (Ptr Futhark_context_config)

foreign import ccall unsafe "futhark_context_config_free"
  context_config_free
    :: Ptr Futhark_context_config
    -> IO ()

foreign import ccall unsafe "futhark_context_config_add_build_option"
  context_config_add_build_option
    :: Ptr Futhark_context_config
    -> Ptr Word8
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

foreign import ccall unsafe "futhark_context_config_set_device"
  context_config_set_device
    :: Ptr Futhark_context_config
    -> Ptr Word8
    -> IO ()

foreign import ccall unsafe "futhark_context_config_set_platform"
  context_config_set_platform
    :: Ptr Futhark_context_config
    -> Ptr Word8
    -> IO ()

foreign import ccall unsafe "futhark_context_config_select_device_interactively"
  context_config_select_device_interactively
    :: Ptr Futhark_context_config
    -> IO ()

foreign import ccall unsafe "futhark_context_config_dump_program_to"
  context_config_dump_program_to
    :: Ptr Futhark_context_config
    -> Ptr Word8
    -> IO ()

foreign import ccall unsafe "futhark_context_config_load_program_from"
  context_config_load_program_from
    :: Ptr Futhark_context_config
    -> Ptr Word8
    -> IO ()

foreign import ccall unsafe "futhark_context_config_dump_binary_to"
  context_config_dump_binary_to
    :: Ptr Futhark_context_config
    -> Ptr Word8
    -> IO ()

foreign import ccall unsafe "futhark_context_config_load_binary_from"
  context_config_load_binary_from
    :: Ptr Futhark_context_config
    -> Ptr Word8
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

foreign import ccall unsafe "futhark_context_config_set_size"
  context_config_set_size
    :: Ptr Futhark_context_config
    -> Ptr Word8
    -> CSize
    -> IO Int

data Futhark_context
foreign import ccall unsafe "futhark_context_new"
  context_new
    :: Ptr Futhark_context_config
    -> IO (Ptr Futhark_context)

foreign import ccall unsafe "futhark_context_new_with_command_queue"
  context_new_with_command_queue
    :: Ptr Futhark_context_config
    -> CL_command_queue
    -> IO (Ptr Futhark_context)

foreign import ccall unsafe "futhark_context_free"
  context_free
    :: Ptr Futhark_context
    -> IO ()

foreign import ccall unsafe "futhark_context_sync"
  context_sync
    :: Ptr Futhark_context
    -> IO Int

foreign import ccall unsafe "futhark_context_get_error"
  context_get_error
    :: Ptr Futhark_context
    -> IO (Ptr Word8)

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

foreign import ccall unsafe "futhark_context_get_command_queue"
  context_get_command_queue
    :: Ptr Futhark_context
    -> IO CL_command_queue

-- Arrays
data Futhark_f32_1d
foreign import ccall unsafe "futhark_new_f32_1d"
  new_f32_1d
    :: Ptr Futhark_context
    -> Ptr Float
    -> Int64
    -> IO (Ptr Futhark_f32_1d)

foreign import ccall unsafe "futhark_new_raw_f32_1d"
  new_raw_f32_1d
    :: Ptr Futhark_context
    -> CL_mem
    -> Int
    -> Int64
    -> IO (Ptr Futhark_f32_1d)

foreign import ccall unsafe "futhark_free_f32_1d"
  free_f32_1d
    :: Ptr Futhark_context
    -> Ptr Futhark_f32_1d
    -> IO Int

foreign import ccall unsafe "futhark_values_f32_1d"
  values_f32_1d
    :: Ptr Futhark_context
    -> Ptr Futhark_f32_1d
    -> Ptr Float
    -> IO Int

foreign import ccall unsafe "futhark_values_raw_f32_1d"
  values_raw_f32_1d
    :: Ptr Futhark_context
    -> Ptr Futhark_f32_1d
    -> IO CL_mem

foreign import ccall unsafe "futhark_shape_f32_1d"
  shape_f32_1d
    :: Ptr Futhark_context
    -> Ptr Futhark_f32_1d
    -> IO (Ptr Int64)

data Futhark_f32_2d
foreign import ccall unsafe "futhark_new_f32_2d"
  new_f32_2d
    :: Ptr Futhark_context
    -> Ptr Float
    -> Int64
    -> Int64
    -> IO (Ptr Futhark_f32_2d)

foreign import ccall unsafe "futhark_new_raw_f32_2d"
  new_raw_f32_2d
    :: Ptr Futhark_context
    -> CL_mem
    -> Int
    -> Int64
    -> Int64
    -> IO (Ptr Futhark_f32_2d)

foreign import ccall unsafe "futhark_free_f32_2d"
  free_f32_2d
    :: Ptr Futhark_context
    -> Ptr Futhark_f32_2d
    -> IO Int

foreign import ccall unsafe "futhark_values_f32_2d"
  values_f32_2d
    :: Ptr Futhark_context
    -> Ptr Futhark_f32_2d
    -> Ptr Float
    -> IO Int

foreign import ccall unsafe "futhark_values_raw_f32_2d"
  values_raw_f32_2d
    :: Ptr Futhark_context
    -> Ptr Futhark_f32_2d
    -> IO CL_mem

foreign import ccall unsafe "futhark_shape_f32_2d"
  shape_f32_2d
    :: Ptr Futhark_context
    -> Ptr Futhark_f32_2d
    -> IO (Ptr Int64)

data Futhark_f32_3d
foreign import ccall unsafe "futhark_new_f32_3d"
  new_f32_3d
    :: Ptr Futhark_context
    -> Ptr Float
    -> Int64
    -> Int64
    -> Int64
    -> IO (Ptr Futhark_f32_3d)

foreign import ccall unsafe "futhark_new_raw_f32_3d"
  new_raw_f32_3d
    :: Ptr Futhark_context
    -> CL_mem
    -> Int
    -> Int64
    -> Int64
    -> Int64
    -> IO (Ptr Futhark_f32_3d)

foreign import ccall unsafe "futhark_free_f32_3d"
  free_f32_3d
    :: Ptr Futhark_context
    -> Ptr Futhark_f32_3d
    -> IO Int

foreign import ccall unsafe "futhark_values_f32_3d"
  values_f32_3d
    :: Ptr Futhark_context
    -> Ptr Futhark_f32_3d
    -> Ptr Float
    -> IO Int

foreign import ccall unsafe "futhark_values_raw_f32_3d"
  values_raw_f32_3d
    :: Ptr Futhark_context
    -> Ptr Futhark_f32_3d
    -> IO CL_mem

foreign import ccall unsafe "futhark_shape_f32_3d"
  shape_f32_3d
    :: Ptr Futhark_context
    -> Ptr Futhark_f32_3d
    -> IO (Ptr Int64)

data Futhark_i32_1d
foreign import ccall unsafe "futhark_new_i32_1d"
  new_i32_1d
    :: Ptr Futhark_context
    -> Ptr Int32
    -> Int64
    -> IO (Ptr Futhark_i32_1d)

foreign import ccall unsafe "futhark_new_raw_i32_1d"
  new_raw_i32_1d
    :: Ptr Futhark_context
    -> CL_mem
    -> Int
    -> Int64
    -> IO (Ptr Futhark_i32_1d)

foreign import ccall unsafe "futhark_free_i32_1d"
  free_i32_1d
    :: Ptr Futhark_context
    -> Ptr Futhark_i32_1d
    -> IO Int

foreign import ccall unsafe "futhark_values_i32_1d"
  values_i32_1d
    :: Ptr Futhark_context
    -> Ptr Futhark_i32_1d
    -> Ptr Int32
    -> IO Int

foreign import ccall unsafe "futhark_values_raw_i32_1d"
  values_raw_i32_1d
    :: Ptr Futhark_context
    -> Ptr Futhark_i32_1d
    -> IO CL_mem

foreign import ccall unsafe "futhark_shape_i32_1d"
  shape_i32_1d
    :: Ptr Futhark_context
    -> Ptr Futhark_i32_1d
    -> IO (Ptr Int64)

data Futhark_i8_1d
foreign import ccall unsafe "futhark_new_i8_1d"
  new_i8_1d
    :: Ptr Futhark_context
    -> Ptr Int8
    -> Int64
    -> IO (Ptr Futhark_i8_1d)

foreign import ccall unsafe "futhark_new_raw_i8_1d"
  new_raw_i8_1d
    :: Ptr Futhark_context
    -> CL_mem
    -> Int
    -> Int64
    -> IO (Ptr Futhark_i8_1d)

foreign import ccall unsafe "futhark_free_i8_1d"
  free_i8_1d
    :: Ptr Futhark_context
    -> Ptr Futhark_i8_1d
    -> IO Int

foreign import ccall unsafe "futhark_values_i8_1d"
  values_i8_1d
    :: Ptr Futhark_context
    -> Ptr Futhark_i8_1d
    -> Ptr Int8
    -> IO Int

foreign import ccall unsafe "futhark_values_raw_i8_1d"
  values_raw_i8_1d
    :: Ptr Futhark_context
    -> Ptr Futhark_i8_1d
    -> IO CL_mem

foreign import ccall unsafe "futhark_shape_i8_1d"
  shape_i8_1d
    :: Ptr Futhark_context
    -> Ptr Futhark_i8_1d
    -> IO (Ptr Int64)

-- Opaque values
data Futhark_opaque_arr_association_1d
foreign import ccall unsafe "futhark_free_opaque_arr_association_1d"
  free_opaque_arr_association_1d
    :: Ptr Futhark_context
    -> Ptr Futhark_opaque_arr_association_1d
    -> IO Int

data Futhark_opaque_arr_dXdt_1d
foreign import ccall unsafe "futhark_free_opaque_arr_dXdt_1d"
  free_opaque_arr_dXdt_1d
    :: Ptr Futhark_context
    -> Ptr Futhark_opaque_arr_dXdt_1d
    -> IO Int

data Futhark_opaque_arr_forceTorque_1d
foreign import ccall unsafe "futhark_free_opaque_arr_forceTorque_1d"
  free_opaque_arr_forceTorque_1d
    :: Ptr Futhark_context
    -> Ptr Futhark_opaque_arr_forceTorque_1d
    -> IO Int

data Futhark_opaque_arr_rigid_1d
foreign import ccall unsafe "futhark_free_opaque_arr_rigid_1d"
  free_opaque_arr_rigid_1d
    :: Ptr Futhark_context
    -> Ptr Futhark_opaque_arr_rigid_1d
    -> IO Int

data Futhark_opaque_box
foreign import ccall unsafe "futhark_free_opaque_box"
  free_opaque_box
    :: Ptr Futhark_context
    -> Ptr Futhark_opaque_box
    -> IO Int

-- Entry points
foreign import ccall unsafe "futhark_entry_rodNetworkPairInteraction"
  entry_rodNetworkPairInteraction
    :: Ptr Futhark_context
    -> Ptr (Ptr Futhark_opaque_arr_forceTorque_1d)
    -> Float
    -> Float
    -> Ptr Futhark_f32_2d
    -> Ptr Futhark_f32_3d
    -> Ptr Futhark_f32_2d
    -> Ptr Futhark_opaque_box
    -> Ptr Futhark_opaque_arr_rigid_1d
    -> Ptr Futhark_opaque_arr_association_1d
    -> IO Int

foreign import ccall unsafe "futhark_entry_asgd"
  entry_asgd
    :: Ptr Futhark_context
    -> Ptr Float
    -> Ptr (Ptr Futhark_f32_2d)
    -> Ptr (Ptr Futhark_f32_3d)
    -> Ptr (Ptr Futhark_f32_2d)
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Int32
    -> Int32
    -> Ptr Futhark_f32_2d
    -> Ptr Futhark_f32_3d
    -> Ptr Futhark_f32_2d
    -> Ptr Futhark_f32_2d
    -> Ptr Futhark_f32_2d
    -> Int32
    -> IO Int

foreign import ccall unsafe "futhark_entry_testNet"
  entry_testNet
    :: Ptr Futhark_context
    -> Ptr Float
    -> Ptr Futhark_f32_2d
    -> Ptr Futhark_f32_3d
    -> Ptr Futhark_f32_2d
    -> Ptr Futhark_f32_2d
    -> Ptr Futhark_f32_2d
    -> IO Int

foreign import ccall unsafe "futhark_entry_bulkEval"
  entry_bulkEval
    :: Ptr Futhark_context
    -> Ptr (Ptr Futhark_f32_2d)
    -> Ptr Futhark_f32_2d
    -> Ptr Futhark_f32_3d
    -> Ptr Futhark_f32_2d
    -> Ptr Futhark_f32_2d
    -> IO Int

foreign import ccall unsafe "futhark_entry_sgd"
  entry_sgd
    :: Ptr Futhark_context
    -> Ptr (Ptr Futhark_f32_2d)
    -> Ptr (Ptr Futhark_f32_3d)
    -> Ptr (Ptr Futhark_f32_2d)
    -> Float
    -> Float
    -> Int32
    -> Int32
    -> Ptr Futhark_f32_2d
    -> Ptr Futhark_f32_3d
    -> Ptr Futhark_f32_2d
    -> Ptr Futhark_f32_2d
    -> Ptr Futhark_f32_2d
    -> Int32
    -> IO Int

foreign import ccall unsafe "futhark_entry_gradientDescent"
  entry_gradientDescent
    :: Ptr Futhark_context
    -> Ptr Float
    -> Ptr (Ptr Futhark_f32_2d)
    -> Ptr (Ptr Futhark_f32_3d)
    -> Ptr (Ptr Futhark_f32_2d)
    -> Float
    -> Ptr Futhark_f32_2d
    -> Ptr Futhark_f32_3d
    -> Ptr Futhark_f32_2d
    -> Ptr Futhark_f32_2d
    -> Ptr Futhark_f32_2d
    -> Int32
    -> IO Int

foreign import ccall unsafe "futhark_entry_dXdtsAndUpdatedSystem"
  entry_dXdtsAndUpdatedSystem
    :: Ptr Futhark_context
    -> Ptr (Ptr Futhark_opaque_arr_dXdt_1d)
    -> Ptr (Ptr Futhark_opaque_arr_rigid_1d)
    -> Float
    -> Ptr Futhark_opaque_box
    -> Ptr Futhark_opaque_arr_rigid_1d
    -> Ptr Futhark_opaque_arr_forceTorque_1d
    -> IO Int

foreign import ccall unsafe "futhark_entry_dXdts"
  entry_dXdts
    :: Ptr Futhark_context
    -> Ptr (Ptr Futhark_opaque_arr_dXdt_1d)
    -> Ptr Futhark_opaque_arr_rigid_1d
    -> Ptr Futhark_opaque_arr_forceTorque_1d
    -> IO Int

foreign import ccall unsafe "futhark_entry_rk4weightedUpdate"
  entry_rk4weightedUpdate
    :: Ptr Futhark_context
    -> Ptr (Ptr Futhark_opaque_arr_rigid_1d)
    -> Float
    -> Ptr Futhark_opaque_box
    -> Ptr Futhark_opaque_arr_rigid_1d
    -> Ptr Futhark_opaque_arr_dXdt_1d
    -> Ptr Futhark_opaque_arr_dXdt_1d
    -> Ptr Futhark_opaque_arr_dXdt_1d
    -> Ptr Futhark_opaque_arr_dXdt_1d
    -> IO Int

foreign import ccall unsafe "futhark_entry_coordinatesFrom"
  entry_coordinatesFrom
    :: Ptr Futhark_context
    -> Ptr (Ptr Futhark_f32_1d)
    -> Ptr (Ptr Futhark_f32_1d)
    -> Ptr (Ptr Futhark_f32_1d)
    -> Ptr (Ptr Futhark_f32_1d)
    -> Ptr (Ptr Futhark_f32_1d)
    -> Ptr (Ptr Futhark_f32_1d)
    -> Ptr (Ptr Futhark_f32_1d)
    -> Ptr (Ptr Futhark_f32_1d)
    -> Ptr (Ptr Futhark_f32_1d)
    -> Ptr (Ptr Futhark_f32_1d)
    -> Ptr (Ptr Futhark_f32_1d)
    -> Ptr (Ptr Futhark_f32_1d)
    -> Ptr (Ptr Futhark_f32_1d)
    -> Ptr Futhark_opaque_arr_rigid_1d
    -> IO Int

foreign import ccall unsafe "futhark_entry_toAssociations"
  entry_toAssociations
    :: Ptr Futhark_context
    -> Ptr (Ptr Futhark_opaque_arr_association_1d)
    -> Ptr Futhark_i32_1d
    -> Ptr Futhark_i32_1d
    -> Ptr Futhark_i8_1d
    -> Ptr Futhark_i8_1d
    -> Ptr Futhark_i8_1d
    -> IO Int

foreign import ccall unsafe "futhark_entry_toRigids"
  entry_toRigids
    :: Ptr Futhark_context
    -> Ptr (Ptr Futhark_opaque_arr_rigid_1d)
    -> Ptr Futhark_f32_1d
    -> Ptr Futhark_f32_1d
    -> Ptr Futhark_f32_1d
    -> Ptr Futhark_f32_1d
    -> Ptr Futhark_f32_1d
    -> Ptr Futhark_f32_1d
    -> Ptr Futhark_f32_1d
    -> Ptr Futhark_f32_1d
    -> Ptr Futhark_f32_1d
    -> Ptr Futhark_f32_1d
    -> Ptr Futhark_f32_1d
    -> Ptr Futhark_f32_1d
    -> Ptr Futhark_f32_1d
    -> Ptr Futhark_f32_1d
    -> Ptr Futhark_f32_1d
    -> Ptr Futhark_f32_1d
    -> Ptr Futhark_f32_1d
    -> Ptr Futhark_i32_1d
    -> IO Int

foreign import ccall unsafe "futhark_entry_addForceTorques"
  entry_addForceTorques
    :: Ptr Futhark_context
    -> Ptr (Ptr Futhark_opaque_arr_forceTorque_1d)
    -> Ptr Futhark_opaque_arr_forceTorque_1d
    -> Ptr Futhark_opaque_arr_forceTorque_1d
    -> IO Int

foreign import ccall unsafe "futhark_entry_toForceTorques"
  entry_toForceTorques
    :: Ptr Futhark_context
    -> Ptr (Ptr Futhark_opaque_arr_forceTorque_1d)
    -> Ptr Futhark_f32_1d
    -> Ptr Futhark_f32_1d
    -> Ptr Futhark_f32_1d
    -> Ptr Futhark_f32_1d
    -> Ptr Futhark_f32_1d
    -> Ptr Futhark_f32_1d
    -> IO Int

foreign import ccall unsafe "futhark_entry_box"
  entry_box
    :: Ptr Futhark_context
    -> Ptr (Ptr Futhark_opaque_box)
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> CBool
    -> CBool
    -> CBool
    -> IO Int

-- Miscellaneous
foreign import ccall unsafe "futhark_debugging_report"
  debugging_report
    :: Ptr Futhark_context
    -> IO ()
