{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
module Rigid.Types where
import qualified Rigid.Raw as Raw
import Rigid.Utils
import Rigid.TypeClasses

newtype F32_1d c = F32_1d (F.ForeignPtr Raw.F32_1d)
instance FutharkObject (F32_1d c) Raw.F32_1d where
  wrapper = F32_1d
  freeFO = Raw.free_f32_1d
  withFO (F32_1d fp) = F.withForeignPtr fp
instance FutharkArray (F32_1d c) M.Sz1 Float where
  shape  = to1d Raw.shape_f32_1d
  new    = from1d Raw.new_f32_1d
  values = Raw.values_f32_1d

newtype F32_2d c = F32_2d (F.ForeignPtr Raw.F32_2d)
instance FutharkObject (F32_2d c) Raw.F32_2d where
  wrapper = F32_2d
  freeFO = Raw.free_f32_2d
  withFO (F32_2d fp) = F.withForeignPtr fp
instance FutharkArray (F32_2d c) M.Sz2 Float where
  shape  = to2d Raw.shape_f32_2d
  new    = from2d Raw.new_f32_2d
  values = Raw.values_f32_2d

newtype F32_3d c = F32_3d (F.ForeignPtr Raw.F32_3d)
instance FutharkObject (F32_3d c) Raw.F32_3d where
  wrapper = F32_3d
  freeFO = Raw.free_f32_3d
  withFO (F32_3d fp) = F.withForeignPtr fp
instance FutharkArray (F32_3d c) M.Sz3 Float where
  shape  = to3d Raw.shape_f32_3d
  new    = from3d Raw.new_f32_3d
  values = Raw.values_f32_3d

newtype I32_1d c = I32_1d (F.ForeignPtr Raw.I32_1d)
instance FutharkObject (I32_1d c) Raw.I32_1d where
  wrapper = I32_1d
  freeFO = Raw.free_i32_1d
  withFO (I32_1d fp) = F.withForeignPtr fp
instance FutharkArray (I32_1d c) M.Sz1 Int32 where
  shape  = to1d Raw.shape_i32_1d
  new    = from1d Raw.new_i32_1d
  values = Raw.values_i32_1d

newtype I8_1d c = I8_1d (F.ForeignPtr Raw.I8_1d)
instance FutharkObject (I8_1d c) Raw.I8_1d where
  wrapper = I8_1d
  freeFO = Raw.free_i8_1d
  withFO (I8_1d fp) = F.withForeignPtr fp
instance FutharkArray (I8_1d c) M.Sz1 Int8 where
  shape  = to1d Raw.shape_i8_1d
  new    = from1d Raw.new_i8_1d
  values = Raw.values_i8_1d

newtype Opaque_arr_association_1d c = Opaque_arr_association_1d (F.ForeignPtr Raw.Opaque_arr_association_1d)
instance FutharkObject (Opaque_arr_association_1d c) Raw.Opaque_arr_association_1d where
  wrapper = Opaque_arr_association_1d
  freeFO = Raw.free_opaque_arr_association_1d
  withFO (Opaque_arr_association_1d fp) = F.withForeignPtr fp

newtype Opaque_arr_dXdt_1d c = Opaque_arr_dXdt_1d (F.ForeignPtr Raw.Opaque_arr_dXdt_1d)
instance FutharkObject (Opaque_arr_dXdt_1d c) Raw.Opaque_arr_dXdt_1d where
  wrapper = Opaque_arr_dXdt_1d
  freeFO = Raw.free_opaque_arr_dXdt_1d
  withFO (Opaque_arr_dXdt_1d fp) = F.withForeignPtr fp

newtype Opaque_arr_forceTorque_1d c = Opaque_arr_forceTorque_1d (F.ForeignPtr Raw.Opaque_arr_forceTorque_1d)
instance FutharkObject (Opaque_arr_forceTorque_1d c) Raw.Opaque_arr_forceTorque_1d where
  wrapper = Opaque_arr_forceTorque_1d
  freeFO = Raw.free_opaque_arr_forceTorque_1d
  withFO (Opaque_arr_forceTorque_1d fp) = F.withForeignPtr fp

newtype Opaque_arr_rigid_1d c = Opaque_arr_rigid_1d (F.ForeignPtr Raw.Opaque_arr_rigid_1d)
instance FutharkObject (Opaque_arr_rigid_1d c) Raw.Opaque_arr_rigid_1d where
  wrapper = Opaque_arr_rigid_1d
  freeFO = Raw.free_opaque_arr_rigid_1d
  withFO (Opaque_arr_rigid_1d fp) = F.withForeignPtr fp

newtype Opaque_box c = Opaque_box (F.ForeignPtr Raw.Opaque_box)
instance FutharkObject (Opaque_box c) Raw.Opaque_box where
  wrapper = Opaque_box
  freeFO = Raw.free_opaque_box
  withFO (Opaque_box fp) = F.withForeignPtr fp
