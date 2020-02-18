
module Rigid.Entries where
import qualified Rigid.Raw as Raw
import qualified Rigid.Context as C
import qualified Rigid.FT as FT
import qualified Rigid.Utils as U
import qualified Rigid.Types as T
import qualified Foreign as F

rodNetworkPairInteraction
  :: Float
  -> Float
  -> T.F32_2d c
  -> T.F32_3d c
  -> T.F32_2d c
  -> T.Opaque_box c
  -> T.Opaque_arr_rigid_1d c
  -> T.Opaque_arr_association_1d c
  -> FT.FT c (T.Opaque_arr_forceTorque_1d c)
rodNetworkPairInteraction in0 in1 in2 in3 in4 in5 in6 in7
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in2 $ \in2'
  -> T.withFO in3 $ \in3'
  -> T.withFO in4 $ \in4'
  -> T.withFO in5 $ \in5'
  -> T.withFO in6 $ \in6'
  -> T.withFO in7 $ \in7'
  -> F.malloc >>= $ \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_rodNetworkPairInteraction context' out0 in0 in1 in2' in3' in4' in5' in6' in7')
  >> U.peekFreeWrapIn context out0

asgd
  :: Float
  -> Float
  -> Float
  -> Float
  -> Float
  -> Int32
  -> Int32
  -> T.F32_2d c
  -> T.F32_3d c
  -> T.F32_2d c
  -> T.F32_2d c
  -> T.F32_2d c
  -> Int32
  -> FT.FT c (Float, T.F32_2d c, T.F32_3d c, T.F32_2d c)
asgd in0 in1 in2 in3 in4 in5 in6 in7 in8 in9 in10 in11 in12
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in7 $ \in7'
  -> T.withFO in8 $ \in8'
  -> T.withFO in9 $ \in9'
  -> T.withFO in10 $ \in10'
  -> T.withFO in11 $ \in11'
  -> F.malloc >>= $ \out0
  -> F.malloc >>= $ \out1
  -> F.malloc >>= $ \out2
  -> F.malloc >>= $ \out3
  -> C.inContextWithError context (\context'
  -> Raw.entry_asgd context' out0 out1 out2 out3 in0 in1 in2 in3 in4 in5 in6 in7' in8' in9' in10' in11' in12)
  >> U.peekFree out0 >>= \out0'
  -> U.peekFreeWrapIn context out1 >>= \out1'
  -> U.peekFreeWrapIn context out2 >>= \out2'
  -> U.peekFreeWrapIn context out3 >>= \out3'
  -> return (out0', out1', out2', out3')

testNet
  :: T.F32_2d c
  -> T.F32_3d c
  -> T.F32_2d c
  -> T.F32_2d c
  -> T.F32_2d c
  -> FT.FT c Float
testNet in0 in1 in2 in3 in4
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> T.withFO in1 $ \in1'
  -> T.withFO in2 $ \in2'
  -> T.withFO in3 $ \in3'
  -> T.withFO in4 $ \in4'
  -> F.malloc >>= $ \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_testNet context' out0 in0' in1' in2' in3' in4')
  >> U.peekFree out0

bulkEval
  :: T.F32_2d c
  -> T.F32_3d c
  -> T.F32_2d c
  -> T.F32_2d c
  -> FT.FT c (T.F32_2d c)
bulkEval in0 in1 in2 in3
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> T.withFO in1 $ \in1'
  -> T.withFO in2 $ \in2'
  -> T.withFO in3 $ \in3'
  -> F.malloc >>= $ \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_bulkEval context' out0 in0' in1' in2' in3')
  >> U.peekFreeWrapIn context out0

sgd
  :: Float
  -> Float
  -> Int32
  -> Int32
  -> T.F32_2d c
  -> T.F32_3d c
  -> T.F32_2d c
  -> T.F32_2d c
  -> T.F32_2d c
  -> Int32
  -> FT.FT c (T.F32_2d c, T.F32_3d c, T.F32_2d c)
sgd in0 in1 in2 in3 in4 in5 in6 in7 in8 in9
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in4 $ \in4'
  -> T.withFO in5 $ \in5'
  -> T.withFO in6 $ \in6'
  -> T.withFO in7 $ \in7'
  -> T.withFO in8 $ \in8'
  -> F.malloc >>= $ \out0
  -> F.malloc >>= $ \out1
  -> F.malloc >>= $ \out2
  -> C.inContextWithError context (\context'
  -> Raw.entry_sgd context' out0 out1 out2 in0 in1 in2 in3 in4' in5' in6' in7' in8' in9)
  >> U.peekFreeWrapIn context out0 >>= \out0'
  -> U.peekFreeWrapIn context out1 >>= \out1'
  -> U.peekFreeWrapIn context out2 >>= \out2'
  -> return (out0', out1', out2')

gradientDescent
  :: Float
  -> T.F32_2d c
  -> T.F32_3d c
  -> T.F32_2d c
  -> T.F32_2d c
  -> T.F32_2d c
  -> Int32
  -> FT.FT c (Float, T.F32_2d c, T.F32_3d c, T.F32_2d c)
gradientDescent in0 in1 in2 in3 in4 in5 in6
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in1 $ \in1'
  -> T.withFO in2 $ \in2'
  -> T.withFO in3 $ \in3'
  -> T.withFO in4 $ \in4'
  -> T.withFO in5 $ \in5'
  -> F.malloc >>= $ \out0
  -> F.malloc >>= $ \out1
  -> F.malloc >>= $ \out2
  -> F.malloc >>= $ \out3
  -> C.inContextWithError context (\context'
  -> Raw.entry_gradientDescent context' out0 out1 out2 out3 in0 in1' in2' in3' in4' in5' in6)
  >> U.peekFree out0 >>= \out0'
  -> U.peekFreeWrapIn context out1 >>= \out1'
  -> U.peekFreeWrapIn context out2 >>= \out2'
  -> U.peekFreeWrapIn context out3 >>= \out3'
  -> return (out0', out1', out2', out3')

dXdtsAndUpdatedSystem
  :: Float
  -> T.Opaque_box c
  -> T.Opaque_arr_rigid_1d c
  -> T.Opaque_arr_forceTorque_1d c
  -> FT.FT c (T.Opaque_arr_dXdt_1d c, T.Opaque_arr_rigid_1d c)
dXdtsAndUpdatedSystem in0 in1 in2 in3
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in1 $ \in1'
  -> T.withFO in2 $ \in2'
  -> T.withFO in3 $ \in3'
  -> F.malloc >>= $ \out0
  -> F.malloc >>= $ \out1
  -> C.inContextWithError context (\context'
  -> Raw.entry_dXdtsAndUpdatedSystem context' out0 out1 in0 in1' in2' in3')
  >> U.peekFreeWrapIn context out0 >>= \out0'
  -> U.peekFreeWrapIn context out1 >>= \out1'
  -> return (out0', out1')

dXdts
  :: T.Opaque_arr_rigid_1d c
  -> T.Opaque_arr_forceTorque_1d c
  -> FT.FT c (T.Opaque_arr_dXdt_1d c)
dXdts in0 in1
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> T.withFO in1 $ \in1'
  -> F.malloc >>= $ \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_dXdts context' out0 in0' in1')
  >> U.peekFreeWrapIn context out0

rk4weightedUpdate
  :: Float
  -> T.Opaque_box c
  -> T.Opaque_arr_rigid_1d c
  -> T.Opaque_arr_dXdt_1d c
  -> T.Opaque_arr_dXdt_1d c
  -> T.Opaque_arr_dXdt_1d c
  -> T.Opaque_arr_dXdt_1d c
  -> FT.FT c (T.Opaque_arr_rigid_1d c)
rk4weightedUpdate in0 in1 in2 in3 in4 in5 in6
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in1 $ \in1'
  -> T.withFO in2 $ \in2'
  -> T.withFO in3 $ \in3'
  -> T.withFO in4 $ \in4'
  -> T.withFO in5 $ \in5'
  -> T.withFO in6 $ \in6'
  -> F.malloc >>= $ \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_rk4weightedUpdate context' out0 in0 in1' in2' in3' in4' in5' in6')
  >> U.peekFreeWrapIn context out0

coordinatesFrom
  :: T.Opaque_arr_rigid_1d c
  -> FT.FT c (T.F32_1d c, T.F32_1d c, T.F32_1d c, T.F32_1d c, T.F32_1d c, T.F32_1d c, T.F32_1d c, T.F32_1d c, T.F32_1d c, T.F32_1d c, T.F32_1d c, T.F32_1d c, T.F32_1d c)
coordinatesFrom in0
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> F.malloc >>= $ \out0
  -> F.malloc >>= $ \out1
  -> F.malloc >>= $ \out2
  -> F.malloc >>= $ \out3
  -> F.malloc >>= $ \out4
  -> F.malloc >>= $ \out5
  -> F.malloc >>= $ \out6
  -> F.malloc >>= $ \out7
  -> F.malloc >>= $ \out8
  -> F.malloc >>= $ \out9
  -> F.malloc >>= $ \out10
  -> F.malloc >>= $ \out11
  -> F.malloc >>= $ \out12
  -> C.inContextWithError context (\context'
  -> Raw.entry_coordinatesFrom context' out0 out1 out2 out3 out4 out5 out6 out7 out8 out9 out10 out11 out12 in0')
  >> U.peekFreeWrapIn context out0 >>= \out0'
  -> U.peekFreeWrapIn context out1 >>= \out1'
  -> U.peekFreeWrapIn context out2 >>= \out2'
  -> U.peekFreeWrapIn context out3 >>= \out3'
  -> U.peekFreeWrapIn context out4 >>= \out4'
  -> U.peekFreeWrapIn context out5 >>= \out5'
  -> U.peekFreeWrapIn context out6 >>= \out6'
  -> U.peekFreeWrapIn context out7 >>= \out7'
  -> U.peekFreeWrapIn context out8 >>= \out8'
  -> U.peekFreeWrapIn context out9 >>= \out9'
  -> U.peekFreeWrapIn context out10 >>= \out10'
  -> U.peekFreeWrapIn context out11 >>= \out11'
  -> U.peekFreeWrapIn context out12 >>= \out12'
  -> return (out0', out1', out2', out3', out4', out5', out6', out7', out8', out9', out10', out11', out12')

toAssociations
  :: T.I32_1d c
  -> T.I32_1d c
  -> T.I8_1d c
  -> T.I8_1d c
  -> T.I8_1d c
  -> FT.FT c (T.Opaque_arr_association_1d c)
toAssociations in0 in1 in2 in3 in4
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> T.withFO in1 $ \in1'
  -> T.withFO in2 $ \in2'
  -> T.withFO in3 $ \in3'
  -> T.withFO in4 $ \in4'
  -> F.malloc >>= $ \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_toAssociations context' out0 in0' in1' in2' in3' in4')
  >> U.peekFreeWrapIn context out0

toRigids
  :: T.F32_1d c
  -> T.F32_1d c
  -> T.F32_1d c
  -> T.F32_1d c
  -> T.F32_1d c
  -> T.F32_1d c
  -> T.F32_1d c
  -> T.F32_1d c
  -> T.F32_1d c
  -> T.F32_1d c
  -> T.F32_1d c
  -> T.F32_1d c
  -> T.F32_1d c
  -> T.F32_1d c
  -> T.F32_1d c
  -> T.F32_1d c
  -> T.F32_1d c
  -> T.I32_1d c
  -> FT.FT c (T.Opaque_arr_rigid_1d c)
toRigids in0 in1 in2 in3 in4 in5 in6 in7 in8 in9 in10 in11 in12 in13 in14 in15 in16 in17
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> T.withFO in1 $ \in1'
  -> T.withFO in2 $ \in2'
  -> T.withFO in3 $ \in3'
  -> T.withFO in4 $ \in4'
  -> T.withFO in5 $ \in5'
  -> T.withFO in6 $ \in6'
  -> T.withFO in7 $ \in7'
  -> T.withFO in8 $ \in8'
  -> T.withFO in9 $ \in9'
  -> T.withFO in10 $ \in10'
  -> T.withFO in11 $ \in11'
  -> T.withFO in12 $ \in12'
  -> T.withFO in13 $ \in13'
  -> T.withFO in14 $ \in14'
  -> T.withFO in15 $ \in15'
  -> T.withFO in16 $ \in16'
  -> T.withFO in17 $ \in17'
  -> F.malloc >>= $ \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_toRigids context' out0 in0' in1' in2' in3' in4' in5' in6' in7' in8' in9' in10' in11' in12' in13' in14' in15' in16' in17')
  >> U.peekFreeWrapIn context out0

addForceTorques
  :: T.Opaque_arr_forceTorque_1d c
  -> T.Opaque_arr_forceTorque_1d c
  -> FT.FT c (T.Opaque_arr_forceTorque_1d c)
addForceTorques in0 in1
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> T.withFO in1 $ \in1'
  -> F.malloc >>= $ \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_addForceTorques context' out0 in0' in1')
  >> U.peekFreeWrapIn context out0

toForceTorques
  :: T.F32_1d c
  -> T.F32_1d c
  -> T.F32_1d c
  -> T.F32_1d c
  -> T.F32_1d c
  -> T.F32_1d c
  -> FT.FT c (T.Opaque_arr_forceTorque_1d c)
toForceTorques in0 in1 in2 in3 in4 in5
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> T.withFO in1 $ \in1'
  -> T.withFO in2 $ \in2'
  -> T.withFO in3 $ \in3'
  -> T.withFO in4 $ \in4'
  -> T.withFO in5 $ \in5'
  -> F.malloc >>= $ \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_toForceTorques context' out0 in0' in1' in2' in3' in4' in5')
  >> U.peekFreeWrapIn context out0

box
  :: Float
  -> Float
  -> Float
  -> Float
  -> Float
  -> Float
  -> CBool
  -> CBool
  -> CBool
  -> FT.FT c (T.Opaque_box c)
box in0 in1 in2 in3 in4 in5 in6 in7 in8
  =  FT.unsafeLiftFromIO $ \context
  -> F.malloc >>= $ \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_box context' out0 in0 in1 in2 in3 in4 in5 in6 in7 in8)
  >> U.peekFreeWrapIn context out0
