
module TestModule.Entries where
import qualified TestModule.Raw as Raw
import qualified TestModule.Context as C
import qualified TestModule.FT as FT
import TestModule.Types
import qualified Foreign as F

rodNetworkPairInteraction
  :: Float
  -> Float
  -> F32_2d c
  -> F32_3d c
  -> F32_2d c
  -> Opaque_box c
  -> Opaque_arr_rigid_1d c
  -> Opaque_arr_association_1d c
  -> FT.FT c (Opaque_arr_forceTorque_1d c)
rodNetworkPairInteraction in0 in1 in2 in3 in4 in5 in6 in7
  =  FT.unsafeLiftFromIO $ \context
  -> C.withFO in2 $ \in2'
  -> C.withFO in3 $ \in3'
  -> C.withFO in4 $ \in4'
  -> C.withFO in5 $ \in5'
  -> C.withFO in6 $ \in6'
  -> C.withFO in7 $ \in7'
  -> F.malloc >>= $ \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_rodNetworkPairInteraction context' out0 in0 in1 in2' in3' in4' in5' in6' in7')
  >> C.peekFreeWrap out0

asgd
  :: Float
  -> Float
  -> Float
  -> Float
  -> Float
  -> Int32
  -> Int32
  -> F32_2d c
  -> F32_3d c
  -> F32_2d c
  -> F32_2d c
  -> F32_2d c
  -> Int32
  -> FT.FT c (Float, F32_2d c, F32_3d c, F32_2d c)
asgd in0 in1 in2 in3 in4 in5 in6 in7 in8 in9 in10 in11 in12
  =  FT.unsafeLiftFromIO $ \context
  -> C.withFO in7 $ \in7'
  -> C.withFO in8 $ \in8'
  -> C.withFO in9 $ \in9'
  -> C.withFO in10 $ \in10'
  -> C.withFO in11 $ \in11'
  -> F.malloc >>= $ \out0
  -> F.malloc >>= $ \out1
  -> F.malloc >>= $ \out2
  -> F.malloc >>= $ \out3
  -> C.inContextWithError context (\context'
  -> Raw.entry_asgd context' out0 out1 out2 out3 in0 in1 in2 in3 in4 in5 in6 in7' in8' in9' in10' in11' in12)
  >> C.peekFree out0 >>= \out0'
  -> C.peekFreeWrap out1 >>= \out1'
  -> C.peekFreeWrap out2 >>= \out2'
  -> C.peekFreeWrap out3 >>= \out3'
  -> return (out0', out1', out2', out3')

testNet
  :: F32_2d c
  -> F32_3d c
  -> F32_2d c
  -> F32_2d c
  -> F32_2d c
  -> FT.FT c Float
testNet in0 in1 in2 in3 in4
  =  FT.unsafeLiftFromIO $ \context
  -> C.withFO in0 $ \in0'
  -> C.withFO in1 $ \in1'
  -> C.withFO in2 $ \in2'
  -> C.withFO in3 $ \in3'
  -> C.withFO in4 $ \in4'
  -> F.malloc >>= $ \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_testNet context' out0 in0' in1' in2' in3' in4')
  >> C.peekFree out0

bulkEval
  :: F32_2d c
  -> F32_3d c
  -> F32_2d c
  -> F32_2d c
  -> FT.FT c (F32_2d c)
bulkEval in0 in1 in2 in3
  =  FT.unsafeLiftFromIO $ \context
  -> C.withFO in0 $ \in0'
  -> C.withFO in1 $ \in1'
  -> C.withFO in2 $ \in2'
  -> C.withFO in3 $ \in3'
  -> F.malloc >>= $ \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_bulkEval context' out0 in0' in1' in2' in3')
  >> C.peekFreeWrap out0

sgd
  :: Float
  -> Float
  -> Int32
  -> Int32
  -> F32_2d c
  -> F32_3d c
  -> F32_2d c
  -> F32_2d c
  -> F32_2d c
  -> Int32
  -> FT.FT c (F32_2d c, F32_3d c, F32_2d c)
sgd in0 in1 in2 in3 in4 in5 in6 in7 in8 in9
  =  FT.unsafeLiftFromIO $ \context
  -> C.withFO in4 $ \in4'
  -> C.withFO in5 $ \in5'
  -> C.withFO in6 $ \in6'
  -> C.withFO in7 $ \in7'
  -> C.withFO in8 $ \in8'
  -> F.malloc >>= $ \out0
  -> F.malloc >>= $ \out1
  -> F.malloc >>= $ \out2
  -> C.inContextWithError context (\context'
  -> Raw.entry_sgd context' out0 out1 out2 in0 in1 in2 in3 in4' in5' in6' in7' in8' in9)
  >> C.peekFreeWrap out0 >>= \out0'
  -> C.peekFreeWrap out1 >>= \out1'
  -> C.peekFreeWrap out2 >>= \out2'
  -> return (out0', out1', out2')

gradientDescent
  :: Float
  -> F32_2d c
  -> F32_3d c
  -> F32_2d c
  -> F32_2d c
  -> F32_2d c
  -> Int32
  -> FT.FT c (Float, F32_2d c, F32_3d c, F32_2d c)
gradientDescent in0 in1 in2 in3 in4 in5 in6
  =  FT.unsafeLiftFromIO $ \context
  -> C.withFO in1 $ \in1'
  -> C.withFO in2 $ \in2'
  -> C.withFO in3 $ \in3'
  -> C.withFO in4 $ \in4'
  -> C.withFO in5 $ \in5'
  -> F.malloc >>= $ \out0
  -> F.malloc >>= $ \out1
  -> F.malloc >>= $ \out2
  -> F.malloc >>= $ \out3
  -> C.inContextWithError context (\context'
  -> Raw.entry_gradientDescent context' out0 out1 out2 out3 in0 in1' in2' in3' in4' in5' in6)
  >> C.peekFree out0 >>= \out0'
  -> C.peekFreeWrap out1 >>= \out1'
  -> C.peekFreeWrap out2 >>= \out2'
  -> C.peekFreeWrap out3 >>= \out3'
  -> return (out0', out1', out2', out3')

dXdtsAndUpdatedSystem
  :: Float
  -> Opaque_box c
  -> Opaque_arr_rigid_1d c
  -> Opaque_arr_forceTorque_1d c
  -> FT.FT c (Opaque_arr_dXdt_1d c, Opaque_arr_rigid_1d c)
dXdtsAndUpdatedSystem in0 in1 in2 in3
  =  FT.unsafeLiftFromIO $ \context
  -> C.withFO in1 $ \in1'
  -> C.withFO in2 $ \in2'
  -> C.withFO in3 $ \in3'
  -> F.malloc >>= $ \out0
  -> F.malloc >>= $ \out1
  -> C.inContextWithError context (\context'
  -> Raw.entry_dXdtsAndUpdatedSystem context' out0 out1 in0 in1' in2' in3')
  >> C.peekFreeWrap out0 >>= \out0'
  -> C.peekFreeWrap out1 >>= \out1'
  -> return (out0', out1')

dXdts
  :: Opaque_arr_rigid_1d c
  -> Opaque_arr_forceTorque_1d c
  -> FT.FT c (Opaque_arr_dXdt_1d c)
dXdts in0 in1
  =  FT.unsafeLiftFromIO $ \context
  -> C.withFO in0 $ \in0'
  -> C.withFO in1 $ \in1'
  -> F.malloc >>= $ \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_dXdts context' out0 in0' in1')
  >> C.peekFreeWrap out0

rk4weightedUpdate
  :: Float
  -> Opaque_box c
  -> Opaque_arr_rigid_1d c
  -> Opaque_arr_dXdt_1d c
  -> Opaque_arr_dXdt_1d c
  -> Opaque_arr_dXdt_1d c
  -> Opaque_arr_dXdt_1d c
  -> FT.FT c (Opaque_arr_rigid_1d c)
rk4weightedUpdate in0 in1 in2 in3 in4 in5 in6
  =  FT.unsafeLiftFromIO $ \context
  -> C.withFO in1 $ \in1'
  -> C.withFO in2 $ \in2'
  -> C.withFO in3 $ \in3'
  -> C.withFO in4 $ \in4'
  -> C.withFO in5 $ \in5'
  -> C.withFO in6 $ \in6'
  -> F.malloc >>= $ \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_rk4weightedUpdate context' out0 in0 in1' in2' in3' in4' in5' in6')
  >> C.peekFreeWrap out0

coordinatesFrom
  :: Opaque_arr_rigid_1d c
  -> FT.FT c (F32_1d c, F32_1d c, F32_1d c, F32_1d c, F32_1d c, F32_1d c, F32_1d c, F32_1d c, F32_1d c, F32_1d c, F32_1d c, F32_1d c, F32_1d c)
coordinatesFrom in0
  =  FT.unsafeLiftFromIO $ \context
  -> C.withFO in0 $ \in0'
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
  >> C.peekFreeWrap out0 >>= \out0'
  -> C.peekFreeWrap out1 >>= \out1'
  -> C.peekFreeWrap out2 >>= \out2'
  -> C.peekFreeWrap out3 >>= \out3'
  -> C.peekFreeWrap out4 >>= \out4'
  -> C.peekFreeWrap out5 >>= \out5'
  -> C.peekFreeWrap out6 >>= \out6'
  -> C.peekFreeWrap out7 >>= \out7'
  -> C.peekFreeWrap out8 >>= \out8'
  -> C.peekFreeWrap out9 >>= \out9'
  -> C.peekFreeWrap out10 >>= \out10'
  -> C.peekFreeWrap out11 >>= \out11'
  -> C.peekFreeWrap out12 >>= \out12'
  -> return (out0', out1', out2', out3', out4', out5', out6', out7', out8', out9', out10', out11', out12')

toAssociations
  :: I32_1d c
  -> I32_1d c
  -> I8_1d c
  -> I8_1d c
  -> I8_1d c
  -> FT.FT c (Opaque_arr_association_1d c)
toAssociations in0 in1 in2 in3 in4
  =  FT.unsafeLiftFromIO $ \context
  -> C.withFO in0 $ \in0'
  -> C.withFO in1 $ \in1'
  -> C.withFO in2 $ \in2'
  -> C.withFO in3 $ \in3'
  -> C.withFO in4 $ \in4'
  -> F.malloc >>= $ \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_toAssociations context' out0 in0' in1' in2' in3' in4')
  >> C.peekFreeWrap out0

toRigids
  :: F32_1d c
  -> F32_1d c
  -> F32_1d c
  -> F32_1d c
  -> F32_1d c
  -> F32_1d c
  -> F32_1d c
  -> F32_1d c
  -> F32_1d c
  -> F32_1d c
  -> F32_1d c
  -> F32_1d c
  -> F32_1d c
  -> F32_1d c
  -> F32_1d c
  -> F32_1d c
  -> F32_1d c
  -> I32_1d c
  -> FT.FT c (Opaque_arr_rigid_1d c)
toRigids in0 in1 in2 in3 in4 in5 in6 in7 in8 in9 in10 in11 in12 in13 in14 in15 in16 in17
  =  FT.unsafeLiftFromIO $ \context
  -> C.withFO in0 $ \in0'
  -> C.withFO in1 $ \in1'
  -> C.withFO in2 $ \in2'
  -> C.withFO in3 $ \in3'
  -> C.withFO in4 $ \in4'
  -> C.withFO in5 $ \in5'
  -> C.withFO in6 $ \in6'
  -> C.withFO in7 $ \in7'
  -> C.withFO in8 $ \in8'
  -> C.withFO in9 $ \in9'
  -> C.withFO in10 $ \in10'
  -> C.withFO in11 $ \in11'
  -> C.withFO in12 $ \in12'
  -> C.withFO in13 $ \in13'
  -> C.withFO in14 $ \in14'
  -> C.withFO in15 $ \in15'
  -> C.withFO in16 $ \in16'
  -> C.withFO in17 $ \in17'
  -> F.malloc >>= $ \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_toRigids context' out0 in0' in1' in2' in3' in4' in5' in6' in7' in8' in9' in10' in11' in12' in13' in14' in15' in16' in17')
  >> C.peekFreeWrap out0

addForceTorques
  :: Opaque_arr_forceTorque_1d c
  -> Opaque_arr_forceTorque_1d c
  -> FT.FT c (Opaque_arr_forceTorque_1d c)
addForceTorques in0 in1
  =  FT.unsafeLiftFromIO $ \context
  -> C.withFO in0 $ \in0'
  -> C.withFO in1 $ \in1'
  -> F.malloc >>= $ \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_addForceTorques context' out0 in0' in1')
  >> C.peekFreeWrap out0

toForceTorques
  :: F32_1d c
  -> F32_1d c
  -> F32_1d c
  -> F32_1d c
  -> F32_1d c
  -> F32_1d c
  -> FT.FT c (Opaque_arr_forceTorque_1d c)
toForceTorques in0 in1 in2 in3 in4 in5
  =  FT.unsafeLiftFromIO $ \context
  -> C.withFO in0 $ \in0'
  -> C.withFO in1 $ \in1'
  -> C.withFO in2 $ \in2'
  -> C.withFO in3 $ \in3'
  -> C.withFO in4 $ \in4'
  -> C.withFO in5 $ \in5'
  -> F.malloc >>= $ \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_toForceTorques context' out0 in0' in1' in2' in3' in4' in5')
  >> C.peekFreeWrap out0

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
  -> FT.FT c (Opaque_box c)
box in0 in1 in2 in3 in4 in5 in6 in7 in8
  =  FT.unsafeLiftFromIO $ \context
  -> F.malloc >>= $ \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_box context' out0 in0 in1 in2 in3 in4 in5 in6 in7 in8)
  >> C.peekFreeWrap out0
