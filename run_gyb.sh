#!/bin/sh

mkdir -p ./core/src/main/scala/codegen
python project/gyb.py ./core/src/main/resources/gyb/HashEq.scala.gyb    > ./core/src/main/scala/codegen/SpecializedHashEq.scala
python project/gyb.py ./core/src/main/resources/gyb/Opt.scala.gyb       > ./core/src/main/scala/codegen/SpecializedOpt.scala
python project/gyb.py ./core/src/main/resources/gyb/BufferSeq.scala.gyb > ./core/src/main/scala/codegen/SpecializedBufferSeq.scala
python project/gyb.py ./core/src/main/resources/gyb/HashMap.scala.gyb   > ./core/src/main/scala/codegen/SpecializedHashMap.scala
python project/gyb.py ./core/src/main/resources/gyb/HashSet.scala.gyb   > ./core/src/main/scala/codegen/SpecializedHashSet.scala
