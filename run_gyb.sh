#!/bin/sh

mkdir -p ./core/src/main/scala/codegen
python project/gyb.py ./core/src/main/gyb/HashEq.scala.gyb    > ./core/src/main/scala/codegen/SpecializedHashEq.scala
python project/gyb.py ./core/src/main/gyb/BufferSeq.scala.gyb > ./core/src/main/scala/codegen/SpecializedBufferSeq.scala
python project/gyb.py ./core/src/main/gyb/DHashMap.scala.gyb  > ./core/src/main/scala/codegen/SpecializedHashMap.scala
python project/gyb.py ./core/src/main/gyb/DHashSet.scala.gyb  > ./core/src/main/scala/codegen/SpecializedHashSet.scala
