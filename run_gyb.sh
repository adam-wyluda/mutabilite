#!/bin/sh

mkdir -p ./api/src/main/scala/codegen
python project/gyb.py ./api/src/main/resources/gyb/HashEq.scala.gyb > ./api/src/main/scala/codegen/SpecializedHashEq.scala
python project/gyb.py ./api/src/main/resources/gyb/Opt.scala.gyb    > ./api/src/main/scala/codegen/SpecializedOpt.scala

mkdir -p ./specialized/src/main/scala/codegen
python project/gyb.py ./specialized/src/main/resources/gyb/BufferSeq.scala.gyb > ./specialized/src/main/scala/codegen/SpecializedBufferSeq.scala
python project/gyb.py ./specialized/src/main/resources/gyb/HashMap.scala.gyb   > ./specialized/src/main/scala/codegen/SpecializedHashMap.scala
python project/gyb.py ./specialized/src/main/resources/gyb/HashSet.scala.gyb   > ./specialized/src/main/scala/codegen/SpecializedHashSet.scala

mkdir -p ./offheap/src/main/scala/codegen
python project/gyb.py ./offheap/src/main/resources/gyb/BufferSeq.scala.gyb > ./offheap/src/main/scala/codegen/OffheapBufferSeq.scala
python project/gyb.py ./offheap/src/main/resources/gyb/HashMap.scala.gyb   > ./offheap/src/main/scala/codegen/OffheapHashMap.scala
python project/gyb.py ./offheap/src/main/resources/gyb/HashSet.scala.gyb   > ./offheap/src/main/scala/codegen/OffheapHashSet.scala
