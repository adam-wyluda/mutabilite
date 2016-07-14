#!/bin/sh

mkdir -p ./api/src/main/scala/codegen
python project/gyb.py ./api/src/main/resources/gyb/HashEq.scala.gyb > ./api/src/main/scala/codegen/SpecializedHashEq.scala
python project/gyb.py ./api/src/main/resources/gyb/Opt.scala.gyb    > ./api/src/main/scala/codegen/SpecializedOpt.scala

mkdir -p ./specialized-core/src/main/scala/codegen
python project/gyb.py ./specialized-core/src/main/resources/gyb/BufferSeq.scala.gyb > ./specialized-core/src/main/scala/codegen/SpecializedBufferSeq.scala
python project/gyb.py ./specialized-core/src/main/resources/gyb/HashMap.scala.gyb   > ./specialized-core/src/main/scala/codegen/SpecializedHashMap.scala
python project/gyb.py ./specialized-core/src/main/resources/gyb/HashSet.scala.gyb   > ./specialized-core/src/main/scala/codegen/SpecializedHashSet.scala

mkdir -p ./specialized/src/main/scala/codegen
python project/gyb.py ./specialized/src/main/resources/gyb/collection.scala.gyb > ./specialized/src/main/scala/codegen/collection.scala

# mkdir -p ./offheap/src/main/scala/codegen
python project/gyb.py ./offheap/src/main/resources/gyb/BufferSeq.scala.gyb > ./offheap/src/test/scala/AnOffheapBufferSeq.scala
python project/gyb.py ./offheap/src/main/resources/gyb/HashMap.scala.gyb   > ./offheap/src/test/scala/AnOffheapHashMap.scala
python project/gyb.py ./offheap/src/main/resources/gyb/HashSet.scala.gyb   > ./offheap/src/test/scala/AnOffheapHashSet.scala

#python project/gyb.py ./offheap/src/main/resources/gyb/BufferSeq.scala.gyb > ./benchmark/src/main/scala/AnOffheapBufferSeq.scala
#python project/gyb.py ./offheap/src/main/resources/gyb/HashMap.scala.gyb   > ./benchmark/src/main/scala/AnOffheapHashMap.scala
#python project/gyb.py ./offheap/src/main/resources/gyb/HashSet.scala.gyb   > ./benchmark/src/main/scala/AnOffheapHashSet.scala
