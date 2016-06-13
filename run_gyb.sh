#!/bin/sh

python project/gyb.py ./codegen/src/main/resources/gyb/BufferSeq.scala.gyb > ./codegen/src/main/scala/codegen/GenBufferSeq.scala
python project/gyb.py ./codegen/src/main/resources/gyb/HashMap.scala.gyb > ./codegen/src/main/scala/codegen/GenHashMap.scala
python project/gyb.py ./codegen/src/main/resources/gyb/HashSet.scala.gyb > ./codegen/src/main/scala/codegen/GenHashSet.scala
python project/gyb.py ./codegen/src/main/resources/gyb/HashEq.scala.gyb > ./codegen/src/main/scala/codegen/GenHashEq.scala
