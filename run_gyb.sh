#!/bin/sh

python project/gyb.py ./codegen/src/main/resources/gyb/BufferSeq.scala.gyb > ./codegen/src/main/scala/codegen/GenBufferSeq.scala
python project/gyb.py ./codegen/src/main/resources/gyb/HashMap.scala.gyb > ./codegen/src/main/scala/codegen/GenHashMap.scala

