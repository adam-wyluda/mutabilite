#!/bin/sh

python project/gyb.py ./specialized/src/main/resources/gyb/BufferSeq.scala.gyb > ./specialized/src/main/scala/codegen/GenBufferSeq.scala
python project/gyb.py ./specialized/src/main/resources/gyb/HashMap.scala.gyb > ./specialized/src/main/scala/codegen/GenHashMap.scala
python project/gyb.py ./specialized/src/main/resources/gyb/HashSet.scala.gyb > ./specialized/src/main/scala/codegen/GenHashSet.scala
python project/gyb.py ./specialized/src/main/resources/gyb/HashEq.scala.gyb > ./specialized/src/main/scala/codegen/GenHashEq.scala
python project/gyb.py ./specialized/src/main/resources/gyb/Opt.scala.gyb > ./specialized/src/main/scala/codegen/GenOpt.scala
