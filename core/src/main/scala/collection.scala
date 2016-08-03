package offheap

package object collection {

  implicit object BooleanHash extends Hash_Boolean {
    def hash(value: Boolean): Int = if (value) 1231 else 1237
  }

  implicit object CharHash extends Hash_Char {
    def hash(value: Char): Int = value
  }

  implicit object ByteHash extends Hash_Byte {
    def hash(value: Byte): Int = value
  }

  implicit object ShortHash extends Hash_Short {
    def hash(value: Short): Int = value
  }

  implicit object IntHash extends Hash_Int {
    def hash(value: Int): Int = value
  }

  implicit object LongHash extends Hash_Long {
    def hash(value: Long): Int = (value ^ (value >>> 32)).asInstanceOf[Int]
  }

  implicit object FloatHash extends Hash_Float {
    def hash(value: Float): Int = java.lang.Float.floatToIntBits(value)
  }

  implicit object DoubleHash extends Hash_Double {
    def hash(value: Double): Int = {
      val bits = java.lang.Double.doubleToLongBits(value)
      (bits ^ (bits >>> 32)).asInstanceOf[Int]
    }
  }

  implicit object ObjectHash extends Hash_Object {
    def hash(value: Any): Int = value.hashCode()
  }
}
