package offheap.collection

trait SeqOps[A] extends Any {

  def map[B](f: A => B): Seq[B]
  def flatMap[B](f: A => Seq[B]): Seq[B]
  def filter(f: A => Boolean): Seq[A]
}
