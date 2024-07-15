
    def byRow[T](width: Int, seq: Seq[T]):  Seq[Seq[T]] =
      seq.sliding(width, width).toSeq
      

    def byCol[T](height: Int, seq: Seq[T]):  Seq[Seq[T]] =
        byCol(null.asInstanceOf[T])(height, seq)

    def byCol[T](padding: T)(height: Int, seq: Seq[T]):  Seq[Seq[T]] = {
      val deficit = seq.length % height
      if (deficit==0)
         byRow(height, seq).transpose
      else {
         val pad = for { _ <-0 until height-deficit } yield padding
         byRow(height, seq ++ pad).transpose
      }
    }

    val d = 0 until 20
    

