package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal{
      val bval = b()
      bval * bval - 4 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val vdelta = delta()
      val aval = a()
      val bval = b()
     
      if (vdelta < 0)
        Set()
      val r1 = -bval + scala.math.sqrt(vdelta) / 2 * aval
      val r2 = -bval - scala.math.sqrt(vdelta) / 2 * aval
      Set(r1, r2)
    }
  }
}
