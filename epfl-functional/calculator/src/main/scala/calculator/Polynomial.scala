package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
        val av = a(); val bv = b(); val cv = c()
        new Signal(bv*bv - 4*av*cv)
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
        val av = a(); val bv = b(); val cv = c()
        val dv = delta()
        new Signal( Set( (-bv + math.sqrt(dv))/(2*av), (-bv - math.sqrt(dv))/(2*av) ))
  }
}
