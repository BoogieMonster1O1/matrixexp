package io.github.boogiemonster1o1.matrixexp

/**
 * Raises e to the power of a matrix
 */
object MatrixExp {
  def main(args: Array[String]): Unit = {
    val matrix: Array[Array[BigDecimal]] = Array(
      Array(0.6931471806, 0),
      Array(0, 0.6931471806)
    )
    val result: Array[Array[BigDecimal]] = Array(Array(0, 0), Array(0, 0))
    for (i <- 0 to 10) {
      println("Iteration " + i)
      val nextTerm = pow(matrix, i)
      divideInPlace(nextTerm, factorial(i))
      addInPlace(result, nextTerm)
    }
    println(result.map(row => row.mkString("[", ", ", "]")).mkString("\n"))
  }

  def addInPlace(a: Array[Array[BigDecimal]], b: Array[Array[BigDecimal]]): Array[Array[BigDecimal]] = {
    for (i <- a.indices) {
      for (j <- a(i).indices) {
        a(i)(j) += b(i)(j)
      }
    }
    a
  }

  def factorial(n: Int): Int = {
    if (n <= 1) 1
    else n * factorial(n - 1)
  }

  def multiply(first: Array[Array[BigDecimal]], second: Array[Array[BigDecimal]]): Array[Array[BigDecimal]] = {
    val result: Array[Array[BigDecimal]] = Array.ofDim[BigDecimal](first.length, second(0).length)
    for (i <- result.indices) {
      for (j <- result(i).indices) {
        result(i)(j) = 0
      }
    }
    for (i <- first.indices) {
      for (j <- second(0).indices) {
        for (k <- first(0).indices) {
          result(i)(j) += first(i)(k) * second(k)(j)
        }
      }
    }
    result
  }

  def pow(matrix: Array[Array[BigDecimal]], exponent: Int): Array[Array[BigDecimal]] = {
    if (exponent == 0) {
      identity(matrix.length)
    } else if (exponent == 1) {
      matrix
    } else {
      multiply(pow(matrix, exponent - 1), matrix)
    }
  }

  def divideInPlace(matrix: Array[Array[BigDecimal]], divisor: BigDecimal): Array[Array[BigDecimal]] = {
    for (i <- matrix.indices) {
      for (j <- matrix(i).indices) {
        matrix(i)(j) /= divisor
      }
    }
    matrix
  }

  def identity(size: Int): Array[Array[BigDecimal]] = {
    val identity = Array.ofDim[BigDecimal](size, size)
    for (i <- identity.indices) {
      for (j <- identity(i).indices) {
        identity(i)(j) = 0
      }
    }
    for (i <- 0 until size) {
      identity(i)(i) = 1
    }
    identity
  }
}
