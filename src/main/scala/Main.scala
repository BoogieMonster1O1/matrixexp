package io.github.boogiemonster1o1.matrixexp

object Main {
  def main(args: Array[String]): Unit = {
    val matrix: Array[Array[Double]] = Array(
      Array(0, -Math.PI),
      Array(Math.PI, 0)
    )
    val result: Array[Array[Double]] = Array(Array(0, 0), Array(0, 0))
  }

  def addInPlace(a: Array[Array[Double]], b: Array[Array[Double]]): Array[Array[Double]] = {
    for (i <- a.indices) {
      for (j <- a(i).indices) {
        a(i)(j) += b(i)(j)
      }
    }
    a
  }

  def factorial(n: Int): Int = {
    if (n < 1) 1
    else n * factorial(n - 1)
  }

  def multiply(first: Array[Array[Double]], second: Array[Array[Double]]): Array[Array[Double]] = {
    val result: Array[Array[Double]] = Array.ofDim[Double](first.length, second(0).length)
    for (i <- first.indices) {
      for (j <- second(0).indices) {
        for (k <- first(0).indices) {
          result(i)(j) += first(i)(k) * second(k)(j)
        }
      }
    }
    result
  }

  def pow(matrix: Array[Array[Double]], exponent: Int): Array[Array[Double]] = {
    if (exponent == 0) {
      identity(matrix.length)
    } else if (exponent == 1) {
      matrix
    } else {
      val result: Array[Array[Double]] = pow(matrix, exponent / 2)
      multiply(result, result)
      if (exponent % 2 == 1) {
        multiply(result, matrix)
      }
      result
    }
  }

  def identity(size: Int): Array[Array[Double]] = {
    val identity = Array.ofDim[Double](size, size)
    for (i <- 0 until size) {
      identity(i)(i) = 1
    }
    identity
  }
}
