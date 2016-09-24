// This is a Erdős–Gallai theorem implementation in Scala

def isASimpleGraph(degrees: List[Int]): Boolean = {
  val n = degrees.length

  def isASimpleGraphForK(degrees: List[Int], k: Int): Boolean = {
    if (k == 1)
      true
    else
      checkFormula(degrees, k) && isASimpleGraphForK(degrees, k - 1)
  }

  def checkFormula(degrees: List[Int], k: Int): Boolean = {
    k * (k - 1) - sumOfTheFirstK(degrees, k) + sumOfTheFirstKMin(degrees.reverse, n - k - 1, k) >= 0
  }

  def sumOfTheFirstK(degrees: List[Int], i: Int): Int = {
    if (i == 0)
      0
    else
      degrees.head + sumOfTheFirstK(degrees.tail, i - 1)
  }

  def sumOfTheFirstKMin(reversedDegrees: List[Int], i: Int, k: Int): Int = {
    if (k == n)
      0
    else if (i == 0)
      math.min(reversedDegrees.head, k)
    else
      math.min(reversedDegrees.head, k) + sumOfTheFirstKMin(reversedDegrees.tail, i - 1, k)
  }

  def sumOfDegrees(degrees: List[Int]): Int = {
    if (degrees.isEmpty)
      0
    else
      degrees.head + sumOfDegrees(degrees.tail)
  }

  def isSumOfDegreesEven(degrees: List[Int]): Boolean = {
    sumOfDegrees(degrees) % 2 == 0
  }

  isASimpleGraphForK(degrees.sortWith(_>_), n) && isSumOfDegreesEven(degrees)
}

isASimpleGraph(List(1,1,1,1))
isASimpleGraph(List(1,1,1,3))
isASimpleGraph(List(1,1,3,3))
isASimpleGraph(List(1,3,3,3))
isASimpleGraph(List(3,3,3,3))