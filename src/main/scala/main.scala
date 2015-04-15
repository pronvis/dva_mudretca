import scala.annotation.tailrec

object main {

  lazy val primes: Stream[Int] = 2 #:: Stream.from(3).filter(i => isPrime(i))

  def isPrime(x: Int): Boolean = {
    primes.takeWhile(i => i*i <= x).forall { k => x % k > 0}
  }

  def expandBySum(x: Int): List[(Int, Int)] = {
    @tailrec def helper(accum: List[(Int, Int)], i: Int, j: Int): List[(Int, Int)] = {
      if (i < j) accum
      else helper((i, j) :: accum, i - 1, j + 1)
    }

    helper(List.empty, x - 2, 2)
  }

  def expandByProduct(x: Int): List[(Int, Int)] = {
    var biggestPossibleDivision = x / 2

    @tailrec def helper(accum: List[(Int, Int)], i: Int): List[(Int, Int)] = {
      if (i == biggestPossibleDivision) return accum

      if (x % i == 0) {
        biggestPossibleDivision = if(x / i <= i) biggestPossibleDivision else x / i
        helper((x / i, i) :: accum, i + 1)
      }
      else helper(accum, i + 1)
    }

    helper(List.empty, 2)
  }

  // there is only those numbers that cant be expanded by sum of two prime numbers
  lazy val ValiNumbers: Stream[Int] = Stream.from(4).filter(i => !expandBySum(i).exists(expanded => isPrime(expanded._1) && isPrime(expanded._2)))

  def inValiNumbers(x: Int): Boolean = {
    ValiNumbers.takeWhile(valis => valis <= x).contains(x)
  }

  // there is only those numbers that after expanding by product have only one pair (x, y) that sum(x+y) result is in the Valis numbers
  lazy val AliNumbers: Stream[Int] = Stream.from(4).filter(i => expandByProduct(i).count(expanded => inValiNumbers(expanded._1 + expanded._2)) == 1)

  def inAliNumbers(x: Int): Boolean = {
    AliNumbers.takeWhile(alis => alis <= x).contains(x)
  }

  lazy val ValiNumbersWithSolution: Stream[Int] = ValiNumbers.filter(valis => expandBySum(valis).map(expanded => expanded._1 * expanded._2).count(inAliNumbers) == 1)

  lazy val solution: Stream[String] = ValiNumbersWithSolution.map(valis => {
    val solution = expandBySum(valis).filter(expanded => inAliNumbers(expanded._1 * expanded._2)).head
    "Alis number: " + solution._1 * solution._2 + "; Valis number: " + (solution._1 + solution._2) + "; solution: " + solution._1 + " & " + solution._2
  })

  def main(args: Array[String]) {
    println("Vali's possible numbers: " + (ValiNumbers.take(20).toList mkString ", "))
    println("Ali's possible numbers: " + (AliNumbers.take(30).toList mkString ", "))
    println("Vali's numbers that give solution to Ali: " + (ValiNumbersWithSolution.take(2).toList mkString ", "))
    println(solution.take(2).toList mkString "\n")
  }
}

