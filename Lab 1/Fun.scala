import scala.collection.mutable

//singleton object
object Fun {

  def func1(x : Double, y :Int) = x + (70*y)

  def ex(a: Double) = 50 * a

  def ex2a(p1:(Int, Int), p2:(Int, Int)) = (p1._1+p1._2,p2._1*p2._2)

  def ex2b(a: Int, b: Int, c:Int) : (Int, Int) =
  {
    val list: mutable.PriorityQueue[Int] = collection.mutable.PriorityQueue(a,b,c)
    (list.dequeue(),list.dequeue())
  }

  def ex2c(a: (Int, Int, Int)) : (Int, Int, Int)  =
  {
    val list1: mutable.PriorityQueue[Int] = collection.mutable.PriorityQueue(a._1, a._2, a._3)
    (list1.dequeue(),list1.dequeue(), list1.dequeue())
  }

  def ex2d(a: Int, b: Int, c: Int): Boolean =
  {
    val list2: mutable.PriorityQueue[Int] = collection.mutable.PriorityQueue(a, b, c)
    val bigger = list2.dequeue()
    val side1 = list2.dequeue()
    val side2 = list2.dequeue()
    (side1 + side2) < bigger
  }

  //ex 2 e
  def abrev(name: String): String =
  {
    val separatedName: Array[String] = name.split(" ")
    if (separatedName.length >= 2) separatedName.head + " " + separatedName.last
    else name
  }

  //Ex 3
  def fact(n: Int):Int = if (n==0) 1 else n * fact(n-1)

  //ex 3 a
  def exp(x: Int, y: Int): Int= if(y==0) 1 else x * exp(x, y-1)

  def ex3b(list: List[Int]): (Int, Int) = (list.head, list.last)

  def ex3bRecur(list: List[Int]): (Int, Int) =
  {
    if(list.length == 1)
      (list.head, list.head)
    else
    {
      val (first, last) = ex3bRecur(list.tail)    //tail -> all elements except the 1st one
      (list.head, last)
    }
  }

  def ex3c(list: List[Int]): (List[Int], Int) = (list, list.length)

  //rever
  def ex3d(list: List[Double]): Double = {
    def sumHelper(list: List[Double], acc: Double): Double = {
      if(list.isEmpty) acc
      else sumHelper(list.tail, acc + list.head)
    }
    val sum = sumHelper(list, 0.0)
    if (list.isEmpty) 0 else sum / list.length
  }


}