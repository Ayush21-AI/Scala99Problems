import scala.annotation.tailrec

object Scala99Problems extends App {

 // find sum from 0 to 5  = 15 (0+1+2+3+4+5)
 def sumBetween(from:Int, to:Int): Int = {
  @tailrec
  def inner(from:Int, to:Int, acc:Int=0): Int = {
   if (from == to+1) acc
   else inner(from+1, to, acc+from)
  }
  inner(from, to)
 }
 println(sumBetween(from = 0, to = 5))

 //  Print sum of any numbers in decreasing order till 0.
 def printDecreasing(n:Int):Int =  {
    @tailrec
    def inner(n:Int, accumulator:Int):Int = {
     if(n<=0) accumulator
     else inner(n-1, accumulator + n)
     }
    inner(n,0)
 }
 println(printDecreasing(5))

 // Calculate the Factorial of a Number.
  def fact(n:Int):Int = {
    def inFact(n:Int, acc:Int):Int ={
      if(n<=0) acc
      else inFact(n-1, acc*n)
    }
    inFact(n, 1)
  }
  println(fact(5))

 // Check the last element of a List
 @tailrec
 def last[A](n:List[A]):A = n match {
   case head :: Nil => head
   case _ :: tail => last(tail)
   case _ => throw new NoSuchElementException
  }

 // Check a List is Empty or not
 val lst = List(1,2,3,6,4,9,7,5)
 def iSEmpty[A](lst: List[A]): String = lst match {
    case head :: tail => "not empty"
    case Nil => "Empty"
  }
 iSEmpty(lst)

///////////// Find the Second Last Element of a List ///////////
  def secondLastEle[A](inputList: List[A]): A = inputList match {
        case head :: Nil => head
        case _ :: tail => last(tail)
        case _ => throw new NoSuchElementException
  }
 println(secondLastEle(List(1,2,3,4,5,6)))


////////////// Find the Last Element of a List ///////////
  @tailrec                                  // numLst.last
  def lastElement(numLst:List[Int]):Int = numLst match {
    case Nil => 0
    case head::tail => if(numLst.size ==1) head else lastElement(tail)
  }
  @tailrec
  def lastElement2(numLst: List[Int]):Int = if (numLst.isEmpty) 0
  else if (numLst.size == 1) numLst.head
  else lastElement2(numLst.tail)
//////////////////////////////////////////////////////////


  val strToFind = "My name is Ayush with  years of ex also run  business & made over  billion turnover"
  val pattern = "[0-9]".r
  val res = ( pattern findAllIn strToFind).mkString(",")
  println(res)

  print("Kindly put 1st number: ")
  val input1 = scala.io.StdIn.readInt()
  print("Kindly put 2nd number: ")
  val input2 = scala.io.StdIn.readInt()
  println(s"value of both the inputs are $input1 & $input2")

///////////////////// Basics of List //////////////////
  val emptyLst = List.empty
  val emptyLst2 = Nil
  val aList = List.apply("Aayush","Gour", "Xyz")
  val aNumList = List(2,4,6,8,10,12)
  val aList2 = List("Aayush","Gour", "Xyz")
  val aList3 = "Aayush" :: "Gour" :: "Xyz" :: Nil
  val prepAlst = 5 +: aList      //Prepending the value
  val appendAlst = aList :+ "Raveena"   //Appending a List
  val Size = aNumList.size          // 6
  val Length = aNumList.length      // 6
  val fistEle = aNumList.head       // 2
  val lastEle = aNumList.last       // 12
  val Tail = aNumList.tail          // (4,6,8,10,12)
  val Sort = aNumList.sorted.reverse
/////////////////////////////////////////////////////////



}