package common

object orangeapple extends App {
  
  // Calculate the total cost where each apple is worth 0.6 and each orange worth 0.25
def totalPuchases (list: List[String]): Double =
  list.foldLeft(0.0) { (t,i) =>  i match {
  	case "apple" => t + 0.6
  	case "orange" => t + 0.25
  	case _ => t
  	}
  } 


case class Counters(var a: Int, var o: Int) {
	}

def specialPurchases (list: List[String]): Double = {
	val start: Counters = new Counters(0,0)

	// Find the total numbers of apples and oranges bought
  val counts = list.foldLeft(start) { (t,i) =>  i match {
  	case "apple" => new Counters(t.a + 1, t.o)
  	case "orange" => new Counters(t.a, t.o + 1)
  	case _ => t
  	}
  }
  val applePay = counts.a  / 2  + counts.a % 2         // how many apples to pay for    
  val orangePay = (counts.o / 3) * 2   + counts.o % 3  // how many oranges to pay for
 
  applePay * 0.6 + orangePay * 0.25
}

}