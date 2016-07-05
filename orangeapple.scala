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


case class Counters(a: Int, o: Int) {
	}

def specialPurchases (list: List[String]): Double = {
	val start: Counters = new Counters(0,0)

	// Find the total numbers of apples and oranges bought
  val counts = list.foldLeft(start) { (t,i) =>  i match {
  	case "apple" => Counters(t.a + 1, t.o)
  	case "orange" => Counters(t.a, t.o + 1)
  	case _ => t
  	}
  }
  val applePay = counts.a  / 2  + counts.a % 2         // how many apples to pay for    
  val orangePay = (counts.o / 3) * 2   + counts.o % 3  // how many oranges to pay for
 
  applePay * 0.6 + orangePay * 0.25
}

// For a more extensible system, this would need to calculate the total on the fly, keeping track of the 
// numbers of both apples and oranges that have been added.
// If this was to include more different items, then the item counts could be stored in a larger structure eg List or Set
// The details of the specific special offer for each item type could be defined as a function to be called by the main loop
case class Counters2(a: Int, o: Int, amount: Double) {
	}

def specials2(list: List[String]): Double = {
	val start: Counters2 = new Counters2(0,0, 0.0)
  val counts = list.foldLeft(start) { (t,i) =>  i match {
  	case "apple" => { 
  			if (t.a > 0) Counters2(0, t.o, t.amount)		// This apple is second one that is free
  			else Counters2(1, t.o, t.amount + 0.25)		// Pay for this apple
  		}
  	case "orange" => { 
  			if (t.o == 2) Counters2(t.a, 0, t.amount)	// 3rd orange is free (2 for price of 1)
  			else Counters2(t.a, t.o + 1, t.amount + 0.6)
  	}
  	case _ => t
  	}
  }
	counts.amount
}
}
