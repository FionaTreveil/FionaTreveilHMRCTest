package common

object orangeapple extends App {
  
def totalPuchases (list: List[String]): Double =
  list.foldLeft(0.0) { (t,i) =>  i match {
  	case "apple" => t + 0.6
  	case "orange" => t + 0.25
  	case _ => t
  	}
  } 
}