package recfun

import java.util.Collections.EmptyList

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
     
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = pascalString(r, List[Int]())(c)

  private def pascalString(row: Int, lst: List[Int]): List[Int] = {
    if (row >= 0) 
      pascalString(row - 1, pascalLower(lst))
    else
      lst
  }
  
  private def pascalLower(lst: List[Int]): List[Int] = {
    lst match {
      case Nil => List(1) 
      case _ => 1::plHelper(lst)
    }
  }
  
  private def plHelper(lst: List[Int]): List[Int] = {
    lst match {
      case Nil => List()
      case x::Nil => List(x)
      case f::s::xs => (f + s)::plHelper(s::xs)
    }   
  }
    
  /**
   * Exercise 2
   */
   def balance(chars: List[Char]): Boolean = checkPrts(chars, List[Char]()) isEmpty   
   
   private def checkPrts(chars: List[Char], prts: List[Char]): List[Char] = {
     chars match {
      case Nil => prts 
      case '('::xs => checkPrts(xs, '('::prts)
      case ')'::xs => if (prts isEmpty) List(')') else checkPrts(xs, prts tail) 
      case _::xs => checkPrts(xs, prts) 
    }
   }
      
       
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (coins.isEmpty || money < 0) 0
      else countChange(money, coins.tail) + countChange(money - coins.head, coins) 
    }
  }
