package cj2012.r3

import java.io.FileReader
import java.io.PrintStream
import java.util.Scanner
import java.io.File

object APerfectGame extends App {
  
  val f = new Scanner(new File("A-large-practice.in"))
  
  val T = f.nextInt
  
  for (i <- 1 to T) {
    
    val N = f.nextInt
    
    val L = new Array[Int](N)
    val P = new Array[Int](N)
    for (j <- 0 until N) {
      L(j) = f.nextInt
    }
    for (j <- 0 until N) {
      P(j) = f.nextInt
    }
    
    case class LPI(l: Int, p: Int, i: Int) { val ppp = 100 - p; val pp = ppp/100.0 }
    val lpi = L.zip(P).zipWithIndex.map(a => LPI(a._1._1, a._1._2, a._2))

    val result = lpi.sortWith((a, b) => {
//      if (a.p > b.p) {
//        true
//      } else {
//        if (a.p == b.p) {
//          if (a.p == 0) {
//            a.i < b.i
//          } else {
//            if (a.l < b.l) {
//              true
//            } else {
//              if (a.l == b.l) {
//                a.i < b.i
//              } else {
//                false
//              }
//            }
//          }
//        } else {
//          false
//        }
//      }
      //a.p > b.p || ((a.p == b.p && a.p != 0 && a.l < b.l) || (a.p == b.p && a.l == b.l && a.i < b.i))
      //a.l + b.l*a.pp < b.l + a.l*b.pp
      // a.l - b.l < a.l*b.pp - b.l*a.pp
      //(a.l/a.pp + b.l)/b.pp < (b.l/b.pp + a.l)/a.pp
//      a.l - b.l match {
//        case 0 => a.p > b.p
//        case _ => a.l - b.l < a.l * b.pp - b.l * a.pp
//      }
      a.l * b.p < b.l * a.p
//      if (a.l == b.l) {
//        a.p > b.p // A
//      } else if (a.l < b.l) {
//        if (a.pp * b.pp == 1) {
//          false
//        } else {
//          a.l < b.l
//        }
//      }

        //10  + 9 * (x<1)  < 9 + 10*(y<1)
    })
    
    def solveExhaustive(lpi: Array[LPI]): Array[LPI] = {
      var minCost = Double.MaxValue
      val minResult: Array[LPI] = new Array[LPI](lpi.length)
      val result = new Array[LPI](lpi.length)
      val visited = new Array[Boolean](lpi.length) 
      def rec(costCarry: Double, pos: Int) {
        if (pos == lpi.length) {
          minCost = costCarry
          result.copyToArray(minResult)
          return
        }
        for (i <- 0 until lpi.length if !visited(i)) {
          val x = lpi(i)
          val cost = (costCarry + x.l) / (1.0 - x.p/100.0)
          if (cost < minCost) {
            visited(i) = true
            result(pos) = x
            rec(cost, pos + 1)
            visited(i) = false
          }
        }
      }
      rec(0, 0)
      return minResult
    }
    
    //val result2 = solveExhaustive(lpi)
    
    print("Case #" + i + ":")
    for (r <- result) {
      print(" " + r.i)
    }
    println

  }

}