package edu.hcu.assignment

import jdk.nashorn.internal.ir.Assignment

object JustTest extends App {
  implicit class Square(n: Int) {
    def square = n * n
  }
  def merge2(list1: List[Int], list2: List[Int]): List[Int] = {
    val a = collection.mutable.ListBuffer[Int]()
    list1.foreach(n => a.append(n))
    list2.foreach(n => a.append(n))

    a.tail.foldLeft(List[Int]()){(temp,ele)=> if(a.head>ele) temp:+a.head else temp:+ele }
    a.toList
  }

  // println(2.square)
  println(merge2(List(1,3,2),List(6,4)))
}