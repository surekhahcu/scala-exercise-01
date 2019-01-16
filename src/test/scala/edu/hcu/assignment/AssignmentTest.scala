package edu.hcu.assignment

import org.scalatest.FunSuite

class AssignmentTest extends FunSuite {

  val obj = new Assignment

  //1st
  test("Implement show function") {
    val result = obj.show2(2, 3)
    val expectResult = List(1, 1, 2, 2, 3, 3)
    assert(result === expectResult)
  }

  //2nd
  test("reverse of a list") {
    val result = obj.reverse(List(2, 3, 4, 5))
    val expectResult = List(5, 4, 3, 2)
    assert(result === expectResult)
  }

  //4th
  test("Sum Of Two Options") {
    val result = obj.sum(Some(2), None)
    val expectResult = Some(2)
    assert(result === expectResult)
  }

  //5th
  test("Implementation Of fill method") {
    val result = obj.fill2(3, 4)
    val expectResult = List(3, 3, 3, 3)
    assert(result === expectResult)
  }

  //6th
  test("Remove Duplication From A List") {
    val result = obj.dedupe(List(1, 2, 3, 4, 1, 2, 3, 4))
    val expectResult = List(1, 2, 3, 4)
    assert(result === expectResult)
  }
  //7th
  test("Word Counter") {
    val result = obj.wordCount("hello how are you hello ")
    val expectResult = Map("you" -> 1, "how" -> 1, "are" -> 1, "hello" -> 2)
    assert(result === expectResult)
  }


  //8th
  test("Implicit class") {
   // val result =
    val expectResult =4
  //  assert(result === expectResult)
  }

  //9th

  test("Directory") {
     val result =obj.countFiles("/home/surekha/learning-scala/scala-exercise-01/src/main/scala/edu/hcu/assignment")
    val expectResult =Some(2)
     assert(result === expectResult)
  }


  //12th
  test("Merge Of Two List") {
    val result = obj.merge(List(1,2),List(3,4))
    val expectResult = List(1, 2, 3, 4)
    assert(result === expectResult)
  }
  //13th
  test("Merging two Maps") {
    val result = obj.concatenate(Map("a" -> 1, "b" -> 3), Map("a" -> 2))
    val expectResult = Map("a" -> 3, "b" -> 3)
    assert(result === expectResult)
  }
  //14th
  test("Zip") {
    val result = obj.zip(List(1,2,3),List("a","b","c"))
    val expectResult = List((1,"a"),(2,"b"),(3,"c"))
    assert(result === expectResult)
  }
}
