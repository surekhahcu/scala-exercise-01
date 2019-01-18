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

  //3rd
  test("Rotation Of An Array") {
    val result = obj.rotate(Array(1, 2, 3, 4, 5), 2)
    val expectResult = Array(3, 4, 5, 1, 2)
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
    val expectResult = 4
    //  assert(result === expectResult)
  }

  //9th

  test("Directory") {
    val result = obj.countFiles("/home/surekha/learning-scala/scala-exercise-01/src/main/scala/edu/hcu/assignment")
    val expectResult = Some(1)
    assert(result === expectResult)
  }

  //10th
  test("Remove all keys from Map if their value are odd") {
    val result = obj.removeOdd(Map("a" -> 1, "b" -> 2, "c" -> 3, "d" -> 4))
    val expectResult = Map("b" -> 2, "d" -> 4)
    assert(result === expectResult)
  }
  //11th
  test("Remove list of keys from Map(just like Map’s “ --” function )") {
    val result = obj.removeKeys(List("a", "b"), Map("c" -> 1, "a" -> 22, "b" -> 4, "d" -> 5))
    val expectResult = Map("c" -> 1, "d" -> 5)
    assert(result === expectResult)
  }

  //12th
  test("Merge Of Two List") {
    val result = obj.merge(List(1, 2), List(3, 4))
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
    val result = obj.zip(List(1, 2, 3), List("a", "b", "c"))
    val expectResult = List((1, "a"), (2, "b"), (3, "c"))
    assert(result === expectResult)
  }
  //15th
  test("Merge Of Two sorted List") {
    val result = obj.merge2(List(1, 3, 5), List(2, 4))
    val expectResult = List(1, 2, 3, 4, 5)
    assert(result === expectResult)
  }

  //16th
  val sal = obj.Salary(20.0, 30.0, 40.0)
  val emp = obj.Employee(1, "gmail", sal, 60)
  val emp1 = obj.Employee(2, "gmail2", sal, 50)
  test("appraisal") {
    val result = obj.appraisal(List(emp, emp1))
    val expectResult = List(obj.Employee(1, "gmail", obj.Salary(22.0, 36.0, 40.0), 60), obj.Employee(2, "gmail2", obj.Salary(22.0, 30.0, 40.0), 50))
    assert(result === expectResult)
  }

  //17th
  val stu1 = obj.Student(1, "Surekha", 22, "CS")
  val stu2 = obj.Student(2, "Sur", 23, "IT")
  val stu3 = obj.Student(3, "Rekha", 24, "ME")
  val stu4 = obj.Student(4, "Rakhi", 25, "EC")
  val stu5 = obj.Student(5, "Suman", 32, "CS")
  test("Student List") {
    val result = obj.spiltByBranch(List(stu1, stu2, stu3, stu4, stu5))
    val expectResult = (List(obj.Student(1, "Surekha", 22, "CS"), obj.Student(5, "Suman", 32, "CS")), List(obj.Student(2, "Sur", 23, "IT")), List(obj.Student(4, "Rakhi", 25, "EC")), List(obj.Student(3, "Rekha", 24, "ME")))
    assert(result === expectResult)
  }

  //18th
  val c1 = obj.Customer(2)
  val c2 = obj.Customer(5)
  val c3 = obj.Customer(6)
  val c4 = obj.Customer(8)
  val c5 = obj.Customer(10)
  val c6 = obj.Customer(18)


  val consultant1 = obj.Consultant(List(c1, c2))
  val consultant2 = obj.Consultant(List(c3, c4))
  val consultant3 = obj.Consultant(List(c2, c3))
  val consultant4 = obj.Consultant(List(c1, c2, c3, c4))
  val consultant5 = obj.Consultant(List(c1, c2, c5, c6))

  val branch1 = obj.Branch(List(consultant1, consultant2, consultant3))
  val branch2 = obj.Branch(List(consultant1, consultant3, consultant4))
  val branch3 = obj.Branch(List(consultant3, consultant4, consultant5))
  val branch4 = obj.Branch(List(consultant4, consultant5))


  val company = obj.Company(List(branch1, branch2, branch3, branch4))
  test("Company Value") {
    val result = obj.getCompanyValue(company)
    val expectResult = 194
    assert(result === expectResult)
  }

}
