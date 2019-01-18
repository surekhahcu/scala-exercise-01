package edu.hcu.assignment

import java.io.File

class Assignment {

  //1st
  def show(f: Int, n: Int): List[Int] = {
    val list = collection.mutable.ListBuffer[Int]()
    for (i <- 1 to n) {
      for (j <- 1 to f)
        list.append(i)
    }
    list.toList
  }


  def show2(f: Int, n: Int): List[Int] = {
    (1 to n).toList.flatMap { i => List.fill(f)(i) }
  }

  //2nd
  def reverse(list: List[Int]): List[Int] = {
    list.foldLeft(List[Int]()) { (temp, ele) => if (list.nonEmpty) ele :: temp else temp }
  }

  //3rd
  def rotate(a: Array[Int], n: Int): Array[Int] = {
    a.drop(n) ++ a.take(n)
  }


  //4th
  def sum(opt1: Option[Int], opt2: Option[Int]): Option[Int] = {
    opt1 match {
      case Some(n) =>
        opt2 match {
          case Some(n1) => Some(n + n1)
          case None => Some(n)

        }
      case None => opt2
    }

  }

  //5th
  def fill(element: Int, noOfTimes: Int): List[Int] = {
    val list = collection.mutable.ListBuffer[Int]()
    for (i <- 1 to noOfTimes)
      list.append(element)
    list.toList

  }


  def fill2(element: Int, noOfTimes: Int): List[Int] = {
    (1 to noOfTimes).toList.map(_ => element)
  }

  //6th
  def dedupe(list: List[Int]): List[Int] = {
    list.foldLeft(List[Int]()) { (temp, ele) => if (!temp.contains(ele)) temp :+ ele else temp }
  }

  //7th
  def wordCount(str: String): Map[String, Int] = {
    val str2 = str.split(" ")
    str2.groupBy { word => word }.map { case (word, list) => (word, list.length) }
  }


  /*  //8th
    implicit class Square(n: Int) {
      def square = n * n
    }*/


  //9th write the program that count the number files in directory if directory is valid otherwise return  None

  def countFiles(dir: String): Option[Int] = {
    try {
      Some(new File(dir).listFiles().length)
    } catch {
      case ex: Exception =>
        None
    }
  }

  //10th Remove all keys from Map if their value are odd
  def removeOdd(map: Map[String, Int]): Map[String, Int] = {
    map.filter { case (key, value) => value % 2 == 0 }

  }

  //11th Remove list of keys from Map(just like Map’s “ --” function )
  def removeKeys(keys: List[String], map: Map[String, Int]): Map[String, Int] = {
    map.filter { case (key, value) => !keys.contains(key) }
  }

  //12th Merge two list
  def merge(list1: List[Int], list2: List[Int]): List[Int] = {
    if (list2.nonEmpty) merge(list1 :+ list2.head, list2.tail) else list1
  }

  //13th Merge Two Map
  def concatenate(map1: Map[String, Int], map2: Map[String, Int]): Map[String, Int] = {
    map1.foldLeft(map2) { case (acc, (k, v)) => (acc + (k -> (v + acc.getOrElse(k, 0)))) }
  }

  //14th zip two list into one list without using list zip method
  def zip(list1: List[Int], list2: List[String]): List[(Int, String)] = {
    def zip(list1: List[Int], list2: List[String], acc: List[(Int, String)]): List[(Int, String)] = {
      if (list1.nonEmpty && list1.nonEmpty) {
        zip(list1.tail, list2.tail, acc :+ (list1.head, list2.head))
      } else {
        acc
      }
    }

    zip(list1, list2, List())
  }

  //15th Merge two sorted List Int A One Sorted List
  def merge2(list1: List[Int], list2: List[Int]): List[Int] = {
    val combinedList = list1.foldLeft(list2) { (acc, ele) => acc :+ ele }
    sort(combinedList)
  }


  def sort(list: List[Int]) = {
    def insert(list: List[Int], ele: Int): List[Int] = {
      list match {
        case Nil => ele :: Nil
        case head :: tail =>
          if (head > ele) ele :: head :: tail else head :: insert(tail, ele)
      }
    }

    def sort(list: List[Int], acc: List[Int]): List[Int] = {
      if (list.nonEmpty) {
        sort(list.tail, insert(list, list.head))
      } else {
        acc
      }
    }

    sort(list, Nil)
  }


  //16th
  //a) Increase 10 % basic
  // b) Increase 20% hra if age greater than 50
  case class Salary(basic: Double, hra: Double, ta: Double)


  case class Employee(id: Int, email: String, salary: Salary, age: Int)

  /*
  def appraisal(emps: List[Employee]): List[Employee] = {
    emps.map { emp =>
      val salary = emp.salary
      val updatedBasic = (salary.basic + (salary.basic * 10) / 100)
      val updatesHRA = if (emp.age > 50) (salary.hra + (salary.hra * 20) / 100) else salary.hra
      val updateSalary = Salary(updatedBasic, updatesHRA, salary.ta)
      Employee(emp.id, emp.email, updateSalary, emp.age)
    }
  }
*/

  def appraisal(emps: List[Employee]): List[Employee] = {
    emps.map { emp =>
      val salary = emp.salary
      val updatedBasic = (salary.basic + (salary.basic * 10) / 100)
      val updatesHRA = if (emp.age > 50) (salary.hra + (salary.hra * 20) / 100) else salary.hra
      val updateSalary = Salary(updatedBasic, updatesHRA, salary.ta)
      emp.copy(salary = updateSalary)
    }
  }


  //17th CS,IT,EC,ME
  case class Student(id: Int, name: String, age: Int, branch: String)


  def spiltByBranch(list: List[Student]): (List[Student], List[Student], List[Student], List[Student]) = {
    /*   val cs: List[Student] = list.filter(student => student.branch == "CS")
       val it: List[Student] = list.filter(student => student.branch == "IT")
       val ec: List[Student] = list.filter(student => student.branch == "EC")
       val me: List[Student] = list.filter(student => student.branch == "ME")*/

    val cs: List[Student] = list.filter { case Student(id, name, age, branch) => branch == "CS" }
    val it: List[Student] = list.filter { case Student(id, name, age, branch) => branch == "IT" }
    val ec: List[Student] = list.filter { case Student(id, name, age, branch) => branch == "EC" }
    val me: List[Student] = list.filter { case Student(id, name, age, branch) => branch == "ME" }
    (cs, it, ec, me)
  }


  //18th
  case class Customer(value: Int)

  case class Consultant(portfolio: List[Customer])

  case class Branch(consultants: List[Consultant])

  case class Company(branches: List[Branch])


  def getCompanyValue(company: Company): Int = {
    val valuesList: List[Int] =
      for {
        branch <- company.branches
        consultant <- branch.consultants
        customer <- consultant.portfolio
      } yield {
        customer.value
      }
    valuesList.foldLeft(0) { (acc, elem) => (acc + elem) }
  }


  def getCompanyValue2(company: Company): Int = {
    val valuesList: List[Int] =
      company.branches.flatMap { branch =>
        branch.consultants.flatMap { consultant =>
          consultant.portfolio.map(customer => customer.value)
        }
      }
    valuesList.foldLeft(0) { (acc, elem) => (acc + elem) }
  }


}