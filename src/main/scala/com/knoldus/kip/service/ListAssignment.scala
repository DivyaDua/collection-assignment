package com.knoldus.kip.service

import com.knoldus.kip.RamDatabase
import com.knoldus.kip.models.{Marks, Student}

trait ListAssignment {

  //Assignment -1
  def failedStudents(subjectId: Long, percentage: Double, passOrFail: String): Int = {
    val list: List[Marks] = RamDatabase.marksList.filter(_.subjectId == subjectId)
    val totalStudents = list.length
    val passedStudents = list.filter(_.marksObtained  >= percentage).length

    passOrFail match {
      case "pass" => passedStudents
      case "fail" => totalStudents - passedStudents
    }
  }


  def topBottomStudents(subjectId: Long, count: Int, topOrBottom: String): List[Student] = {
    val marksList: List[Marks] = RamDatabase.marksList.filter(_.subjectId == subjectId).sortBy(_.marksObtained)

    val studentsList: List[Student] = marksList.flatMap(x => RamDatabase.studentList.filter(_.id == x.studentId))

   topOrBottom match {
     case "top" => studentsList.reverse.take(count)
     case "bottom" => studentsList.take(count)
   }
  }

  def topAndLeastScorers(topOrBottom: String, count: Int): List[Student] = {
    val marksList: List[Marks] = RamDatabase.marksList
    val studentsList = RamDatabase.studentList

    val totalMarks: List[Float] = studentsList.map(x => marksList.filter(_.studentId == x.id).map(_.marksObtained).sum)

    val mergedList = studentsList.zip(totalMarks).sortBy(_._2)

    topOrBottom match {
      case "top" => mergedList.reverse.take(count).map(_._1)
      case "bottom" => mergedList.take(count).map(_._1)
    }
  }


  def getScholarshipGroups(percentage: Float, goodScholarship: Int, normalScholarship: Int)
  : (List[(Student, Int)], List[(Student, Int)]) = {

    val marksList: List[Marks] = RamDatabase.marksList
    val studentsList = RamDatabase.studentList

    val subjectsCount = marksList.length / studentsList.length
    val percentageList: List[Float] = studentsList.map(x => marksList.filter(_.studentId == x.id).map(_.marksObtained).sum/subjectsCount)


    val mergedList = studentsList.zip(percentageList).sortBy(_._2)

    val goodScholarshipList: List[(Student, Int)] = mergedList.filter(_._2 >= percentage).map(_._1).map(x => (x, goodScholarship))
    val normalScholarshipList: List[(Student, Int)] = mergedList.filter(_._2 < percentage).map(_._1).map(x => (x, normalScholarship))

    /*val goodScholarshipList: List[(Student, Int)] = for {student <- studentsList
                                                         mergedData <- mergedList
                                                         if student.id == mergedData._1.id && mergedData._2 >= percentage
    }yield (student,goodScholarship)

    val normalScholarshipList: List[(Student, Int)] = for {student <- studentsList
                                                         mergedData <- mergedList
                                                         if student.id == mergedData._1.id && mergedData._2 < percentage
    }yield (student,normalScholarship)*/

    (goodScholarshipList, normalScholarshipList)
  }

  def passedOrFailed(passOrFail: String, percentage: Float): List[Student] = {
    val marksList: List[Marks] = RamDatabase.marksList
    val studentsList = RamDatabase.studentList

    val subjectsCount = marksList.length / studentsList.length
    val percentageList: List[Float] = studentsList.map(x => marksList.filter(_.studentId == x.id).map(_.marksObtained).sum/subjectsCount)

    val mergedList = studentsList.zip(percentageList).sortBy(_._2)

    passOrFail match {
      case "pass" => mergedList.filter(_._2 >= percentage).map(_._1)
      case "fail" => mergedList.filter(_._2 < percentage).map(_._1)
    }
  }


  def studentsWithMoreThan95: List[Student] = {
    val marksList: List[Marks] = RamDatabase.marksList
    val studentsList = RamDatabase.studentList

    val subjectsCount = marksList.length / studentsList.length

    val percentageList: List[Float] = studentsList.map(x => marksList.filter(_.studentId == x.id).map(_.marksObtained).sum/subjectsCount)

    val mergedList = studentsList.zip(percentageList).sortBy(_._2)

    mergedList.filter(_._2 >= 95).map(_._1)
  }


  def generateReport: List[(String, List[Int])] = {
    val studentsList = RamDatabase.studentList
    val marksList = RamDatabase.marksList.sortBy(_.studentId)

    val studentsName: List[String] = studentsList.map(_.name)
    val marks: List[List[Int]] = studentsList.map(x => marksList.groupBy(x.id == _.studentId)(true).map(_.marksObtained.toInt))

    studentsName.zip(marks)
  } //Must use the groupBy() Method of the List*/


  //Assignment - 2
  def getLastElementWithIndex(list: List[String]): (String, Int) = {

    def getLastElementWithIndex1(list: List[String], index: Int): (String, Int) = {
      list match {
        case Nil => ("",-1)
        case head :: Nil=> (head, index)
        case head :: tail => getLastElementWithIndex1(tail, index + 1)
      }
    }
    getLastElementWithIndex1(list,index = 0)
  }


  def printTable(list: List[Long]): List[Long] = {
    for { l <- list
          x <- 1 to 10
    }yield l*x
  }


  def aggregateLists(list1: List[String], list2: List[Long]): List[List[(String, Long)]] = {

    list1.zip(list2).map(x => List(x))
  }

    /*def aggregate(list1: List[String], list2: List[Long], list3: List[List[(String, Long)]]): List[List[(String, Long)]] = {
      (list1, list2) match {
      case (h1 :: t1, h2 :: t2) => aggregate(t1, t2, list3 ::: List(List(h1, h2)))
        case (Nil, Nil) => list3
      }
    }

    aggregate(list1,list2,list3 = List(List(("",-1)))*/



  def getSumOfList(list: List[Long]): Long = {
    list match {
      case head :: tail => getSumOfList(tail) + head
      case Nil => 0
    }
  }


  def getMultiplicationOfList(list: List[Long]) : Long = {
      list match {
        case head :: tail => getMultiplicationOfList(tail)* head
        case head :: Nil => head
        case Nil => 1
      }
    }


  def quickSortList(list: List[Long]): List[Long] = {
    list match {
    case Nil => Nil
    case head :: tail => quickSortList(tail.filter(_ < head)) ::: head :: quickSortList(tail.filter(_ >= head))
    }
  }

  def mergeSortList(list: List[Long]): List[Long] = {

    def merge(l1: List[Long], l2: List[Long]): List[Long] = {
      (l1, l2) match {
        case (Nil, _) => l2
        case (_, Nil) => l1
        case (h1::t1, h2::t2) => if(h1 <= h2) {
          h1 :: merge(t1, l2)
        }
        else{
          h2 :: merge(l1, t2)
        }

      }
    }

    val n = list.length/2
    if(n == 0) {
      list
    }
    else {
      val (l1, l2) = list splitAt(n)
      merge(mergeSortList(l1), mergeSortList(l2))
    }
  }

}
