package com.knoldus.kip.service

import com.knoldus.kip.RamDatabase
import com.knoldus.kip.models.Gender._
import com.knoldus.kip.models.{Marks, ScoreCard, Student}

trait CollectionAssignment {

  def computeScorecards(id: Long): ScoreCard = {
    val list: List[Marks] = RamDatabase.marksList.filter(id == _.studentId)

    val subjectList : List[Long] = list.map(_.subjectId.toLong)
    val subjectsCount: Int = subjectList.length

    val marksList: List[Float] = list.map(_.marksObtained)

    val percentage: Float = marksList.sum / subjectsCount

    val map: Map[Long, Float] = subjectList zip marksList toMap

    ScoreCard(id, map, percentage)
  }


  //Collection Based - Assignment 1
  def generateScorecards: Map[String, AnyRef] = {

    /*val names: List[String] = RamDatabase.studentList.map(_.name)
    val scorecards: List[ScoreCard] = for{ s <- RamDatabase.studentList}
      yield computeScorecard(s.id)
    names zip scorecards toMap*/

    def computeMarks(id: Long): Map[Long, Float] = {
      val marks = RamDatabase.marksList.filter(id == _.studentId)
      marks.map(x => x.subjectId.toLong -> x.marksObtained).toMap[Long, Float]
    }

    def computePercentage(id: Long) = {
      val marks: List[Float] = RamDatabase.marksList.filter(id == _.studentId).map(_.marksObtained)
      marks.sum / marks.length
    }

    def computeScoreCard(student: List[Student]): List[ScoreCard] = {
      for (s <- student)
        yield new ScoreCard(s.id, computeMarks(s.id), computePercentage(s.id))
    }

    def compute(list: List[Student], map: Map[String, AnyRef]): Map[String, AnyRef] = {
      if (list.isEmpty) {
        map
      }
      else if (!list.tail.exists(_.name == list.head.name)) {

        val newmap = map + (list.head.name ->
          ScoreCard(list.head.id, computeMarks(list.head.id), computePercentage(list.head.id)))
        compute(list.tail, newmap)

      }
      else {
        val newmap = map + (list.head.name -> computeScoreCard(list.head :: list.tail.filter(_.name == list.head.name)))
        compute(list.tail.filter(_.name != list.head.name), newmap)
      }

    }
    compute(RamDatabase.studentList, Map[String, AnyRef]())
  }

  def getScorecardsByName(name: String): List[ScoreCard] = {

    val students: List[Student] = RamDatabase.studentList.filter(_.name == name).sortBy(_.id)
    val scorecardList: List[ScoreCard] = for{s <- students
                                              }yield computeScorecards(s.id)

    if(scorecardList.isEmpty) {
      throw new Exception("No Data Found")
    }
    else {
      scorecardList
    }

  }

  
  //Collection Based - Assignment 2
  def getScoreCardByGender: (List[ScoreCard], List[ScoreCard]) = {

    val maleStudents: List[Student] = RamDatabase.studentList.filter(_.gender == MALE).sortBy(_.id)
    val femaleStudents: List[Student] = RamDatabase.studentList.filter(_.gender == FEMALE).sortBy(_.id)

    val maleScorecard: List[ScoreCard] = for{s <- maleStudents
    }yield computeScorecards(s.id)

    val femaleScorecard: List[ScoreCard] = for{s <- femaleStudents
    }yield computeScorecards(s.id)

    (maleScorecard, femaleScorecard)
  }

  def getScoreCardsWithHigherPercentage: (List[ScoreCard], List[ScoreCard]) = {
    val t:(List[ScoreCard], List[ScoreCard]) = getScoreCardByGender

    val maleScorecard: List[ScoreCard] = t._1.filter(_.percentage >= 50)
    val femaleScorecard: List[ScoreCard] = t._2.filter(_.percentage >= 50)

    (maleScorecard, femaleScorecard)

  } //Internally calls getScoreCardByGender

  def getSimilarPercentageBwGroups: List[((String, ScoreCard), (String, ScoreCard))] = {

    val t:(List[ScoreCard], List[ScoreCard]) = getScoreCardByGender
    val maleScorecard: List[ScoreCard] = t._1
    val femaleScorecard: List[ScoreCard] = t._2

    val selectedFemales: List[ScoreCard] =for{f <- femaleScorecard
                                              m <- maleScorecard
                                              if f.percentage == m.percentage
    }yield f
    val femaleNames: List[String] = selectedFemales.flatMap(x => RamDatabase.studentList.filter(x.studentId == _.id).map(_.name))

    val selectedMales: List[ScoreCard] =for{m <- maleScorecard
                                            f <- femaleScorecard
                                              if f.percentage == m.percentage
    }yield m
    val maleNames: List[String] = selectedMales.flatMap(x => RamDatabase.studentList.filter(x.studentId == _.id).map(_.name))

    val list: List[((String, ScoreCard), (String, ScoreCard))] = maleNames.zip(selectedMales).zip(femaleNames.zip(selectedFemales))
    list
  }

  def femalePercentageNotInMales: List[(String, ScoreCard)] = {
    val t:(List[ScoreCard], List[ScoreCard]) = getScoreCardByGender

    val maleScorecard: List[ScoreCard] = t._1
    val femaleScorecard: List[ScoreCard] = t._2

    val selectedFemales: List[ScoreCard] =for{f <- femaleScorecard
                                              m <- maleScorecard
                                             if f.percentage != m.percentage
    }yield f
    val names: List[String] = selectedFemales.flatMap(x => RamDatabase.studentList.filter(x.studentId == _.id).map(_.name))

    names.zip(selectedFemales)
  }

}
