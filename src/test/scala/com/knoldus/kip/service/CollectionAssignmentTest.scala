package com.knoldus.kip.service

import com.knoldus.kip.models.ScoreCard
import org.scalatest.FunSuite

class CollectionAssignmentTest extends FunSuite with CollectionAssignment{

  test("testing getScorecardsByName method"){
    val m: Map[Long, Float] = Map(1.toLong->100.toFloat,2.toLong->90.toFloat,3.toLong->85.toFloat,4.toLong->60.toFloat,5.toLong->90.toFloat)
    val s = ScoreCard(1,m,85.toFloat)
    assert(getScorecardsByName("Anmol") == List(s))
  }

}
