package com.knoldus.kip.models

class Queue(queueWithList: List[Int]) {

  def enqueue(x: Int): Queue = {
    new Queue(queueWithList ::: List(x))
  }

  def dequeue: Queue = {
    new Queue(queueWithList.tail)
  }
}
