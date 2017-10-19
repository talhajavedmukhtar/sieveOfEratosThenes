package com.talhajavedmukhtar.eratos

object eratos extends App{
  def getList(last: Int): List[Int] = {
    2 to last toList
  }

  def removeMultiplesOf(num: Int, list: List[Int]): List[Int] = {
    list.filter(a => !(a%num == 0 && a != num) )
  }

  def nextPrime(lastPrime: Int, threshold: Int, list: List[Int]): Int = {
    val filtered = list.filter(a=>a>lastPrime)

    if(filtered.isEmpty || filtered.head > threshold){
      -1
    }else{
      filtered.head
    }
  }

  def removeAllNonPrime(currentPrime: Int, list: List[Int], size: Int): List[Int] = {

    val filtered = removeMultiplesOf(currentPrime,list)
    val nextPrimeNo = nextPrime(currentPrime,scala.math.sqrt(size.toDouble).toInt,filtered)

    if(nextPrimeNo == -1){ //nextPrime not found
      filtered
    }else{
      removeAllNonPrime(nextPrimeNo,filtered,size)
    }
  }



  def eratos(num: Int): List[Int] = {
    val myList = getList(num)

    removeAllNonPrime(2,myList,num)
  }

  val er = eratos(100)

}
