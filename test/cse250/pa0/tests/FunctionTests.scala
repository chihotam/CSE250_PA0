/**
 * cse250.pa0.tests.FunctionTests.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: chihotam
 * Person#: 50301678
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa0.tests

import cse250.pa0.objects.Functions
import org.scalatest.FlatSpec

class FunctionTests extends FlatSpec {
  // Tests for problem 1.
  behavior of "FunctionsTest.genNum"

  it should "take 0 and return 0" in {
    val a = Functions.genNum(0)
    assert(a == 0)
  }
  it should "take 100 and return # between 0 and 100" in {
    val a = Functions.genNum(100)
    assert(a >= 0 && a <= 100)
  }

  // test("FunctionsTest.genNum should ...")

  // Tests for problem 2.
  behavior of "FunctionsTest.genSeq"

  it should "take 0 and return empty sequence" in {
    val a = Functions.genSeq(0)
    assert(a.isEmpty)
  }

  it should "take 10 and return a sequence of length 10 and all positive numbers" in {
    val a = Functions.genSeq(10)
    val boo = true
    for(x <- a){
      if(x < 1){
        assert(a.length == 10 && !boo)
      }
    }
    assert(a.length == 10 && boo)
  }

  // Tests for problem 3.
  behavior of "FunctionsTest.funThree"

  it should "take 1 and return that this is not a bitonic sequence" in {
    val boo = true
    Functions.funThree(1)
    assert(!boo)
  }
  it should "take 1,2 and return that this is not a bitonic sequence" in {
    val boo = true
    Functions.funThree(1)
    Functions.funThree(2)
    assert(!boo)
  }
  it should "take 1,2,3 and return a bitonic sequence" in {
    val boo = true
    val a = Functions.funThree(1)
    val b = Functions.funThree(2)
    val c = Functions.funThree(3)
    if(a < b && b > c){
      assert(boo)
    }
    else{
      assert(!boo)
    }
  }

  it should "take 1-10 and return a bitonic sequence" in {
    val boo = true
    val a = Functions.funThree(1)
    val b = Functions.funThree(2)
    val c = Functions.funThree(3)
    val d = Functions.funThree(4)
    val e = Functions.funThree(5)
    val f = Functions.funThree(6)
    val g = Functions.funThree(7)
    val h = Functions.funThree(8)
    val i = Functions.funThree(9)
    val j = Functions.funThree(10)
    val lis: List[Int] = List(a, b, c, d, e, f, g, h, i, j)
    var current = 0
    var previous = -1
    var count = 0
    var peak = false
    for (x <- lis) {
      current = x
      if (current == previous) {
        assert(!boo)
      }
      else {
        if (!peak) {
          if (current < previous) {
            if (count > 0) {
              peak = true
              count = 0
            }
            else {
              assert(!boo)
            }
          }
          else {
            count = count + 1
          }
        }
        else {
          if (current > previous) {
            assert(!boo)
          }
          else {
            count = count + 1
          }
        }
      }
      previous = current
    }
    if (count > 0 && peak) {
      assert(boo)
    }
    else {
      assert(!boo)
    }
  }

  // Tests for problem 4.
  behavior of "FunctionsTest.mapSum"

  def f1(x: Int) = { x + 1 }

  it should "take 1 and return f1(1)" in {
    val a = Functions.mapSum(1, f1)
    assert(a == f1(1))
  }

  it should "take 100 and return the sum of each # from 1-100 times f(#)" in {
    val a = Functions.mapSum(100, f1)
    var sum = 0
    for(x <- 1 to 100){
      sum = sum + (x * f1(x))
    }
    assert(a == sum)
  }
}
