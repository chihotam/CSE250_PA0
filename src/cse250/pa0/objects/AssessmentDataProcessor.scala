/**
 * cse250.pa0.objects.AssessmentDataProcessor.scala
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
package cse250.pa0.objects

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import cse250.objects.TaxParcel
import scala.io.Source

object AssessmentDataProcessor {
  def sanitizeData(filename: String): Unit = {
    // For opening files, look at Scala Cookbook File I/O Excerpt
    val inputFile = scala.io.Source.fromFile(filename)
    // Note: lines is an iterator to the file. This is only valid as long as the file is open.
    //       Ensure you do not close the file prior to finishing the file usage.
    val outputFile = new BufferedWriter(new FileWriter(new File(filename + "-updated")))
    val lines = inputFile.getLines()
    val remove: List[Int] = List(11, 12, 13, 14, 15, 18, 22, 23, 24, 25, 26, 31, 32, 33, 34, 35, 36, 37, 38, 39, 42, 45, 46, 47)
    var data = ""
    var quote = false
    var indexCount = 1
    for(line <- lines){
      for(letter <- line){
        data = data + letter
        if(letter.toString == ","){
          if(!quote){
            if(!remove.contains(indexCount)){
              outputFile.write(data)
            }
            data = ""
            indexCount = indexCount + 1
          }
        }
        else{
          if(letter.toString == "\""){
            if(quote){
              quote = false
            }
            else{
              quote = true
            }
          }
        }
      }
      outputFile.write(data)
      outputFile.write("\n")
      data = ""
      indexCount = 1
    }

    // Without the '\n' character, all output will be written as one long line.
    // Process the lines.

    // Close the files at the end.
    inputFile.close()
    outputFile.close()
  }

  def computeOldestEntry(filename: String): TaxParcel = {
    val inputFile = scala.io.Source.fromFile(filename)
    val lines = inputFile.getLines()
    var header: List[String] = List()
    var lis: List[String] = List()
    var holder: List[String] = List()
    var firstLine = true
    var quote = false
    var oldest = false
    var data = ""
    var old = 5000
    var indexCount = 1
    for(line <- lines){
      if(firstLine){
        val temp: List[String] = line.split(",").toList
        for(x <- temp){
          header = header :+ x
        }
        firstLine = false
      }
      else{
        for(letter <- line){
          data = data + letter
          if(letter.toString == ","){
            if(!quote){
              if(indexCount == 19 && data != "NA," && data != ","){
                val x = data.dropRight(1)
                if(x.toInt != 1 && x.toInt < old){
                  oldest = true
                  old = x.toInt
                }
              }
              holder = holder :+ data.dropRight(1)
              data = ""
              indexCount = indexCount + 1
            }
          }
          else{
            if(letter.toString == "\""){
              if(quote){
                quote = false
              }
              else{
                quote = true
              }
            }
          }
        }
      }
      if(oldest){
        holder = holder :+ data
        lis = holder
      }
      data = ""
      holder = List()
      oldest = false
      indexCount = 1
    }
    val file: TaxParcel = new TaxParcel
    for(x <- header.indices){
      file.parcelInfo += (header(x) -> lis(x))
    }
    file
  }

  def countPriceRange(filename: String, lower: Int, upper: Int): Int = {
    val inputFile = scala.io.Source.fromFile(filename)
    val lines = inputFile.getLines()
    var data = ""
    var count = 0
    var indexCount = 1
    var firstLine = true
    var quote = false
    for(line <- lines){
      if(firstLine){
        firstLine = false
      }
      else{
        for(letter <- line){
          data = data + letter
          if(letter.toString == ","){
            if(!quote){
              if(indexCount == 17){
                val x = data.dropRight(1)
                if(x.toInt >= lower && x.toInt < upper){
                  count = count + 1
                }
              }
              data = ""
              indexCount = indexCount + 1
            }
          }
          else {
            if(letter.toString == "\""){
              if(quote){
                quote = false
              }
              else{
                quote = true
              }
            }
          }
        }
        data = ""
        indexCount = 1
      }
    }
    count
  }
}