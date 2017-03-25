/**
  * Created by constie on 20.03.2017.
  */

import scala.io.StdIn

class ticTacToe {

  type Move = (Int, Int)
  type Field = IndexedSeq[(Move, String)]

  var board: Field = for (i <- 1 to 3; j <- 1 to 3) yield (i, j) -> ""
  var draw: Boolean = false


  val winningPattern = List(
    //horizontal
    ((1, 1), (1, 2), (1, 3)),
    ((2, 1), (2, 2), (2, 3)),
    ((3, 1), (3, 2), (3, 3)),
    //vertical
    ((1, 1), (2, 1), (3, 1)),
    ((2, 1), (2, 2), (2, 3)),
    ((3, 1), (3, 2), (3, 3)),
    //across
    ((1, 1), (2, 2), (3, 3)),
    ((3, 1), (2, 2), (1, 3))
  )

  def printBoard(): Unit ={
    this.board.grouped(3).foreach(x => println(x.mkString(" | ")))
    println
    printf("----------\n")
  }

  def generateAIMove(): Move = {
    val rand = util.Random
    var avalFields = this.board.filter(f => f._2 == "")
    println("yugdu: " + avalFields.length)
    if(avalFields.length == 2) this.draw = true
    var r = rand.nextInt(avalFields.length - 1)
    var (a, s) = avalFields(r)
    return a
  }

  def AIMark(): Unit = {
    val (row, column) = this.generateAIMove
    this.board = this.board.map(f => if(f._2 == "" && (row, column) == f._1) (f._1, "o") else f)
  }

  def playerMark(row: Int, column: Int): Unit = {
    this.board = this.board.map(f => if(f._2 == "" && (row, column) == f._1) (f._1, "x") else f)
  }

  def checkWin(): Boolean ={
    if(!this.draw){

      if(this.winningPattern.exists(a => this.board.contains(a._1, "o") && this.board.contains(a._2, "o") && this.board.contains(a._3, "o"))){
        println("you lose")
        return true
      } else if(this.winningPattern.exists(a => this.board.contains(a._1, "x") && this.board.contains(a._2, "x") && this.board.contains(a._3, "x"))){
        println("you win")
        return true
      } else {
        return false
      }
    } else {
      this.printBoard
      println("the game is a DRAW")
      return true
    }
  }


}

var game = new ticTacToe
var win = false
while (!win) {

  game.printBoard
  println("your move")
  println("give row")
  val row = StdIn.readInt()
  println("give column")
  val column = StdIn.readInt()
  game.playerMark(row, column)
  game.AIMark

  win = game.checkWin

}
