package games

import java.util.Scanner
import scala.util.control.Breaks.break
import algorithms.{MCTSMove, getSimpleTTTBHeuristic, greedyTTTMove, humanMove, minimaxTTTMove, parallelGreedyTTTMove, parallelMinimaxTTTMove, randomMove}
import utils.timeIt

@main
def run = {
    val result = (1 to 100).map(x => runGame(minimaxTTTMove(10), randomMove, verbose = false, TicTacToeBoard()))
//    val result = (1 to 100).map(x => runGame(randomMove, randomMove, verbose = false, TicTacToeBoard())).toList
    println(s"Player 1 won: ${result.count(_==1)} and drawed: ${result.count(_==(-1))}")
}

def runGame(player1Algo: TwoPlayerGame => Int, player2Algo: TwoPlayerGame => Int, verbose: Boolean = false, startingBoard: TwoPlayerGame): Int = {

  if (verbose) {
    println(startingBoard.getGameIntroText)
  }

  def gameLoop(gameBoard: TwoPlayerGame): Int = {
    if (gameBoard.hasWinner.isDefined) {
        if (gameBoard.hasWinner.get) {
          println(s"Game Over! Player ${gameBoard.getWinner}")
        }
        else {
          println("Game Over. Draw!")
        }
        println("Final game state: ")
        println(gameBoard.toString)
      return if(gameBoard.hasWinner.get) then gameBoard.getWinner else -1
    }
    else{
      //            if (verbose) {
      //                println(s"Current Board: Player ${if gameBoard.getNextPlayer then 1 else 2}")
      //                println(gameBoard.toString)
      //            }
      val moveToMake = if gameBoard.getNextPlayer then player1Algo(gameBoard) else player2Algo(gameBoard)

      // -1 is a resignation
      if (moveToMake == -1) {
        if (verbose) {
          println("Resignation...")
          println("Final Board State: ")
          println(gameBoard.toString)
          println("Quiting...")
        }
        return -2
      }
      else {
        try {
          val newBoard = gameBoard.makeMove(moveToMake)
          gameLoop(newBoard)
        }
        catch {
          case e => println(e.getMessage)
            gameLoop(gameBoard)
        }
      }
    }
  }
  gameLoop(startingBoard)
}