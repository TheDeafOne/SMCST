package games

import java.util.Scanner
import scala.util.control.Breaks.break
import algorithms.{humanMove, minimaxTTTMove, greedyTTTMove, randomMove, parallelMinimaxTTTMove, parallelGreedyTTTMove, getSimpleTTTBHeuristic}
import utils.timeIt

@main
def run = {
  //    runGame(humanMove, humanMove, verbose = true, TicTacToeBoard())
  //    runGame(humanMove, randomMove, verbose = true, TicTacToeBoard())
  //    runGame(humanMove, greedyTTTMove(), verbose = true, TicTacToeBoard())
//  runGame(humanMove, minimaxTTTMove(10), verbose = true, TicTacToeBoard())
    timeRuns
}

def timeRuns = {
    println("Running Random: ")
    val outR = timeIt(runGame(randomMove, randomMove, verbose = false, TicTacToeBoard()))
    println(s"Ran in ${outR._1}ms\n")

    println("Running Greedy: ")
    val outG = timeIt(runGame(minimaxTTTMove(10), minimaxTTTMove(10), verbose = false, TicTacToeBoard()))
    println(s"Ran in ${outG._1}ms\n")
    println("Running ParGreedy: ")
    val outPG = timeIt(runGame(parallelMinimaxTTTMove(10), parallelMinimaxTTTMove(10), verbose = false, TicTacToeBoard()))
    println(s"Ran in ${outPG._1}ms\n")

    println("Running SimpleMinimax: ")
    val outSMM = timeIt(runGame(minimaxTTTMove(10, getSimpleTTTBHeuristic), minimaxTTTMove(10, getSimpleTTTBHeuristic), verbose = false, TicTacToeBoard()))
    println(s"Ran in ${outSMM._1}ms\n")
    println("Running SimpleParallelMinimax: ")
    val outPSMM = timeIt(runGame(parallelMinimaxTTTMove(10, getSimpleTTTBHeuristic), parallelMinimaxTTTMove(10, getSimpleTTTBHeuristic), verbose = false, TicTacToeBoard()))
    println(s"Ran in ${outPSMM._1}ms\n")

    println("Running Minimax: ")
    val outMM = timeIt(runGame(minimaxTTTMove(10), minimaxTTTMove(10), verbose = false, TicTacToeBoard()))
    println(s"Ran in ${outMM._1}ms\n")
    println("Running ParMinimax: ")
    val outPMM = timeIt(runGame(parallelMinimaxTTTMove(10), parallelMinimaxTTTMove(10), verbose = false, TicTacToeBoard()))
    println(s"Ran in ${outPMM._1}ms\n")
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