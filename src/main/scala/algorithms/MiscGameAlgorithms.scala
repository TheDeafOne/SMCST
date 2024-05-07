import games.{TicTacToeBoard, TwoPlayerGame}

import scala.math.random
import utils.padBinaryString
import utils.getIntegerInput

def humanMove(gameBoard: TwoPlayerGame): Int  = {
    println(s"Current Board: Player ${if gameBoard.getNextPlayer then 1 else 2}")
    println(gameBoard.toString)
    println("Enter your move on the board, or -1 to quit")
    getIntegerInput
}

def randomMove(gameBoard: TwoPlayerGame): Int  = {
    val move = gameBoard.getValidMoves((random() * gameBoard.getValidMoves.length).toInt)
    println(s"Random move of: ${move}")
    move
}

/**
 * Greedy Implementation of a TicTacToe AI
 * @param heuristic the function which takes a games.TicTacToeBoard and scores it
 * @param gameBoard A games.TicTacToeBoard, If not will return -1 because it can't play another game
 * @return the move the greedy algo thinks is best
 */
def greedyTTTMove(heuristic: TicTacToeBoard => Int = getScoringTTTBHeuristic)(gameBoard: TwoPlayerGame): Int = {
    if(!gameBoard.isInstanceOf[TicTacToeBoard]){
        return -1
    }

    val board = gameBoard.asInstanceOf[TicTacToeBoard]
    val playerCoeff = if(gameBoard.getNextPlayer) then 1 else -1
    gameBoard.getValidMoves.map(move => (move, heuristic(gameBoard.makeMove(move).asInstanceOf[TicTacToeBoard]))).maxBy(_._2*playerCoeff)._1
}


/**
 * Minimax Implementation of a TicTacToe AI
 * @param depth the depth which the AI will search
 * @param heuristic the function which takes a games.TicTacToeBoard and scores it
 * @param gameBoard A games.TicTacToeBoard, If not will return -1 because it can't play another game
 * @return the move the algo thinks is best
 */
def minimaxTTTMove(depth: Int, heuristic: TicTacToeBoard => Int = getScoringTTTBHeuristic)(gameBoard: TwoPlayerGame): Int = {
    if (!gameBoard.isInstanceOf[TicTacToeBoard]) {
        return -1
    }
    val playerCoeff = if (gameBoard.getNextPlayer) then 1 else -1

    def minimaxSearch(d: Int, boardState: TicTacToeBoard): Int = {
        if(d == 0 || boardState.hasWinner.isDefined) {
            return heuristic(boardState)
        }
        val localPlayerCoeff = if (boardState.getNextPlayer) then 1 else -1
        boardState.getValidMoves.map(move => (move, minimaxSearch(d - 1, boardState.makeMove(move)))).maxBy(_._2 * localPlayerCoeff)._2
    }
    gameBoard.getValidMoves.map(move => (move, minimaxSearch(depth, gameBoard.makeMove(move).asInstanceOf[TicTacToeBoard]))).maxBy(_._2 * playerCoeff)._1
}


/**
 * Scores the board based on how close to a 3 in a row it is for either player
 * @param gameBoard a games.TicTacToeBoard
 * @return a Int representing how close it is to either player winning, More negative is more in favor of player 2, positive, player 1
 */

def getScoringTTTBHeuristic(gameBoard: TicTacToeBoard): Int = {
    val p1Binary = padBinaryString(gameBoard.player1Pieces.toBinaryString, gameBoard.boardSize).toList
    val p2Binary = padBinaryString(gameBoard.player2Pieces.toBinaryString, gameBoard.boardSize).toList
    val boardString = p1Binary.zip(p2Binary).map(x => if (x._2 == '1') then "2" else x._1).mkString.reverse

    val rowStrings = boardString.grouped(gameBoard.boardLength).toList
    val colStrings = rowStrings.transpose.map(_.mkString)
    val posDiagString = (for i <- rowStrings.indices yield rowStrings(i)(i)).mkString
    val negDiagString = (for i <- rowStrings.indices yield rowStrings(i)(gameBoard.boardLength-1 - i)).mkString

    val lines = (rowStrings concat colStrings) :+ posDiagString :+ negDiagString

    val scores = lines.map(line => line.foldLeft(1)((score, item) =>{
        val scalingFactor = 8
        if(item == '0') {
            score
        }
        else if(item == '1' && score > 0) {
            score * scalingFactor
        }
        else if(item == '2' && (score == 1 || score < 0)){
            if(score == 1) score * -scalingFactor else score * scalingFactor
        }
        else{
            0
        }
    }))
//    println(rowStrings)
//    println(colStrings)
//    println(posDiagString)
//    println(negDiagString)
//
//    println(lines)
//    println(scores)
//    println(scores.sum)
    scores.sum
}
