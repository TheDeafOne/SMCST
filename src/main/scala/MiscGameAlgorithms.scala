import scala.math.random


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
