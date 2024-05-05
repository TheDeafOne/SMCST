
import java.util.Scanner
import scala.util.control.Breaks.break

@main
def run = {
//    runGame(humanMove, humanMove, verbose = true, TicTacToeBoard())
//    runGame(humanMove, randomMove, verbose = true, TicTacToeBoard())
    runGame(humanMove, greedyTTTMove, verbose = true, TicTacToeBoard())

}

def runGame(player1Algo: TwoPlayerGame => Int, player2Algo: TwoPlayerGame => Int, verbose: Boolean = false, startingBoard: TwoPlayerGame): Int = {

    if (verbose) {
        println(startingBoard.getGameIntroText)
    }

    def gameLoop(gameBoard: TwoPlayerGame): Int = {
        if (gameBoard.hasWinner.isDefined) {
            if(verbose) {
                if (gameBoard.hasWinner.get) {
                    println(s"Game Over! Player ${gameBoard.getWinner}")
                }
                else {
                    println("Game Over. Draw!")
                }
                println("Final game state: ")
                println(gameBoard.toString)
            }
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



trait TwoPlayerGame {
    /**
     * If the game is not over, will return None.
     *
     * If someone has won, it will return Option[true]
     * If the game is over, and it's a draw, it will return Option[false].
     * @return Option[Boolean]
     */
    def hasWinner: Option[Boolean]

    /**
     * @return the winner as an int
     * @throws IllegalAccessError If the game is not over or a draw
     */
    def getWinner: Int

    /**
     * @param space The move on which the player should play on
     * @return The next board after the move was played
     * @throws IllegalArgumentException Invalid move, space already occupied
     * @throws IllegalStateException    Cannot play, Game already over
     */
    def makeMove(space: Int): TwoPlayerGame

    /**
     * @return true if player 1 is next else false
     */
    def getNextPlayer: Boolean

    /**
     * @return a string describing how to play the game
     */
    def getGameIntroText: String

    /**
     * @return All the values 1-boardSize which are legal moves
     */
    def getValidMoves: List[Int]

}


/**
 * Asks the user through the command line for an integer
 * @return the int
 */
def getIntegerInput: Int = {
    val inputScanner = new Scanner(System.in)

    def getInput(scanner: Scanner, validResult: Boolean, num: Int): Int = {
        if (validResult) {
            num
        }
        else {
            try {
                val input = inputScanner.nextInt()
                getInput(scanner, true, input)
            } catch {
                case e => {
                    inputScanner.nextLine()
                    println("Invalid Integer")
                    getInput(scanner, false, num)
                }
            }
        }
    }

    val outputNum = getInput(inputScanner, false, 0)
    outputNum
}