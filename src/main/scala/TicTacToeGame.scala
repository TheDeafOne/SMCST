
// Space values
// 1   2    4
// 8   16  32
// 64 128 256

//Space names
// 1 2 3
// 4 5 6
// 7 8 9

class TicTacToeBoard(val player1Pieces: Int = 0, val player2Pieces: Int = 0, val playerOneNext: Boolean = true) extends TwoPlayerGame{

    // The length of one row / column
    val boardLength = 3;
    // The total number of spaces on the board
    val boardSize = boardLength * boardLength;

    /**
     * If the game is not over, will return None.
     *
     * If someone has won, it will return Option[true]
     * If the game is over, and it's a draw, it will return Option[false].
     * @return Option[Boolean]
     */
    def hasWinner: Option[Boolean] = {
        if(player1Pieces + player2Pieces == getBoardSpace(boardSize + 1) - 1){
            return Option(false)
        }

        val horizontal = for(x <- 1 to boardLength) yield (x to boardSize by boardLength).map(getBoardSpace).sum
        val vertical = for (x <- 0 until boardLength) yield (1 + x*boardLength to boardLength + x*boardLength).map(getBoardSpace).sum

        val forwardDiag = (1 to boardSize by boardLength + 1).map(getBoardSpace).sum
        val backwardDiag = (boardLength to 1 by -1).map(x => x + (boardLength - x) * boardLength).map(getBoardSpace).sum

        val masks = (horizontal concat vertical).toList :+ forwardDiag :+ backwardDiag

        masks.foreach(mask => {
            if (((player1Pieces & mask) ^ mask) == 0 || ((player2Pieces & mask) ^ mask) == 0) {
                return Some(true)
            }
        })
        None
    }

    /**
     * If player one has won, it will return 1
     * If player two has won, it will return 2.
     * @return Int of the corresponding player
     * @throws IllegalAccessError If the game is not over or a draw
     */
    def getWinner: Int = {
        if(hasWinner.isEmpty || !hasWinner.get){
            throw IllegalAccessError("The game is not over or it's a draw")
        }
        if(!playerOneNext) then 1 else 2
    }

    /**
     * @return true if player 1 is next else false
     */
    def getNextPlayer: Boolean = {
        playerOneNext
    }

    /**
     * @param space The square from 1-boardSize which is being picked to play
     * @return If it is a valid move
     */
    def isValidMove(space: Int): Boolean = space > 0 && space <= boardSize && ((player1Pieces + player2Pieces) & getBoardSpace(space)) == 0

    /**
     * @return All the values 1-boardSize which are legal moves
     */
    def getValidMoves: List[Int] = (for i <- 1 to boardSize if isValidMove(i) yield i).toList

    /**
     * @param space The move on which the player should play on
     * @return The next board after the move was played
     * @throws IllegalArgumentException Invalid move, space already occupied
     * @throws IllegalStateException Cannot play, Game already over
     */
    def makeMove(space: Int): TicTacToeBoard = {
        if (!isValidMove(space)){
            throw IllegalArgumentException("Invalid move, space already occupied")
        }
        if(hasWinner.isDefined){
            throw IllegalStateException("Cannot play, Game already over")
        }

        val boardSpace = getBoardSpace(space)
        if (playerOneNext) {
            new TicTacToeBoard(player1Pieces + boardSpace, player2Pieces, !playerOneNext)
        }
        else {
            new TicTacToeBoard(player1Pieces, player2Pieces + boardSpace, !playerOneNext)
        }
    }

    override def toString: String = {
        val boardTuples = padBinaryString(player1Pieces.toBinaryString, boardSize).zip(padBinaryString(player2Pieces.toBinaryString, boardSize))
        val outStr = boardTuples.map(x => if x._1 == '1' then "x" else (if x._2 == '1' then "O" else "-")).mkString
        outStr.reverse.grouped(boardLength).map(_+"\n").mkString
    }

    /**
     * @param str The base string
     * @param desiredNumDigits The number of digits in the resulting string
     * @return A binary string of length desiredNumDigits
     */
    private def padBinaryString(str: String, desiredNumDigits: Int) = ("0" * (desiredNumDigits - str.length)) + str

    /**
     *
     * @param readableValue The human readable value of the board, ie. 1-boardSize
     * @return The base 2 value for that space
     */
    private def getBoardSpace(readableValue: Int): Int = Math.pow(2, readableValue - 1).toInt


    def getGameIntroText = {
        "Playing TicTacToe: \n" +
        "Board spaces: \n" +
        "1 2 3\n" +
        "4 5 6\n" +
        "7 8 9\n"
    }


}


def humanMove(gameBoard: TwoPlayerGame): Int  = {
    println(s"Current Board: Player ${if gameBoard.getNextPlayer then 'X' else 'O'}")
    println(gameBoard.toString)
    println("Enter your move on the board, or -1 to quit")
    getIntegerInput
}