package games

import algorithms.{Move, Players}
import algorithms.Players.Player

trait TwoPlayerGame {
  /**
   * If the game is not over, will return None.
   *
   * If someone has won, it will return Option[true]
   * If the game is over, and it's a draw, it will return Option[false].
   *
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


trait State(var currentPlayer: Player = Players.Player1) {
  def getMoves: List[Move]
  def makeMove(move: Move): Unit
  def isTerminal: Boolean
  def getWinner: Player

  def copy: State
}

trait BitBoard(val board: Long = 0) extends State

class ABGame(var board: List[Player] = List(Players.Player1, Players.None, Players.None, Players.Player2)) extends State {
  override def getMoves: List[Move] = {
    val playerIndex = board.indexOf(currentPlayer)
    val leftIndex = if playerIndex == 0 then -1 else if board(playerIndex - 1) == Players.None then playerIndex - 1 else playerIndex - 2
    val rightIndex = if playerIndex == board.length - 1 then -1 else if board(playerIndex + 1) == Players.None then playerIndex + 1 else playerIndex + 2
    List(new Move(leftIndex, 0), new Move(rightIndex, 0)).filter(m => m.x >= 0 && m.x < board.size)
  }

  override def makeMove(move: Move): Unit = {
    board = board.updated(board.indexOf(currentPlayer), Players.None)
    board = board.updated(move.x, currentPlayer)
    println("make move board: " + board)
    currentPlayer = if (currentPlayer == Players.Player1) Players.Player2 else Players.Player1
  }

  override def isTerminal: Boolean = {
    board.indexOf(Players.Player1) == board.length - 1 || board.indexOf(Players.Player2) == 0
  }

  override def getWinner: Player = {
    if (board.indexOf(Players.Player1) == board.length - 1 && board.indexOf(Players.Player2) == 0) Players.None
    else if (board.indexOf(Players.Player1) == board.length - 1) Players.Player1
    else Players.Player2
  }

  def copy: State = new ABGame(board)

  override def toString: String = board.mkString(" ")
}
