package games

import games.Player

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


//object Players extends Enumeration {
//  type Player = Value
//  val None, Player1, Player2 = Value
//}
//trait Player {
//  def common: Boolean
//}
//object Players {
//  case object None extends Player {
//    val common: Boolean = true
//  }
//
//  case object Player1 extends Player {
//    val common: Boolean = true
//  }
//
//  case object Player2 extends Player {
//    val common: Boolean = true
//  }
//}
trait Player {
  var common: Boolean
}

class MutablePlayer(var common: Boolean = false, val name: String = "") extends Player {
  override def toString: String = name
}
object Players {
  val None = new MutablePlayer(true, "None")
  val Player1 = new MutablePlayer(true, "Player1")
  val Player2 = new MutablePlayer(true, "Player2")
}

class Move(val x: Int, val y: Int, val currX: Int = 0, val currY: Int = 0) {
  override def toString: String = s"($x, $y) ($currX, $currY)"
}

//class CheckersMove(override val x: Int, override val y: Int, val currX: Int, val currY: Int) extends Move(x, y) {
//  override def toString: String = s"($x, $y) currPos:($currX, $currY)"
//}


trait State {
  var currentPlayer: Player
  def getMoves: List[Move]

  def makeMove(move: Move): Unit

  def hasWinner: Boolean

  def getWinner: Player

  def copy: State
}

trait BitBoard(val board: Long = 0) extends State
