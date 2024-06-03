package games

import games.Players.Player

object Players extends Enumeration {
  type Player = Value
  val None: Players.Value = Value
}

class Move(val x: Int, val y: Int) {
  override def toString: String = s"($x, $y)"
}

trait State {
  var currentPlayer: Player
  def getMoves: List[Move]

  def makeMove(move: Move): Unit

  def hasWinner: Boolean

  def winner: Player

  def copy: State
}