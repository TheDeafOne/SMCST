package games

enum Player(val value: Int) {
  case None extends Player(0)
  case PlayerOne extends Player(1)
  case PlayerTwo extends Player(2)
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