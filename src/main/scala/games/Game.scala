package games

enum Players(val value: Int) {
  case None extends Players(0)
  case P1 extends Players(1)
  case P2 extends Players(2)
}

class Move(val x: Int, val y: Int) {
  override def toString: String = s"($x, $y)"
}

trait State {
  var currentPlayer: Players
  def getMoves: List[Move]

  def makeMove(move: Move): Unit

  def hasWinner: Boolean

  def winner: Players

  def copy: State
}