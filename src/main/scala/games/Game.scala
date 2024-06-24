package games

enum Players(val value: Int) {
  case None extends Players(0)
  case P1 extends Players(1)
  case P2 extends Players(2)
}

class Move(val x: Int, val y: Int) {
  override def toString: String = s"($x, $y)"
}

trait Game {
  trait GameState extends State

  val initialState: State

  val info: String

  def display(state: GameState): Unit
}

trait State {
  var currentPlayer: Players
  def getMoves: List[Move]

  def makeMove(move: Move): State

  def hasWinner: Boolean

  def winner: Players
}