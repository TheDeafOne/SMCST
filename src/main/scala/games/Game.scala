package games

enum Players(val value: Int) {
  case None extends Players(0)
  case P1 extends Players(1)
  case P2 extends Players(2)
}

class Move(val x: Int, val y: Int) {
  override def toString: String = s"($x, $y)"
}

class GameInfo(val name: String, val description: String)

trait Board(val dimensions: List[Int]) {
  def get(nVector: List[Int]): Players

  def set(nVector: List[Int], player: Players): Unit
}

trait DimensionalBoard extends Board {
  val board: Array[Any]
}

trait Game {
  val initialState: State
  var state: State

  val info: GameInfo

  def display: Unit
}

trait State {
  var currentPlayer: Players
  def getMoves: List[Move]

  def makeMove(move: Move): State

  def hasWinner: Boolean

  def winner: Players
}