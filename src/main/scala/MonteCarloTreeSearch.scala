import Players.Player

import scala.annotation.tailrec

def monteCarloTreeSearch(root: Node): Move = {
  // selection
  var iterations = 0
  val maxIters = 10
  var current = root
  while (iterations < maxIters) {
    if (current.isTerminal) {
      current = current.children.maxBy(_.UCB1)
    } else {
      if (current.visits == 0) {
        // rollout

      }
    }
    iterations += 1
  }


  // expansion
  // simulation
  // backpropagation
  Move(0, 0)
}

object Players extends Enumeration {
  type Player = Value
  val None, Player1, Player2 = Value
}
class Move(x: Int, y: Int)

trait State(var currentPlayer: Player = Players.Player1) {
  def getMoves: List[Move]
  def makeMove(move: Move): State
  def isTerminal: Boolean
  def getWinner: Player

  def copy: State
}

trait BitBoard(val board: Long) extends State

class Node(state: State, parent: Node = null) {
  private val C: Double = 1.41
  val children: List[Node] = List()
  var wins: Double = 0
  var visits: Int = 0
  @tailrec
  private def backprop(playerWon: Player): Unit = {
    visits += 1
    wins += (if (playerWon == state.currentPlayer) 1 else if (playerWon == Players.None) 0.5 else 0)
    if (parent != null) parent.backprop(playerWon)
  }

  def UCB1: Double = {
    if (visits == 0) Double.MaxValue
    else wins / visits + C * Math.sqrt(Math.log(parent.visits) / visits)
  }

  def rollout: Player = {
    var currentState = state.copy
    while (!currentState.isTerminal) {
      val moves = currentState.getMoves
      val move = moves((Math.random() * moves.size).toInt)
      currentState = currentState.makeMove(move)
    }
    currentState.getWinner

  }

  def isTerminal: Boolean = state.isTerminal
}