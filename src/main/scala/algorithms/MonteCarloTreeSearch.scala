package algorithms

import Players.Player

import scala.annotation.tailrec

@main def tester(): Unit = {
  val root = new Node(new ABGame(), null, null)
  val MCTS = new MonteCarloTreeSearch(root)
  val (node, move) = MCTS.search(root)
  println(move.x)
}


class MonteCarloTreeSearch(val root: Node) {
  def search(root: Node): (Node, Move) = {
    // selection
    var iterations = 0
    val maxIters = 2
    var current = root
    while (iterations < maxIters) {
      if (current.children.nonEmpty) {
        current = current.children.maxBy(_.UCB1)
      } else {
        if (current.visits == 0) {
          // rollout
          val playerWon = current.rollout
          current.backprop(playerWon)
        } else {
          // expansion
          val moves = current.state.getMoves
          val move = moves((Math.random() * moves.size).toInt)
          val newState = current.state.copy
          newState.makeMove(move)
          val newNode = new Node(newState, current, move)
          current.children = current.children :+ newNode
          current = newNode
        }
      }
      iterations += 1
    }
    val node = root.children.maxBy(n => n.wins/n.visits)
    (node, node.move)
  }
}


object Players extends Enumeration {
  type Player = Value
  val None, Player1, Player2 = Value
}
class Move(val x: Int, val y: Int)

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
}

class Node(val state: State, val parent: Node, val move: Move) {
  private val C: Double = 1.41
  var children: List[Node] = List()
  var wins: Double = 0
  var visits: Int = 0
  @tailrec
  final def backprop(playerWon: Player): Unit = {
    visits += 1
    wins += (if (playerWon == state.currentPlayer) 1 else if (playerWon == Players.None) 0.5 else 0)
    if (parent != null) parent.backprop(playerWon)
  }

  def UCB1: Double = {
    if (visits == 0) Double.MaxValue
    else wins / visits + C * Math.sqrt(Math.log(parent.visits) / visits)
  }

  def rollout: Player = {
    val currentState = state.copy
    while (!currentState.isTerminal) {
      val moves = currentState.getMoves
      val move = moves((Math.random() * moves.size).toInt)
      currentState.makeMove(move)
    }
    currentState.getWinner

  }
}