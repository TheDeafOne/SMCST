package algorithms

import games.*
import games.Players.*
import scala.util.Random

import scala.annotation.tailrec

val RANDOM_SEED = 42
val rand = new Random(RANDOM_SEED)


class MonteCarloTreeSearch(val root: Node, val maxIterations: Int = 10) {
  def search(root: Node = root): (Node, Move) = {
    // selection
    var iterations = 0
    var current = root
    while (iterations < maxIterations) {
      if (current.children.nonEmpty) {
        current = current.children.maxBy(_.UCB1)
      } else {
        if (current.visits == 0) {
          // rollout
          println("rollout")
          val playerWon = current.rollout
          current.backprop(playerWon)
        } else {
          // expansion
          println("expansion")
          val moves = current.state.getMoves
          val move = moves(rand.nextInt(moves.size))
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
      val move = moves(rand.nextInt(moves.size))
      currentState.makeMove(move)
    }
    currentState.getWinner

  }
}