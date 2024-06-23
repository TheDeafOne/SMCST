package algorithms

import games.*

import scala.util.Random
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*

val RANDOM_SEED = 42
val rand = new Random(RANDOM_SEED)
val C: Double = 1.41

class MonteCarloTreeSearch(val root: Node, val maxIterations: Int = 10, val maxRolloutsPerIteration: Int = 10) {
  def search(root: Node = root): (Node, Move) = {
    // selection
    var iterations = 0
    var current = root
    while (iterations < maxIterations) {
      if (current.children.nonEmpty) {
        // exploration
        current = current.children.maxBy(_.UCB1)
      } else {
        if (current.visits == 0) {
          // rollout
          (1 to maxRolloutsPerIteration)
            .par
            .foreach(_ => current.backprop(current.rollout))
          current = root // reset algorithm to root
        } else {
          // expansion
          if (!current.state.hasWinner) {
            current.state.getMoves.foreach(move => {
              current.children = new Node(current.state.makeMove(move), current, move) :: current.children
            })
          }
        }
      }

      iterations += 1
    }


    val node = root.children.maxBy(n => 1 - n.wins / n.visits)
    (node, node.move)
  }
}

class Node(val state: State, val parent: Node, val move: Move) {
  var children: List[Node] = List()
  var wins: Double = 0
  var visits: Int = 0

  /**
   * Backpropagates the result of a rollout to the root node'
   * each node that is passed gets 1 for winning, 0 for losing, and 0.5 for drawing
   * @param playerWon the player that won the game
   */
  @tailrec
  final def backprop(playerWon: Players): Unit = {
    visits += 1
    wins += (if (playerWon == state.currentPlayer) 1 else if (playerWon == Players.None) 0.5 else 0)
    if (parent != null) parent.backprop(playerWon)
  }

  /**
   * Calculates the UCB1 value of the node using the following formula
   * UCB1 = wins / visits + C * sqrt(log(parent.visits) / visits)
   * described further in https://en.wikipedia.org/wiki/Monte_Carlo_tree_search
   * @return the UCB1 value of the node
   */
  def UCB1: Double = {
    if (visits == 0) Double.MaxValue
    else wins / visits + C * Math.sqrt(Math.log(parent.visits) / visits)
  }

  /**
   * Simulates a game from the current state to the end of the game (draw, win, or loss) using uniformly random moves
   * @return the player that won the game
   */
  def rollout: Players = {
    var currentState = state
    while (!currentState.hasWinner) {
      val moves = currentState.getMoves
      val move = moves(rand.nextInt(moves.size))
      currentState = currentState.makeMove(move)
    }
    currentState.winner

  }
  
  override def toString: String = {
    s"[wins: $wins, visits: $visits, move: $move]"
  }
}