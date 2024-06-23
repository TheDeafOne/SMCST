import algorithms.{MonteCarloTreeSearch, Node}
import games.{ABGame, TicTacToeGameState}
import utils.timeIt

def tester(): Unit = {
  val root = new Node(new ABGame(), null, null)
  val MCTS = new MonteCarloTreeSearch(root, 1000, 100)
  var current = root
  while (!current.state.hasWinner) {
    val (node, move) = MCTS.search(current)
    current = node
    println(current.state)
  }
}

@main def main(): Unit = {
  println(timeIt(tester()))
}