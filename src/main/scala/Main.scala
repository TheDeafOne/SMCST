import algorithms.{MonteCarloTreeSearch, Node}
import games.{ABGame, TicTacToeGameState}

@main def tester(): Unit = {
  val root = new Node(new TicTacToeGameState(), null, null)
  val MCTS = new MonteCarloTreeSearch(root, 20000)
  var current = root
  while (!current.state.hasWinner) {
    val (node, move) = MCTS.search(current)
    current = node
    println(current.state)
  }
}