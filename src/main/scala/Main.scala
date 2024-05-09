import algorithms.{MonteCarloTreeSearch, Node}
import games.ABGame

@main def tester(): Unit = {
  val root = new Node(new ABGame(), null, null)
  val MCTS = new MonteCarloTreeSearch(root, 20)
  val (node, move) = MCTS.search(root)
  var current = root
  println("\n\ndisplay")
  while (!current.state.hasWinner) {
    println(current.state)
    println(current.state.currentPlayer)
    println(current.state.getMoves)
    println()
    current = current.children.maxBy(n => n.wins / n.visits)
  }
  println(current.state)
  println(current.state.currentPlayer)
  println(current.state.getMoves)
  println()
}