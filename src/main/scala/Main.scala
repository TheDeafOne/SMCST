import algorithms.{MonteCarloTreeSearch, Node}
import games.ABGame
import utils.timeIt

def tester(): Unit = {
  println("starting")
  val abGame = new ABGame()
  val root = new Node(abGame.initialState, null, null)
  println("generating MCTS")
  val MCTS = new MonteCarloTreeSearch(root, 1000, 1000)
  var current = root
  while (!current.state.hasWinner) {
    println("searching tree...")
    val (node, move) = MCTS.search(current)
    current = node
    println(current.state)
  }
}

@main def main(): Unit = {
  println(timeIt(tester()))
}