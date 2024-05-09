import algorithms.{MonteCarloTreeSearch, Node}
import games.{ABGame, CheckersGame, Move, Players}

@main def tester(): Unit = {
  val root = CheckersGame(currentPlayer = Players.Player1)
  println(root.board)
  root.board = root.board.updated(3, root.board(3).updated(1, Players.Player2))
  root.board = root.board.updated(6, root.board(6).updated(0, Players.None))
  val xc = root.getMove(2, 0)
  val xs = xc.map(x => root.getPiece(x.currX, x.currY))
  root.board.foreach(println)
  root.makeMove(xc.head)
  println(xs)
  println(xc)
  println(root.getMoves)
  root.board.foreach(println)

//  board.
//  val root = new Node(new ABGame(), null, null)
//  val MCTS = new MonteCarloTreeSearch(root, 20)
//  val (node, move) = MCTS.search(root)
//  var current = root
//  println("\n\ndisplay")
//  while (!current.state.hasWinner) {
//    println(current.state)
//    println(current.state.currentPlayer)
//    println(current.state.getMoves)
//    println()
//    current = current.children.maxBy(n => n.wins / n.visits)
//  }
//  println(current.state)
//  println(current.state.currentPlayer)
//  println(current.state.getMoves)
//  println()
}