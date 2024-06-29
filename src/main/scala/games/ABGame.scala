package games


class ABGame extends Game {
  override val initialState = new ABState(List.fill(5)(Players.None).updated(0, Players.P1).updated(4, Players.P2))
  var state: State = initialState
  val info = new GameInfo(
    "AB",
    """A simple game where two players move a token left or right on a 1d vector.
    Players can move their token one space to the left or right if the adjacent space is empty, 
    or two spaces to the left or right if the adjacent space is occupied by another player
    Players cannot move their token off the board or onto a space occupied by the other player.
    Players take turns moving their token.
    The first player to move their token to the other side wins.""",
  )

  override def display: Unit = {
    println(state) //TODO: make this better
  }
}

class ABState(
  var board: List[Players],
  var currentPlayer: Players=Players.P1
) extends State {
  override def getMoves: List[Move] = {
    val playerIndex = board.indexOf(currentPlayer)
    val leftIndex = playerIndex match {
      case 0 => -1
      case _ if board(playerIndex - 1) == Players.None => playerIndex - 1
      case _ => playerIndex - 2
    }
    val rightIndex = playerIndex match {
      case i if i == board.length - 1 => -1
      case _ if board(playerIndex + 1) == Players.None => playerIndex + 1
      case _ => playerIndex + 2
    }
    List(
      new Move(leftIndex, 0),
      new Move(rightIndex, 0)
    ).filter(m => m.x >= 0 && m.x < board.size)
  }

  override def makeMove(move: Move): State = {
    // return a new state with the given move applied
    new ABState(
      board.updated(
        board.indexOf(currentPlayer),
        Players.None
      ).updated(move.x, currentPlayer),
      if (currentPlayer == Players.P1) Players.P2 else Players.P1
    )
  }

  override def hasWinner: Boolean = {
    board.indexOf(Players.P1) == board.length - 1 || board.indexOf(Players.P2) == 0
  }

  override def winner: Players = {
    (board.indexOf(Players.P1), board.indexOf(Players.P2)) match {
      case (i, _) if i == board.length - 1 => Players.P1
      case (_, i) if i == 0 => Players.P2
      case _ => Players.None
    }
  }

  override def toString: String = board.mkString(" ")
}
