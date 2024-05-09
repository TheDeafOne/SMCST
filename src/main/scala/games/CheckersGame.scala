package games
import games.Players.{Player1, Player2}
import scala.math.sqrt
import scala.math.pow

class CheckersGame(var currentPlayer: Player=Players.Player1) extends State {
  private def oppositePlayer: Player = currentPlayer match
    case Player1 => Player2
    case _ => Player1
  private def fill(even: Boolean, player: Player): List[Player] = {
    (0 to 7).map {
        case i if (i % 2 == 0) == even => player
        case _ => Players.None
    }.toList
  }
  var board: List[List[Player]] = (0 to 7).map {
    case x if List(0, 1, 2).contains(x) => {
      fill(x % 2 == 0, Players.Player1)
    }
    case y if List(5, 6, 7).contains(y) => {
      fill(y % 2 == 0, Players.Player2)
    }
    case _ => {
      fill(true, Players.None)
    }
  }.toList

  def getPiece(currX: Int, currY: Int): Player = {
    board(currX)(currY)
  }
  def inBounds(pos: Int): Boolean = {
    pos >= 0 && pos <= 7
  }

  def checkJump(checkersMove: Move): Option[Move] = {
    val newX = checkersMove.currX + (checkersMove.x - checkersMove.currX)*2
    val newY = checkersMove.currY + (checkersMove.y - checkersMove.currY)*2
    if (!inBounds(newX) || !inBounds(newY)) {
      None
    } else {
      if (getPiece(newX, newY) == Players.None) {
        Some(new Move(newX, newY, checkersMove.currX, checkersMove.currY))
      } else {
        None
      }
    }
  }
  def getMove(currX: Int, currY: Int): List[Move] = {
    val filteredDirs =
      if getPiece(currX, currY).common
      then List(-1, 1)
        .filter(x => if (currentPlayer == Players.Player1) then x > 0 else x < 0)
      else List(-1, 1)

    val possibleMoves = filteredDirs.flatMap(x => {
      List(-1, 1).map(y => {
        Move(currX + x, currY + y, currX, currY)
      })
    }).filter(move => (move.x >= 0) && (move.x <= 7) && (move.y <= 7) && (move.y >= 0))

    possibleMoves
      .filter(move => getPiece(move.x, move.y) != currentPlayer)
      .flatMap(x => if getPiece(x.x, x.y) == oppositePlayer then checkJump(x) else Some(x))
  }
  override def getMoves: List[Move] = {
    (0 to 7).flatMap(row => {
      (0 to 7).map(col => {
        (row, col)
      })
    }).filter(getPiece(_, _) == currentPlayer).flatMap(getMove).toList
  }

  def updateSpot(x: Int, y: Int, player: Player): List[List[Player]] = {
    board.updated(x, board(x).updated(y, player))
  }

  override def makeMove(move: Move): Unit = {

    if (Math.abs(move.currX - move.x) == 2) {
      val dx = move.currX + (move.x - move.currX)/2
      val dy = move.currY + (move.y - move.currY)/2
      board = updateSpot(move.currX, move.currY, Players.None)
      board = updateSpot(dx, dy, Players.None)
      board = updateSpot(move.x, move.y, currentPlayer)
      val mv = getMove(move.x, move.y)
      val newMoves = mv.filter(move => Math.abs(move.currX - move.x) == 2)
      if (newMoves.nonEmpty) {
        makeMove(newMoves.head)
      } else {
        if (move.x == 7 && currentPlayer == Player1) {
          val piece = getPiece(move.x, move.y)
          piece.common = false
          board = board.updated(move.x, board(move.x).updated(move.y, piece))
        } else if (move.x == 0 && currentPlayer == Player2) {
          val piece = getPiece(move.x, move.y)
          piece.common = false
          board = board.updated(move.x, board(move.x).updated(move.y, piece))
        }
        currentPlayer = oppositePlayer
      }
    } else {
      board = updateSpot(move.currX, move.currY, Players.None)
      board = updateSpot(move.x, move.y, currentPlayer)
      if (move.x == 7 && currentPlayer == Player1) {
        val piece = getPiece(move.x, move.y)
        piece.common = false
        board = board.updated(move.x, board(move.x).updated(move.y, piece))
      } else if (move.x == 0 && currentPlayer == Player2) {
        val piece = getPiece(move.x, move.y)
        piece.common = false
        board = board.updated(move.x, board(move.x).updated(move.y, piece))
      }
      currentPlayer = oppositePlayer
    }
  }




  override def hasWinner: Boolean = {
    // eliminate squares that are not empty and leaving squares that have pieces
    val distinctPieces = board.flatten.filter(_ != Players.None).distinct
    val board_copy = copy
    if (board_copy.getMoves.isEmpty) {
      return true
    }
    board_copy.currentPlayer = oppositePlayer
    if (board_copy.getMoves.isEmpty) {
      return true
    }
    distinctPieces.size == 1 || distinctPieces.isEmpty

  }

  override def getWinner: Player = {
    val board_copy = copy
    if (board_copy.getMoves.isEmpty) {
      return board_copy.currentPlayer
    }
    board_copy.currentPlayer = oppositePlayer
    if (board_copy.getMoves.isEmpty) {
      return board_copy.currentPlayer
    }
    if(board.flatten.count(_ == Players.Player1) == 0) Players.Player2
    else Players.Player1

  }

  def copy: State = {
    val game = new CheckersGame(currentPlayer)
    game.board = board
    game
  }

  override def toString: String = board.map(row => row.map(_.toString).mkString(" ")).mkString("\n")
}



//object checkersPlayer extends Players {
//
//}

