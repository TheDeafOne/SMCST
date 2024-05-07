package games

trait TwoPlayerGame {
  /**
   * If the game is not over, will return None.
   *
   * If someone has won, it will return Option[true]
   * If the game is over, and it's a draw, it will return Option[false].
   *
   * @return Option[Boolean]
   */
  def hasWinner: Option[Boolean]

  /**
   * @return the winner as an int
   * @throws IllegalAccessError If the game is not over or a draw
   */
  def getWinner: Int

  /**
   * @param space The move on which the player should play on
   * @return The next board after the move was played
   * @throws IllegalArgumentException Invalid move, space already occupied
   * @throws IllegalStateException    Cannot play, Game already over
   */
  def makeMove(space: Int): TwoPlayerGame

  /**
   * @return true if player 1 is next else false
   */
  def getNextPlayer: Boolean

  /**
   * @return a string describing how to play the game
   */
  def getGameIntroText: String

  /**
   * @return All the values 1-boardSize which are legal moves
   */
  def getValidMoves: List[Int]

}
