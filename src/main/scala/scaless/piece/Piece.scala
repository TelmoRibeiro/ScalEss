package scaless.piece

/**
 * this is my representation of a chess piece
 * @param color this is the color of the piece, i.e., "White" or "Black"
 * */
abstract class Piece(val color: String):
  def piece: String           // type of piece
  def status: String          // i.e., checked, checkmate, stalemate ...
  def position: (Int, Int)    // 0 <= row < 7 && 0 <= column <= 7
  def moves: List[(Int, Int)] // set of legal moves
end Piece

case class King(override val color: String) extends Piece(color):
  def piece: String = "King"
  def status: String = "Inactive"
  def position: (Int, Int) = null

  def moves: List[(Int, Int)] =
    val currentRow = position._1
    val currentCol = position._2

    val standardMoves = for
      row <- (currentRow - 1) to (currentRow + 1)
      col <- (currentCol - 1) to (currentCol + 1)
      if row >= 0 && row < 8 // could get coordinates from scaless.board.Board
      if col >= 0 && col < 8 // could get coordinates from scaless.board.Board
      if !(row == currentRow && col == currentCol)
    yield (row, col)

    standardMoves.toList
    // gotta remove all illegal moves
  end moves
end King

case class Queen(override val color: String) extends Piece(color):
  def piece: String = "Queen"
  def status: String = "Inactive"
  def position: (Int, Int) = null

  def moves: List[(Int, Int)] =
    val currentRow = position._1
    val currentCol = position._2

    val horizontalMoves = for
      col <- 0 until 8  // could get coordinates from scaless.board.Board
      if col != currentCol
    yield (currentRow, col)

    val verticalMoves = for
      row <- 0 until 8  // could get coordinates from scaless.board.Board
      if row != currentRow
    yield (row, currentCol)

    val diagonalMoves = for
      offset <- 1 until 8 // could get coordinates from scaless.board.Board
      (row, col) <- List(
        (currentRow + offset, currentCol + offset), // down-right
        (currentRow + offset, currentCol - offset), // down-left
        (currentRow - offset, currentCol + offset), // up-right
        (currentRow - offset, currentCol - offset)  // up-left
      )
      if row >= 0 && row < 8 // could get coordinates from scaless.board.Board
      if col >= 0 && col < 8 // could get coordinates from scaless.board.Board
    yield (row, col)

    val standardMoves = horizontalMoves ++ verticalMoves ++ diagonalMoves
    standardMoves.toList
    // gotta remove all illegal moves
  end moves
end Queen

case class Knight(override val color: String) extends Piece(color):
  def piece: String = "Knight"
  def status: String = "Inactive"
  def position: (Int, Int) = null

  def moves: List[(Int, Int)] =
    ???
  end moves
end Knight

case class Bishop(override val color: String) extends Piece(color):
  def piece: String = "Bishop"
  def status: String = "Inactive"
  def position: (Int, Int) = null

  def moves: List[(Int, Int)] =
    val currentRow = position._1
    val currentCol = position._2

    val standardMoves = for
      offset <- 1 until 8 // could get coordinates from scaless.board.Board
      (row, col) <- List(
        (currentRow + offset, currentCol + offset), // down-right
        (currentRow + offset, currentCol - offset), // down-left
        (currentRow - offset, currentCol + offset), // up-right
        (currentRow - offset, currentCol - offset)  // up-left
      )
      if row >= 0 && row < 8 // could get coordinates from scaless.board.Board
      if col >= 0 && col < 8 // could get coordinates from scaless.board.Board
    yield (row, col)

    standardMoves.toList
    // gotta remove all illegal moves
  end moves
end Bishop

case class Rook(override val color: String) extends Piece(color):
  def piece: String = "Rook"
  def status: String = "Inactive"
  def position: (Int, Int) = null

  def moves: List[(Int, Int)] =
    val currentRow = position._1
    val currentCol = position._2

    val horizontalMoves = for
      col <- 0 until 8 // could get coordinates from scaless.board.Board
      if col != currentCol
    yield (currentRow, col)

    val verticalMoves = for
      row <- 0 until 8 // could get coordinates from scaless.board.Board
      if row != currentRow
    yield (row, currentCol)

    val standardMoves = horizontalMoves ++ verticalMoves
    standardMoves.toList
    // gotta remove all illegal moves
  end moves
end Rook

case class Pawn(override val color: String) extends Piece(color):
  def piece: String = "Pawn"
  def status: String = "Inactive"
  def position: (Int, Int) = null

  def moves: List[(Int, Int)] =
    val currentRow = position._1
    val currentCol = position._2

    val forwardMoves =
      if this.color == "White" && currentRow - 1 >= 0 then // could get coordinates from scaless.board.Board
        List((currentRow - 1, currentCol))
      else if this.color == "Black" && currentRow + 1 <  8 then // could get coordinates from scaless.board.Board
        List((currentRow + 1, currentCol))
      else Nil

    def isFirstMove = (this.color == "White" && currentRow == 6) || (this.color == "Black" && currentRow == 1) // could get coordinates from scaless.board.Board

    val initialMoves =
      if isFirstMove then
        if this.color == "White" then
          List((currentRow - 2, currentCol))
        else if this.color == "Black" then
          List((currentRow + 2, currentCol))
        else Nil
      else Nil

    initialMoves ++ forwardMoves
    // gotta remove all illegal moves
  end moves
end Pawn