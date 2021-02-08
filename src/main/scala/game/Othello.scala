package game

import mcts.GameState

class Othello(n: Int) extends GameState {
  var lastMovedPlayer = 2
  var numPlayers = 2

  var board = Array.fill(n, n)(0)
  board(n / 2)(n / 2) = 2
  board(n / 2 - 1)(n / 2 - 1) = 2
  board(n / 2)(n / 2 - 1) = 1
  board(n / 2 - 1)(n / 2) = 1

  var directions = for {
    i <- -1 to 1
    j <- -1 to 1
  } yield (i, j)

  def outOfBounds(x: Int, y: Int): Boolean = {
    x < 0 || y < 0 || x >= n || y >= n
  }

  def isValid(x: Int, y: Int): Boolean = {
    if (board(x)(y) != 0) {
      return false
    }

    val curPlayer = (numPlayers + 1) - lastMovedPlayer

    directions.filter(_ != (0, 0)).exists { dir =>
      var posX = x
      var posY = y
      do {
        posX += dir._1
        posY += dir._2
      } while (!outOfBounds(posX, posY) && board(posX)(posY) == lastMovedPlayer)

      (!outOfBounds(posX, posY) &&
      board(posX)(posY) == curPlayer &&
      (posX, posY) != (x + dir._1, y + dir._2))
    }
  }

  override def getCopy: GameState = {
    val state = new Othello(n)

    state.lastMovedPlayer = this.lastMovedPlayer
    state.board = board.map(_.clone())

    return state
  }

  override def getLastMovedPlayer: Int = {
    lastMovedPlayer
  }

  override def getAvailableActions: Set[Int] = {
    var availableIndices: Set[Int] = Set.empty

    for (i <- 0 to n - 1) {
      for (j <- 0 to n - 1) {
        if (isValid(i, j)) {
          availableIndices += (i * n + j)
        }
      }
    }

    return availableIndices
  }

  override def doAction(action: Int): Unit = {
    val (x, y) = (action / n, action % n)
    val curPlayer = (numPlayers + 1) - lastMovedPlayer

    directions.filter(_ != (0, 0)).foreach { dir =>
      var toFlip: List[(Int, Int)] = List.empty

      var posX = x
      var posY = y
      do {
        toFlip = (posX, posY) :: toFlip
        posX += dir._1
        posY += dir._2
      } while (!outOfBounds(posX, posY) && board(posX)(posY) == lastMovedPlayer)

      if (
        !outOfBounds(posX, posY) &&
        board(posX)(posY) == curPlayer &&
        (posX, posY) != (x + dir._1, y + dir._2)
      ) {
        toFlip.foreach { case (i, j) => board(i)(j) = curPlayer }
      }
    }

    lastMovedPlayer = curPlayer
  }

  override def getResult(playerIdx: Int): Double = {
    var whiteScore = 0
    var blackScore = 0
    for (i <- 0 to n - 1) {
      for (j <- 0 to n - 1) {
        if (board(i)(j) == 1) {
          whiteScore += 1
        } else if (board(i)(j) == 2) {
          blackScore += 1
        }
      }
    }
    if (whiteScore == blackScore) return 0.5

    if (playerIdx == 1) {
      return if (whiteScore > blackScore) 1.0 else 0.0
    }
    return if (whiteScore > blackScore) 0.0 else 1.0
  }

  override def toString: String = {
    var s = ""
    for (row <- board) {
      for (col <- row) {
        s += ".OX" (col)
        s += " "
      }
      s += "\n"
    }
    s
  }

}
