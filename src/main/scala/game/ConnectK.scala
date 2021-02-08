package game

import mcts.GameState

class ConnectK(n: Int, k: Int) extends GameState {
  var lastMovedPlayer = 2
  var numPlayers = 2

  var board = Array.fill(n, n)(0)

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

  override def getCopy: GameState = {
    val state = new ConnectK(n, k)

    state.lastMovedPlayer = this.lastMovedPlayer
    state.board = board.map(_.clone())

    return state
  }

  override def getLastMovedPlayer: Int = {
    lastMovedPlayer
  }

  override def getAvailableActions: Set[Int] = {
    if (getPlayerInWinConditions > 0) {
      return Set.empty
    }

    var availableIndices: Set[Int] = Set.empty

    for (colIdx <- 0 to n - 1) {
      for (rowIdx <- 0 to n - 1) {
        if (board(rowIdx)(colIdx) == 0) {
          availableIndices += (rowIdx * n + colIdx)
        }
      }
    }

    return availableIndices
  }

  override def doAction(action: Int): Unit = {
    lastMovedPlayer = (numPlayers + 1) - lastMovedPlayer
    board(action / n)(action % n) = lastMovedPlayer
  }

  override def getResult(playerIdx: Int): Double = {
    var winner: Int = getPlayerInWinConditions;
    if (winner > 0) {
      if (winner == playerIdx) {
        return 1.0
      } else {
        return 0.0
      }
    } else {
      if (getAvailableActions.isEmpty) {
        return 0.5
      } else {
        return 0.0
      }
    }
  }

  def getPlayerInWinConditions: Int = {
    // Check cols
    val cols = for {
      x <- 0 to n - 1
      y <- 0 to n - k
    } yield for {
      i <- 0 to k - 1
    } yield (x, y + i)

    val colsWin = cols
      .filter(x =>
        x.forall { case (i, j) => board(i)(j) == 1 } || x.forall {
          case (i, j) => board(i)(j) == 2
        }
      )
      .map(x => board(x.head._1)(x.head._2))

    if (colsWin.nonEmpty) {
      return colsWin.head
    }

    // Check rows
    val rows = for {
      x <- 0 to n - k
      y <- 0 to n - 1
    } yield for {
      i <- 0 to k - 1
    } yield (x + i, y)

    val rowsWin = rows
      .filter(x =>
        x.forall { case (i, j) => board(i)(j) == 1 } || x.forall {
          case (i, j) => board(i)(j) == 2
        }
      )
      .map(x => board(x.head._1)(x.head._2))

    if (rowsWin.nonEmpty) {
      return rowsWin.head
    }

    // check diags
    val rdiags = for {
      x <- 0 to n - k
      y <- 0 to n - k
    } yield for {
      i <- 0 to k - 1
    } yield (x + i, y + i)

    val rWin = rdiags
      .filter(x =>
        x.forall { case (i, j) => board(i)(j) == 1 } || x.forall {
          case (i, j) => board(i)(j) == 2
        }
      )
      .map(x => board(x.head._1)(x.head._2))

    if (rWin.nonEmpty) {
      return rWin.head
    }

    val ldiags = for {
      x <- 0 to n - k
      y <- 4 to n - 1
    } yield for {
      i <- 0 to k - 1
    } yield (x + i, y - i)

    val lWins = ldiags
      .filter(x =>
        x.forall { case (i, j) => board(i)(j) == 1 } || x.forall {
          case (i, j) => board(i)(j) == 2
        }
      )
      .map(x => board(x.head._1)(x.head._2))

    if (lWins.nonEmpty) {
      return lWins.head
    }

    return 0
  }
}
