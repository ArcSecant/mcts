import game.ConnectK
import game.Othello
import mcts.MCTS

import scala.util.Try

object Main extends App {
  var state = new Othello(10)
  // var state = new ConnectK(9, 5)

  while (state.getAvailableActions.nonEmpty) {
    println(s"Player ${state.numPlayers + 1 - state.getLastMovedPlayer}'s turn")
    println(state.toString)

    var action: Int = -1
    if (state.getLastMovedPlayer == 1) {
      action = MCTS.search(state, 500, numNodes = 50)
    } else {
      action = MCTS.search(state, 500, numNodes = 50)
      // val input = scala.io.StdIn.readLine("your move >")
      // action = Try(input.split(" ").map(_.toInt)).toOption match {
      //   case Some(Array(x, y)) => x * n + y
      //   case None => MCTS.search(state, 500)
      // }
    }

    println(
      s"Player ${state.numPlayers + 1 - state.lastMovedPlayer}'s best action is ${action}"
    )
    println()
    state.doAction(action)
  }

  println(state.toString)

  if (state.getResult(state.lastMovedPlayer) == 1.0) {
    println(s"Player ${state.lastMovedPlayer} wins!")
  } else if (state.getResult(state.lastMovedPlayer) == 0.0) {
    println(s"Player ${state.numPlayers + 1 - state.lastMovedPlayer} wins!")
  } else {
    println(s"It's a draw!")
  }
}
