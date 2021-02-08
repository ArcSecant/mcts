package mcts

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.Random

trait GameState {
  def getCopy: GameState
  def getLastMovedPlayer: Int
  def getAvailableActions: Set[Int]
  def getResult(playerIdx: Int): Double
  def doAction(action: Int): Unit
}

case class Node(
    action: Int = -1,
    parent: Node = null,
    state: GameState = null
) {
  var numWins: Double = 0
  var numVisits: Int = 0
  var children: ListBuffer[Node] = ListBuffer.empty
  var untriedActions: Set[Int] = state.getAvailableActions
  var playerIdx: Int = state.getLastMovedPlayer

  val epsilon: Double = 1e-6

  def findChild: Node = {
    val sortedChildren = children
      .map(node =>
        (
          node,
          (node.numWins.toDouble / node.numVisits) + Math.sqrt(
            2 * Math.log(numVisits + 1) / (node.numVisits + epsilon)
          )
        )
      )
      .sortBy(_._2)

    sortedChildren.last._1
  }

  def update(result: Double): Unit = {
    numVisits += 1;
    numWins += result;
  }

  def addChild(action: Int, state: GameState): Node = {
    val n = new Node(action, this, state)
    untriedActions -= action
    children += n

    n
  }

  override def toString(): String = {
    s"[Actions: $action; " +
      s"Wins/Visits: ${numWins}/${numVisits} = ${numWins.toDouble / numVisits}; " +
      s"Untried: ${untriedActions}"
  }
}

object MCTS {
  def search(
      root: GameState,
      maxIter: Int,
      numNodes: Int = 100,
      k: Int = 1,
      verbose: Boolean = false
  ): Int = {
    var count = 1
    var rootPar = Array.fill(numNodes)(new Node(state = root))
    val action = rootPar.par.map { rootNode =>
      var node: Node = null
      var state: GameState = null
      for (i <- 1 to maxIter) {
        node = rootNode
        state = root.getCopy

        // Select
        while (node.untriedActions.isEmpty && node.children.nonEmpty) {
          node = node.findChild
          state.doAction(node.action)
        }

        // Expand
        if (node.untriedActions.nonEmpty) {
          if (count >= k) {
            val action: Int =
              node.untriedActions.toList(
                Random.nextInt(node.untriedActions.size)
              )
            state.doAction(action)
            node.addChild(action, state)
          } else {
            count += 1
          }

        }

        // Rollout
        while (!state.getAvailableActions.isEmpty) {
          state.doAction(
            state.getAvailableActions.toList(
              Random.nextInt(state.getAvailableActions.size)
            )
          )
        }

        // Backprop
        while (node != null) {
          node.update(state.getResult(node.playerIdx))
          node = node.parent
        }
      }

      rootNode.children.sortBy(_.numVisits).last.action
    }

    // Select most voted action
    // action.groupBy(identity).mapValues(_.size).maxBy(_._2)._1
    val sortedActions = action.groupBy(identity).mapValues(_.size).toArray.sortBy(_._2).reverse
    sortedActions.foreach{ action =>
      println(s"Action: ${action._1}, Votes: ${action._2}")
    }
    sortedActions(0)._1
  }

  def normalize[T](v: Vector[T])(implicit t: Fractional[T]): Vector[T] = {
    val s = t.times(v.sum, v.sum)
    return v.map { x => t.div(x, s) }
  }
}
