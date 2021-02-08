package game

import mcts.GameState
import util.control.Breaks._

sealed trait Colour
final object White extends Colour
final object Black extends Colour

class Group(s: Set[(Int, Int)], id: Int) {
  var border: Set[(Int, Int)] = Set.empty
  var player = id
  var stones = s

  def add(other: Group): Option[Group] = {
    if (this.player != other.player) {
      None
    }
    var grp = new Group(this.stones | other.stones, id)
    grp.border = this.border.union(other.border).diff(grp.stones)
    Some(grp)
  }

  def size: Int = {
    this.stones.size
  }
}

class Go(n: Int) extends GameState {

  override def getCopy: GameState = {
    val state = new Go(n)

    state.lastMovedPlayer = this.lastMovedPlayer
    state.board = board.clone()

    state
  }

  override def getLastMovedPlayer: Int = {
    lastMovedPlayer
  }

  override def getAvailableActions: Set[Int] = ???

  override def getResult(playerIdx: Int): Double = ???

  override def doAction(action: Int): Unit = {
    lastMovedPlayer = (lastMovedPlayer + 1) % 2

    var (actionX, actionY) = (action / n, action % n)
    
  }

  override def toString: String = {
    board.map(_.mkString(" ")).mkString("\n")
  }
  
  var curPlayer: Colour = Black
  var board: Array[Array[Option[Group]]] = Array.fill(n,n)(None)
  var territory = Array.fill(n, n)(-1)
  var lastMovedPlayer = 1
  var captured = Array(0, 0)
  var score = Array(0, 0)
  var blockedField = (-1, -1)
  
  def _add(grp: Group): Unit = {
    grp.stones.foreach{case (x, y) => this.board(x)(y) = Some(grp)}
  }

  def _remove(grp: Group): Unit = {
    grp.stones.foreach{case (x, y) => this.board(x)(y) = None}
  }

  def _kill(grp: Group): Unit = {
    this.captured((grp.player + 1) % 2) += grp.size
    this._remove(grp)
  }

  def _liberties(grp: Group): Int = {
    grp.border.filter{case (x, y) => this.board(x)(y).isEmpty}.size
  }

  def _nextTurn(): Colour = {
    if (this.curPlayer == White) Black else White
  }

//   def isValid(x: Int, y: Int): Boolean = {
//     if (this.board(x)(y).isEmpty) {
//       false
//     } 
//   }

  def placeStone(x: Int, y: Int): Boolean = {
    if (this.board(x)(y).isEmpty) {
      false
    }

    var newGroup = new Group(Set((x, y)), lastMovedPlayer)

    var groupsToRemove: Set[Group] = Set.empty
    var groupsToKill: Set[Group] = Set.empty

    def checkValid(x: Int, y: Int): Boolean = {
      if (x < 0 || y < 0 || x >= n || y >= n) {
        false
      }

      this.board(y)(x) match {
        case None => true
        case Some(otherGroup) => {
          if (newGroup.player == otherGroup.player) {
            newGroup.add(otherGroup)
            groupsToRemove += otherGroup
            true
          }
          else if (this._liberties(otherGroup) == 1) {
            groupsToKill += otherGroup
            true
          }
          false
        }
      }
    }

    var neighbours = List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
    if (neighbours.forall{case (x, y) => checkValid(x, y)}) {
      groupsToRemove.foreach(grp => this._remove(grp))
      groupsToKill.foreach(grp => this._kill(grp))
      this._add(newGroup)
      true
    }
    else false

    if (newGroup.size == 1 && groupsToKill.size == 1 && groupsToKill.head.size == 1) {
      groupsToKill.head.stones.foreach(x => this.blockedField = x)
    }
    else {
      this.blockedField = (-1, -1)
    }

    this.lastMovedPlayer = (this.lastMovedPlayer + 1) % 2

    true
  }
}