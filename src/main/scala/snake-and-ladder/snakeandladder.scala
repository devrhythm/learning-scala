import scala.collection.immutable.HashMap
import scala.util.Random

// NOTE: this code still not work

sealed abstract class GameStatus
case class GameStatusPlaying() extends GameStatus
case class GameStatusWon() extends GameStatus

case class SnakeLadderGameState(
    playersWithOrder: HashMap[Int, Player],
    firstPlayerOrder: Int,
    lastPlayerOrder: Int,
    currentPlayerOrder: Int,
    board: Board,
    goalPoint: Int,
    portals: HashMap[Int, Int],
    gameStatus: GameStatus
)

object SnakeLadderGameState {
  def apply() = {
    val portals = HashMap[Int, Int]()
    val board = Board(50, portals)
    val players = HashMap[Int,Player]()
    SnakeAndLadderGameState(players, players.head, players.last, players.head._1, board, GameStatusPlaying)
  }
}



case class Player(color: String, location: Int)

object Dice {
  def roll(): Int = {
      Random(1).nextInt(6)
  }
}

object SnakeAndLadder {
  def main(args: Array[String]) = {
    val snakeAndLadder = SnakeLadderGameState 
    val gameWinState = play(snakeAndLadder, Dice.roll())
      println(s"${gameWinState.playersWithOrder(gameWinState.currentPlayerOrder).color} Win.")
  }

  def getNextPoint(portals: HashMap[Int, Int], diceResult: Int): Int = {
    if (portals.contains(moveToPoint)) diceResult + portals(moveToPoint)
    else diceResult
  }

  def play(
      gameState: SnakeLadderGameState,
      diceResult: Int
  ): SnakeLadderGameState = {
    val currentPlayer = gameState.playersWithOrder(gameState.currentPlayerOrder)
    val newPlayerLocation =
      getNextPoint(gameState.board.portals, currentPlayer.location + diceResult)
    val nextPlayer = currentPlayer match {
      case player if player == gameState.lastPlayer =>
        gameState.firstPlayer
      case _ => gameState.playersWithOrder(gameState.currentPlayerOrder + 1)
    }
    val playerUpdated =
      Player(currentPlayer.color, gameState.playersWithOrder, newPlayerLocation)

    if (gameState.board.goalPoint == newPlayerLocation)
      SnakeAndLadderGameState(
        gameState.playersWithOrder(currentPlayer).updated(playerUpdated),
        gameState.playersWithOrder.head,
        gameState.playersWithOrder.last,
        currentPlayer,
        gameState.board,
        GameStatusWon
      )
    else
      play(SnakeAndLadderGameState(
        gameState.playersWithOrder.updated((gameState.currentPlayerOrder -> playerUpdated)),
        gameState.playersWithOrder.head,
        gameState.playersWithOrder.last,
        nextPlayer,
        gameState.board,
        GameStatusPlaying
      ), Dice.roll())    
  }
}

