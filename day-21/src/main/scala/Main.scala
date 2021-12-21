import scala.io.Source

val filename = "input.txt"

type PlayerState = Map[String, (Int, Int)]
case class DiceState(value: Int, usedTimes: Int)
type CurrentPlayer = String

case class GameState(playerState: PlayerState, diceState: DiceState, currentPlayer: CurrentPlayer)


val finalValue: Int = 1000

def didWon(state: GameState): Option[GameState] = {

  state.playerState.find(p => p._2._2 >= finalValue) match {
    case Some(p) => Some(state)
    case None => None
  }
}

def roll(diceState: DiceState): DiceState = {
  diceState.value + 1 match {
    case 101 => DiceState(1, diceState.usedTimes + 1)
    case x => DiceState(x, diceState.usedTimes + 1)
  }
}
def newPositionValue(position: Int, rolls: Array[DiceState]) : Int = 
  val newPosition = position + rolls.map(_.value).sum
  newPosition match {
    case x if x > 10 => 
      x % 10 match {
        case 0 => 10
        case y => y
      }
    case x => x
  }

def play(state: GameState): GameState = {
  println(state)
  didWon(state) match {
    case Some(s) => s
    case None => 
      val currentPlayerState = state.playerState(state.currentPlayer)
      val rolls = (1 to 2).scanLeft(state.diceState)((s, _) => roll(s)).toArray
      val adjustedPosition = newPositionValue(currentPlayerState._1, rolls)
      println(s"${state.currentPlayer} rolls ${rolls.mkString(", ")} and moves to $adjustedPosition")
      val newScore = currentPlayerState._2 + adjustedPosition
      val newPlayerState = (adjustedPosition, newScore)
      val nextPlayer = state.playerState.filterKeys(_ != state.currentPlayer).keys.head
      val newDice = roll(rolls.last)
      val newState = GameState(state.playerState + (state.currentPlayer -> newPlayerState), newDice, nextPlayer)
      play(newState)

  }
}

def partOne(playerStartState: PlayerState): Int = {
  val diceStartState = DiceState(1, 0)
  val currentPlayerStartState = "1"
  val startState = GameState(playerStartState, diceStartState, currentPlayerStartState)

  val finalState = play(startState)
  val loosingPlayer = finalState.playerState.filter(p => p._2._2 < finalValue).keys.head
  val loosingScore = finalState.playerState(loosingPlayer)._2
  val dice = finalState.diceState
  loosingScore * dice.usedTimes
}

@main def hello: Unit = 
  val playerStartState = Map[String, (Int, Int)](
    "1" -> (7, 0),
    "2" -> (1, 0)
  )
  println(partOne(playerStartState))
