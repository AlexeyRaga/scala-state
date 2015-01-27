import State._

case class GameState(turns: Int)
object Game extends App {

  def initState = GameState(0)

  def shot : State[GameState, Boolean] = for {
    p <- get
    _ <- put(GameState(p.turns + 1))
  } yield scala.util.Random.nextInt(6) >= 5

  def nextTurn(dead: Boolean) : State[GameState, Boolean] =
    if (dead) State.point(dead)
    else shot >>= nextTurn
  
  def play = nextTurn(dead = false)

  val GameState(turns) = play.execState(initState)
  println(s"You are dead after $turns turns.")
}
