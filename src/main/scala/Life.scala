case class Karma(health: Int, strength: Int, intellect: Int)

object Life extends App {

  def newbornKarma = Karma(0, 0, 0)

  //some basics
  def changeHealth(delta: Int): State[Karma, Unit] =
    State.modify(s => s.copy(health = s.health + delta))
  def changeStrength(delta: Int): State[Karma, Unit] =
    State.modify(s => s.copy(strength = s.strength + delta))
  def changeIntellect(delta: Int): State[Karma, Unit] =
    State.modify(s => s.copy(intellect = s.intellect + delta))

  def attach[A](a: A) : State[Karma, A] = State.gets(_ => a)

  //activities
  def sleep       = changeHealth(2)    >> changeStrength(1)
  def drinkBeer   = changeHealth(-1)   >> changeIntellect(-1)
  def drinkVodka  = changeHealth(-6)   >> changeIntellect(-5)
  def fight       = changeHealth(-2)   >> changeStrength(-3)
  def workHard    = changeIntellect(2) >> attach(scala.util.Random.nextInt(50))

  //time spent
  def workDay = sleep >> drinkBeer >> workHard
  def workWeek = for {
    mondaySalary    <- workDay
    tuesdaySalary   <- workDay
    wednesdaySalary <- workDay
    thursdaySalary  <- workDay
    fridaySalary    <- workDay
  } yield mondaySalary + tuesdaySalary + wednesdaySalary + thursdaySalary + fridaySalary

  def weekend(budget: Int) =
    if (budget >= 125) drinkBeer >> drinkVodka >> fight
    else drinkBeer >> drinkBeer >> sleep


  def typicalWeek = workWeek >>= weekend
  
  def month = typicalWeek >> typicalWeek >> typicalWeek >> typicalWeek

  //run this life for a year
  Stream.iterate(newbornKarma, 12)(month.execState).foreach(println)

}
