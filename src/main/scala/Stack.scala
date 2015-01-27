import Stack._

object Stack {
  type Stack = List[Int]

  def pop: State[Stack, Int] = State { s => (s.head, s.tail) }
  def push(value: Int) : State[Stack, Unit] = State.modify(s => value :: s)

  def emptyStack = List.empty[Int]
}

//just a low level demonstration of the for comprehension, look at BasicCalc below!
object BasicStack extends App {
  val res = for {
    _ <- push(3)
    _ <- push(5)
    a <- pop
    b <- pop
  } yield a + b
  println(res.runState(List.empty))

  val res2 = for {
    _ <- push(3) >> push(5)
    a <- pop
    b <- pop
  } yield a + b
  println(res2.runState(List.empty))

  val res3 = push(3) >> push(5) >> pop >>= (x => pop >>= (y => State.point(x + y)))
  println(res3.runState(List.empty))

  def add = for {
    x <- pop
    y <- pop } yield x + y

  val res4 = push(3) >> push(5) >> add
  println(res4.runState(List.empty))

}

//a very simple DSL, but shows how different independent combinators
//such as, mul, add, etc. can be composed together into bigger expressions
object BasicCalc extends App {

  def add = for {
    a <- pop
    b <- pop } yield a + b
  
  def mul = for {
    a <- pop
    b <- pop } yield a * b

  def addp = add >>= push
  def mulp = mul >>= push
  
  def exp1 = for {
    _ <- push(2)
    _ <- push(3)
    _ <- addp
    _ <- push(5)
    _ <- mulp
    res <- pop } yield res
  
  val res1 = exp1.evalState(emptyStack)
  println("(2 + 3) * 5 = " + res1)

  //Once we have our 'DLS' combinators we can build expressions easily
  //(2 + 3) * 5 = 25
  val exp2 = push(10) >> push(2) >> push(3) >> addp >> push(5) >> mulp >> pop

  //I don't care about the result stack, I am only interested in the result
  val res2 = exp2.evalState(emptyStack)

  println("(2 + 3) * 5 = " + res2)
}