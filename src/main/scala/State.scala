case class State[S, A](runState: S => (A, S)) {
  import State._

  def flatMap[B](f: A => State[S, B]) : State[S, B] = bind(this)(f)
  def map[B](f: A => B) : State[S, B] = fmap(this)(f)

  def >>=[B](f: A => State[S, B]) : State[S, B] = bind(this)(f)
  def >>[B](s: State[S, B]) : State[S, B] = this >>= (_ => s)

  def evalState(s: S) : A = runState(s)._1
  def execState(s: S) : S = runState(s)._2
}

object State {
  private def bind[S, A, B](src: State[S, A])(f: A => State[S, B]) : State[S, B] = State[S, B] { s =>
    val (a, newState) = src.runState(s)
    val stateB = f(a)
    stateB.runState(newState)
  }

  def fmap[S, A, B](src: State[S, A])(f: A => B) : State[S, B] = src.flatMap(a => point(f(a)))

  def point[S, A](x: A): State[S, A] = State(s => (x, s))

  def get[S] : State[S, S] = State(s => (s, s))
  def gets[S, A](f: S => A) : State[S, A] = get map f
  def put[S](s: S) : State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S) : State[S, Unit] = get[S].flatMap(s => put(f(s)))
}
