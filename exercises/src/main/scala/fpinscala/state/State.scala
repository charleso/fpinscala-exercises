package fpinscala.state

object RNGTest {
  import RNG._
  def main(args: Array[String]) {
    println("positiveInt", positiveInt(simple(100l)))
    println("positiveInt", positiveInt(simple(101l)))
    println("positiveInt", positiveInt(simple(102l)))
    println("intDouble", intDouble(simple(102l)))
    println("double", double(simple(100l)))
    println("boolean", boolean(simple(100l)))
    println("ints", ints(10)(simple(100l)))
    println("positiveMax", positiveMax(50)(simple(30l)))
    println("positiveInt2", positiveInt2(simple(102l)))

    println(State.simulateMachine(List(Coin, Turn, Turn, Coin, Coin, Coin, Turn, Coin)).run(Machine(true, 5, 0)))
  }
}

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We will later define other functions in terms of `nextInt`.
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) & // `&` is bitwise AND
                  ((1L << 48) - 1) // `<<` is left binary shift
      ((seed2 >>> 16).asInstanceOf[Int], // `>>>` is right binary shift with zero fill
       simple(seed2))
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def positiveInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    (math.abs(i), rng2)
  }

  def doubleOld(rng: RNG): (Double, RNG) = {
    val (i, rng2) = positiveInt(rng)
    ((i.toDouble / Int.MaxValue), rng2)
  }

  def intDoubleOld(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleIntOld(rng: RNG): ((Double,Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((d, i), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }


  def intsOld(count: Int)(rng: RNG): (List[Int], RNG) = {
    List.fill(count)(int).foldLeft((List[Int](), rng)) {
      case ((t, rng2), f) => {
        val (i, rng3) = f(rng2)
        (i :: t, rng3)
      }
    }
  }

  def positiveMax(n: Int): Rand[Int] = map(positiveInt)(_ % n)

  def double = map(positiveInt)(_.toDouble / Int.MaxValue)

  def boolean = map(positiveMax(2))(_ == 0)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def intDouble = map2(int, double)((_, _))

  def doubleInt = map2(double, int)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => fs.foldRight((List[A](), rng)) {
    case (r, (t, rng2)) => {
      val (i, rng3) = r(rng2)
      (i :: t, rng3)
    }
  }

  def ints(count: Int) = sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def positiveInt2: Rand[Int] = flatMap(int)(i => if(i == Int.MinValue) positiveInt2 else unit(i.abs))

  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))

}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- getState
    _ <- setState(f(s))
  } yield ()

  def getState[S]: State[S, S] = State(s => (s, s))
  def setState[S](s: S): State[S, Unit] = State(_ => ((), s))

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
  type Rand[A] = State[RNG, A]

  def sequence[S, A](as: List[State[S, A]]): State[S, List[A]] = State(s => as.foldRight((List[A](), s)) {
    case (r, (t, s2)) => {
      val (i, s3) = r.run(s2)
      (i :: t, s3)
    }
  })

  def simulateMachine(inputs: List[Input]): State[Machine, Int] = for {
    _ <- sequence(inputs.map(i => modify(handleInput(i))))
    s <- getState
  } yield s.coins

  /**
   * Inserting a coin into a locked machine will cause it to unlock if there is any candy left.
   * Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
   * Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
   * A machine that is out of candy ignores all inputs.
   */
  def handleInput(input: Input)(machine: Machine) = (input, machine) match {
    case (_, Machine(_, 0, coins)) => machine
    case (Coin, Machine(true, _, coins)) => machine.copy(locked = false, coins = coins + 1)
    case (Turn, Machine(false, candies, _)) => machine.copy(locked = true, candies = candies - 1)
    case _ => machine
  }
}