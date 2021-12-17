
def y_next(y_v: Int, y_prev: Int) : Int = 
    y_v + y_prev

def y_v_next(y_v_prev: Int) : Int = 
    y_v_prev -1

def x_next(x_v: Int, x_prev: Int) : Int = 
     x_v + x_prev

def x_v_next(x_v_prev: Int) : Int = 
    if (x_v_prev > 0) x_v_prev -1 else 0


case class State(x: Int, y: Int, x_v: Int, y_v: Int)

def run(state: State) : State = {

    val x = x_next(state.x, state.x_v)
    val y = y_next(state.y, state.y_v)
    val x_v = x_v_next(state.x_v)
    val y_v = y_v_next(state.y_v)
    return State(x, y, x_v, y_v)
}

case class Boundaries(minX: Int, maxX: Int, minY: Int, maxY: Int)

def checkBoundaries(state: State, boundaries: Boundaries) : Boolean = {
    return state.x >= boundaries.minX && state.x <= boundaries.maxX && state.y >= boundaries.minY && state.y <= boundaries.maxY
}

def checkOverShoot(state: State, boundaries: Boundaries) : Boolean = {
  if (state.x > boundaries.maxX) {
    return true
  }
  if (state.y < boundaries.minY) {
    return true
  }
  false
}


def run(boundaries: Boundaries)(state: State, maxY: Int): Option[Int] = {
  
    val nextState = run(state)
    val currentMaxY = Array[Int](nextState.y, maxY).max
    checkBoundaries(nextState, boundaries) match {
        case true => Some(currentMaxY)
        case false => 
          checkOverShoot(nextState, boundaries) match {
            case true => return None
            case false => return run(boundaries)(nextState, currentMaxY)
          }
    }
}


def bruteForce(boundaries: Boundaries) : IndexedSeq[Int] = {
  
  (-0 to 300).map(
    xv => (-300 to 300).map(
      yv => run(boundaries)(State(0, 0, xv, yv), 0)
    )
  ).flatten.flatten
}

def partOne(boundaries: Boundaries) : Int = {
  bruteForce(boundaries).max
}


def partTwo(boundaries: Boundaries) : Int = {
  bruteForce(boundaries).size
}
@main def hello: Unit = 
  val boundaries = Boundaries(217, 240, -126, -69)
  println(partOne(boundaries))
  println(partTwo(boundaries))

