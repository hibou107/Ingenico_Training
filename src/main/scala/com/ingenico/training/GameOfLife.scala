sealed trait Cell
case object Death extends Cell
case object Alive extends Cell
object Game {
  type Board = Vector[Vector[Cell]]
  def step(board: Board): Board = {
    ???
  }
  def evolute(cell: Cell, AliveNeighborhoods: List[Cell]): Cell = {
    val size = AliveNeighborhoods.length
    (cell, size) match {
      case (Alive, x) if x < 2 => Death
      case (Alive, x) if x == 2 || x == 3 => Alive
      case (Alive, x) if x > 3 => Death
      case (Death, x) if x == 3 => Alive
      case (c, _) => c
    }
  }
  def lookForNeighbourhs(posX: Int, posY: Int, board: Board): List[Cell] = {
    val positions = List(
      (posX - 1, posY + 1),
      (posX + 1, posY + 1),
      (posX + 1, posY - 1),
      (posX - 1, posY - 1),
      (posX, posY + 1),
      (posX, posY - 1),
      (posX + 1, posY),
      (posX - 1, posY))
    val filteredPos = positions.filter {
      case (x, y) if x >= 0 && y >= 0 && x < board.head.size && y < board.size => true
      case _ => false
    }
    filteredPos.map {case (x, y) => board(y)(x)}
  }
}
object Tests {
  def main(args: Array[String]): Unit = {
    val board = Vector(Vector(Alive, Death, Death),
      Vector(Death, Death, Alive),
      Vector(Alive, Alive, Alive))
    val test1 = Game.lookForNeighbourhs(0, 0, board)
    val expected1 = List(Death, Death, Death)
    println(expected1 == test1)
    val test2 = Game.lookForNeighbourhs(2, 0, board)
    val expected2 = Set(Death, Death, Alive)
    println(expected2 == test2.toSet)
  }
}
class Cellule() {
}
class GameOfLife {
}