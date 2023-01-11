class BF(val text: String, val debug: Boolean):
  val limit: Int = text.length - 1

  var pc: Int = 0
  var loopStack: List[Int] = List()
  var pointer: Int = 0
  var cells: Map[Int, Int] = Map().withDefault(i => 0)

  private def movePointerRight(): Unit =
    if debug then println("movePointerRight")
    pointer += 1
    pc += 1

  private def movePointerLeft(): Unit =
    if debug then println("movePointerLeft")
    pointer -= 1
    pc += 1

  private def incrementCell(): Unit =
    if debug then println("incrementCell")
    cells = cells + (pointer -> (cells(pointer) + 1))
    pc += 1

  private def decrementCell(): Unit =
    if debug then println("decrementCell")
    cells = cells + (pointer -> (cells(pointer) - 1))
    pc += 1

  private def outputChar(): Unit =
    if debug then println("outputChar")
    pc += 1
    print(cells(pointer).toChar)

  private def inputChar(): Unit =
    if debug then println("inputChar")
    val char = Console.in.read.toChar
    cells = cells + (pointer -> char.toInt)
    pc += 1

  private def openBrace(): Unit =
    if debug then println("openBrace")
    if cells(pointer) == 0 then
      // go just pass matching ]
      var list: List[Boolean] = List()
      var flag: Boolean = true
      while flag do
        text(pc) match
          case '[' => list = true :: list
          case ']' => list = list.tail
          case _ => ()
        if list.isEmpty then
          flag = false
        else
          flag = true
        pc += 1
    else
      pc += 1

  private def closeBrace(): Unit =
    if debug then println("closeBrace")
    if cells(pointer) != 0 then
      // go just before matching [
      var list: List[Boolean] = List()
      var flag: Boolean = true
      while flag do
        text(pc) match
          case ']' => list = true :: list
          case '[' => list = list.tail
          case _ => ()
        if list.isEmpty then
          flag = false
        else
          flag = true
          pc -= 1
    else
      pc += 1

  private def comment(): Unit =
    pc += 1

  private def runOnce(): Unit =
    text(pc) match
      case '>' => movePointerRight()
      case '<' => movePointerLeft()
      case '+' => incrementCell()
      case '-' => decrementCell()
      case '.' => outputChar()
      case ',' => inputChar()
      case '[' => openBrace()
      case ']' => closeBrace()
      case _ => comment()

  def run(): Unit =
    while pc <= limit do
      runOnce()

end BF

@main def hello(filename: String, debug: Boolean): Unit = 
  import scala.io.Source

  val program = Source
    .fromFile(filename)
    .getLines
    .toList
    .mkString

  val bf = BF(program, debug)
  bf.run()

