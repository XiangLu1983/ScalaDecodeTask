import scala.util.{Failure, Success, Try}

object Solution extends SolutionConfig {
  def main(args: Array[String]): Unit = {
    val firstMultipleInput = scala.io.StdIn.readLine()
    val gridSetting = parseMultipleInput(firstMultipleInput)
    implicit val gs = gridSetting match {
      case None => return true
      case Some(config) => config
    }
    val matrixInput = getMatrixInput()
    val parsedInput = parseToMatrix(matrixInput).zipWithIndex.map(g => GridCell(g._1,g._2))
    val firstSplitPos = findFirstValidPosition(parsedInput).getOrElse(0)
    val firstSplitSet = parsedInput.splitAt(firstSplitPos)
    val firstInput = firstSplitSet._1
    val readyInput = firstSplitSet._2
    val lastDecodedInput = findLastValidPosition(readyInput).getOrElse(0)
    val secondSplitInputs = readyInput.splitAt(lastDecodedInput)
    val secondInput = decodeInputs(secondSplitInputs._1)
    val finalInput = outputResults(firstInput ++ secondInput ++ secondSplitInputs._2)
    println(finalInput)
  }

  /** Parse the first line input to grid setting
   *
   * @param input
   * @return
   */
  def parseMultipleInput(input: String): Option[GridSetting] = Try {
    val sepInputs = input.split(sepSpace).take(2).map(_.toInt)
    GridSetting(sepInputs(0), sepInputs(1))
  }.toOption

  /** Receive N line inputs
   *
   * @param gs
   * @return
   */
  def getMatrixInput()(implicit gs: GridSetting): Seq[Char] = {
    val inputs = for {
      i <- 1 to gs.N
      input <- formatInput(scala.io.StdIn.readLine())
    } yield input
    inputs
  }

  /** Reformat the inputs with required format
   *
   * @param input
   * @param gs
   * @return
   */
  def formatInput (input: String)(implicit gs: GridSetting) : String ={
    if (input.length < gs.M) {
      input.concat(repeatChar(emptyString, gs.M - input.length))
    } else input.substring(0,gs.M)
  }

  /**
   *
   * @param c
   * @param n
   * @return repeat the c with n times
   */
  def repeatChar(c: Char, n: Int): String = (for (i <- 1 to n) yield c).mkString

  def parseToMatrix(inputs: Seq[Char])(implicit gs: GridSetting): Seq[Char] = {
    for {
      i <- 0 to gs.M - 1
      j <- 0 to gs.N - 1
      ch = inputs(j * gs.M + i)
    } yield ch

  }

  /** Find the last digit for valid char in the seq
   *
   * @param inputs
   * @param gs
   * @return
   */
  def findLastValidPosition(inputs: Seq[GridCell])(implicit gs: GridSetting): Option[Int] = Try {
    inputs.filter(_.isValidCharachter).maxBy(_.cellPosition).cellPosition
  }.toOption

  /** Find the first digit for valid char in the seq
   *
   * @param inputs
   * @param gs
   * @return
   */
  def findFirstValidPosition(inputs: Seq[GridCell])(implicit gs: GridSetting): Option[Int] = Try {
    inputs.filter(_.isValidCharachter).minBy(_.cellPosition).cellPosition
  }.toOption

  /** replaces the symbols or spaces between two alphanumeric characters with a single space ' '
   *
   * @param inputs
   * @param gs
   * @return
   */
  def decodeInputs(inputs: Seq[GridCell])(implicit gs: GridSetting): Seq[GridCell] = {
    val validSeq = inputs.filter(_.isValidCharachter)
    val x = for {
      i<- 1 to validSeq.length-1
      if (validSeq(i).cellPosition - validSeq(i-1).cellPosition > 1)
    } yield  GridCell(emptyString,validSeq(i-1).cellPosition+1)
    x.toSeq ++ validSeq
  }

  /** Output the result
   *
   * @param inputs
   * @param gs
   * @return
   */
  def outputResults(inputs: Seq[GridCell])(implicit gs: GridSetting): String = {
    inputs.sortBy(_.cellPosition).map(_.cellChar) mkString ""
  }
}
