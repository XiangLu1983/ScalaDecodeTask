import Solution.validPattern

case class GridCell(
                   cellChar: Char
                   ,cellPosition : Int
                   ) {
  def isValidCharachter = cellChar.toString.matches(validPattern)
}
