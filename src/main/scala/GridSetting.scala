import Solution.maxCols

case class GridSetting (
                          N : Int
                          ,M : Int
                        ) {
  require(N > 0)
  require(M > 0 && M < maxCols)
}

