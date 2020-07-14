def gridScan(base:Int,mx:Int): List[List[Int]] = {
  if (mx==1)(0 until base).map(x => List(x)).toList
  else (0 until base).map(x => gridScan(base,mx-1).map(y => x :: y))
    .toList.flatten.toList
}
def change(money: Int, coins: List[Int]) : Int = {
  gridScan(money/coins.min + 1,coins.length)
    .map(x => (x zip coins)
      .map{ Function.tupled(_ * _)}.sum)
    .count(_==money)
}

change(7,List(1,2)) == 4
change(7,List(1,2,4)) == 6
change(7,List(2,4)) == 0
change(9,List(4,5)) == 1