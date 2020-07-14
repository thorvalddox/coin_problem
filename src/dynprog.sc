def change(money: Int, coins: List[Int]) : Int = {

  if (money < 0) return 0
  if (coins.isEmpty) return 0

  val dp = new Array[Int](money + 1)
  dp(0) = 1
  var i = 0
  while (i < coins.length) {
    var j = coins(i)
    while (j <= money) {
      dp(j) += dp(j - coins(i))

      j += 1
    }

    i += 1
  }
  return dp(money)
}


change(7,List(1,2)) == 4
change(7,List(1,2,4)) == 6
change(7,List(2,4)) == 0
change(9,List(4,5)) == 1