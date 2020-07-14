def change(money: Int, coins: List[Int]) : Int = {
  if (money < 0) 0
  else if (money == 0) 1
  else coins.tail.map(x => change(money-x,coins.filter(y => y<=x))).sum +
    (if (money % coins.head == 0) 1 else 0)
}

change(7,List(1,2)) // 4
change(7,List(1,2,4)) // 6
change(7,List(2,4)) // 0
change(9,List(4,5)) // 1