import scala.collection.immutable._

def gcd2(a:Int, b:Int): Int = {
  if (a==0|| b==0) a+b
  else gcd2(b%a,a%b)
}

def lcm2(a:Int, b:Int): Int = a*b / gcd2(a,b)


def LCM(l:List[Int]): Int = l.reduce(lcm2)

def isPrime(x:Int) : Boolean = {
  if (x==1) false
  else !(2 to math.ceil(math.sqrt(x)).toInt).map(x % _).contains(0)
}

def findPrime(D:Int,mn:Int=1):Int = {
  Stream.from(1, D).filter(_>mn).find(isPrime)match {
    case None => throw new IllegalStateException ("Prime should always exist")
    case Some(i) => i
  }
}


case class PrimeRing(modp:Int) {
  def fix(value:Int):Int = if (value >= 0) value%modp else modp-fix(-value)
  def powers(base:Int): Stream[Int] = {
    lazy val foos: Stream[Int] = 1 #:: foos.map(_*fix(base) % modp)
    return foos
  }
  def isGenerator(root:Int):Boolean = {
    lazy val t = powers(root).take(modp);
    (t.head == 1) && (t.reverse.head == 1) && !(t.tail.reverse.tail).contains(1)
  }
  def findGenerators : IndexedSeq[Int] = {
    (1 until modp).filter(isGenerator)
  }
  def inverse(root:Int):Int = {
    lazy val rootf = fix(root)
  Stream.from(1,modp).find(_%rootf==0) match {
    case None => throw new IllegalStateException ("Inverse should always exist")
    case Some(t) => t/rootf
  }
  }
  def LPcof(ind:Int,roots:List[Int]) : Int = {
    Tuple2(ind,roots) match {
      case (0,Nil) => 1
      case (_,Nil) => 0
      case (0, x::Nil) => (modp-x)%modp
      case (1, x::Nil) => 1
      case (_, x::Nil) => 0
      case (y, x:: tail) => (0 to y).map(yy=>LPcof(y-yy,List(x))*LPcof(yy,tail)).sum%modp
    }
  }
}



case class RingMatrix(ring:PrimeRing, values:List[List[Int]]) {
  def removeRow(index:Int): RingMatrix = RingMatrix(ring,values.patch(index,Nil,1))
  def T: RingMatrix = RingMatrix(ring,values.transpose)
  def removeCol(index:Int): RingMatrix = T.removeRow(index).T
  def patchRow(index:Int, newValues: List[Int]):  RingMatrix = RingMatrix(ring,values.patch(index,List(newValues),1))
  def patchCol(index:Int, newValues: List[Int]):  RingMatrix = T.patchRow(index,newValues).T
  def det: Int = {
    if (values.length == 1) values.head.head else
    values.indices.map(
      row => removeRow(row).removeCol(0).det * (if (row%2==0) 1 else -1) * values(row).head
    ).sum%ring.modp
  }
  def solve(b:List[Int]): List[Int] = {
    lazy val lDet = ring.inverse(det)
    values.indices.map( row =>
      (lDet * patchCol(row,b).det +ring.modp) %ring.modp
    ).toList
  }
}

case class UnityRoots(roots:List[Int]) {
  def rootsMult: List[(Int,Int)]= {
    roots.distinct.sorted.map(k => (k, roots.count(k == _)))
  }
  def rootsMap: Map[Int,Int] = {
    rootsMult.map(x => x._1 -> x._2).toMap
  }
  def rem1(root:Int, order:Int): UnityRoots = {
    UnityRoots(rootsMult.flatMap( x => List.fill(x._2 - (if (root==x._1) order else 0))(x._1)))
  }
  def countingMult: List[(Int,Int)] = {
    rootsMult.flatMap(x => (1 to x._2).map((x._1,_)))
  }
  def reductions: List[UnityRoots] = {
    countingMult.map(x => rem1(x._1,x._2))
  }
}

def paritySign(a:Int):Int = if (a%2==0) 1 else -1

case class SimpleRational(ring: PrimeRing, num:Int,shift:Int,power:Int) {
  def derivative: SimpleRational = SimpleRational(ring,ring.fix(-num*power),shift,power+1)
  def constant: Int = paritySign(power) * num * ring.inverse(ring.powers(shift)(power))
  def coef(index:Int) : Int = {
    if (index<=0) constant
    else {
      ring.fix(derivative.coef(index-1)*ring.inverse(index))
    }
  }
}



case class RingAlgebra(coins: List[Int], mn: Int=1) {
  def getRing: PrimeRing = {
    PrimeRing(findPrime(LCM(coins),mn))
  }
  def generator: Int = getRing.findGenerators.head
  def gPower(exp:Int):Int = getRing.powers(generator)(exp)
  def getUnityRoots: UnityRoots = {
    UnityRoots(coins.flatMap(d => (0 until d).map(i => gPower((getRing.modp - 1) / d * i))))
  }


  def undeterminedValCoef(power: Int):List[Int] = {
    getUnityRoots.reductions.map(x => getRing.LPcof(power,x.roots))
  }
  def uValMatrix:RingMatrix = {
    RingMatrix(getRing,getUnityRoots.roots.indices.map(undeterminedValCoef).toList)
  }
  def PartialFractionNumerators: List[Int] ={
    uValMatrix.solve(1:: List.fill(getUnityRoots.roots.length-1)(0))
  }
  def getSimpleRationals: List[SimpleRational] = {
    PartialFractionNumerators.lazyZip(getUnityRoots.countingMult).toList.map {
      case (num, (shift, power)) => SimpleRational(getRing, num, shift, power)
    }
  }
  def getTaylorCoef(order:Int) : Int = {
    getRing.fix(
      getSimpleRationals.map(_.coef(order)).sum * paritySign(coins.length)
    )
  }
}

def change(money:Int, coins:List[Int]): Int = {
  val ra = RingAlgebra(coins,money*money);
    println(ra.getRing.modp)
    ra.getTaylorCoef(money)
}







UnityRoots(List(1,1,1,2,2,3)).countingMult




(0 to 3).map(PrimeRing(7).LPcof(_,List(1,3,5)))


PrimeRing(7).findGenerators

PrimeRing(7).inverse(2)

isPrime(1)
isPrime(2)
isPrime(3)
isPrime(4)
isPrime(5)
isPrime(6)
isPrime(7)
isPrime(8)
isPrime(9)
isPrime(10)
isPrime(11)

findPrime(5)

lazy val ra = RingAlgebra(List(1,2),49)
ra.getRing.modp
ra.getUnityRoots
ra.uValMatrix
ra.PartialFractionNumerators

SimpleRational(ra.getRing,13,1,1).coef(7) //40
SimpleRational(ra.getRing,1,1,2).coef(7) //8
SimpleRational(ra.getRing,27,1,2).coef(7) //216
SimpleRational(ra.getRing,40,-1,1).coef(7) //13
SimpleRational(ra.getRing,40,52,1).coef(7) //13

change(2,List(1)) // 1
change(7,List(1,2)) // 4
change(7,List(1,2,4)) // 6
change(7,List(2,4)) // 0
change(9,List(4,5)) // 1