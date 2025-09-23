import Comparador._
import scala.util.Random

val random = new Random()

def listaAlAzar(long:Int): List[Int] = {

  val v = Vector.fill(long){
    random.nextInt(long *2)+1
  }
  v.toList
}

def menorQue(a:Int, b:Int): Boolean = a < b
def mayorQue(a:Int, b:Int): Boolean = a > b

val iSort_Asc = insertionSort[Int](menorQue)
val iSort_Desc = insertionSort[Int](mayorQue)

iSort_Asc(List(4,5,6,1,2,3))
iSort_Asc(List(10, 3, 15, 7, 2, 1, 5))
iSort_Asc(List(8, 12, 4, 6, 14, 2, 10))
iSort_Asc(List(20, 18, 25, 5, 7, 13, 11))
iSort_Asc(List(100, 50, 75, 25, 10, 90))

val qSort_Asc = quickSort[Int](menorQue)
val qSort_Desc = quickSort[Int](mayorQue)

qSort_Asc(List(4,5,6,1,2,3))
qSort_Asc(List(3, 1, 2))
qSort_Asc(List(9, 8, 7, 6, 5, 4, 3, 2, 1))
qSort_Asc(List(42, 17, 23, 8, 15, 4, 16))
qSort_Asc(List(30, 25, 40, 20, 10, 35))

comparar(iSort_Asc,qSort_Asc,List(4,5,6,1,2,3))
comparar(iSort_Asc,qSort_Desc,List(4,5,6,1,2,3))
comparar(iSort_Asc, qSort_Desc, List(3, 1, 2))
comparar(iSort_Desc, qSort_Asc, List(42, 17, 23, 8, 15, 4, 16))
comparar(iSort_Desc, qSort_Desc, List(42, 17, 23, 8, 15, 4, 16))

val lAsc100 = (1 to 100).toList
val lAsc1000 = (1 to 1000).toList
val lDsc100 = (1 to 100).toList.reverse
val lDsc1000 = (1 to 1000).toList.reverse

comparar(iSort_Asc,qSort_Asc,lAsc100)
comparar(iSort_Asc,qSort_Asc,lAsc1000)
comparar(iSort_Asc,qSort_Asc,lDsc100)
comparar(iSort_Asc,qSort_Asc,lDsc100)

val l5 = listaAlAzar(5)
val l10 = listaAlAzar(10)
val l20 = listaAlAzar(20)
val l50 = listaAlAzar(50)

iSort_Asc(l5)
iSort_Desc(l5)
iSort_Asc(l10)
iSort_Desc(l10)
iSort_Asc(l20)
iSort_Desc(l20)
iSort_Asc(l50)
iSort_Desc(l50)

qSort_Asc(l5)
qSort_Desc(l5)
qSort_Asc(l10)
qSort_Desc(l10)
qSort_Asc(l20)
qSort_Desc(l20)
qSort_Asc(l50)
qSort_Desc(l50)

comparar(iSort_Asc ,qSort_Asc , l5)
comparar(iSort_Asc ,qSort_Asc , l10)
comparar(iSort_Asc ,qSort_Asc , l20)
comparar(iSort_Asc ,qSort_Asc , l50)