import Comparador._


def menorQue(a:Int, b:Int): Boolean = a < b

insert(5, List(), menorQue)
insert(1, List(3, 5, 7), menorQue)
insert(10, List(2, 4, 6, 8), menorQue)
insert(5, List(1, 3, 7, 9), menorQue)
insert(3, List(1, 3, 5, 7), menorQue)

insertionSort(menorQue)(List())
insertionSort(menorQue)(List(7))
insertionSort(menorQue)(List(1, 2, 3, 4))
insertionSort(menorQue)(List(5, 4, 3, 2, 1))
insertionSort(menorQue)(List(3, 1, 3, 2))

menoresQue_noMenoresQue(List(), 5, menorQue)
menoresQue_noMenoresQue(List(5), 5, menorQue)
menoresQue_noMenoresQue(List(1, 2, 3, 4), 1, menorQue)
menoresQue_noMenoresQue(List(5, 2, 7, 1, 6), 5, menorQue)
menoresQue_noMenoresQue(List(3, 1, 3, 2, 3), 3, menorQue)

val menor: (Int, Int) => Boolean = (a, b) => a < b
val quick = quickSort(menor)

quick(List())
quick(List(7))
quick(List(1,2,3,4,5))
quick(List(3,1,4,5,2))
quick(List(4,2,4,1,2))

def noOrdena[T](comp: Comparador[T]): AlgoritmoOrd[T] = {
  def algoritmo(l: List[T]): (List[T], Int) = (l, 0)
  algoritmo
}

comparar(insertionSort(menor), quickSort(menor), List())
comparar(insertionSort(menor), quickSort(menor), List(5))
comparar(insertionSort(menor), quickSort(menor), List(1,2,3,4))
comparar(insertionSort(menor), quickSort(menor), List(3,1,4,2))
comparar(noOrdena(menor), quickSort(menor), List(3,1,2))



