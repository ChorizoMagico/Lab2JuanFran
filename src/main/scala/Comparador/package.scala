package object Comparador {
  type AlgoritmoOrd[T] = List[T] => (List[T], Int)
  type Comparador[T] = (T, T) => Boolean

  def insert[T](e: T, l: List[T], comp: Comparador[T]): (List[T], Int) = {

    def ordenar(e: T, l: List[T], comp: Comparador[T], counter: Int, prevList: List[T]): (List[T], Int) = {

      if(l.isEmpty){
        (prevList++(e::l), counter+1)
      } else{

        if (comp(e, l.head)) {
          ordenar(e, l.tail, comp, counter + 1, l.head :: prevList)
        } else {
          (prevList ++(e::l), counter)
        }

      }
    }
    ordenar(e, l, comp, 0, List())
  }

  def insertionSort[T](comp: Comparador[T]): AlgoritmoOrd[T] ={


    def AlgoritmoOrd(l:List[T]): (List[T], Int) = {

      def ordenar(newList:List[T], remainingList:List[T], counter:Int): (List[T], Int)={
        if(remainingList.isEmpty) (newList, counter)
        else{
          val workingList = insert(remainingList.head, newList, comp)

          ordenar(workingList._1, remainingList.tail, counter + workingList._2)
        }
      }
      ordenar(List(), l, 0)
    }

    AlgoritmoOrd

  }


}
