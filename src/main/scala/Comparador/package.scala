package object Comparador{
  type AlgoritmoOrd[T] = List[T] => (List[T], Int)
  type Comparador[T] = (T, T) => Boolean

  def insert[T](e: T, l: List[T], comp: Comparador[T]): (List[T], Int) = {

    def ordenar(e: T, l: List[T], comp: Comparador[T], counter: Int, prevList: List[T]): (List[T], Int) = {

      if(l.isEmpty){
        (prevList++(e::l), counter+1)
      } else{

        if (comp(e, l.head)) {
          (prevList ++ (e :: l), counter + 1)
        } else {
          ordenar(e, l.tail, comp, counter + 1, prevList ++ List(l.head))
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

  def menoresQue_noMenoresQue[T](l: List[T], v: T, comp: Comparador[T]): (List[T], List[T], Int) =
  {

    def ordenar(l1: List[T], l2: List[T], l3: List[T], v: T, counter: Int, comp: Comparador[T]): (List[T], List[T], Int) = {

      if(l3.isEmpty){
        (l1, l2, counter)
      }else{
        if(comp(l3.head, v)) ordenar((l3.head)::l1, l2, l3.tail, v, counter+1, comp)
        else ordenar(l1, (l3.head)::l2, l3.tail, v, counter+1, comp)
      }

    }

    ordenar(List(), List(), l.tail, v, 0, comp)

  }

  def quickSort[T](comp:Comparador[T]): AlgoritmoOrd[T] = {

    def AlgoritmoOrd (l:List[T]):  (List[T], Int) = {

      def ordenar(l: List[T], counter: Int, comp: Comparador[T]): (List[T], Int) = {

        if(l.isEmpty) (l, counter)
        else{

          val parList = menoresQue_noMenoresQue(l, l.head, comp)

          val subParList = ordenar(parList._1, counter + parList._3, comp)
          val overParList = ordenar(parList._2, subParList._2, comp)

          (subParList._1 ++ List(l.head) ++ overParList._1, overParList._2)

        }
      }

      ordenar(l, 0, comp)
    }

    AlgoritmoOrd
  }


  def comparar[T](a1:AlgoritmoOrd[T] , a2:AlgoritmoOrd[T] , l : List [T]):( Int , Int) = {

    val output1 = a1(l)
    val output2 = a2(l)

    val l1 = output1._1
    val l2 = output2._1

    val int1 = output1._2
    val int2 = output2._2



    def sonIguales (l1 : List[T], l2 : List[T]): (Int, Int) = {

      if(l1.isEmpty){
        return (int1, int2)
      }

      if(l1.head == l2.head) sonIguales(l1.tail, l2.tail)
      else{
        (-1,-1)
      }
    }

    sonIguales(l1, l2)
  }


}
