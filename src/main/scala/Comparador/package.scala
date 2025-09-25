package object Comparador{
  type AlgoritmoOrd[T] = List[T] => (List[T], Int) // Plantilla de función que ordena una lista y cuenta sus pasos
  type Comparador[T] = (T, T) => Boolean // Compara dos valores y devuelve true o false

  def insert[T](e: T, l: List[T], comp: Comparador[T]): (List[T], Int) = {

    def ordenar(e: T, l: List[T], comp: Comparador[T], counter: Int, prevList: List[T]): (List[T], Int) = {

      /*Recorre una lista, disminuyendo su tamaño de uno en uno y crea una lista en principio vacía que se la va
      * aumentando la lista recorrida. De esta manera, se forma la lista completa. Cuando la comparación indicada es verdad, se concatena el elemento
      * en ese espacio entre dos números, en donde la comparación es verdad y su posición es correcta.
      * T odo esto asumiendo que la lista original está correctamenta ordenada como dice el pdf
      * */

      if(l.isEmpty){
        (prevList++(e::l), counter+1) // Caso base, si la lista a ordenar está vacía se le concatena el elemento que se quiere concatenar :p + la lista que va atrás del elemento (y se suma 1 al contador)
      } else{

        if (comp(e, l.head)) {
          (prevList ++ (e :: l), counter + 1) // Si la lista no está vacía y la comparación es verdad, se concatena el elemento atrás de lo que falte de lista y al frente de la lista ya recorrida
        } else {
          ordenar(e, l.tail, comp, counter + 1, prevList ++ List(l.head)) //Sino, sigue recorriendo la lista, quitándole un elemento a la original y poniéndole a la de atrás
        }

      }
    }
    ordenar(e, l, comp, 0, List()) //Se llama a la función con un elemento e cualquiera que se va a insertar en la lista ya ordenada l, con un comparador cualquiera comp, con un contador en 0 y una lista vacía que tendrá la lista de atrás hasta e
  }

  def insertionSort[T](comp: Comparador[T]): AlgoritmoOrd[T] ={

    //Generaliza el metodo anterior pero ahora para organizar una lista cualquiera desorganizada (usando insert)

    def AlgoritmoOrd(l:List[T]): (List[T], Int) = {
      //Crea una nueva lista vacía, a esta lista le va insertando poco a poco los elementos de la lista original (remaining list) en orden (es decir, usando insert())

      def ordenar(newList:List[T], remainingList:List[T], counter:Int): (List[T], Int)={
        if(remainingList.isEmpty) (newList, counter+1) // Caso base, si la lista está vacía, devuelve la lista construida más el contador
        else{
          val workingList = insert(remainingList.head, newList, comp) //Sino, inserta la cabeza de la lista original en una nueva lista (en su posición correcta gracias a insert(), en principio vacía

          ordenar(workingList._1, remainingList.tail, counter + workingList._2) //Ahora avanza en la lista original (le quita uno), y llama de nuevo a la función con: la nueva lista que se está ordenando (y que lleva un elemento más que antes), lo que falta de la lista original y el contador acumulado
        }
      }
      ordenar(List(), l, 0) //Se llama a la función con una lista vacía como nueva lista y la lista indicada como lista original
    }

    AlgoritmoOrd

  }

  def menoresQue_noMenoresQue[T](l: List[T], v: T, comp: Comparador[T]): (List[T], List[T], Int) =
  {
    def ordenar(l1: List[T], l2: List[T], l3: List[T], v: T, counter: Int, comp: Comparador[T]): (List[T], List[T], Int) = {

      /*
      Divide una lista en dos usando el primer elemento de la lista original como pivote
      La primera parte será menor al pivote y la siguiente será mayor al pivote (esto dependiendo del comparador que se esté utilizando)
      Por último, se concatena primera lista, pivote y segunda lista.

      Esto se hace de elemento en elemento, comparando cada elemento de la lista original (o sobrante) con el pivote, y mandándolo a una
      de las dos dependiendo de si es mayor o menor
       */
      if(l3.isEmpty){
        (l1, l2, counter+1) //Caso base, la lista sobrante está vacía, se devuelve el contador más lo que se tenga de primera lista y de segunda lista
      }else{
        if(comp(l3.head, v)) ordenar((l3.head)::l1, l2, l3.tail, v, counter+1, comp) //Si la cabeza de la lista sobrante es "menor" al pivote, se agrega a la primera lista, se le quita uno a la lista sobrante y se vuelve a llamar a la función
        else ordenar(l1, (l3.head)::l2, l3.tail, v, counter+1, comp) //En caso contrario, si la cabeza es "mayor" al pivote, se manda a la parte 2 y se vuelve a llamar a la función con uno menos en la lista sobramte
      }

    }

    if (l.isEmpty) (List(), List(), 1)
    else ordenar(List(), List(), l.tail, v, 0, comp) //Se llama a la función, siendo el pivote el primer elemento de la lista y la lista original o sobrante el resto (o la cola)

  }

  def quickSort[T](comp:Comparador[T]): AlgoritmoOrd[T] = {
    /*Generaliza la función anterior, ahora para organizar listas cualesquiera diviéndolas constantemente en dos partes,
    como en recursión de árbol. Primero se hace una división inicial, usando el primero elemento como pivote e implícitamente la cola de la lista
    como lista original o sobrante. Luego, se vuelve a llamar la función para la lista de la izquierda y se le pasa el contador que se lleve hasta ese momento.
    Como se llama por valor, se hace recursivamente este mismo proceso hasta que toda la parte izquierda esté ordenada. Luego, se hace lo mismo con la parte derecha,
    llamando la función de nuevo por valor, pero ahora con el contador que quedó de todos los procesos en la parte izquierda.

    Finalmente, se concatenan el resultado de la parte izquierda con el pivote original con el resultado de la parte derecha

    */
    def AlgoritmoOrd (l:List[T]):  (List[T], Int) = {

      def ordenar(l: List[T], counter: Int, comp: Comparador[T]): (List[T], Int) = {

        if(l.isEmpty) (l, counter) // Caso base. Si la lista es vacía, se devuelve la lista vacía y el contador
        else{

          val parList = menoresQue_noMenoresQue(l, l.head, comp) //Primera división, implícitamente con la cola de la lista como lista original y su cabeza como pivote

          val subParList = ordenar(parList._1, counter + parList._3, comp) // División de la parte izquierda. Como es por valor, se realiza inmediatamente. También acumula todos los contadores
          val overParList = ordenar(parList._2, subParList._2, comp) // División de la parte derecha. También es por valor y se hace inmediatamente. Ahora se le pasa el contador que se lleva hasta ese momento

          (subParList._1 ++ List(l.head) ++ overParList._1, overParList._2) //Se concatenan los resultados

        }
      }

      ordenar(l, 0, comp) //Se llama a la función con una lista cualquiera y un contador en cero
    }

    AlgoritmoOrd
  }


  def comparar[T](a1:AlgoritmoOrd[T] , a2:AlgoritmoOrd[T] , l : List [T]):( Int , Int) = {
    /*
    Compara dos ordenamientos de listas, es decir, cómo quedan ordenadas las listas y cuántas comparaciones se hicieron.
    Si las listas quedaron ordenadas de forma diferente, devuelve (-1,-1)
    Si quedaron ordenadas de igualmente, devuelve (contador del primer ordenamiento, contador del segundo ordenamiento)
     */

    val output1 = a1(l) // Primer ordenamiento
    val output2 = a2(l) //Segundo ordenamiento

    val l1 = output1._1 //Primera lista ordenada
    val l2 = output2._1 //Segunda lista ordenada

    val int1 = output1._2 //Contador del primer ordenamiento
    val int2 = output2._2 //Contador del segundo ordenamiento



    def sonIguales (l1 : List[T], l2 : List[T]): (Int, Int) = {
      /*
      Lo mismo de arriba. Compara si los resultados son iguales, en tal caso devuelve los contadores. Sino, devuelve (-1,-1)
      Como es sobre la misma lista, basta con saber si la primera lista es vacía para saber si terminó de comparar
       */
      if(l1.isEmpty){
        return (int1, int2) //Caso base, la lista es vacía. Devuelve los contadores. Sirve para saber cuándo terminan las comparaciones
      }

      if(l1.head == l2.head) sonIguales(l1.tail, l2.tail) //Sino, va compara de cabeza en cabeza, primer comparando las cabezas iniciales y luego llamando a la función con un elemento menos en la lista
      else{
        (-1,-1) // En caso de que no sean iguales
      }
    }

    sonIguales(l1, l2) //Llama a la función con dos listas dadas
  }


}
