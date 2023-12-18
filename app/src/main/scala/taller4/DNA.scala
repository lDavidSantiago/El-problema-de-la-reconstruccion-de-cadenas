package taller4
import common._
import scala.annotation.tailrec

/**
 * La clase DNA representa un generador de secuencias de ADN.
 * Contiene métodos para generar secuencias de ADN aleatorias.
 */
class DNA {
  // El alfabeto de ADN que consiste en los cuatro nucleótidos
  val alfabeto = Seq('A', 'C', 'G', 'T')

  // Alias de tipo para una función que toma una secuencia de caracteres y devuelve un booleano
  type Oraculo = Seq[Char] => Boolean

  /**
   * Genera una secuencia de ADN aleatoria de una longitud dada.
   *
   * @param tam La longitud de la secuencia de ADN a generar.
   * @return Una secuencia de caracteres que representa una secuencia de ADN aleatoria.
   */
  def randomDna(tam: Int): Seq[Char] = {
    val r = scala.util.Random
    (1 to tam).map(_ => alfabeto(r.nextInt(4)))
  }

  /**
   * Verifica si una subsecuencia está presente en una secuencia dada.
   *
   * @param subCadena La subsecuencia a buscar.
   * @param cadena    La secuencia en la que buscar la subsecuencia.
   * @return Un booleano que indica si la subsecuencia está presente en la secuencia.
   */
  def verificarSecuencia(subCadena: Seq[Char], cadena: Seq[Char]): Boolean = {
    val oraculo: Oraculo = (s: Seq[Char]) => s.containsSlice(subCadena)
    oraculo(cadena)
  }

  /**
   * Genera todas las posibles combinaciones del alfabeto de ADN de una longitud dada y
   * devuelve la primera que satisface una condición dada.
   *
   * @param n La longitud de las combinaciones a generar.
   * @param o La condición que deben satisfacer las combinaciones.
   * @return Una secuencia de caracteres que representa una combinación que satisface la condición.
   */
  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    def generarCombinaciones(n: Int): List[Seq[Char]] = {
      if (n <= 0) {
        List(Seq[Char]())
      } else {
        for {
          letra <- alfabeto.toList
          combinacion <- generarCombinaciones(n - 1)
        } yield letra +: combinacion
      }
    }

    val s = generarCombinaciones(n).to(LazyList).filter(o).head
    s
  }

  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    def test(n: Int): List[Seq[Char]] = {
      if (n <= 0) {
        List(Seq[Char]())
      } else {
        val alfabeto1 = List(alfabeto.head)
        val alfabeto2 = List(alfabeto(1))
        val alfabeto3 = List(alfabeto(2))
        val alfabeto4 = List(alfabeto(3))

        val combinaciones1 = task {
          alfabeto1.flatMap { letra =>
            test(n - 1).map { combinaciones =>
              letra +: combinaciones
            }
          }
        }

        val combinaciones2 = task {
          alfabeto2.flatMap { letra =>
            test(n - 1).map { combinaciones =>
              letra +: combinaciones
            }
          }
        }

        val combinaciones3 = task {
          alfabeto3.flatMap { letra =>
            test(n - 1).map { combinaciones =>
              letra +: combinaciones
            }
          }
        }

        val combinaciones4 = task {
          alfabeto4.flatMap { letra =>
            test(n - 1).map { combinaciones =>
              letra +: combinaciones
            }
          }
        }

        val combinaciones = parallel(combinaciones1, combinaciones2, combinaciones3, combinaciones4)
        combinaciones._1.join() ++ combinaciones._2.join() ++ combinaciones._3.join() ++ combinaciones._4.join()
      }
    }

    val s = test(n).to(LazyList).filter(o).head
    s
  }


  /**
   * Genera todas las posibles combinaciones del alfabeto de ADN de una longitud dada y
   * devuelve la primera que satisface una condición dada. Este método está optimizado
   * para generar combinaciones de una manera más eficiente.
   *
   * @param n La longitud de las combinaciones a generar.
   * @param o La condición que deben satisfacer las combinaciones.
   * @return Una secuencia de caracteres que representa una combinación que satisface la condición.
   */
  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    @tailrec
    def GenerarCadenaMejorada(k: Int, SC: Seq[Seq[Char]]): Seq[Char] = {
      if (k > n) Seq.empty[Char]
      else {
        val newSC = SC.flatMap(seq => alfabeto.map(c => seq :+ c)).filter(o)
        newSC.find(_.length == n) match {
          case Some(resultado) => resultado
          case None => GenerarCadenaMejorada(k + 1, newSC)
        }
      }
    }
    GenerarCadenaMejorada(1, Seq(Seq.empty[Char]))
  }

  /**
   * Genera todas las posibles combinaciones del alfabeto de ADN de una longitud dada y
   * devuelve la primera que satisface una condición dada. Este método está optimizado
   * para generar combinaciones de una manera más eficiente y paralelizada.
   *
   * @param umbral El umbral para la paralelización.
   * @param n La longitud de las combinaciones a generar.
   * @param o La condición que deben satisfacer las combinaciones.
   * @return Una secuencia de caracteres que representa una combinación que satisface la condición.
   */

  def reconstruirCadenaMejoradoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    @tailrec
    def GenerarCadenaMejoradapar(k: Int, SC: Seq[Seq[Char]]): Seq[Char] = {
      if (k > n) Seq.empty[Char]
      else {
        if (k <= umbral) {
          val newSC = SC.flatMap(seq => alfabeto.map(c => seq :+ c)).filter(o)
          newSC.find(_.length == n) match {
            case Some(resultado) => resultado
            case None => GenerarCadenaMejoradapar(k + 1, newSC)
          }
        } else {
          val (alfabeto1, alfabeto2) = alfabeto.splitAt(alfabeto.length / 2)
          val newSecuenceCaracter1 = task {
            SC.flatMap(seq => alfabeto1.map(c => seq :+ c)).filter(o)
          }
          val newSecuenceCaracter2 = task {
            SC.flatMap(seq => alfabeto2.map(c => seq :+ c)).filter(o)
          }
          val newSecuenceCaracter = parallel(newSecuenceCaracter1, newSecuenceCaracter2)

          newSecuenceCaracter._1.join().find(_.length == n) match {
            case Some(resultado) => resultado
            case None => newSecuenceCaracter._2.join().find(_.length == n) match {
              case Some(resultado) => resultado
              case None => GenerarCadenaMejoradapar(k + 1, newSecuenceCaracter._1.join() ++ newSecuenceCaracter._2.join())
            }
          }
        }
      }
    }
    GenerarCadenaMejoradapar(1, Seq(Seq.empty[Char]))
  }



  /**
   * Genera todas las posibles combinaciones del alfabeto de ADN de una longitud dada y
   * devuelve la primera que satisface una condición dada. Este método está optimizado
   * para generar combinaciones de la manera más eficiente posible.
   *
   * @param n La longitud de las combinaciones a generar.
   * @param o La condición que deben satisfacer las combinaciones.
   * @return Una secuencia de caracteres que representa una combinación que satisface la condición.
   */
  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    if (n == 1) {
      alfabeto.map(Seq(_)).find(o).getOrElse(Seq.empty)
    } else {
      @tailrec
      def generarCadenaTurbo(k: Int, SC: Seq[Seq[Char]]): Seq[Char] = {
        val newSC = SC.flatMap(seq => alfabeto.flatMap(c => Some(seq :+ c).filter(o)))
        val resultado = newSC.find(w => w.length == n)

        if ((resultado.nonEmpty)) {
          resultado.head
        } else {
          generarCadenaTurbo(k + 1, newSC)
        }
      }

      val conjuntoInicial: Seq[Seq[Char]] = alfabeto.map(Seq(_))
      generarCadenaTurbo(1, conjuntoInicial)
    }
  }

  /**
   * Genera todas las posibles combinaciones del alfabeto de ADN de una longitud dada y
   * devuelve la primera que satisface una condición dada. Este método está optimizado
   * para generar combinaciones de la manera más eficiente posible.
   *
   * @param n La longitud de las combinaciones a generar.
   * @param o La condición que deben satisfacer las combinaciones.
   * @return Una secuencia de caracteres que representa una combinación que satisface la condición.
   */
  def reconstruirCadenaTurbopar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    if (n == 1) {
      alfabeto.map(Seq(_)).find(o).getOrElse(Seq.empty)
    } else {
      def generarCadenaTurboPar(k: Int, SC: Seq[Seq[Char]]): Seq[Char] = {
        if (k <= umbral) {
          val newSC = SC.flatMap(seq => alfabeto.map(c => seq :+ c)).filter(o)
          newSC.find(_.length == n).getOrElse(generarCadenaTurboPar(k + 1, newSC))
        } else {
          val (alfabeto1, alfabeto2) = alfabeto.splitAt(alfabeto.length / 2)
          val nuevaSecuencia1 = task {
            SC.flatMap(seq => alfabeto1.map(c => seq :+ c)).filter(o)
          }
          val nuevaSecuencia2 = task {
            SC.flatMap(seq => alfabeto2.map(c => seq :+ c)).filter(o)
          }
          val NuevaSecuenciaCombi = parallel(nuevaSecuencia1, nuevaSecuencia2)
          val resultado = NuevaSecuenciaCombi._1.join().find(_.length == n).orElse(NuevaSecuenciaCombi._2.join().find(_.length == n))

          resultado.getOrElse(generarCadenaTurboPar(k + 1, NuevaSecuenciaCombi._1.join() ++ NuevaSecuenciaCombi._2.join()))
        }
      }

      generarCadenaTurboPar(1, Seq(Seq.empty[Char]))
    }
  }


  /**
   * Genera todas las posibles combinaciones del alfabeto de ADN de una longitud dada y
   * devuelve la primera que satisface una condición dada. Este método está optimizado
   * para generar combinaciones de la manera más eficiente posible y mejorada.
   *
   * @param n La longitud de las combinaciones a generar.
   * @param o La condición que deben satisfacer las combinaciones.
   * @return Una secuencia de caracteres que representa una combinación que satisface la condición.
   */

  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
    if (n == 1) {
      alfabeto.map(Seq(_)).find(o).getOrElse(Seq.empty)
    }
    else {
      @tailrec
      def generarCadenaTurbo(k: Int, SC: Seq[Seq[Char]]): Seq[Char] = {
        val newSC = SC.flatMap(seq => alfabeto.flatMap(c => Some(seq :+ c).filter(o)))
        val resultado = newSC.find(w => w.length == n)
        if (resultado.nonEmpty) {
          resultado.head
        } else {
          generarCadenaTurbo(k + 1, newSC) // probar velocidades multiplicando en vez de sumando (k+1) => (k*2)
        }                                  //Considerar volver funcion fuera de la funcion para reutilizar arriba
      }
      def filtrar(SC: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
        SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2)).filter {
          s => (0 to s.length - k).forall(i => SC.contains(s.slice(i, i + k)))
        }
      }

      val conjuntoInicial: Seq[Seq[Char]] = alfabeto.map(Seq(_))
      generarCadenaTurbo(1, conjuntoInicial)
    }
  }


}






