package taller4
import common._
import scala.annotation.tailrec

/**
 * DNA class represents a DNA sequence generator.
 * It contains methods to generate random DNA sequences.
 */
class DNA {
  //The DNA alphabet consisting of the four nucleotides
  val alfabeto = Seq('A', 'C', 'G', 'T')

  //Type alias for a function that takes a sequence of characters and returns a boolean
  type Oraculo = Seq[Char] => Boolean

  /**
   * Generates a random DNA sequence of a given length.
   *
   * @param tam The length of the DNA sequence to generate.
   * @return A sequence of characters representing a random DNA sequence.
   */
  def randomDna(tam: Int): Seq[Char] = {
    val r = scala.util.Random
    (1 to tam).map(_ => alfabeto(r.nextInt(4)))
  }

  /**
   * Verifies if a subsequence is present in a given sequence.
   *
   * @param subCadena The subsequence to look for.
   * @param cadena    The sequence in which to look for the subsequence.
   * @return A boolean indicating whether the subsequence is present in the sequence.
   */
  def verificarSecuencia(subCadena: Seq[Char], cadena: Seq[Char]): Boolean = {
    val oraculo: Oraculo = (s: Seq[Char]) => s.containsSlice(subCadena)
    oraculo(cadena)
  }

  /**
   * Generates all possible combinations of the DNA alphabet of a given length and
   * returns the first one that satisfies a given condition.
   *
   * @param n The length of the combinations to generate.
   * @param o The condition that the combinations must satisfy.
   * @return A sequence of characters representing a combination that satisfies the condition.
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
        val (alfabeto1, alfabeto2) = alfabeto.splitAt(alfabeto.length / 2)
        val combinaciones1 = task {
          alfabeto1.flatMap { letra =>
            test(n - 1).map { combinacion =>
              letra +: combinacion
            }
          }
        }
        val combinaciones2 = task {
          alfabeto2.flatMap { letra =>
            test(n - 1).map { combinacion =>
              letra +: combinacion
            }
          }
        }
        val combinaciones = parallel(combinaciones1, combinaciones2)
        (combinaciones._1.join() ++ combinaciones._2.join()).toList
      }
    }
    val s = test(n).to(LazyList).filter(o).head
    s
  }


  /**
   * Generates all possible combinations of the DNA alphabet of a given length and
   * returns the first one that satisfies a given condition. This method is optimized
   * to generate combinations in a more efficient way.
   *
   * @param n The length of the combinations to generate.
   * @param o The condition that the combinations must satisfy.
   * @return A sequence of characters representing a combination that satisfies the condition.
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
   * Generates all possible combinations of the DNA alphabet of a given length and
   * returns the first one that satisfies a given condition. This method is further optimized
   * to generate combinations in the most efficient way.
   *
   * @param n The length of the combinations to generate.
   * @param o The condition that the combinations must satisfy.
   * @return A sequence of characters representing a combination that satisfies the condition.
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

  def reconstruirCadenaTurboMejoradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    if (n == 1) {
      alfabeto.map(Seq(_)).find(o).getOrElse(Seq.empty)
    }
    else {
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

      def filtrar(SC: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
        SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2)).filter {
          s => (0 to s.length - k).forall(i => SC.contains(s.slice(i, i + k)))
        }
      }

      val conjuntoInicial: Seq[Seq[Char]] = alfabeto.map(Seq(_))
      generarCadenaTurboPar(1, conjuntoInicial)
    }
  }


}






