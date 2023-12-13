
package taller4

import scala.annotation.tailrec

/**
 * DNA class represents a DNA sequence generator.
 * It contains methods to generate random DNA sequences.
 */
class   DNA{
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

  def verificarSecuencia(subCadena: Seq[Char], cadena: Seq[Char]): Boolean = {
    val oraculo: Oraculo = (s: Seq[Char]) => s.containsSlice(subCadena)
    oraculo(cadena)
  }

  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    def generarCombinaciones(n: Int): Iterator[Seq[Char]] = {
      if (n <= 0) {
        Iterator(Seq[Char]())
      } else {
        for {
          letra <- alfabeto.iterator
          combinacion <- generarCombinaciones(n - 1)
        } yield letra +: combinacion
      }
    }
    val e = generarCombinaciones(4).toList
    println(e)
    val s = generarCombinaciones(n).to(LazyList).filter(o).head
    s
  }


  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq [Char] = {
    @tailrec
    def generarCadenaTurbo(k: Int, SC: Seq[Seq[Char]]): Seq[Char] = {
      val newSC = SC.flatMap(seq => alfabeto.flatMap(c => Some(seq :+ c).filter(o)))
      val resultado = newSC.find(w => w.length == n)

      if (resultado.nonEmpty) {
        resultado.head
      } else if (k > n) {
        Seq.empty[Char]
      } else {
        generarCadenaTurbo(k * 2, newSC)
      }
    }

    val conjuntoInicial: Seq[Seq[Char]] = alfabeto.map(Seq(_))
    generarCadenaTurbo(1, conjuntoInicial)
  }
}






