
package taller4

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
    (1 to n).foldLeft(Seq(Seq[Char]())) { (acc, _) =>
      acc.flatMap(seq => alfabeto.map(char => seq :+ char))
    }.filter(o).head
  }
}

