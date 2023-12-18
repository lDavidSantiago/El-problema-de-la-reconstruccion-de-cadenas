/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package taller4

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer
import scala.annotation.tailrec
import org.scalameter._



object App {
  type Oraculo = Seq[Char] => Boolean


  def main(args: Array[String]): Unit = {
    val dna = new DNA()
    val cadena = dna.randomDna(4)
    val or = Oraculo.crearOraculo(cadena)
    println(cadena)
    println(dna.reconstruirCadenaTurbo(cadena.length, or))
    println(dna.reconstruirCadenaIngenuo(cadena.length, or))
    println(dna.reconstruirCadenaIngenuoPar(cadena.length,or))
    println(dna.reconstruirCadenaTurboMejorada(cadena.length,or))
    println(dna.reconstruirCadenaTurboMejoradaPar(cadena.length,or))
    println(dna.reconstruirCadenaTurbopar(cadena.length,or))


  }
}
