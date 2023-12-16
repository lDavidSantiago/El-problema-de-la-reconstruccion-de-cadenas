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




object Taller4 {
  type Oraculo = Seq[Char] => Boolean


  def main(args: Array[String]): Unit = {
    val dna = new DNA()
    val cadena = dna.randomDna(10)
    val or = Oraculo.crearOraculo(cadena)

    println("Cadena aleatoria:")
    println(cadena)

    //println("Resultado de reconstruirCadenaIngenuo:")
    //println(dna.reconstruirCadenaIngenuo(cadena.length,or))

    //println("Resultado de reconstruirCadenaTurbo:")
    //println(dna.reconstruirCadenaTurbo(cadena.length, or))

    //println("Resultado de reconstruirCadenaTurboMejorada:")
    //println(dna.reconstruirCadenaTurboMejorada(cadena.length, or))

    //println("Resultado de ReconstruirCadenaMejorada: ")
    //println(dna.reconstruirCadenaMejorado(cadena.length, or))

    println("Resultado de paralelizacion de Cadena Mejorada: ")
    println(dna.reconstruirCadenaMejoradoPar(cadena.length,or))

    //val dna = new DNA()
    //val duro = dna.reconstruirCadenaIngenuoPar(2,2)
    //println(duro)


  }
}
