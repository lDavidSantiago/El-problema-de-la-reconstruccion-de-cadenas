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



object Taller4 {
  type Oraculo = Seq[Char] => Boolean


  def main(args: Array[String]): Unit = {
    val dna = new DNA()
    val cadena = dna.randomDna(250)
    val or = Oraculo.crearOraculo(cadena)
    val umbralcant = 501
    val sizes = List(4, 8, 16, 32, 64, 128, 256, 512)
/*
    sizes.foreach { size =>
      val cadena = dna.randomDna(size)
      val or = Oraculo.crearOraculo(cadena)

      println(s"Cadena aleatoria de tamaño $size:")
      // println(cadena)

      val timeMejo = withWarmer(new Warmer.Default) measure {
        dna.reconstruirCadenaTurboMejorada(cadena.length, or)
      }
      println(s"Time taken by turbo mejorada: ${timeMejo.value * 1000} ms")

      val timeMejopar = withWarmer(new Warmer.Default) measure {
        dna.reconstruirCadenaTurboMejoradaPar(cadena.length, or)
      }
      println(s"Time taken by turbo mejorada paralelizada: ${timeMejopar.value * 1000} ms")

      val aceleracion = timeMejo.value / timeMejopar.value
      println(s"Aceleracion:  $aceleracion")
    }
*/
    //println("Resultado de reconstruirCadenaIngenuo:")
    //println(dna.reconstruirCadenaIngenuo(cadena.length,or))

    println("Resultado de ReconstruirCadenaMejorada: ")
    println(dna.reconstruirCadenaMejorado(cadena.length, or))

    println("Resultado de paralelizacion de Cadena Mejorada: ")
    println(dna.reconstruirCadenaMejoradoPar(cadena.length, or))


    println("Resultado de reconstruirCadenaTurbo:")
    println(dna.reconstruirCadenaTurbo(cadena.length, or))

    println("Resultado de pralelizacon de Turbo: ")
    println(dna.reconstruirCadenaTurbopar(cadena.length,or))

    println("Resultado de reconstruirCadenaTurboMejorada:")
    println(dna.reconstruirCadenaTurboMejorada(cadena.length, or))
  }
}
