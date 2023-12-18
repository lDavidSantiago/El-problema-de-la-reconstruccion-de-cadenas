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
    val cadena = dna.randomDna(4)
    val or = Oraculo.crearOraculo(cadena)
    val umbralcant = 4

    println("Cadena aleatoria:")
    println(cadena)

    //println("Resultado de reconstruirCadenaIngenuo:")
    //println(dna.reconstruirCadenaIngenuo(cadena.length,or))

    //println("Resultado de ReconstruirCadenaMejorada: ")
    //println(dna.reconstruirCadenaMejorado(cadena.length, or))

    //println("Resultado de paralelizacion de Cadena Mejorada: ")
    //println(dna.reconstruirCadenaMejoradoPar(4)(cadena.length, or))


    //println("Resultado de reconstruirCadenaTurbo:")
    //println(dna.reconstruirCadenaTurbo(cadena.length, or))

    //println("Resultado de pralelizacon de Turbo: ")
    //println(dna.reconstruirCadenaTurbopar(4)(cadena.length,or))



    //println("Resultado de reconstruirCadenaTurboMejorada:")
    //println(dna.reconstruirCadenaTurboMejorada(cadena.length, or))


    val timeMejo = withWarmer(new Warmer.Default) measure {
      dna.reconstruirCadenaMejorado(cadena.length, or)
    }
    println(s"Time taken by mejorada: ${timeMejo.value * 1000} ms")

    val timeMejopar = withWarmer(new Warmer.Default) measure {
      dna.reconstruirCadenaMejoradoPar(umbralcant)(cadena.length, or)
    }
    println(s"Time taken by mejorada paralelizada: ${timeMejopar.value * 1000} ms")

    val timeTurbo = withWarmer(new Warmer.Default) measure {
      dna.reconstruirCadenaTurbo(cadena.length, or)
    }
    println(s"Time taken by turbo mejorada: ${timeTurbo.value * 1000} ms")

    val timeTurbopar = withWarmer(new Warmer.Default) measure {
      dna.reconstruirCadenaTurbopar(umbralcant)(cadena.length,or)
    }
    println(s"Time taken by turbo mejorada paralelizada: ${timeTurbopar.value * 1000} ms")


  }

}
