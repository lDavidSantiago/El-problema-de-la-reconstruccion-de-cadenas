/**
 * Plantilla para pruebas
* @author Carlos Delgado
* @version 1.0
* @note 22 de Noviembre de 2023 
 */
package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestProyectoFinal extends AnyFunSuite{
    test("Test 1: Reconstruir cadena ingenuo tamano 4"){
        val dna = new DNA()
        val cadena = dna.randomDna(4)
        val or = Oraculo.crearOraculo(cadena)
        assert(dna.reconstruirCadenaIngenuo(cadena.length, or) == cadena)
    }
    test("Test 1.1: Reconstruir cadena ingenuo par tamano 4"){
        val dna = new DNA()
        val cadena = dna.randomDna(4)
        val or = Oraculo.crearOraculo(cadena)
        assert(dna.reconstruirCadenaIngenuoPar(cadena.length, or) == cadena)
    }
    test("Test 2: Reconstruir cadena mejorada tamano 4"){
        val dna = new DNA()
        val cadena = dna.randomDna(4)
        val or = Oraculo.crearOraculo(cadena)
        assert(dna.reconstruirCadenaTurboMejorada(cadena.length, or) == cadena)

    }
    test("Test 2.1: Reconstruir cadena mejorada par tamano 4"){
        val dna = new DNA()
        val cadena = dna.randomDna(4)
        val or = Oraculo.crearOraculo(cadena)
        assert(dna.reconstruirCadenaTurboMejoradaPar(cadena.length, or) == cadena)
    }
    test("Test 3: Reconstruir cadena turbo tamano 4"){
        val dna = new DNA()
        val cadena = dna.randomDna(4)
        val or = Oraculo.crearOraculo(cadena)
        assert(dna.reconstruirCadenaTurbo(cadena.length, or) == cadena)
    }
    test("Test 3.1: Reconstruir cadena turbo par tamano 4"){
        val dna = new DNA()
        val cadena = dna.randomDna(4)
        val or = Oraculo.crearOraculo(cadena)
        assert(dna.reconstruirCadenaTurbopar(cadena.length, or) == cadena)
    }
    test("Test 4: Reconstruir cadena turbo mejorada tamano 4"){
        val dna = new DNA()
        val cadena = dna.randomDna(4)
        val or = Oraculo.crearOraculo(cadena)
        assert(dna.reconstruirCadenaTurboMejorada(cadena.length, or) == cadena)
    }
    test("Test 4.1: Rescontruir cadena turbo mejorada par tamano 4"){
        val dna = new DNA()
        val cadena = dna.randomDna(4)
        val or = Oraculo.crearOraculo(cadena)
        assert(dna.reconstruirCadenaTurboMejoradaPar(cadena.length, or) == cadena)
    }
}
