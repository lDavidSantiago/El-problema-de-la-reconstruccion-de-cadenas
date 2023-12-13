package object Oraculo {
  type Oraculo = Seq[Char] => Boolean

  def crearOraculo(c:Seq[Char]):Oraculo ={
    def esSubcadena(s:Seq[Char]):Boolean = {
      c.containsSlice(s)
    }
    esSubcadena
  }
}