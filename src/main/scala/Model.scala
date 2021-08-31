import java.io.File

class Model(counts: Map[String, Int]) {
  def count(ngram: String): Int = counts.get(ngram).getOrElse(0)
}

object Model {
  def apply(file: File): Model = {
    val counts = scala.collection.mutable.Map[String, Int]().withDefaultValue(0)
    scala.io.Source.fromFile(file)
      .sliding(3, 1)
      .foreach(s => counts.put(s.mkString(""), counts(s.mkString("")) + 1))
    println(counts.toMap)
    new Model(counts.toMap)
  }
}