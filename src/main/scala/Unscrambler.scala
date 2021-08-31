import java.io.File

class Unscrambler(model: Model) {

  def unscrambleWord(context: String, word: String, candidates: Map[String, Double] = Map("" -> 1)): String =
    def contextSpace = context.trim + " "

    if (word.count(_.isUpper) == 1)
      unscrambleWord(contextSpace + word.find(_.isUpper).get, word.filter(!_.isUpper))
    else if (word.intersect("„").sizeIs == 1)
      unscrambleWord(contextSpace + "„", word.filter(_ != '„'))
    else if (word.intersect(".,!?\"”").sizeIs == 1)
      unscrambleWord(context, word diff ".,!?\"”„") + word.intersect(".,!?\"”")
    else {
      def prob(s: String, c: Char): Double =
        println(s"prob($s, $c): ${(model.count(s.takeRight(2) + c) + 1) / (model.count(s.takeRight(2)) + 1)}")
        (model.count(s.takeRight(2) + c) + 1) / (model.count(s.takeRight(2)) + 1)

      val newCands = (for (
        (cand, cProb) <- candidates;
        c <- word diff cand
      ) yield (cand + c, cProb * prob(context + cand, c)))
        .toMap
      if (word.sizeIs <= 1 || newCands.isEmpty)
        context + candidates.maxBy(_._2)._1
      else
        unscrambleWord(context, word, newCands)
    }

  def unscramble(original: String): String =
    original
      .split(" ")
      .foldLeft("")(unscrambleWord(_, _))
}

object Unscrambler {
  def main(args: Array[String]): Unit = {
    println(new Unscrambler(Model(new File("train.txt"))).unscrambleWord("Hello ", "ro!dlW"))
  }
}
