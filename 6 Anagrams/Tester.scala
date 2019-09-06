package forcomp

import forcomp.Anagrams._

object Tester extends App {
  sentenceAnagrams(List("Linux", "rulez")).foreach(println)
}