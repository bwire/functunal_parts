package forcomp


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = w
  	.toLowerCase
  	.groupBy(ch => ch)
  	.mapValues(_.size)
  	.toList
  	.sortWith((c1, c2) => c1._1.compareTo(c2._1) < 0)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.foldLeft("")(_ ++ _))

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = 
    dictionary.groupBy(word => wordOccurrences(word)) 

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.get(wordOccurrences(word)).get

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def combinations(): List[List[Occurrences]] = {
      val combs = occurrences.map(
        pair => 
          for {
              num <- 0 to pair._2
           } yield List((pair._1, num))
        )
      val mapped = combs.map(s => s.toList)
      mapped.map(ulist => ulist.map(llist => if (llist.head._2 == 0) List() else llist))
    }
    
    def permute(combs: List[List[Occurrences]]): List[Occurrences] = {
      val perms = combs.foldRight(List(List[Occurrences]()))((list, acc) => 
         list.flatMap(x => acc.map(a => x::a)))
      perms.map(list => list.flatten)
    }
    permute(combinations())
  }
    
  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = 
    x.foldRight(List[(Char, Int)]())((pair, acc) => {
      val yNum = y.find(p => p._1 == pair._1) match {
        case Some(p) => p._2
        case None => 0
      }
      val diff =pair._2 - yNum
      if (diff == 0)
        acc
      else
        (pair._1, diff)::acc
    })

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams1(sentence: Sentence): List[Sentence] = {
    def findAnagrams(list: Sentence, listOccs: Occurrences): List[Sentence] = {
      val possibleResults = list.map { 
        word => {
          val resOccs = subtract(listOccs, wordOccurrences(word))
          val newCombs = combinations(resOccs)
          val newWords = list.groupBy(elem => wordOccurrences(elem)).foldRight (List[String]()) ((pair, acc) => if (newCombs.contains(pair._1)) pair._2 ++ acc else acc)
          if (newWords isEmpty) 
            List(List(word))
          else
            findAnagrams(newWords, resOccs).map(nl => word::nl)
        }
      }
      possibleResults.flatten.filter(list => sentenceOccurrences(list) == listOccs)
    }
    
    sentence match {
      case Nil => List(List())
      case _ => {
        // first step (all words from the dictionary that match)
        val occs = sentenceOccurrences(sentence)
        val combs = combinations(occs)
        val words = dictionaryByOccurrences.foldRight (List[String]()) ((pair, acc) => if (combs.contains(pair._1)) pair._2 ++ acc else acc)
        findAnagrams(words, occs)  
      }
    }
  }
  
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def findAnagrams(occurrences: Occurrences, dict: Map[Occurrences, List[Word]]): List[Sentence] = {
      val combs = combinations(occurrences) 
      val newDict = dict.foldRight (Map[Occurrences, List[Word]]()) ((pair, acc) => if (combs.contains(pair._1)) acc + (pair._1 -> pair._2) else acc)
      val wordLists = newDict.values.toList
      if (wordLists isEmpty)
        List()
      else {
        for {
          wordList <- wordLists
          word <- wordList
          rest <- findAnagrams(subtract(occurrences, wordOccurrences(word)), newDict)
        } yield word::rest
      }
    }
    sentence match {
      case Nil => List(List())
      case _ => findAnagrams(sentenceOccurrences(sentence), dictionaryByOccurrences) 
    }
  }
}
