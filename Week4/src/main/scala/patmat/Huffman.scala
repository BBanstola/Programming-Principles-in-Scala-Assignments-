package patmat

object Huffman{

  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars:List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  def weight(tree: CodeTree):Int = tree match {
    case Leaf(c,w) => w
    case Fork(l,r,c,w) => weight(l) + weight(r)
  }

  def chars(tree: CodeTree):List[Char] =  tree match {
    case Leaf(c,w) => List(c)
    case Fork(l,r,c,w) => c
  }

  def makeCodeTree(left: CodeTree, right: CodeTree): CodeTree = Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] = {
    def timesChar(char: Char): (Char, Int) = (char, chars.count(x => char == x))

    def timesAcc(restChars: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] =
      if (restChars.isEmpty) acc else {
        val found = timesChar(restChars.head)
        val rest = timesAcc(restChars.tail, acc)
        if (rest.contains(found)) rest else found :: rest
      }
    timesAcc(chars, List())
  }

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    def makeLeafList(ordered: List[(Char, Int)], acc: List[Leaf]): List[Leaf] =
      if (ordered.isEmpty) acc else {
        Leaf(ordered.head._1, ordered.head._2) :: makeLeafList(ordered.tail, acc)
      }

    makeLeafList(freqs.sortWith((a: (Char, Int), b: (Char, Int)) => a._2 <= b._2), Nil)
  }

  def singleton(trees: List[CodeTree]): Boolean = trees.length == 1

  def combine(trees: List[CodeTree]): List[CodeTree] = {
    def insertByWeight(toInsert: CodeTree, existingList: List[CodeTree]) =
      insertBetween(toInsert, existingList.partition(weight(_) < weight(toInsert)))

    def insertBetween[T](toInsert: T, tuple: (List[T], List[T])) = tuple._1 ::: toInsert :: tuple._2

    trees match {
      case Nil => trees
      case head :: Nil => trees
      case first :: second :: tail => insertByWeight(makeCodeTree(first, second), tail)
    }
  }

  def until(singletonMeth: List[CodeTree] => Boolean, combineMeth: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if (singletonMeth(trees)) trees else until(singletonMeth, combineMeth)(combineMeth(trees))
  }

  def createCodeTree(chars: List[Char]): CodeTree = until(singleton, combine)(makeOrderedLeafList(times(chars)))(0)

  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def findBranch(givenTree: CodeTree, bit: Bit): CodeTree = (givenTree, bit) match {
      case (_: Leaf, _) => throw new IllegalArgumentException("leaf.findBranch")
      case (Fork(_, right, _, _), 1) => right
      case (Fork(left, _, _, _), 0) => left
    }

    def mapBits(givenTree: CodeTree, restBits: List[Bit]): (Char, List[Bit]) = givenTree match {
      case Leaf(ch, _) => (ch, restBits)
      case _: Fork => mapBits(findBranch(givenTree, restBits.head), restBits.tail)
    }

    def decodeAcc(tree: CodeTree, mappedBits: (Char, List[Bit]), acc: List[Char]): List[Char] = mappedBits match {
      case (ch, Nil) => (ch :: acc).reverse
      case (ch, bits) => decodeAcc(tree, mapBits(tree, bits), ch :: acc)
    }

    decodeAcc(tree, mapBits(tree, bits), Nil)
  }

  /**
    * A Huffman coding tree for the French language.
    * Generated from the data given at
    *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
    */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  /**
    * What does the secret message say? Can you decode it?
    * For the decoding use the `frenchCode' Huffman tree defined above.
    */
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  /**
    * Write a function that returns the decoded secret
    */
  def decodedSecret: List[Char] = decode(frenchCode, secret)

  // Part 4a: Encoding using Huffman tree

  /**
    * This function encodes `text` using the code tree `tree`
    * into a sequence of bits.
    */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def encodeAcc(givenTree: CodeTree, givenText: List[Char], acc: List[Bit]): List[Bit] = {
      if (givenText.isEmpty) acc.reverse else
        encodeAcc(givenTree, givenText.tail, encodeMapBits(givenTree, givenText.head).reverse ::: acc)
    }

    encodeAcc(tree, text, Nil)
  }

  def encodeMapBits(givenTree: CodeTree, givenChar: Char): List[Bit] = {
    def encodeMapBitsAcc(tree: CodeTree, char: Char, acc: List[Bit]): List[Bit] = tree match {
      case _: Leaf => acc.reverse
      case Fork(left, right, _, _) =>
        if (chars(left).contains(char)) encodeMapBitsAcc(left, char, 0 :: acc)
        else encodeMapBitsAcc(right, char, 1 :: acc)
    }

    encodeMapBitsAcc(givenTree, givenChar, Nil)
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
    * This function returns the bit sequence that represents the character `char` in
    * the code table `table`.
    */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = ???

  /**
    * Given a code tree, create a code table which contains, for every character in the
    * code tree, the sequence of bits representing that character.
    *
    * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
    * a valid code tree that can be represented as a code table. Using the code tables of the
    * sub-trees, think of how to build the code table for the entire tree.
    */
  def convert(tree: CodeTree): CodeTable = ???

  /**
    * This function takes two code tables and merges them into one. Depending on how you
    * use it in the `convert` method above, this merge method might also do some transformations
    * on the two parameter code tables.
    */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = ???

  /**
    * This function encodes `text` according to the code tree `tree`.
    *
    * To speed up the encoding process, it first converts the code tree to a code table
    * and then uses it to perform the actual encoding.
    */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = ???
}

