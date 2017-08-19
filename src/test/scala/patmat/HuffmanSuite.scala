package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("2: makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('u', 8), ('t', 2), ('e', 1), ('c', 42), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3), Leaf('u', 8), Leaf('c', 42)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine of leaf list of two elements") {
    val leaflist = List( Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('t',2),Leaf('x',4),List('t', 'x'),6)))
  }

  test("combine of leaf list of one element") {
    val leaflist = List(Leaf('x', 4))
    assert(combine(leaflist) === List(Leaf('x', 4)))
  }

  test("combine of leaf list of one Fork element") {
    val leaflist = List(Fork(Leaf('t',2),Leaf('x',4),List('t', 'x'),6))
    assert(combine(leaflist) === List(Fork(Leaf('t',2),Leaf('x',4),List('t', 'x'),6)))
  }

  test("combine of leaf list of two Fork elements") {
    val leaflist = List( Fork(Leaf('t',2),Leaf('x',4),List('t', 'x'),6), Fork(Leaf('z',3),Leaf('k',8),List('z', 'k'),11))
    assert(combine(leaflist) === List(Fork(Fork(Leaf('t',2),Leaf('x',4),List('t', 'x'),6),Fork(Leaf('z',3),Leaf('k',8),List('z', 'k'),11),List('t', 'x', 'z', 'k'),17)))
  }

  test("until funciton") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) === List(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e','t'), 3), Leaf('x',4), List('e','t','x'), 7)))
  }

  test("createCodeTree funciton with 3 letters") {
    val text = "texxtxx"
    assert(createCodeTree(text.toList) === Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e','t'), 3), Leaf('x',4), List('e','t','x'), 7))
  }

  test("createCodeTree funciton with 4 letters") {
    val text = "texzxtxxzz"
    assert(createCodeTree(text.toList) === Fork(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e','t'), 3), Leaf('z',3), List('e','t','z'), 6), Leaf('x',4), List('e','t','z','x'), 10))
  }



  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a slightly longer text should be identity") {
    new TestTrees {
      assert(decode(t2, encode(t2)("abd".toList)) === "abd".toList)
    }
  }

  test("decode and encode a more longer text should be identity") {
    new TestTrees {
      val t3 = Fork(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e','t'), 3), Leaf('z',3), List('e','t','z'), 6), Leaf('x',4), List('e','t','z','x'), 10)
      assert(decode(t3, encode(t3)("etzxze".toList)) === "etzxze".toList)
    }
  }


}
