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
    assert(string2Chars("hello, world") ===
      List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) ===
      List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) ===
      List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine of some leaf list, which requires reordering") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4))
    assert(combine(leaflist) ===
      List(Leaf('x',4), Fork(Leaf('e',2),Leaf('t',3),List('e', 't'),5)))
  }

  test("find the optimal codes") {
    val codeTree = createCodeTree("abcdabcaba".toList)
    assert(codeTree ===
      makeCodeTree(Leaf('a', 4), makeCodeTree(makeCodeTree(
         Leaf('d', 1), Leaf('c', 2)), Leaf('b', 3))))
  }

  test("decode a very short text") {
    new TestTrees {
      val x = List(0, 1)
      assert(decode(t1, x) === List('a', 'b'))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      val x = encode(t1)("ab".toList)
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and quick-encode a very short text should be identity") {
    new TestTrees {
      val x = encode(t1)("ab".toList)
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
