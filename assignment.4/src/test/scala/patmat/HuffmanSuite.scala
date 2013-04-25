package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times of \"aabbac\"") {
    val timesList = times(string2Chars("aabbac"))
    assert(timesList.contains(('a', 3)))
    assert(timesList.contains(('b', 2)))
    assert(timesList.contains(('c', 1)))
    assert(timesList.size === 3)
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("makeOrderedLeafList for string \"aabbac\"") {
    assert(makeOrderedLeafList(times(string2Chars("aabbac"))) === List(Leaf('c', 1), Leaf('b', 2), Leaf('a', 3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
    assert(combine(combine(leaflist)) === List(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7)))
    assert(combine(combine(combine(leaflist))) === List(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7)))
  }

  test("combine preserves order") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4))
    assert(combine(leaflist) === List(Leaf('x', 4), Fork(Leaf('e', 2), Leaf('t', 3), List('e', 't'), 5)))
  }
  
  test("combine until list contains single tree") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) === List(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("abaaabaab".toList)) === "abaaabaab".toList)
    }
  }

  test("decoded secret") {
    assert(decodedSecret === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  }

  test("code bits") {
    val table = List(('a', List(0, 0, 0)), ('b', List(0, 1, 1)), ('c', List(0, 0, 1)))
    assert(codeBits(table)('a') == List(0, 0, 0))
    assert(codeBits(table)('b') == List(0, 1, 1))
    assert(codeBits(table)('c') == List(0, 0, 1))
  }

  test("convert") {
    new TestTrees {
      val table = convert(t2)
      assert(codeBits(table)('a') == List(0, 0))
      assert(codeBits(table)('b') == List(0, 1))
      assert(codeBits(table)('d') == List(1))
    }
  }

  test("decode and quick encode a text with t1 should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("abaaabaab".toList)) === "abaaabaab".toList)
    }
  }
  
  test("decode and quick encode a text with t2 should be identity") {
    new TestTrees {
      assert(decode(t2, quickEncode(t2)("abdddaadabadab".toList)) === "abdddaadabadab".toList)
    }
  }
}
