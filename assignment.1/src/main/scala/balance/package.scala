package object balance {

  def b(chars: List[Char], opened: Int, closed: Int): Boolean = {
    if (chars.isEmpty) opened == closed
    else if (opened < closed) false
    else
      b(chars.tail,
        addIfSatisfies(opened, chars.head, ('(' == _)),
        addIfSatisfies(closed, chars.head, (')' == _)))
  }

  def addIfSatisfies(count: Int, c: Char, predicate: (Char) => Boolean): Int = {
    if (predicate(c)) count + 1
    else count
  }
}