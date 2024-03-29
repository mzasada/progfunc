package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal

  test("pascal: col=0,row=0") {
    assert(pascal(0, 0) === 1)
  }
  
  test("pascal: col=0,row=1") {
    assert(pascal(0, 1) === 1)
  }
  
  test("pascal: col=1,row=1") {
    assert(pascal(1, 1) === 1)
  }
  
  test("pascal: col=0,row=4") {
    assert(pascal(0, 4) === 1)
  }

  test("pascal: col=1,row=4") {
    assert(pascal(1, 4) === 4)
  }
  
  test("pascal: col=2,row=4") {
    assert(pascal(2, 4) === 6)
  }
  
  test("pascal: col=3,row=4") {
    assert(pascal(3, 4) === 4)
  }
  
  test("pascal: col=4,row=4") {
    assert(pascal(4, 4) === 1)
  }
}
