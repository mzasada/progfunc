package objsets

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.util.HashSet
import java.util.ArrayList
import scala.collection.JavaConversions._

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  trait GoogleAndAppleTweets {
    private val gizmodoTweets = TweetReader.ParseTweets.getTweetData("gizmodo", TweetData.gizmodo)
    private val techCrunchTweets = TweetReader.ParseTweets.getTweetData("TechCrunch", TweetData.TechCrunch)
    private val engadgetTweets = TweetReader.ParseTweets.getTweetData("engadget", TweetData.engadget)
    private val amazondealsTweets = TweetReader.ParseTweets.getTweetData("amazondeals", TweetData.amazondeals)
    private val cnetTweets = TweetReader.ParseTweets.getTweetData("CNET", TweetData.CNET)
    private val gadgetlabTweets = TweetReader.ParseTweets.getTweetData("gadgetlab", TweetData.gadgetlab)
    private val mashableTweets = TweetReader.ParseTweets.getTweetData("mashable", TweetData.mashable)

    val listOfTweetSets = new ArrayList[TweetSet]
    listOfTweetSets.add(asTweetSet(gizmodoTweets, new Empty))
//    listOfTweetSets.add(asTweetSet(techCrunchTweets, new Empty))
//    listOfTweetSets.add(asTweetSet(engadgetTweets, new Empty))
//    listOfTweetSets.add(asTweetSet(amazondealsTweets, new Empty))
//    listOfTweetSets.add(asTweetSet(cnetTweets, new Empty))
//    listOfTweetSets.add(asTweetSet(gadgetlabTweets, new Empty))
//    listOfTweetSets.add(asTweetSet(mashableTweets, new Empty))

    val allTweetsSet = union(listOfTweetSets.toList, new Empty)

    def asTweetSet(tweets: List[Tweet], acc: TweetSet): TweetSet = {
      if (tweets.isEmpty) acc
      else asTweetSet(tweets.tail, acc incl tweets.head)
    }

    def union(sets: List[TweetSet], acc: TweetSet): TweetSet = {
      if (sets.isEmpty) acc
      else union(sets.tail, acc union (sets.head))
    }
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  test("find apple tweets") {
    new GoogleAndAppleTweets {
      val appleTweets = GoogleVsApple.getTweetsWithKeywords(allTweetsSet, GoogleVsApple.apple)
      val googleTweets = GoogleVsApple.getTweetsWithKeywords(allTweetsSet, GoogleVsApple.google)
      println("apple: " + size(appleTweets))
      println("google: " + size(googleTweets))
    }
  }
}
