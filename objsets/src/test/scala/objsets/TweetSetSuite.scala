package objsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 30))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  trait TestTweetSets {
    val set1 = (new Empty) incl
      (new Tweet("gizmodo", "This $50 stick turns any HDTV into an Android-powered smart TV: http://t.co/8FpZUnIE", 101)) incl
      (new Tweet("x", "Warning: Security bug can wipe out your Android phone just by visiting a web page-not only limited to Samsung http://t.co/0y6vnOKw", 290)) incl
      (new Tweet("TechCrunch", "Soon-To-Be-Acquired BlueSprig's AirCover Family Locator Is An iOS/Android App That Lets You Track ... http://t.co/qSQquuLS by @ingridlunden", 24)) incl
      (new Tweet("TechCrunch", "Music-Sharing Startup MyStream Preps Android Launch, Looks Beyond Music http://t.co/CusbX5mh by @anthonyha", 18)) incl
      (new Tweet("engadget", "Cubify lets you skin, 3D print your own personal Android -  http://t.co/S6nimh5R", 23)) incl
      (new Tweet("engadget", "NPD: Android users chew an average 870MB of cellular data per month, youngest gobble the most -  http://t.co/tUHgRYn8", 25)) incl
      (new Tweet("CNET", "How to switch from iPhone to Android http://t.co/M8I9lwua", 131)) incl
      (new Tweet("CNET", "How to lock down and find Android and Windows phones http://t.co/mRw8P80z", 25)) incl
      (new Tweet("gadgetlab", "Galaxy Tab 10.1 Injunction Still Stands in Apple v. Samsung http://t.co/JwOCDnw1 by @redgirlsays", 11)) incl
      (new Tweet("gadgetlab", "First iPhone 5 Benchmarks: Screaming Fast, Yes, But Just Shy of Galaxy S III  http://t.co/QIAhda3L by @redgirlsays", 79)) incl
      (new Tweet("gizmodo", "The 20 most obvious PINs are painfully obvious http://t.co/Du9BYjeR", 38)) incl
      (new Tweet("gizmodo", "This is the closest, clearest view of Mars yet http://t.co/VrbZRVgR", 45)) incl
      (new Tweet("gizmodo", "Meet the new Apple, where things don't just work http://t.co/XS36kHWi", 146))
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
      assert(size(set5.filter(tw => tw.retweets == 20)) === 1)
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
      assert(trends.head.user === "a", "the most retweets is a")
    }
  }

  test("descending: set1") {
    new TestTweetSets {
      val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
      val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")
      val foo = List("foo", "blabla")
      val googleTweets: TweetSet = set1.filter(tw => google.exists(tw.text.contains))
      val appleTweets: TweetSet = set1.filter(tw => apple.exists(tw.text.contains))
      val fooTweets: TweetSet = set1.filter(tw => foo.exists(tw.text.contains))
      val filteredTweets = googleTweets.filter(tw => tw.retweets == 25)
      val trends = (appleTweets union googleTweets).descendingByRetweet
      assert(size(googleTweets) == 10)
      assert(size(filteredTweets) == 2)
      assert(size(fooTweets) == 0)
      assert(trends.head.user === "x", "the most retweets is x")
      trends foreach println
    }
  }

  }
