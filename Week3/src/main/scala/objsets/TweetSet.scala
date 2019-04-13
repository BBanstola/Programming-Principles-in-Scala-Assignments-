package objsets


class Tweet(val user: String, val text : String, val retweets:Int){
  override def toString: String = {
    "User: " + user + "\n" + "Text: " +text + "[" + retweets + "]"
  }
}

abstract class TweetSet {

  def filter(p: Tweet => Boolean): TweetSet = {
    filterAcc(p, new Empty)
  }

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  def union(that: TweetSet): TweetSet

  def mostRetweeted: Tweet

  def isEmpty: Boolean

  def descendingByRetweet: TweetList

  def incl(tweet: Tweet): TweetSet

  def remove(tweet: Tweet): TweetSet

  def contains(tweet: Tweet): Boolean

  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {

  override def filterAcc(p: (Tweet) => Boolean, acc: TweetSet):TweetSet = acc

  override def union(that: TweetSet):TweetSet = that

  override def mostRetweeted = throw new java.util.NoSuchElementException("Most Retweeted on Empty")

  override def isEmpty = true

  override def descendingByRetweet: TweetList = Nil

  def contains(tweet: Tweet) = false

  def incl(tweet: Tweet):TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet):TweetSet = this

  def foreach(f: (Tweet) => Unit):Unit = ()
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet{

  override def mostRetweeted:Tweet = {
    val leftBiggest = if (!left.isEmpty) left.mostRetweeted else elem
    val rightBiggest = if (!right.isEmpty) right.mostRetweeted else elem
    val bigger = if (leftBiggest.retweets > rightBiggest.retweets) leftBiggest else rightBiggest
    if (elem.retweets > bigger.retweets) elem else bigger
  }

  override def isEmpty:Boolean = false

  override def descendingByRetweet:TweetList = {
    descendingByRetweetAcc(this, Nil)
  }

  def descendingByRetweetAcc(space: TweetSet, acc:TweetList):TweetList ={
    if (space.isEmpty) acc else {
      val biggest = space.mostRetweeted
      val newAcc = descendingByRetweetAcc(space.remove(biggest), acc)
      new Cons(biggest, newAcc)
    }
  }

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}

object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  def containsKeywords(tw: Tweet, keywords: List[String]): Boolean = {
    if (keywords.isEmpty) false else {
      if (tw.text.contains(keywords.head)) true else containsKeywords(tw, keywords.tail)
    }
  }

  def findTweets(allTweets: TweetSet, keywords: List[String]): TweetSet = {
    if (keywords.isEmpty) allTweets else {
      allTweets.filter(tw => containsKeywords(tw, keywords))
    }
  }

  lazy val googleTweets: TweetSet = findTweets(TweetReader.allTweets, google)
  lazy val appleTweets: TweetSet = findTweets(TweetReader.allTweets, apple)

  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet
}

object Main extends App {
  GoogleVsApple.trending foreach println
}


