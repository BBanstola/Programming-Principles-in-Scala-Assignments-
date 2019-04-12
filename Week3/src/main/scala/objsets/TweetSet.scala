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

}


