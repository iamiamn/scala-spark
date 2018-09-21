package objsets

import TweetReader._

/**
  * A class to represent tweets.
  */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"

  def mentions(keywords: List[String]): Boolean = keywords.exists(x => text.contains(x))
}

/**
  * This represents a set of objects of type `Tweet` in the form of a binary search
  * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
  * invariant which always holds: for every branch `b`, all elements in the left
  * subtree are smaller than the tweet at `b`. The elements in the right subtree are
  * larger.
  *
  * Note that the above structure requires us to be able to compare two tweets (we
  * need to be able to say which of two tweets is larger, or if they are equal). In
  * this implementation, the equality / order of tweets is based on the tweet's text
  * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
  * text from different users.
  *
  *
  * The advantage of representing sets as binary search trees is that the elements
  * of the set can be found quickly. If you want to learn more you can take a look
  * at the Wikipedia page [1], but this is not necessary in order to solve this
  * assignment.
  *
  * [1] http://en.wikipedia.org/wiki/Binary_search_tree
  */
abstract class TweetSet {

  /**
    * This method takes a predicate and returns a subset of all the elements
    * in the original set for which the predicate is true.
    *
    * Question: Can we implement this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)
  //传入一个p, p为一个函数tweet=>boolean

  /**
    * This is a helper method for `filter` that propagates the accumulated tweets.
    */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
    * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
    *
    * Question: Should we implement this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  def union(that: TweetSet): TweetSet

  def isEmpty: Boolean

  /**
    * Returns the tweet from this set which has the greatest retweet count.
    *
    * Calling `mostRetweeted` on an empty set should throw an exception of
    * type `java.util.NoSuchElementException`.
    *
    * Question: Should we implement this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  def mostRetweeted: Tweet

  /**
    * Returns a list containing all tweets of this set, sorted by retweet count
    * in descending order. In other words, the head of the resulting list should
    * have the highest retweet count.
    *
    * Hint: the method `remove` on TweetSet will be very useful.
    * Question: Should we implement this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  def descendingByRetweet: TweetList = {
    def loop(in: TweetSet, ts: TweetList): TweetList = {//当tweetSet 为empty，返回传入的ts，一般为nil,一层层往上建cons
      if (in.isEmpty) ts
      else {val mostPopularTweet: Tweet = in.mostRetweeted//创建值，然后把当前最频繁的tweet加入到cons的开头
        new Cons(mostPopularTweet, loop(in.remove(mostPopularTweet), ts))
      }
    }
    loop(this, Nil)
  }

  def mentions(keywords: List[String]): TweetSet = filter(x => x.mentions(keywords))

  /**
    * The following methods are already implemented
    */

  /**
    * Returns a new `TweetSet` which contains all elements of this set, and the
    * the new element `tweet` in case it does not already exist in this set.
    *
    * If `this.contains(tweet)`, the current set is returned.
    */
  def incl(tweet: Tweet): TweetSet

  /**
    * Returns a new `TweetSet` which excludes `tweet`.
    */
  def remove(tweet: Tweet): TweetSet

  /**
    * Tests if `tweet` exists in this `TweetSet`.
    */
  def contains(tweet: Tweet): Boolean

  /**
    * This method takes a function and applies it to every element in the set.
    */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = new Empty//？？？为什么返回empty，new得到的是object

  def union(that: TweetSet): TweetSet = that

  def mostRetweeted: Tweet = throw new NoSuchElementException

  def isEmpty = true

  /**
    * The following methods are already implemented
    */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    //树的每个节点有elem:Int, 还有左右两个tweetSet,
    //filterAcc对每个elem进行检验，假设是最底层的elemLowest，下联两个Empty，l和r都是new Empty
    //union后变为noEmpty, 如果elem不符合p，返回为一个Empty，其实可以先做p(elem）判定，然后判定empty，减少new Empty操作
    //但是仔细想每次都判断，而且很多情况下会是单个empty和noEmpty组成两个子树

    val l = left.filterAcc(p, acc)
    val r = right.filterAcc(p, acc)
    val lr = l union r
    if (p(elem)) lr.incl(elem) else lr
  }

  def union(that: TweetSet): TweetSet = right.union(left.union(that.incl(elem)))

  def isEmpty = false

  def mostRetweeted: Tweet = {
    val all = right.union(left)
    val morePopular = all.filter(p => p.retweets > elem.retweets)
    if (morePopular.isEmpty) elem else morePopular.mostRetweeted
  }

  /**
    * The following methods are already implemented
    */

  def contains(x: Tweet): Boolean =
  if (x.text < elem.text) left.contains(x)
  else if (elem.text < x.text) right.contains(x)
  else true

  def incl(x: Tweet): TweetSet = {
    //只在nonEmpty的filterAcc和union函数中用到
    //是在本身为一颗完整树的情况下，构建一颗新的包含x的书
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)//每次incl后其实是构造出新的分支出来啊
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)//可以利用remove来把重复的枝节删除了
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

  lazy val googleTweets: TweetSet = TweetReader.allTweets.mentions(google)
  lazy val appleTweets: TweetSet = TweetReader.allTweets.mentions(apple)

  /**
    * A list of all tweets mentioning a keyword from either apple or google,
    * sorted by the number of retweets.
    */
  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}