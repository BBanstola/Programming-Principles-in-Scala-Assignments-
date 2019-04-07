/*  Write a recursive function that counts how many different ways you can make change
    for an amount, given a list of coin denominations. */

def count(amount: Int, denom : List[Int]):Int={
  def coinCount(amount: Int, denom: List[Int]):Int={
    if (amount <= 0 || denom.isEmpty) 0
    else {
      if (amount == denom.head) 1
      else {
        count(amount - denom.head, denom) + count(amount, denom.tail)
      }
    }
  }
  coinCount(amount, denom.sorted)
}