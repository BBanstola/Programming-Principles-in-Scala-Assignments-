import scala.annotation.tailrec

/* Write a recursive function which verifies the balancing of parentheses in a string */


def balance(str: List[Char]): Boolean = {
  def traverse(count: Int, str: List[Char]): Boolean = {
    if (str.isEmpty && count == 0) true
    else if (str.isEmpty && count != 0) false
    else {
      if (str.head == '(')
        traverse(count + 1, str.tail)
      else {
        if (str.head == ')') {
          if (count - 1 < 0) false
          else traverse(count - 1, str.tail) }
        else traverse(count, str.tail) }
    } }
  traverse(0, str) }
