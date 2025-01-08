package leetcode

import scala.collection.mutable

object _20_ValidParentheses extends App {

  def isValid(s: String): Boolean = {
    if (s.length == 1) false
    else {
      var i = 0
      val stack = mutable.Stack.empty[Char]
      var _isValid = true
      while (_isValid && i <= s.length - 1) {
        s.charAt(i) match {
          case '{' => stack.push('{')
          case '[' => stack.push('[')
          case '(' => stack.push('(')
          case '}' =>
            if (stack.nonEmpty) {
              val pop = stack.pop()
              _isValid = pop == '{'
            } else _isValid = false
          case ']' =>
            if (stack.nonEmpty) {
              val pop = stack.pop()
              _isValid = pop == '['
            } else _isValid = false
          case ')' =>
            if (stack.nonEmpty) {
              val pop = stack.pop()
              _isValid = pop == '('
            } else _isValid = false

        }
        i += 1
      }
      _isValid && stack.isEmpty
    }
  }

  println(isValid("()[]{}"))
  println(isValid("()"))
  println(isValid("(]"))
  println(isValid("([)]"))
  println(isValid("("))
  println(isValid("(("))

}
