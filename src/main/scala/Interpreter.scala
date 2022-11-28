import scala.collection.mutable
import scala.io.StdIn.readLine

object Interpreter {
  enum Token {
    case Inc
    case Dec
    case Prev
    case Next
    case Out
    case In
    case L_Box
    case R_Box
    case Whitespace
  }

  //Takes a character and match it into a token
  def lexChar(c: Char): Token = c match {
    case '+' => Token.Inc
    case '-' => Token.Dec
    case '<' => Token.Prev
    case '>' => Token.Next
    case '.' => Token.Out
    case ',' => Token.In
    case '[' => Token.L_Box
    case ']' => Token.R_Box
    case _ =>   Token.Whitespace
  }

  //Takes the code and makes it into corresponding tokens
  def lexer(code: String): Array[Token] = code.foldLeft(Array(): Array[Token])((acc, curr) => acc :+ lexChar(curr))

  def eval(e: Array[Token], input: String): String = {
    val inputStack = mutable.Stack().pushAll(input.reverse) //Makes the inputs into a stack where first input char is on top
    val output: mutable.StringBuilder = mutable.StringBuilder() //Builds the output
    var expPointer = 0 //Points to the current token in the token array
    val scopeStack: mutable.Stack[Int] = mutable.Stack()
    //Contains indices of the L_Box tokens the top will be the innermost scope currently
    var pointer = 0 //Points to the current byte in env
    val ENV_SIZE = 3000 //Bytes in env
    val env: Array[Byte] = Array.fill(ENV_SIZE)(0.toByte) //Contains the memory of the program
    while (expPointer < e.length) { //Runs until reached end of tokens
      val exp = e(expPointer) //current token at expPointer
      exp match
        case Token.Inc =>
          env(pointer) = (env(pointer) + 1).toByte //increment the current byte
        case Token.Dec =>
          env(pointer) = (env(pointer) - 1).toByte //decrement the current byte
        case Token.Prev =>
          pointer -= 1 //Move pointer one left
        case Token.Next =>
          pointer += 1 //Move pointer one right
        case Token.Out =>
          output.append(env(pointer).toChar) //Adds byte to output
        case Token.In =>
          env(pointer) = if (input.isEmpty) {
            println("Write user input:")
            val userInput = readLine().head.toByte
            userInput
          } else {
            inputStack.pop.toByte
          }
        case Token.L_Box =>
          scopeStack.push(expPointer) //Enters scope
        case Token.R_Box =>
          if (env(pointer) != 0)      //If current byte is not 0 we go back to start of scope
            expPointer = scopeStack.head
          else                        //Else we exit scope
            scopeStack.pop()
        case Token.Whitespace => //Do nothing
      expPointer += 1 //Increments the pointer to get to the next token
    }
    output.toString //Returns the output of the program
  }

  def run(code: String): Unit = println(eval(lexer(code), ""))

  def runWithInput(code: String, input: String): Unit = println(eval(lexer(code), input))

}

