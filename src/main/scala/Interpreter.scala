import scala.collection.mutable
import scala.io.StdIn.readLine

object Interpreter {
  enum Token {      //Meaning:   Symbol:  Scala equiv:
    case Inc        //Increment  '+'      env(pointer) += 1
    case Dec        //Decrement  '-'      env(pointer) -= 1
    case Prev       //Previous   '<'      pointer -= 1
    case Next       //Next       '>'      pointer += 1
    case Out        //Output     '.'      print(env(pointer))
    case In         //Input      ','      readline.head.toChar
    case L_Box      //Enter loop '['      while(env(pointer) != 0) {
    case R_Box      //Exit loop  ']'      }
    case Whitespace //---                 //comment
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

  def eval(tokens: Array[Token], input: String): String = {
    val inputStack = mutable.Stack().pushAll(input.reverse) //Makes the inputs into a stack where first input char is on top
    val output: mutable.StringBuilder = mutable.StringBuilder() //Builds the output
    var tokenPointer = 0 //Points to the current token in the token array
    val scopeStack: mutable.Stack[Int] = mutable.Stack()
    //Contains indices of the L_Box tokens the top will be the innermost scope currently
    var pointer = 0 //Points to the current byte in env
    val ENV_SIZE = 3000 //Bytes in env
    val env: Array[Byte] = Array.fill(ENV_SIZE)(0.toByte) //Contains the memory of the program
    while (tokenPointer < tokens.length) { //Runs until reached end of tokens
      tokens(tokenPointer) match
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
          env(pointer) = if (input.isEmpty) { //if there is no input string
            println("Write user input:")
            val userInput = readLine().head.toByte //we get user input at runtime
            userInput
          } else {
            inputStack.pop.toByte //else we get next char from input string
          }
        case Token.L_Box =>
          if (env(pointer) != 0) { //The loop is only entered if we are pointing at 0
            scopeStack.push(tokenPointer) //Enters scope
          } else {
            //We need to find the next corresponding ]
            var enterLoops = 0
              //We have to count how many [ we find along the way so we don't find the wrong ]
              //example: [[]] the first [ belongs to the last ] and the in between should be skipped
            tokenPointer += 1 //to get one beyond the initial [
            while (tokens(tokenPointer) != Token.R_Box || enterLoops > 0) { //we skip until we find the corresponding ]
              tokens(tokenPointer) match
                case Token.L_Box => enterLoops += 1 //enter a new loop scope
                case Token.R_Box => enterLoops -= 1 //exit a loop scope
                case _ => //continue
              tokenPointer += 1 //look at the next token
            }
          }
        case Token.R_Box =>
          if (env(pointer) != 0)      //If current byte is not 0 we go back to start of scope
            tokenPointer = scopeStack.head
          else                        //Else we exit scope
            scopeStack.pop()
        case Token.Whitespace => //Do nothing
      tokenPointer += 1 //Increments the pointer to get to the next token
    }
    output.mkString //Returns the output of the program
  }

  def run(code: String): Unit = println(eval(lexer(code), ""))

  def runWithInput(code: String, input: String): Unit = println(eval(lexer(code), input))

}

