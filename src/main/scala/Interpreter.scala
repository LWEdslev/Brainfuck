import scala.collection.mutable
import scala.io.StdIn.readLine

object Interpreter {
  /*
   Meaning:   Symbol:  Scala equiv:
   Increment  '+'      env(pointer) += 1
   Decrement  '-'      env(pointer) -= 1
   Previous   '<'      pointer -= 1
   Next       '>'      pointer += 1
   Output     '.'      print(env(pointer))
   Input      ','      readline.head.toChar
   Enter loop '['      while(env(pointer) != 0) {
   Exit loop  ']'      }
   ---                 //comment
*/
  def eval(tokens: String, input: String): String = {
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
        case '+' =>
          env(pointer) = (env(pointer) + 1).toByte //increment the current byte
        case '-' =>
          env(pointer) = (env(pointer) - 1).toByte //decrement the current byte
        case '<' =>
          pointer -= 1 //Move pointer one left
        case '>' =>
          pointer += 1 //Move pointer one right
        case '.' =>
          output.append(env(pointer).toChar) //Adds byte to output
        case ',' =>
          env(pointer) = if (input.isEmpty) { //if there is no input string
            println("Write user input:")
            val userInput = readLine().head.toByte //we get user input at runtime
            userInput
          } else {
            inputStack.pop.toByte //else we get next char from input string
          }
        case '[' =>
          if (env(pointer) != 0) { //The loop is only entered if we are pointing at 0
            scopeStack.push(tokenPointer) //Enters scope
          } else {
            //We need to find the next corresponding ]
            var enterLoops = 0
              //We have to count how many [ we find along the way so we don't find the wrong ]
              //example: [[]] the first [ belongs to the last ] and the in between should be skipped
            tokenPointer += 1 //to get one beyond the initial [
            while (tokens(tokenPointer) != ']' || enterLoops > 0) { //we skip until we find the corresponding ]
              tokens(tokenPointer) match
                case '[' => enterLoops += 1 //enter a new loop scope
                case ']' => enterLoops -= 1 //exit a loop scope
                case _ => //continue
              tokenPointer += 1 //look at the next token
            }
          }
        case ']' =>
          if (env(pointer) != 0)      //If current byte is not 0 we go back to start of scope
            tokenPointer = scopeStack.head
          else                        //Else we exit scope
            scopeStack.pop()
        case _ => //Do nothing
      tokenPointer += 1 //Increments the pointer to get to the next token
    }
    output.mkString //Returns the output of the program
  }

  def run(code: String): Unit = println(eval(code, ""))

  def runWithInput(code: String, input: String): Unit = println(eval(code, input))

}

