# Brainfuck 🧠 in Scala 
A simple Brainfuck interpreter written in Scala
- Use 
`Interpreter.run(bf code)` 
to get user inputs at runtime
- use `Interpreter.runWithInput(bf code, input string)`
to input the user input at parse time

### Example 1
Program that prints "hi"
- `Interpreter.run(">+++++++++++++[<++>-]<[>++++<-]>[>+<-]>[<+<+>>-]<+<.>.")`
- Output: `hi`


### Example 2
Program that adds two small numbers together here with input 3 and 4 it adds 3+4 = 7 it only works if sum is smaller than 10
- `Interpreter.runWithInput(",>,>++++++++>+++++[<++++++++>-]<[>+>+<<-]>[<<<->->>-]<<[<+>-]>>>[<<<<+>>>>-]<<<<.", "34")`
- Input: `34`
- Output: `7`
