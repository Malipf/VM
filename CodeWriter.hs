module CodeWriter where
import Parser (arg1)

toStack :: String
toStack = "@SP\n\
	  \A=M\n\
	  \M=D\n\
	  \@SP\n\
	  \M=M+1"

fromStack :: String
fromStack = "@SP\n\
	    \AM=M-1\n\
	    \D=M"

preop :: String
preop = "@SP\n\
	\D=M"

writeArithmetic :: String -> String
writeArithmetic = undefined

writePopPush :: String -> String
writePopPush line = case arg1 line of 
  "local" -> undefined


{-

// add D to stack and increase = D>SP+
@SP
A=M
M=D
@SP
M=M+1

// remove from stack to D = SP>D-
@SP
AM=M-1
D=M

// needed before op = PREOP
@SP
A=M-1




// push constant i
@i
D=A
\ D>SP+

// pop seg4 i  (i > 6)
@SEG
D=M
@i
D=D+A
@R13
M=D
\ SP>D-
@R13
A=M
M=D

// pop seg4 0
\ SP>D-
@SEG
A=M
M=D

// pop seg4 i (1-6)
\ SP>D-
@SEG
A=M+1
A=A+1 * (i-1)
M=D

// push seg4 i (i > 3)
@SEG
D=M
@i
A=D+A
D=M
\ D>SP+

// push seg4 0
@SEG
D=M
\ D>SP+

// push seg4 1
@SEG
D=M+1
\ D>SP+

// push seg4 2
@SEG
D=M+1
D=D+1
\ D>SP+

// push seg4 3
@SEG
D=M+1
D=D+1
D=D+1
\ D>SP+

// pop static i
\ SP>D-
@filename.i
M=D

// push static i
@filename.i
D=M
\ D>SP+

// pop temp i
\ SP>D-
@5+i
M=D

// push temp i
@5+i
D=M
\ D>SP+

// pop pointer 0
\ SP>D-
@THIS
M=D

// push pointer 0
@THIS
D=M
\ D>SP+

// pop pointer 1
\ SP>D-
@THAT
M=D

// push pointer 1
@THAT
D=M
\ D>SP+

// add
\ SP>D-
\ PREOP
M=D+M

// sub
\ SP>D-
\ PREOP
M=M-D

// neg
\ PREOP
M=-M

// and
\ SP>D-
\ PREOP
M=D&M

// or
\ SP>D-
\ PREOP
M=D|M

// not
\ PREOP
M=!M

// eq
\ SP>D-
\ PREOP
D=M-D
@JUMP_cj
D;JEQ
\ PREOP
M=-1
@END_ce
0;JMP
(JUMP_cj)
\ PREOP
M=0
(END_ce)

// lt
\ SP>D-
\ PREOP
D=M-D
@JUMP_cj
D;JLT
\ PREOP
M=-1
@END_ce
0;JMP
(JUMP_cj)
\ PREOP
M=0
(END_ce)

// gt
\ SP>D-
\ PREOP
D=M-D
@JUMP_cj
D;JGT
\ PREOP
M=-1
@END_ce
0;JMP
(JUMP_cj)
\ PREOP
M=0
(END_ce)

-}
