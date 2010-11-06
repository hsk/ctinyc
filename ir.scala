package ctinyc

/*
 * register machine intermediate opcode
 */
abstract class IR()
case class LOADI(r:Int, n:Int) extends IR // load int
case class LOADA(r:Int, n:Int) extends IR // load arg
case class LOADL(r:Int, n:Int) extends IR // load local var
case class STOREA(r:Int, n:Int) extends IR // store arg
case class STOREL(r:Int, n:Int) extends IR // store local var
case class ADD(t:Int, r1:Int, r2:Int) extends IR
case class SUB(t:Int, r1:Int, r2:Int) extends IR
case class MUL(t:Int, r1:Int, r2:Int) extends IR
case class GT(t:Int, r1:Int, r2:Int) extends IR
case class LT(t:Int, r1:Int, r2:Int) extends IR
case class BEQ0(t:Int, l:Int) extends IR // branch if eq 0
case class JUMP(l:Int) extends IR
case class ARG(r:Int) extends IR // set Argument
case class CALL(t:Int, n:Int, f:String) extends IR
case class RET(r:Int) extends IR // return
case class PRINTLN(r:Int, l:Int) extends IR // println function
case class LABEL(l:Int) extends IR // label
/*
case class LOADS() extends IR  // load string label

case class LOADADDR() extends IR 
case class LOAD() extends IR 
case class STORE() extends IR 
*/
