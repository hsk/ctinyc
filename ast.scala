package ctinyc

object Ast extends Ast
class Ast {

	case class Program(l:List[ExternalDefinition])

	sealed abstract class ExternalDefinition
	case class DeclareArray(sym:String,o:AST) extends ExternalDefinition
	case class DeclareVariable(sym:String,o:AST) extends ExternalDefinition
	case class DefineFunction(sym:String,o:List[String], o2:AST) extends ExternalDefinition

	sealed abstract class AST
	case class NUM(vl:Int) extends AST
	case class STR(vl:String) extends AST
	case class SYM(sym:String) extends AST
	case class EQ_OP(l:String,r:AST) extends AST
	case class PLUS_OP(l:AST,r:AST) extends AST
	case class MINUS_OP(l:AST,r:AST) extends AST
	case class MUL_OP(l:AST,r:AST) extends AST
	case class LT_OP(l:AST,r:AST) extends AST
	case class GT_OP(l:AST,r:AST) extends AST
	case class GET_ARRAY_OP(l:String, r:AST) extends AST
	case class SET_ARRAY_OP(l:String, l2:AST, r:AST) extends AST
	case class CALL_OP(l:String, r:List[AST]) extends AST
	case class PRINTLN_OP(l:List[AST]) extends AST
	case class IF_STATEMENT(l:AST,r1:AST, r2:AST) extends AST
	case class BLOCK_STATEMENT(l:List[String], r:List[AST]) extends AST
	case class RETURN_STATEMENT(l:AST) extends AST
	case class WHILE_STATEMENT(l:AST,r:AST) extends AST
	case class FOR_STATEMENT(l1:AST, l2:AST, l3:AST, r:AST) extends AST

}
