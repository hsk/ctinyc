package ctinyc
import scala.collection.mutable.Map

object Ast extends Ast
class Ast {
	case class SymbolC(var name:String)

	case class Program(l:List[ExternalDefinition])

	sealed abstract class ExternalDefinition
	case class DeclareArray(sym:SymbolC,o:AST) extends ExternalDefinition
	case class DeclareVariable(sym:SymbolC,o:AST) extends ExternalDefinition
	case class DefineFunction(sym:SymbolC,o:List[AST], o2:AST) extends ExternalDefinition

	def list(p:ExternalDefinition):List[ExternalDefinition] = List(p)
	def addList(p:ExternalDefinition,l:List[ExternalDefinition]):List[ExternalDefinition] = p::l
	def reverse(l:List[ExternalDefinition]):List[ExternalDefinition] = l.reverse
	def program(l:List[ExternalDefinition]):Program = Program(l)
	sealed abstract class AST
	case class NUM(vl:Int) extends AST
	case class STR(vl:String) extends AST
	case class SYM(sym:SymbolC) extends AST
	case class EQ_OP(l:AST,r:AST) extends AST
	case class PLUS_OP(l:AST,r:AST) extends AST
	case class MINUS_OP(l:AST,r:AST) extends AST
	case class MUL_OP(l:AST,r:AST) extends AST
	case class LT_OP(l:AST,r:AST) extends AST
	case class GT_OP(l:AST,r:AST) extends AST
	case class GET_ARRAY_OP(l:AST,r:AST) extends AST
	case class SET_ARRAY_OP(l:AST,l2:AST, r:AST) extends AST
	case class CALL_OP(l:AST,r:List[AST]) extends AST
	case class PRINTLN_OP(l:List[AST]) extends AST
	case class IF_STATEMENT(l:AST,r1:AST, r2:AST) extends AST
	case class BLOCK_STATEMENT(l:List[AST], r:List[AST]) extends AST
	case class RETURN_STATEMENT(l:AST) extends AST
	case class WHILE_STATEMENT(l:AST,r:AST) extends AST
	case class FOR_STATEMENT(l1:AST, l2:AST, l3:AST, r:AST) extends AST

	val symbolTable = Map[String, SymbolC]()
	def makeSymbol(name:String):AST = {
		if (symbolTable.contains(name)) {
			SYM(symbolTable(name))
		}else {
			val sym = SymbolC(name)
			symbolTable += ( name -> sym )
			SYM(sym)
		}
	}
	def getSymbol(p:AST):SymbolC = {
		p match {
		case SYM(s) => s
		case _ => throw new Exception("bad access to symbol" + p)
		}
	}
	
}
