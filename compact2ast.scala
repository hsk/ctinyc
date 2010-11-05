/*
 * compact to ast translator
 */
package ctinyc
import java.util.Scanner
import java.io.File

object c extends Ast {
	def main(args:Array[String]) {
		val src = new Scanner(new File(args(0))).useDelimiter("\\Z").next()

		val st = compact.main.read(src)
		println(st)
		val ast = program(st)
		println(ast)
		Compiler.compileProgram(ast.asInstanceOf[Compiler.Program])
	}

	def program(st:Any):Program = {
		Program(externalDefinitions(st))
	}

	def externalDefinitions(st:Any):List[ExternalDefinition] = {
		st match {
		case ( x, "@", xs) => externalDefinition(x) :: externalDefinitions(xs)
		case x => List(externalDefinition(x))
		}
	}

	def externalDefinition(st:Any):ExternalDefinition = {
		st match {
		case ("def",(a,"=",("fun","(",p,")", b))) => DefineFunction(symbol(a), parameterList(p), block(b))
		case ("var",(a,"=",b)) => DeclareVariable(symbol(a), expr(b))
		case ("var", (a,"[",b,"]")) => DeclareArray(symbol(a), expr(b))
		case ("var", a) => DeclareVariable(symbol(a),null)
		case _ => throw new Exception("syntax error")
		}
	}

	def symbol(st:Any):String = {
		st match {
		case a:String => a
		case _ => throw new Exception("syntax error")
		}
	}

	def parameterList(st:Any):List[String] = {
		symbolList(st)
	}

	def block(st:Any):BLOCK_STATEMENT = {
		st match {
		case ("{", l, "}") =>
			var (l1, st2) = localVars(l)
			var l2 = statements(st2)
			BLOCK_STATEMENT(l1, l2)
		case _ => throw new Exception("syntax error")
		}
	}

	def localVars(st:Any):(List[String], Any) = {
		st match {
		case (("var", s),"@", b) => (symbolList(s), b)
		case x => (List(), x)
		}
	}

	def symbolList(st:Any):List[String] = {
		st match {
		case a:String => List(a)
		case (a, ",", b) => symbol(a) :: symbolList(b)
		case _ => throw new Exception("syntax error")
		}
	}

	def statements(st:Any):List[AST] = {
		st match {
		case (a, "@", b) => statement(a)::statements(b)
		case a => List(statement(a))
		}
	}

	def statement(st:Any):AST = {
		st match {
		case ("{", a, "}") => block(st)
		case ("if", "(", a, ")", (b, "else", c)) => IF_STATEMENT(expr(a), block(b), block(c))
		case ("if", "(", a, ")", b) => IF_STATEMENT(expr(a), block(b), null)
		case ("return", ";") => RETURN_STATEMENT(null)
		case ("return", a) => RETURN_STATEMENT(expr(a))
		case ("while", "(", a, ")", b) => WHILE_STATEMENT(expr(a), block(b))
		case ("for", "(", a, ")", b1) =>
			a match {
			case (((a,";"),"@",(b,";")),"@",c) => FOR_STATEMENT(expr(a),expr(b),expr(c),block(b1))
			}
		case (a,";") => statement(a)
		case a => expr(a)
		}
	}
	
	def expr(st:Any):AST = {
		st match {
		case ((a,"[",b,"]"),"=",c) => SET_ARRAY_OP(symbol(a),expr(b),expr(c))
		case (a,"=",b) => EQ_OP(symbol(a),expr(b))
		case (a,"+",b) => PLUS_OP(expr(a),expr(b))
		case (a,"-",b) => MINUS_OP(expr(a),expr(b))
		case (a,"*",b) => MUL_OP(expr(a),expr(b))
		case (a,"<",b) => LT_OP(expr(a),expr(b))
		case (a,">",b) => GT_OP(expr(a),expr(b))
		case "void"=> null
		case compact.Str(a) => STR(a)
		case a:String => SYM(symbol(a))
		case a:Int => NUM(a)
		case (a,"[",b,"]") => GET_ARRAY_OP(symbol(a), expr(b))
		case ("println","(",b,")") => PRINTLN_OP(argList(b))
		case (a,"(","void",")") => CALL_OP(symbol(a), null)
		case (a,"(",b,")") => CALL_OP(symbol(a), argList(b))
		case (a,"@",b) => statement(a); statement(b)
		}
	}
	
	def argList(st:Any):List[AST] = {
		st match {
		case (a,",",b) => argList(a) ::: List(expr(b))
		case a => List(expr(a))
		}
	}

}
