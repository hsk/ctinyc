/*
 * c-tiny-c compiler
 */
package ctinyc

object Compiler extends Ast {

	private sealed class EnvKind()
	private case class EnvVarARG() extends EnvKind
	private case class EnvVarLocal() extends EnvKind
	
	private case class Environment(var vr:String, var vr_kind:EnvKind, var pos:Int)


	def compileProgram(prg:Program) {
		prg match {
		case null =>
		case Program(x) => compileExternalDefinitions(x)
		}
	}

	private def compileExternalDefinitions(l:List[ExternalDefinition]) {
		l match {
		case List() =>
		case DefineFunction(a,b,c)::xs => compileDefineFunction(a, b, c); compileExternalDefinitions(xs)
		case DeclareVariable(a,b)::xs => compileDeclareVariable(a, b); compileExternalDefinitions(xs)
		case DeclareArray(a,b)::xs => compileDeclareArray(a, b); compileExternalDefinitions(xs)
		}
	}

	private var env = List[Environment]()
	
	private var local_var_pos = 0
	private var tmp_counter = 0
	private def genTmp():Int = {
		val tmp = tmp_counter
		tmp_counter += 1
		tmp
	}
	private def compileStoreVar(vr:String, r:Int) {
		def t(env:List[Environment]) {
			env match {
			case List() => throw new Exception("undefined variable\n")
			case x::xs =>
				if (x.vr == vr) {
					x.vr_kind match {
					case EnvVarARG() => emit.genCode(STOREA(r, x.pos))
					case EnvVarLocal() => emit.genCode(STOREL(r, x.pos))
					}
				} else t(xs)
			}
		}
		t(env)
	}

	private def compileLoadVar(target:Int, vr:String) {
		def t(env:List[Environment]) {
			env match {
			case List() => throw new Exception("undefined variable")
			case x::xs =>
				if (x.vr == vr) {
					x.vr_kind match {
					case EnvVarARG() => emit.genCode(LOADA(target, x.pos))
					case EnvVarLocal() => emit.genCode(LOADL(target, x.pos))
					}
				} else t(xs)
			}
		}
		t(env)
	}

	private def compileDefineFunction(fsym:String, params:List[String], body:AST) {

		emit.initGenCode()
		local_var_pos = 0
		
		def addEnv(ps:List[String], param_pos:Int, env:List[Environment]):List[Environment] = {
			ps match {
			case List() => env
			case x::xs =>
				addEnv(
					xs,
					param_pos + 1,
					Environment(x, EnvVarARG(), param_pos)::env
				)
			}
		}
		env = addEnv(params, 0, List[Environment]())

		compileStatement(body)
		emit.genFuncCode(fsym, local_var_pos)
		env = List[Environment]() // reset
	}

	private def compileStatement(p:AST) {
	    p match {
	    case null =>
	    case BLOCK_STATEMENT(l, r) => compileBlock(l, r)
	    case RETURN_STATEMENT(l) => compileReturn(l)
	    case IF_STATEMENT(l, r1, r2) => compileIf(l, r1, r2)
	    case WHILE_STATEMENT(l, r) => compileWhile(l, r)
	    case FOR_STATEMENT(l1, l2, l3, r) => compileFor(l1, l2, l3, r)
		case _ => compileExpr(-1, p)
	    }
	}

	private def compileBlock(local_vars:List[String], statements:List[AST]) {
		val env_save = env
		
		def getEnv(lv:List[String], env:List[Environment]):List[Environment] = {
			lv match {
			case List() => env
			case x::xs =>
				val e = Environment(x, EnvVarLocal(), local_var_pos)::env
				local_var_pos += 1
				getEnv(xs, e)
			}
		}
		env = getEnv(local_vars, env)

		def stm(st:List[AST]) {
			st match {
			case List() =>
			case x::xs =>  compileStatement(x); stm(xs)
			}
		}
		stm(statements)

		env = env_save
	}

	private def compileReturn(expr:AST) {
		if (expr != null) {
			val r = genTmp()
			compileExpr(r, expr)
			emit.genCode(RET(r))
		} else {
			emit.genCode(RET(-1))
		}
	}

	private def compileCallFunc(target:Int, f:String, args:List[AST]) {
		val narg = compileArgs(args)
		emit.genCode(CALL(target, narg, f))
	}

	private def compileArgs(args:List[AST]):Int = {
		args match {
		case List() => 0
		case x::xs => 
			val n = compileArgs(xs)
			val r = genTmp()
			compileExpr(r, x)
			emit.genCode(ARG(r))
			n + 1
		}
	}

	private def compileIf(cond:AST, then_part:AST, else_part:AST) {
		val r = genTmp()
		compileExpr(r, cond)
		val l1 = emit.genLabel()
		emit.genCode(BEQ0(r, l1))
		compileStatement(then_part)
		if (else_part != null) {
			val l2 = emit.genLabel()
			emit.genCode(JUMP(l2))
			emit.genCode(LABEL(l1))
			compileStatement(else_part)
			emit.genCode(LABEL(l2))
		} else {
			emit.genCode(LABEL(l1))
		}
	}

	private def compileWhile(cond:AST, body:AST) {
		val l1 = emit.genLabel()
		val l2 = emit.genLabel()
		val r = genTmp()
		emit.genCode(LABEL(l1))
		compileExpr(r, cond)
		emit.genCode(BEQ0(r, l2))
		compileStatement(body)
		emit.genCode(JUMP(l1))
		emit.genCode(LABEL(l2))
	}

	private def compileFor(init:AST, cond:AST, iter:AST, body:AST) {
	    // not implemented
	}

	private def compileExpr(target:Int, p:AST) {
	    p match {
	    case null =>
	    case NUM(v) => emit.genCode(LOADI(target, v))
	    case SYM(s) => compileLoadVar(target, s);
	    case EQ_OP(l, r) =>
			if(target != -1) throw new Exception("assign has no value")
			val r1 = genTmp()
			compileExpr(r1, r)
			compileStoreVar(l, r1)
	    case PLUS_OP(l, r) =>
			val r1 = genTmp()
			val r2 = genTmp()
			compileExpr(r1, l)
			compileExpr(r2, r)
			emit.genCode(ADD(target, r1, r2))

	    case MINUS_OP(l, r) =>
			val r1 = genTmp()
			val r2 = genTmp()
			compileExpr(r1, l)
			compileExpr(r2, r)
			emit.genCode(SUB(target, r1, r2))

	    case MUL_OP(l, r) =>
			val r1 = genTmp()
			val r2 = genTmp()
			compileExpr(r1, l)
			compileExpr(r2, r)
			emit.genCode(MUL(target, r1, r2))

	    case LT_OP(l, r) =>
			val r1 = genTmp()
			val r2 = genTmp()
			compileExpr(r1, l)
			compileExpr(r2, r)
			emit.genCode(LT(target, r1, r2))

	    case GT_OP(l, r) =>
			val r1 = genTmp()
			val r2 = genTmp()
			compileExpr(r1, l)
			compileExpr(r2, r)
			emit.genCode(GT(target, r1, r2))

	    case CALL_OP(l, r) => compileCallFunc(target,l, r)
	    case PRINTLN_OP(l) =>
			if(target != -1) throw new Exception("println has no value")
			printFunc(l)

	    case GET_ARRAY_OP(_,_) =>
			// not implemented
	    case SET_ARRAY_OP(_,_,_) =>
			// not implemented
		case _ => throw new Exception("unknown operater/statement")
	    }
	}

	private def printFunc(args:List[AST]) {
		args match {
		case STR(a)::b::xs =>
			val l = emit.genString(a)
			val r = genTmp()
			compileExpr(r, b)
			emit.genCode(PRINTLN(r, l))
		case _ => throw new Exception("println param error" + args)
		}
	}

	/**
	 * global variable
	 */
	private def compileDeclareVariable(vsym:String, init_value:AST) {
		// not implemented
	}

	/**
	 * Array
	 */
	private def compileDeclareArray(a:String, size:AST) {
		// not implemented
	}
	
}
