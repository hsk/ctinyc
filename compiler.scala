/*
 * c-tiny-c compiler
 */
package ctinyc
import scala.collection.mutable.Stack
import java.io.FileReader
import java.io.FileWriter
import java.io.PrintWriter
import java.io.BufferedWriter
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.IOException

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

object Compiler extends Ast {

	sealed class EnvKind()
	case class EnvVarARG() extends EnvKind
	case class EnvVarLocal() extends EnvKind
	
	case class Environment(var vr:String, var vr_kind:EnvKind, var pos:Int)


	def compileProgram(prg:Program) {
		val outchan = new FileWriter("output.s")
		asm.p = new PrintWriter(new BufferedWriter(outchan))
		prg match {
		case null =>
		case Program(p) => compileExternalDefinitions(p)
		}
		asm.p.close()
		gcc("output.s", "a.exe")
	}
	def gcc(file:String, out:String):Unit = {
		try {
			var rt = Runtime.getRuntime()
			var p = rt.exec("gcc -o "+out+" "+file+" println.c")
			var br = new BufferedReader(new InputStreamReader(p.getInputStream()))
			var result:String = ""
			while (result != null) {
				result = br.readLine()
				if(result != null) {
					System.out.println(result)
				}
			}
			br = new BufferedReader(new InputStreamReader(p.getErrorStream()))
			result = ""
			while (result != null) {
				result = br.readLine()
				if(result != null) {
					System.out.println(result)
				}
			}

			p.waitFor();
		} catch {
			case ex:IOException =>
				ex.printStackTrace()
		}
	}

	def compileExternalDefinitions(l:List[ExternalDefinition]) {
		l match {
		case List() =>
		case DefineFunction(a,b,c)::xs => compileDefineFunction(a, b, c); compileExternalDefinitions(xs)
		case DeclareVariable(a,b)::xs => compileDeclareVariable(a, b); compileExternalDefinitions(xs)
		case DeclareArray(a,b)::xs => compileDeclareArray(a, b); compileExternalDefinitions(xs)
		}
	}

	var env = List[Environment]()
	
	var label_counter = 0
	var local_var_pos = 0
	var tmp_counter = 0

	def compileStoreVar(vr:String, r:Int) {
		def t(env:List[Environment]) {
			env match {
			case List() => throw new Exception("undefined variable\n")
			case x::xs =>
				if (x.vr == vr) {
					x.vr_kind match {
					case EnvVarARG() => genCode(STOREA(r, x.pos))
					case EnvVarLocal() => genCode(STOREL(r, x.pos))
					}
				} else t(xs)
			}
		}
		t(env)
	}

	def compileLoadVar(target:Int, vr:String) {
		def t(env:List[Environment]) {
			env match {
			case List() => throw new Exception("undefined variable")
			case x::xs =>
				if (x.vr == vr) {
					x.vr_kind match {
					case EnvVarARG() => genCode(LOADA(target, x.pos))
					case EnvVarLocal() => genCode(LOADL(target, x.pos))
					}
				} else t(xs)
			}
		}
		t(env)
	}

	def compileDefineFunction(fsym:String, params:List[String], body:Ast#AST) {

		initGenCode()
		local_var_pos = 0
		
		def getEnv(ps:List[String], param_pos:Int, env:List[Environment]):List[Environment] = {
			ps match {
			case List() => env
			case x::xs =>
				getEnv(
					xs,
					param_pos + 1,
					Environment(x, EnvVarARG(), param_pos)::env
				)
			}
		}
		env = getEnv(params, 0, List[Environment]())

		compileStatement(body.asInstanceOf[AST])
		genFuncCode(fsym, local_var_pos)
		env = List[Environment]() // reset
	}

	def compileStatement(p:AST) {
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

	def compileBlock(local_vars:List[String], statements:List[AST]) {
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

	def compileReturn(expr:AST) {
		if (expr != null) {
			val r = tmp_counter
			tmp_counter += 1
			compileExpr(r, expr)
			genCode(RET(r))
		} else {
			genCode(RET(-1))
		}
	}

	def compileCallFunc(target:Int, f:String, args:List[AST]) {
		val narg = compileArgs(args)
		genCode(CALL(target, narg, f))
	}

	def compileArgs(args:List[AST]):Int = {
		args match {
		case List() => 0
		case x::xs => 
			val n = compileArgs(xs)
			val r = tmp_counter
			tmp_counter += 1
			compileExpr(r, x)
			genCode(ARG(r))
			n + 1
		}
	}

	def compileIf(cond:AST, then_part:AST, else_part:AST) {
		val r = tmp_counter
		tmp_counter += 1
		compileExpr(r, cond)
		val l1 = label_counter
		label_counter += 1
		genCode(BEQ0(r, l1))
		compileStatement(then_part)
		if (else_part != null) {
			val l2 = label_counter
			label_counter += 1
			genCode(JUMP(l2))
			genCode(LABEL(l1))
			compileStatement(else_part)
			genCode(LABEL(l2))
		} else {
			genCode(LABEL(l1))
		}
	}

	def compileWhile(cond:AST, body:AST) {
		val l1 = label_counter
		label_counter += 1
		val l2 = label_counter
		label_counter += 1
		val r = tmp_counter
		tmp_counter += 1

		genCode(LABEL(l1))
		compileExpr(r, cond)
		genCode(BEQ0(r, l2))
		compileStatement(body)
		genCode(JUMP(l1))
		genCode(LABEL(l2))
	}

	def compileFor(init:AST, cond:AST, iter:AST, body:AST) {
	    // not implemented
	}

	def compileExpr(target:Int, p:AST) {
	    p match {
	    case null =>
	    case NUM(v) => genCode(LOADI(target, v))
	    case SYM(s) => compileLoadVar(target, s);
	    case EQ_OP(l, r) =>
			if(target != -1) throw new Exception("assign has no value")
			val r1 = tmp_counter
			tmp_counter += 1
			compileExpr(r1, r)
			compileStoreVar(l, r1)
	    case PLUS_OP(l, r) =>
			val r1 = tmp_counter
			tmp_counter += 1
			val r2 = tmp_counter
			tmp_counter += 1
			compileExpr(r1, l)
			compileExpr(r2, r)
			genCode(ADD(target, r1, r2))

	    case MINUS_OP(l, r) =>
			val r1 = tmp_counter
			tmp_counter += 1
			val r2 = tmp_counter
			tmp_counter += 1
			compileExpr(r1, l)
			compileExpr(r2, r)
			genCode(SUB(target, r1, r2))

	    case MUL_OP(l, r) =>
			val r1 = tmp_counter
			tmp_counter += 1
			val r2 = tmp_counter
			tmp_counter += 1
			compileExpr(r1, l)
			compileExpr(r2, r)
			genCode(MUL(target, r1, r2))

	    case LT_OP(l, r) =>
			val r1 = tmp_counter
			tmp_counter += 1
			val r2 = tmp_counter
			tmp_counter += 1
			compileExpr(r1, l)
			compileExpr(r2, r)
			genCode(LT(target, r1, r2))

	    case GT_OP(l, r) =>
			val r1 = tmp_counter
			tmp_counter += 1
			val r2 = tmp_counter
			tmp_counter += 1
			compileExpr(r1, l)
			compileExpr(r2, r)
			genCode(GT(target, r1, r2))

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
			val l = genString(a)
			val r = tmp_counter
			tmp_counter += 1
			compileExpr(r, b)
			genCode(PRINTLN(r, l))
		case _ => throw new Exception("println param error" + args)
		}
	}

	/**
	 * global variable
	 */
	def compileDeclareVariable(vsym:String, init_value:Ast#AST) {
		// not implemented
	}

	/**
	 * Array
	 */
	def compileDeclareArray(a:String, size:Ast#AST) {
		// not implemented
	}

	var codes = new Stack[IR]()

	def initGenCode() {
		codes = new Stack[IR]()
	}

	def genCode(c:IR) {
		codes.push(c)
	}

	/* 
	 *  code generator for x86
	 */ 

	val N_REG = 4
	val N_SAVE = 4

	def TMP_OFF(i:Int):Int = -((i+1)+1)*4
	def LOCAL_VAR_OFF(i:Int):Int = -(N_SAVE+1+(i+1))*4
	def ARG_OFF(i:Int):Int = ((i)+2)*4

	val REG_AX = 0
	val REG_BX = 1
	val REG_CX = 2
	val REG_DX = 3

	val tmpRegName = Array("%eax", "%ebx", "%ecx", "%edx")
	val tmpRegState = Array(-1,-1,-1,-1)
	val tmpRegSave = Array(-1,-1,-1,-1)

	def initTmpReg() {
		for(i <- 0 until N_REG) {
			tmpRegState(i) = -1
		}
		for(i <- 0 until N_SAVE) {
			tmpRegSave(i) = -1
		}
	}

	/**
	 * getReg: get free register
	 */
	def getReg(r:Int):Int = {
		for (i <- 0 until N_REG) {
			if(tmpRegState(i) < 0){
				tmpRegState(i) = r
				return i
			}
		}
		throw new Exception("no temp reg")
	}

	/**
	 * assign r to reg
	 */
	def assignReg(r:Int, reg:Int) {
		if (tmpRegState(reg) == r) return
		saveReg(reg)
		tmpRegState(reg) = r
	}

	/**
	 * load r into reg
	 */
	def useReg(r:Int):Int = {
	    for (i <- 0 until N_REG) {
			if(tmpRegState(i) == r) return i
		}
		// not found in register, then restore from save area.
		for (i <- 0 until N_SAVE) {
			if (tmpRegSave(i) == r) {
				val rr = getReg(r)
				tmpRegSave(i) = -1
				// load into regsiter
				asm.movl(TMP_OFF(i) + "(%ebp)", tmpRegName(rr))
				return rr
			}
		}
		throw new Exception("reg is not found")
	}

	def freeReg(reg:Int) {
		tmpRegState(reg) = -1
	}

	def saveReg(reg:Int) {
		if(tmpRegState(reg) < 0) return
		for (i <- 0 until N_SAVE) {
			if (tmpRegSave(i) < 0) {
				asm.movl(tmpRegName(reg), TMP_OFF(reg) + "(%ebp)")
				tmpRegSave(i) = tmpRegState(reg)
				tmpRegState(reg) = -1
				return
			}
		}
		throw new Exception("no temp save")
	}

	def saveAllRegs() {
	    for(i <- 0 until N_REG) saveReg(i)
	}

	/*
	 * Code generation
	 */
//	extern int label_counter;

	def genFuncCode(entry_name:String, n_local:Int) {

		// function header
		asm._text()
		asm._align(4)
		asm._globl("_" + entry_name)
		// asm._type(entry_name, "@function")
		asm.label("_" + entry_name)
		
		asm.pushl("%ebp")
		asm.movl("%esp", "%ebp")

		val frame_size = -LOCAL_VAR_OFF(n_local)
		val ret_lab = label_counter
		label_counter += 1

		asm.subl("$" + frame_size, "%esp")
		asm.movl("%ebx", "-4(%ebp)")

		initTmpReg()

		codes foreach {
		case LOADI(opd1, opd2) =>
			if(opd1 >= 0) {
				val r = getReg(opd1)
				asm.movl("$" + opd2, tmpRegName(r))
			}
		case LOADA(opd1, opd2) =>	// load arg
		    if(opd1 >= 0) {
				val r = getReg(opd1)
				asm.movl(ARG_OFF(opd2) + "(%ebp)", tmpRegName(r))
			}
		case LOADL(opd1, opd2) =>	// load local
			if(opd1 >= 0) {
				val r = getReg(opd1)
				asm.movl(LOCAL_VAR_OFF(opd2) + "(%ebp)", tmpRegName(r))
			}
		case STOREA(opd1, opd2) =>	// store arg
			val r = useReg(opd1)
			freeReg(r)
			asm.movl(tmpRegName(r), ARG_OFF(opd2) + "(%ebp)")
		case STOREL(opd1, opd2) =>	// store local
			val r = useReg(opd1)
			freeReg(r)
			asm.movl(tmpRegName(r), LOCAL_VAR_OFF(opd2) + "(%ebp)")
		case BEQ0(opd1, opd2) => // conditional branch
			val r = useReg(opd1)
			freeReg(r)
			asm.cmpl("$0", tmpRegName(r))
			asm.je(".L" + opd2)
		case LABEL(opd1) =>
			asm.label(".L" + opd1)
		case JUMP(opd1) =>
			asm.jmp(".L" + opd1)

		case CALL(opd1, opd2, opds) =>
			saveAllRegs()
			asm.call("_" + opds)
			if (opd1 >= 0) {
				assignReg(opd1, REG_AX)
				asm.add("$" + (opd2 * 4), "%esp")
			}
		case ARG(opd1) =>
			val r = useReg(opd1)
			freeReg(r)
			asm.pushl(tmpRegName(r))
		case RET(opd1) =>
			val r = useReg(opd1)
			freeReg(r)
			if (r != REG_AX) {
				asm.movl(tmpRegName(r), "%eax")
			}
			asm.jmp(".L" + ret_lab)

		case ADD(opd1, opd2, opd3) =>
			val r1 = useReg(opd2)
			val r2 = useReg(opd3)
			freeReg(r1)
			freeReg(r2)
			if (opd1 >= 0) {
				assignReg(opd1,r1)
				asm.addl(tmpRegName(r2), tmpRegName(r1))
			}
		case SUB(opd1, opd2, opd3) =>
			val r1 = useReg(opd2)
			val r2 = useReg(opd3)
			freeReg(r1)
			freeReg(r2)
			if (opd1 >= 0) {
				assignReg(opd1, r1)
				asm.subl(tmpRegName(r2), tmpRegName(r1))
			}
		case MUL(opd1, opd2, opd3) =>
			val r1 = useReg(opd2)
			val r2 = useReg(opd3)
			freeReg(r1)
			freeReg(r2)
			if (opd1 >= 0) {
				assignReg(opd1, REG_AX)
				saveReg(REG_DX)
				if(r1 != REG_AX) {
					asm.movl(tmpRegName(r1), tmpRegName(REG_AX))
				}
				asm.imull(tmpRegName(r2), tmpRegName(REG_AX))
			}
		case LT(opd1, opd2, opd3) =>
			val r1 = useReg(opd2)
			val r2 = useReg(opd3)
			freeReg(r1)
			freeReg(r2)
			if (opd1 >= 0) {
				val r = getReg(opd1)
				val l1 = label_counter
				label_counter += 1
				val l2 = label_counter
				label_counter += 1
				asm.cmpl(tmpRegName(r2), tmpRegName(r1))
				asm.jl(".L" + l1)
				asm.movl("$0", tmpRegName(r))
				asm.jmp(".L" + l2)
				asm.label(".L" + l1)
				asm.movl("$1", tmpRegName(r))
				asm.label(".L" + l2)
			}
		case GT(opd1, opd2, opd3) =>
			val r1 = useReg(opd2)
			val r2 = useReg(opd3)
		    freeReg(r1)
		    freeReg(r2)
		    if (opd1 >= 0) {
				val r = getReg(opd1)
				val l1 = label_counter
				label_counter += 1
				val l2 = label_counter
				label_counter += 1

				asm.cmpl(tmpRegName(r2), tmpRegName(r1))
				asm.jg(".L" + l1)
				asm.movl("$0", tmpRegName(r))
				asm.jmp(".L" + l2)
				asm.label(".L" + l1)
				asm.movl("$1", tmpRegName(r))
				asm.label(".L" + l2)
			}
		case PRINTLN(opd1, opd2) =>
			val r = useReg(opd1); freeReg(r)
			asm.pushl(tmpRegName(r))
			asm.pushl("$.LC" + opd2)
			saveAllRegs()
			asm.call("_println")
			asm.addl("$8", "%esp")
		}

		// return sequence
		asm.label(".L" + ret_lab)
		asm.movl("-4(%ebp)", "%ebx")
		asm.leave()
		asm.ret()
	}

	def genString(s:String):Int = {
		val l = label_counter
		label_counter += 1
		asm._section(".rodata")
		asm.label(".LC" + l)
		asm._string(s)
		l
	}
}

object asm {
	var p:PrintWriter = null
	
	/**
	 * テキスト
	 */
	def _text() {
		p.println("\t.text")
	}
	/**
	 * アライン
	 */
	def _align(r1:Int) {
		p.println("\t.align\t" + r1)
	}
	/**
	 * グローバル
	 */
	def _globl(r1:String) {
		p.println("\t.globl\t"+ r1)
	}
	/**
	 * タイプ
	 */
	def _type(r1:String, r2:String) {
		p.println("\t.type\t" + r1 + "," + r2)
	}
	/**
	 * セクション
	 */
	def _section(r1:String) {
		p.println("\t.section\t" + r1)
	}
	/**
	 * 文字列
	 */
	def _string(r1:String) {
		p.println("\t.string \"" + r1 + "\"")
	}

	/**
	 * ラベル
	 */
	def label(r1:String) {
		p.println(r1 + ":")
	}
	/**
	 * スタックにPUSH
	 */
	def pushl(r1:String) {
		p.println("\tpushl\t" + r1)
	}
	/**
	 * 掛け算
	 */
	def movl(r1:String, r2:String) {
		p.println("\tmovl\t" + r1 + "," + r2)
	}
	/**
	 * 足し算
	 */
	def add(r1:String, r2:String) {
		p.println("\tadd " + r1 + "," + r2)
	}
	/**
	 * 書け算
	 */
	def imull(r1:String, r2:String) {
		p.println("\timull\t" + r1 + "," + r2)
	}
	/**
	 * 足し算
	 */
	def addl(r1:String, r2:String) {
		p.println("\taddl\t" + r1 + "," + r2)
	}
	/**
	 * 引き算
	 */
	def subl(r1:String, r2:String) {
		p.println("\tsubl\t" + r1 + "," + r2)
	}
	/**
	 * 比較
	 */
	def cmpl(r1:String, r2:String) {
		p.println("\tcmpl\t" + r1 + "," + r2)
	}
	/**
	 * equalだったらjump
	 */
	def je(r1:String) {
		p.println("\tje\t" + r1)
	}
	/**
	 * 小さかったらjump
	 */
	def jl(r1:String) {
		p.println("\tjl\t" + r1)
	}
	/**
	 * 大きかったらjump
	 */
	def jg(r1:String) {
		p.println("\tjg\t" + r1)
	}
	/**
	 * 無条件jump
	 */
	def jmp(r1:String) {
		p.println("\tjmp\t" + r1)
	}

	/**
	 * 関数呼び出し
	 */
	def call(opds:String) {
		p.println("\tcall\t" + opds)
	}

	/**
	 * 関数終了処理
	 */
	def leave() {
		p.println("\tleave")
	}

	/**
	 * 関数リターン
	 */
	def ret() {
		p.println("\tret")
	}
}

