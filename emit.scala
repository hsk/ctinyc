package ctinyc
import scala.collection.mutable.Stack

object emit {
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
	var label_counter = 0
	def genLabel():Int = {
		val l = label_counter
		label_counter += 1
		l
	}
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
		val ret_lab = genLabel()
		
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
				val l1 = genLabel()
				val l2 = genLabel()
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
				val l1 = genLabel()
				val l2 = genLabel()

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
		val l = genLabel()
		asm._section(".rodata")
		asm.label(".LC" + l)
		asm._string(s)
		l
	}
}
