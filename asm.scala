package ctinyc

import java.io.PrintWriter


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
