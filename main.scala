package ctinyc
import java.util.Scanner
import java.io.File
import java.io.FileReader
import java.io.FileWriter
import java.io.PrintWriter
import java.io.BufferedWriter
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.IOException

object c {
	def main(args:Array[String]) {
		val src = new Scanner(new File(args(0))).useDelimiter("\\Z").next()

		val st = compact.main.read(src)
		println(st)
		val ast = compact2ast.program(st)
		println(ast)

		val outchan = new FileWriter("output.s")

		asm.p = new PrintWriter(new BufferedWriter(outchan))

		Compiler.compileProgram(
			ast.asInstanceOf[Compiler.Program]
		)
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

}
