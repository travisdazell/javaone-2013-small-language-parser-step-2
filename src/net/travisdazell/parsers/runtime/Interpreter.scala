package net.travisdazell.parsers.runtime

import net.travisdazell.parsers.model.IfStatement
import net.travisdazell.parsers.model.LoopStatement
import net.travisdazell.parsers.model.VariableDefinition
import net.travisdazell.parsers.model.PrintStatement
import net.travisdazell.parsers.model.Statement

class Interpreter(tree: List[Statement]) {
	def run() {
	  walk(tree)
	}
	
	private def walk(tree: List[Statement]) {
	  if (!tree.isEmpty) {
		  tree.head match {
		    case VariableDefinition(name, value) => {
		      walk(tree.tail)
		    }
		    case PrintStatement(value) => {
		      println(value)
		      walk(tree.tail)
		    }
		    case LoopStatement(iterations, statements) => {
		      for (i <- 0 until iterations) walk(statements)

		      walk(tree.tail)
		    }
		    case IfStatement(condition, trueBranch, falseBranch) => {
		      if (condition) walk(trueBranch) else walk(falseBranch)
		      
		      walk(tree.tail)
		    }
		    case _ => ()
		  }
	  }
	}
}