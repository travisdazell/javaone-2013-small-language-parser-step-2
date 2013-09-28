package net.travisdazell.parsers.model

case class IfStatement(condition: Boolean, trueBranch: List[Statement], falseBranch: List[Statement]) extends Statement