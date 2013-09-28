package net.travisdazell.parsers.model

class Factor

case class IntValue(value: Int) extends Factor

case class Identifier(name: String) extends Factor