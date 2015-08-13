package net.premereur.binaro

sealed trait DefinedMark {
  def isKnown = true

  def matches(mark: DefinedMark): Boolean = mark == this
}

sealed trait Mark {
  def isKnown: Boolean

  def matches(mark: DefinedMark): Boolean
}

case object ONE extends Mark with DefinedMark {
  override def toString: String = "1"
}

case object ZERO extends Mark with DefinedMark {
  override def toString: String = "0"
}

case object UNKNOWN extends Mark {
  override def isKnown: Boolean = false

  override def matches(mark: DefinedMark): Boolean = true

  override def toString: String = "_"
}

object Mark {
  def fromChar(char: Char) = char match {
    case '0' => ZERO
    case '1' => ONE
    case _ => UNKNOWN
  }
}