package net.premereur.binaro

sealed trait Mark {

  def isKnown: Boolean

  def complement: Mark

  def &(position: Mark): Mark = if (this == position) this else UNKNOWN
}

case object ONE extends Mark {

  override def isKnown = true

  override def complement: Mark = ZERO

  override def toString: String = "1"
}

case object ZERO extends Mark {

  override def isKnown: Boolean = true

  override def complement: Mark = ONE

  override def toString: String = "0"
}

case object UNKNOWN extends Mark {

  override def isKnown(): Boolean = false

  override def complement: Mark = this

  override def toString: String = "_"
}

object Mark {
  def fromChar(char:Char) = char match {
    case '0' => ZERO
    case '1' => ONE
    case _ => UNKNOWN
  }
}