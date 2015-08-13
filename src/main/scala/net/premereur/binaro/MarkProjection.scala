package net.premereur.binaro

/**
 * Defines a position in a projection. It may either be in its initial unprojected state, hold a defined mark or be undecided.
 */
sealed trait MarkProjection {
  def get: Mark

  def project(mark: DefinedMark): MarkProjection
}

object MarkProjection {
  def apply(mark: Mark) = mark match {
    case ZERO => ZERO_PROJECTION
    case ONE => ONE_PROJECTION
    case UNKNOWN => INITIAL
  }
}

case object INITIAL extends MarkProjection {
  override def project(mark: DefinedMark) = mark match {
    case ZERO => ZERO_PROJECTION
    case ONE => ONE_PROJECTION
  }

  override def get: Mark = UNKNOWN // Does not happen for well-formed puzzles, but might for invalid ones
}

case object ZERO_PROJECTION extends MarkProjection {
  override def project(mark: DefinedMark) = mark match {
    case ZERO => ZERO_PROJECTION
    case ONE => UNDECIDED_PROJECTION
  }

  override def get: Mark = ZERO
}

case object ONE_PROJECTION extends MarkProjection {
  override def project(mark: DefinedMark) = mark match {
    case ZERO => UNDECIDED_PROJECTION
    case ONE => ONE_PROJECTION
  }

  override def get: Mark = ONE
}

case object UNDECIDED_PROJECTION extends MarkProjection {
  override def project(mark: DefinedMark) = UNDECIDED_PROJECTION

  override def get: Mark = UNKNOWN
}