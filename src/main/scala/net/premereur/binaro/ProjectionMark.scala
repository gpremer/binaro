package net.premereur.binaro

/**
 * Defines a position in a projection. It may either be in its initial unprojected state, hold a mark or still be undecided.
 */
sealed trait ProjectionMark {
  def get: Mark

  def project(mark: Mark): ProjectionMark
}

object ProjectionMark {
  def apply(mark: Mark) = mark match {
    case ZERO => ZERO_PROJECTION
    case ONE => ONE_PROJECTION
    case UNKNOWN => INITIAL
  }
}

case object INITIAL extends ProjectionMark {
  override def project(mark: Mark) = mark match {
    case ZERO => ZERO_PROJECTION
    case ONE => ONE_PROJECTION
    case UNKNOWN => UNDECIDED_PROJECTION // can't happen, so we should get rid of it
  }

  override def get: Mark = UNKNOWN // Can this happen? Think!
}

case object ZERO_PROJECTION extends ProjectionMark {
  override def project(mark: Mark) = mark match {
    case ZERO => ZERO_PROJECTION
    case ONE => UNDECIDED_PROJECTION
    case UNKNOWN => ZERO_PROJECTION // can't happen, so we should get rid of it
  }

  override def get: Mark = ZERO
}

case object ONE_PROJECTION extends ProjectionMark {
  override def project(mark: Mark) = mark match {
    case ZERO => UNDECIDED_PROJECTION
    case ONE => ONE_PROJECTION
    case UNKNOWN => ONE_PROJECTION // can't happen, so we should get rid of it
  }

  override def get: Mark = ONE
}

case object UNDECIDED_PROJECTION extends ProjectionMark {
  override def project(mark: Mark) = UNDECIDED_PROJECTION

  override def get: Mark = UNKNOWN
}