package net.premereur.binaro

import net.premereur.binaro.Binaro.{Segments, SegmentView, Board}

import scala.annotation.tailrec

/**
 * Solves Binaro puzzles. It does this by repeatedly filling positions in a non-completed board until it is completed.
 * Unless of course the puzzle is invalid an a None is returned.
 * No guessing is used, just simple application of the rules. The one trick is that to find which positions to set
 * it does not rely on simple (derived) rules such as "00??" => "0011" but uses those marks that are the same in for all
 * possible valid rows/columns matching the row/column being considered.
 */
object BinaroSolver {
  type DefinedMarkSegment = IndexedSeq[DefinedMark]
  type DefinedMarkSegments = IndexedSeq[DefinedMarkSegment]

  def solve(board: Board): Option[Board] = {
    def allValidSegments(length: Int): DefinedMarkSegments = {

      def allSegments(numZeroesLeft: Int, numOnesLeft: Int,
                      lastMark: Option[DefinedMark], nextToLastMark: Option[DefinedMark],
                      marks: DefinedMarkSegment,
                      segments: DefinedMarkSegments): DefinedMarkSegments = {
        def zeroAllowed = numZeroesLeft > 0 && (nextToLastMark.getOrElse(ONE) != ZERO || lastMark.getOrElse(ONE) != ZERO)
        def oneAllowed = numOnesLeft > 0 && (nextToLastMark.getOrElse(ZERO) != ONE || lastMark.getOrElse(ZERO) != ONE)

        if (numZeroesLeft == 0 && numOnesLeft == 0) {
          segments :+ marks
        } else {
          if (zeroAllowed) {
            val segs = allSegments(numZeroesLeft - 1, numOnesLeft, Some(ZERO), lastMark, marks :+ ZERO, segments)
            if (oneAllowed) {
              allSegments(numZeroesLeft, numOnesLeft - 1, Some(ONE), lastMark, marks :+ ONE, segs)
            } else {
              segs
            }
          } else if (oneAllowed) {
            allSegments(numZeroesLeft, numOnesLeft - 1, Some(ONE), lastMark, marks :+ ONE, segments)
          } else {
            segments
          }
        }
      }

      allSegments(length / 2, length / 2, None, None, Vector[DefinedMark](), Vector[DefinedMarkSegment]())
    }

    val allValidRows = allValidSegments(board.numColumns)
    val allValidColumns = if (board.numColumns == board.numRows) allValidRows else allValidSegments(board.numRows)

    @tailrec
    def solvedWithCachedValidSegments(board: Board): Option[Board] = {
      def solveOnce(board: Board) = {
        def matchPossibilities(board: Board, view: SegmentView, possibilities: DefinedMarkSegments, currentSegments: Segments): Board = {
          type ProjectionMarkSegment = IndexedSeq[MarkProjection]

          def currentSegmentMatches(segment: DefinedMarkSegment) = segment.toStream.zip(view.marks(board)).forall(p => p._2.matches(p._1))

          def project(projection: ProjectionMarkSegment, projected: DefinedMarkSegment): ProjectionMarkSegment =
            projection.zip(projected).map { c =>
              c._1.project(c._2)
            }

          def fillCertainMarks: Board = {
            val base: ProjectionMarkSegment = view.map[MarkProjection](MarkProjection(_))

            val completedSegments = currentSegments.filter(_.isComplete)

            def isInCompletedSegements(possibility: DefinedMarkSegment) = completedSegments.exists(segment =>
              segment.marks.toStream.zip(possibility).forall(p => p._1 == p._2))

            val projected = possibilities.foldLeft(base) { case (projection, possibility) =>
              if (!isInCompletedSegements(possibility) && currentSegmentMatches(possibility)) {
                project(projection, possibility)
              } else {
                projection
              }
            }

            view.update(projected(_).asMark)
          }

          // Optimisation: if the segment is fully populated, we surely can't change the segment
          if (view.isComplete(board)) {
            board
          } else {
            fillCertainMarks
          }
        }

        def forAllRows(board: Board, f: (Board, SegmentView, Segments) => Board): Board =
          (0 until board.numRows).foldLeft(board) { (brd, rowIdx) =>
            f(brd, brd.rowView(rowIdx), brd.rows)
          }

        def forAllColumns(board: Board, f: (Board, SegmentView, Segments) => Board): Board =
          (0 until board.numColumns).foldLeft(board) { (brd, columnIdx) =>
            f(brd, brd.columnView(columnIdx), brd.columns)
          }

        def forAllRowsAndColumns(board: Board, f: (Board, SegmentView, Segments) => Board): Board =
          forAllRows(forAllColumns(board, f), f)

        forAllRowsAndColumns(board, matchPossibilities(_, _, allValidColumns, _))
      }

      val solved = solveOnce(board)
      if (solved.isComplete) {
        Some(solved)
      } else if (solved.version != board.version) {
        solvedWithCachedValidSegments(solved)
      } else {
        None
      }
    }

    solvedWithCachedValidSegments(board)
  }
}
