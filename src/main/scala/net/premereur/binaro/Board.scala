package net.premereur.binaro

import scala.annotation.tailrec

object BoardSolver {

  sealed trait SegmentView {
    def map[T](f: Mark => T): IndexedSeq[T]

    def update(f: Int => Mark): Board

    def summaryCount(board: Board): MarkCount

    def limit: Int

    def isComplete(board: Board): Boolean = summaryCount(board) == MarkCount(limit / 2, limit / 2)

    def marks(board: Board): MarkSegment
  }

  case class MarkCount(zeroCount: Int, oneCount: Int) {
    def add(mark: Mark): MarkCount = mark match {
      case ZERO => copy(zeroCount = zeroCount + 1)
      case ONE => copy(oneCount = oneCount + 1)
      case UNKNOWN => this
    }
  }

  type MarkSegment = IndexedSeq[Mark]
  type Segments = IndexedSeq[Segment]
  type DefinedMarkSegment = IndexedSeq[DefinedMark]
  type DefinedMarkSegments = IndexedSeq[DefinedMarkSegment]

  case class Segment(marks: MarkSegment, count: MarkCount) {

    import Board.update

    def apply(idx: Int) = marks(idx)

    def set(idx: Int, mark: Mark) =
    // once set, a mark cannot be reset
      if (mark != UNKNOWN && this (idx) == UNKNOWN) {
        copy(
          marks = update(marks, idx, { _: Mark => mark }),
          count = count.add(mark)
        )
      } else {
        this
      }

    def size = marks.size

    lazy val isComplete = count.oneCount == marks.size / 2 && count.zeroCount == count.oneCount
  }

  object Segment {
    def empty(size: Int) = Segment(Vector.fill(size)(UNKNOWN), MarkCount(0, 0))
  }

  class Board(val rows: Segments,
              val columns: Segments,
              val version: Int = 0) {

    import Board._

    def numRows = rows.size

    def numColumns = columns.size

    def rowCounts(rowIdx: Int) = rows(rowIdx).count

    def columnCounts(columnIdx: Int) = columns(columnIdx).count

    def apply(rowIdx: Int, columnIdx: Int) = rows(rowIdx)(columnIdx)

    def set(rowIdx: Int, columnIdx: Int, mark: Mark) =
    // once set, we cannot reset
      if (mark != UNKNOWN && this (rowIdx, columnIdx) == UNKNOWN) {
        new Board(
          update(rows, rowIdx, (_: Segment).set(columnIdx, mark)),
          update(columns, columnIdx, (_: Segment).set(rowIdx, mark)),
          version + 1)
      } else {
        this
      }

    def rowView(rowIdx: Int): SegmentView = new RowView(rowIdx) // could cache

    def columnView(columnIdx: Int): SegmentView = new ColumnView(columnIdx) // could cache

    def isComplete = (0 until numRows).map(rowView).forall(_.isComplete(this))

    class RowView(rowIdx: Int) extends SegmentView {
      override def map[T](f: Mark => T): IndexedSeq[T] = rows(rowIdx).marks.map(f)

      override def update(f: Int => Mark): Board = (0 until limit).foldLeft(Board.this) { (b, idx) =>
        b.set(rowIdx, idx, f(idx))
      }

      override def summaryCount(board: Board) = board.rowCounts(rowIdx)

      override def marks(board: Board): MarkSegment = board.rows(rowIdx).marks

      override def limit: Int = numColumns
    }

    class ColumnView(columnIdx: Int) extends SegmentView {
      override def map[T](f: Mark => T): IndexedSeq[T] = columns(columnIdx).marks.map(f)

      override def update(f: Int => Mark): Board = (0 until limit).foldLeft(Board.this) { (b, idx) =>
        b.set(idx, columnIdx, f(idx))
      }

      override def summaryCount(board: Board) = board.columnCounts(columnIdx)

      override def marks(board: Board): MarkSegment = board.columns(columnIdx).marks

      override def limit: Int = numRows
    }

    override def toString =
      (0 until numRows).map { rowIdx =>
        (0 until numColumns).map(this (rowIdx, _).toString).mkString
      }.mkString("\n")
  }

  object Board {
    def apply(graphic: String): Board = {
      assert(graphic.length > 3, "The board should not be trivial")
      val lines: IndexedSeq[String] = graphic.split("[\n|]")
      val lengths = lines.map(_.length).toSet
      assert(lengths.size == 1, "All lines should be the same size")
      assert(lengths.head % 2 == 0, "Lines should be of even length")

      val numColumns = lines.head.length
      val numRows = lines.length

      val emptyBoard = new Board(
        rows = Vector.fill(numRows)(Segment.empty(numColumns)),
        columns = Vector.fill(numColumns)(Segment.empty(numRows))
      )

      lines.zipWithIndex.foldLeft(emptyBoard) { case (rowBoard, (line, rowIdx)) =>
        line.toCharArray.zipWithIndex.foldLeft(rowBoard) {
          case (columnBoard, (chr, columnIdx)) =>
            columnBoard.set(rowIdx, columnIdx, Mark.fromChar(chr))
        }
      }
    }

    def update[A](seq: IndexedSeq[A], idx: Int, f: A => A) = seq.updated(idx, f(seq(idx)))

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

}

object SudokuSolver extends App {

  import BoardSolver.Board.solve
  import BoardSolver._

  println(solve(Board(".1....|0...11|00..1.|..1..0|...1..|1...0.")))
  println()
  println(solve(Board("11....|...0.0|......|...1..|..0..0|.0.00.")))
  println()
  println(solve(Board("111...|...0.0|......|...1..|..0..0|.0.00.")))
  println()
  println(solve(Board("00........|.0.......1|..00...0..|.0.0.0.0..|....00....|..1.....0.|...0...0.0|..1.1.....|...1..00.0|11......1.")))
  println()
  println(solve(Board("0010101101|.0.......1|..00...0..|00.0.0.0..|0...00....|1.1.....0.|...0.010.0|..1.1.....|...1..00.0|11....1.1.")))

}