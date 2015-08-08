package net.premereur.binaro

import scala.annotation.tailrec

object BoardSolver {

  sealed trait SegmentView {
    def get(board: Board, idx: Int): Mark

    def set(board: Board, idx: Int, mark: Mark): Board

    def summaryCount(board: Board): MarkCount

    def limit: Int

    def isComplete(board: Board): Boolean = summaryCount(board) == MarkCount(limit / 2, limit / 2)
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

  case class Segment(marks: MarkSegment, count: MarkCount) {

    import Board.update

    def apply(idx: Int) = marks(idx)

    def set(idx: Int, mark: Mark) =
    // once set, a mark cannot be reset
      if (mark != UNKNOWN && this(idx) == UNKNOWN) {
        copy(
          marks = update(marks, idx, { _: Mark => mark }),
          count = count.add(mark)
        )
      } else {
        this
      }

    def size = marks.size
  }

  object Segment {
    def empty(size: Int) = Segment(Vector.fill(size)(UNKNOWN), MarkCount(0, 0))
  }

  class Board(val rows: Segments,
              val columns: Segments) {

    import Board._

    def numRows = rows.size

    def numColumns = columns.size

    def rowCounts(rowIdx: Int) = rows(rowIdx).count

    def columnCounts(columnIdx: Int) = columns(columnIdx).count

    def apply(rowIdx: Int, columnIdx: Int) = rows(rowIdx)(columnIdx)

    def set(rowIdx: Int, columnIdx: Int, mark: Mark) =
    // once set, we cannot reset
      if (mark != UNKNOWN && this(rowIdx, columnIdx) == UNKNOWN) {
        new Board(
          update(rows, rowIdx, (_: Segment).set(columnIdx, mark)),
          update(columns, columnIdx, (_: Segment).set(rowIdx, mark)))
      } else {
        this
      }

    def rowView(rowIdx: Int): SegmentView = new RowView(rowIdx, numColumns) // could cache

    def columnView(columnIdx: Int): SegmentView = new ColumnView(columnIdx, numRows) // could cache

    def isComplete = (0 until numRows).map(rowView).forall(_.isComplete(this))

    override def toString =
      (0 until numRows).map { rowIdx =>
        (0 until numColumns).map(this(rowIdx, _).toString).mkString
      }.mkString("\n")
  }

  class RowView(rowIdx: Int, val limit: Int) extends SegmentView {
    override def get(board: Board, columnIdx: Int): Mark = board(rowIdx, columnIdx)

    override def set(board: Board, columnIdx: Int, mark: Mark): Board = board.set(rowIdx, columnIdx, mark)

    override def summaryCount(board: Board) = board.rowCounts(rowIdx)

  }

  class ColumnView(columnIdx: Int, val limit: Int) extends SegmentView {
    override def get(board: Board, rowIdx: Int): Mark = board(rowIdx, columnIdx)

    override def set(board: Board, rowIdx: Int, mark: Mark): Board = board.set(rowIdx, columnIdx, mark)

    override def summaryCount(board: Board) = board.columnCounts(columnIdx)
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

    def solve(board: Board): Board = {
      def allValidSegments(length: Int): Segments = {

        def allSegments(numZeroesLeft: Int, numOnesLeft: Int,
                        lastMark: Option[Mark], nextToLastMark: Option[Mark],
                        marks: MarkSegment,
                        segments: Segments): Segments = {
          def zeroAllowed = numZeroesLeft > 0 && (nextToLastMark.getOrElse(ONE) != ZERO || lastMark.getOrElse(ONE) != ZERO)
          def oneAllowed = numOnesLeft > 0 && (nextToLastMark.getOrElse(ZERO) != ONE || lastMark.getOrElse(ZERO) != ONE)

          if (numZeroesLeft == 0 && numOnesLeft == 0) {
            segments :+ Segment(marks, MarkCount(length / 2, length / 2))
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

        allSegments(length / 2, length / 2, None, None, Vector[Mark](), Vector[Segment]())
      }

      val allValidRows = allValidSegments(board.numColumns)
      val allValidColumns = if (board.numColumns == board.numRows) allValidRows else allValidSegments(board.numRows)

      @tailrec
      def solvedWithCachedValidSegments(board: Board): Board = {
        def solveOnce(board: Board) = {
          def matchPossibilities(board: Board, view: SegmentView, possibilities: Segments, forbidden: Segments): Board = {
            type MaybeMarkSegment = IndexedSeq[Option[Mark]]

            def matches(segment: Segment) = segment.marks.toStream.zipWithIndex.forall(p => view.get(board, p._2).matches(p._1))

            def project(projection: MaybeMarkSegment, projected: Segment): MaybeMarkSegment =
              projection.zip(projected.marks).map { c =>
                c._1 match {
                  case None => Some(c._2)
                  case Some(m) => Some(m & c._2)
                }
              }

            def fillCertainMarks: Board = {
              val base: MaybeMarkSegment = (0 until view.limit).map { idx =>
                val mark = view.get(board, idx)
                if (mark.isKnown) Some(mark) else None
              }

              val projected = possibilities.filterNot(forbidden.contains).foldLeft(base) { case (projection, possibility) =>
                if (matches(possibility)) {
                  project(projection, possibility)
                } else {
                  projection
                }
              }

              projected.zipWithIndex.foldLeft(board) { case (brd, (maybeMark, idx)) =>
                view.set(brd, idx, maybeMark.get)
              }
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
          solved
        } else {
          solvedWithCachedValidSegments(solved)
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
  //  println(solve(Board("1.......0...|..........1.")))
  //  println()
  println(solve(Board("00........|.0.......1|..00...0..|.0.0.0.0..|....00....|..1.....0.|...0...0.0|..1.1.....|...1..00.0|11......1.")))
  println()
  println(solve(Board("0010101101|.0.......1|..00...0..|00.0.0.0..|0...00....|1.1.....0.|...0.010.0|..1.1.....|...1..00.0|11....1.1.")))

}