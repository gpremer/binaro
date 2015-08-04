package net.premereur.binaro

import scala.annotation.tailrec
import scalaz.Scalaz._
import scalaz._

object BoardSolver {

  sealed trait SegmentIndexer {
    def get(board: Board, idx: Int): Mark

    def set(board: Board, idx: Int, mark: Mark): Board

    def summaryCount(board: Board): MarkCount

    def limit: Int
  }

  case class MarkCount(zeroCount: Int, oneCount: Int) {
    def add(mark: Mark): MarkCount = mark match {
      case ZERO => copy(zeroCount = zeroCount + 1)
      case ONE => copy(oneCount = oneCount + 1)
      case UNKNOWN => this
    }
  }

  type MarkSegment = IndexedSeq[Mark]
  type MarkBundle = IndexedSeq[MarkSegment]
  type SegmentSummary = IndexedSeq[MarkCount]

  class Board(val rows: MarkBundle,
              val columns: MarkBundle,
              val rowCounts: SegmentSummary,
              val columnCounts: SegmentSummary,
              val version: Int = 0) {

    import Board._

    val numRows = rowCounts.size
    val numColumns = columnCounts.size

    def apply(rowIdx: Int, columnIdx: Int) = rows(rowIdx)(columnIdx)

    def set(rowIdx: Int, columnIdx: Int, mark: Mark) =
      if (this(rowIdx, columnIdx) != mark) {
        new Board(
          update(rows, rowIdx, update(_: MarkSegment, columnIdx, { _: Mark => mark })),
          update(columns, columnIdx, update(_: MarkSegment, rowIdx, { _: Mark => mark })),
          update(rowCounts, rowIdx, (_: MarkCount).add(mark)),
          update(columnCounts, columnIdx, (_: MarkCount).add(mark)),
          version + 1)
      } else {
        this
      }

    def rowIndexer(rowIdx: Int): SegmentIndexer = new RowIndexer(rowIdx, numColumns) // could cache

    def columnIndexer(columnIdx: Int): SegmentIndexer = new ColumnIndexer(columnIdx, numRows) // could cache

    override def toString =
      (0 until numRows).map { rowIdx =>
        (0 until numColumns).map(this(rowIdx, _).toString).mkString
      }.mkString("\n")
  }

  class RowIndexer(rowIdx: Int, val limit: Int) extends SegmentIndexer {
    override def get(board: Board, columnIdx: Int): Mark = board(rowIdx, columnIdx)

    override def set(board: Board, columnIdx: Int, mark: Mark): Board = board.set(rowIdx, columnIdx, mark)

    override def summaryCount(board: Board) = board.rowCounts(rowIdx)
  }

  class ColumnIndexer(columnIdx: Int, val limit: Int) extends SegmentIndexer {
    override def get(board: Board, rowIdx: Int): Mark = board(rowIdx, columnIdx)

    override def set(board: Board, rowIdx: Int, mark: Mark): Board = board.set(rowIdx, columnIdx, mark)

    override def summaryCount(board: Board) = board.rowCounts(columnIdx)
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
        rows = Vector.fill(numRows)(Vector.fill(numColumns)(UNKNOWN)),
        columns = Vector.fill(numColumns)(Vector.fill(numRows)(UNKNOWN)),
        rowCounts = Vector.fill(numRows)(MarkCount(0, 0)),
        columnCounts = Vector.fill(numColumns)(MarkCount(0, 0))
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
      def allValidSegments(length: Int): MarkBundle = {
        def allSegments(numZeroesLeft: Int, numOnesLeft: Int,
                        lastMark: Option[Mark], nextToLastMark: Option[Mark],
                        segment: MarkSegment, segments: MarkBundle): MarkBundle = {
          def zeroAllowed = numZeroesLeft > 0 && (nextToLastMark.getOrElse(ONE) != ZERO || lastMark.getOrElse(ONE) != ZERO)
          def oneAllowed = numOnesLeft > 0 && (nextToLastMark.getOrElse(ZERO) != ONE || lastMark.getOrElse(ZERO) != ONE)


          if (numZeroesLeft == 0 && numOnesLeft == 0) {
            segments :+ segment
          } else {
            if (zeroAllowed) {
              val segs = allSegments(numZeroesLeft - 1, numOnesLeft, Some(ZERO), lastMark, segment :+ ZERO, segments)
              if (oneAllowed) {
                allSegments(numZeroesLeft, numOnesLeft - 1, Some(ONE), lastMark, segment :+ ONE, segs)
              } else {
                segs
              }
            } else if (oneAllowed) {
              allSegments(numZeroesLeft, numOnesLeft - 1, Some(ONE), lastMark, segment :+ ONE, segments)
            } else {
              segments
            }
          }
        }

        allSegments(length / 2, length / 2, None, None, Vector[Mark](), Vector[Vector[Mark]]())
      }

      val allValidRows = allValidSegments(board.numColumns)
      val allValidColumns = if (board.numColumns == board.numRows) allValidRows else allValidSegments(board.numRows)

      @tailrec
      def solvedWithCachedValidSegments(board: Board): Board = {
        def solveOnce(baord: Board) = {
          def extendDoubles(board: Board, indexer: SegmentIndexer): Board = {
            (0 until (indexer.limit - 2)).foldLeft(board) { (brd, idx) =>
              val pos1 = indexer.get(brd, idx)
              val pos2 = indexer.get(brd, idx + 1)
              val pos3 = indexer.get(brd, idx + 2)
              if (pos1.isKnown && pos1 == pos2) {
                // XX_ => XXY
                indexer.set(brd, idx + 2, pos1.complement)
              } else if (pos2.isKnown && pos2 == pos3) {
                // _XX => YXX
                indexer.set(brd, idx, pos2.complement)
              } else if (pos1.isKnown && pos1 == pos3) {
                // X_X => XYX
                indexer.set(brd, idx + 1, pos1.complement)
              } else {
                brd
              }
            }
          }

          def fillIfComplete(board: Board, indexer: SegmentIndexer): Board = {
            def fillAllMissing(mark: Mark) = (0 until indexer.limit).foldLeft(board) { (brd, idx) =>
              if (!indexer.get(brd, idx).isKnown) {
                indexer.set(brd, idx, mark)
              } else {
                brd
              }
            }
            val counts = indexer.summaryCount(board)
            if (counts.zeroCount == indexer.limit / 2 && counts.oneCount != counts.zeroCount) {
              fillAllMissing(ONE)
            } else if (counts.oneCount == indexer.limit / 2 && counts.oneCount != counts.zeroCount) {
              fillAllMissing(ZERO)
            } else {
              board
            }
          }

          def matchPossibilities(board: Board, indexer: SegmentIndexer, possibilities: MarkBundle, forbidden: MarkBundle): Board = {
            type MaybeMarkSegment = IndexedSeq[Option[Mark]]

            def matches(segment: MarkSegment) = segment.toStream.zipWithIndex.forall(p => indexer.get(board, p._2).matches(p._1))

            def project(projection: MaybeMarkSegment, projected: MarkSegment): MaybeMarkSegment = projection.zip(projected).map { c =>
              c._1 match {
                case None => Some(c._2)
                case Some(m) => Some(m & c._2)
              }
            }

            val base: MaybeMarkSegment = (0 until indexer.limit).map { idx =>
              val mark = indexer.get(board, idx)
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
              indexer.set(brd, idx, maybeMark.get)
            }
          }

          def forAllRows(board: Board, f: (Board, SegmentIndexer) => Board): Board =
            (0 until board.numRows).foldLeft(board) { (brd, rowIdx) =>
              f(brd, board.rowIndexer(rowIdx))
            }

          def forAllRows2(board: Board, f: (Board, SegmentIndexer, MarkBundle) => Board): Board =
            (0 until board.numRows).foldLeft(board) { (brd, rowIdx) =>
              f(brd, board.rowIndexer(rowIdx), board.rows)
            }

          def forAllColumns(board: Board, f: (Board, SegmentIndexer) => Board): Board =
            (0 until board.numRows).foldLeft(board) { (brd, columnIdx) =>
              f(brd, board.columnIndexer(columnIdx))
            }

          def forAllColumns2(board: Board, f: (Board, SegmentIndexer, MarkBundle) => Board): Board =
            (0 until board.numRows).foldLeft(board) { (brd, columnIdx) =>
              f(brd, board.columnIndexer(columnIdx), board.columns)
            }

          def forAllRowsAndColumns(board: Board, f: (Board, SegmentIndexer) => Board): Board =
            forAllColumns(forAllRows(board, f), f)

          val (modifiedBoard, _) = (for {
          //          _ <- modify(forAllRowsAndColumns(_: Board, extendDoubles))
          //          _ <- modify(forAllRowsAndColumns(_: Board, fillIfComplete))
            _ <- modify(forAllRows2(_: Board, matchPossibilities(_: Board, _: SegmentIndexer, allValidRows, _: MarkBundle)))
            _ <- modify(forAllColumns2(_: Board, matchPossibilities(_: Board, _: SegmentIndexer, allValidColumns, _: MarkBundle)))
          } yield ()).run(board)
          modifiedBoard
        }

        val solved = solveOnce(board)
        if (board.version == solved.version) {
          solved
        } else {
          solvedWithCachedValidSegments(solved)
        }
      }

      solvedWithCachedValidSegments(board)
    }
  }

}

object X extends App {

  import BoardSolver.Board.solve
  import BoardSolver._

  println(solve(Board(".1....|0...11|00..1.|..1..0|...1..|1...0.")))
  println()
  println(solve(Board("11....|...0.0|......|...1..|..0..0|.0.00.")))
  println()
  println(solve(Board("1.......0...|..........1.")))
  println()
  println(solve(Board("00........|.0.......1|..00...0..|.0.0.0.0..|....00....|..1.....0.|...0...0.0|..1.1.....|...1..00.0|11......1.")))
  println()
  println(solve(Board("0010101101|.0.......1|..00...0..|00.0.0.0..|0...00....|1.1.....0.|...0.010.0|..1.1.....|...1..00.0|11....1.1.")))

}