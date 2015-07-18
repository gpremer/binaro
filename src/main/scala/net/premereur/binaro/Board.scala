package net.premereur.binaro

import scalaz.Scalaz._
import scalaz._

object BoardSolver {

  sealed trait Indexer {
    def get(board: Board, idx: Int): Mark

    def set(board: Board, idx: Int, mark: Mark): Board

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
  type MarkSummary = IndexedSeq[MarkCount]

  class Board(rows: MarkBundle,
              rowCounts: MarkSummary,
              columnCounts: MarkSummary,
              version: Int = 0) {

    import Board._

    val numRows = rowCounts.size
    val numColumns = columnCounts.size

    class RowIndexer(rowIdx: Int) extends Indexer {
      override def get(board: Board, columnIdx: Int): Mark = board(rowIdx, columnIdx)

      override def set(board: Board, columnIdx: Int, mark: Mark): Board = board.set(rowIdx, columnIdx, mark)

      val limit = numColumns
    }

    class ColumnIndexer(columnIdx: Int) extends Indexer {
      override def get(board: Board, rowIdx: Int): Mark = board(rowIdx, columnIdx)

      override def set(board: Board, rowIdx: Int, mark: Mark): Board = board.set(rowIdx, columnIdx, mark)

      val limit = numRows
    }

    def apply(rowIdx: Int, columnIdx: Int) = rows(rowIdx)(columnIdx)

    def set(rowIdx: Int, columnIdx: Int, mark: Mark) =
      if (this(rowIdx, columnIdx) != mark) {
        new Board(
          update(rows, rowIdx, update(_: MarkSegment, columnIdx, { _: Mark => mark })),
          update(rowCounts, rowIdx, (_: MarkCount).add(mark)),
          update(columnCounts, columnIdx, (_: MarkCount).add(mark)),
          version + 1)
      } else {
        this
      }

    def rowIndexer(rowIdx: Int): Indexer = new RowIndexer(rowIdx) // could cache

    def columnIndexer(columnIdx: Int): Indexer = new ColumnIndexer(columnIdx) // could cache

    override def toString =
      (0 until numRows).map { rowIdx =>
        (0 until numColumns).map(this(rowIdx, _).toString).mkString
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
        rows = Vector.fill(numRows)(Vector.fill(numColumns)(UNKNOWN)),
        rowCounts = Vector.fill(numRows)(MarkCount(0, 0)),
        columnCounts = Vector.fill(numColumns)(MarkCount(0, 0))
      )

      lines.zipWithIndex.foldLeft(emptyBoard) { case (rowBoard, (line, rowIdx)) =>
        line.toCharArray.zipWithIndex.foldLeft(rowBoard) {
          case (columnBoard, (chr, columnIdx)) =>
            columnBoard.set(columnIdx, rowIdx, Mark.fromChar(chr))
        }
      }
    }

    def update[A](seq: IndexedSeq[A], idx: Int, f: A => A) = seq.updated(idx, f(seq(idx)))

    def solve(board: Board) = {
      def extendDoubles(board: Board, indexer: Indexer): Board = {
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

      def countSliceMarks(board: Board, indexer: Indexer): (Int, Int) = {
        (0 until indexer.limit).foldLeft((0, 0)) { (counts, idx) =>
          if (indexer.get(board, idx) == ZERO) {
            (counts._1 + 1, counts._2)
          } else if (indexer.get(board, idx) == ONE) {
            (counts._1, counts._2 + 1)
          } else {
            counts
          }
        }
      }


      def fillIfComplete(board: Board, indexer: Indexer): Board = {
        def fillAllMissing(mark: Mark) = (0 until indexer.limit).foldLeft(board) { (brd, idx) =>
          if (!indexer.get(brd, idx).isKnown) {
            indexer.set(brd, idx, mark)
          } else {
            brd
          }
        }
        val counts = countSliceMarks(board, indexer)
        if (counts._1 == indexer.limit / 2 && counts._1 != counts._2) {
          fillAllMissing(ONE)
        } else if (counts._2 == indexer.limit / 2 && counts._1 != counts._2) {
          fillAllMissing(ZERO)
        } else {
          board
        }
      }

      def forAllRows(board: Board, f: (Board, Indexer) => Board): Board =
        (0 until board.numRows).foldLeft(board) { (brd, rowIdx) =>
          f(brd, board.rowIndexer(rowIdx))
        }

      def forAllColumns(board: Board, f: (Board, Indexer) => Board): Board =
        (0 until board.numRows).foldLeft(board) { (brd, columnIdx) =>
          f(brd, board.columnIndexer(columnIdx))
        }

      def forAllRowsAndColumns(board: Board, f: (Board, Indexer) => Board): Board =
        forAllColumns(forAllRows(board, f), f)

      val (brd, _) = (for {
        _ <- modify(forAllRowsAndColumns(_: Board, extendDoubles))
        _ <- modify(forAllRowsAndColumns(_: Board, fillIfComplete))
      } yield ()).run(board)
      brd
    }
  }

}

object X extends App {

  import BoardSolver.Board.solve
  import BoardSolver._

  println(solve(Board("..1...|0...11|00..1.|..1..0|...1..|1...0.")))
}