package net.premereur.binaro

object Binaro {

  type MarkSegment = IndexedSeq[Mark]
  type Segments = IndexedSeq[Segment]

  /**
   * The rules for columns and rows are the same, so we want to make an view onto a board that allows it to be seen as
   * a collection of segments that can either be columns or rows.
   */
  sealed trait SegmentView {
    def map[T](f: Mark => T): IndexedSeq[T]

    def update(f: Int => Mark): Board

    def summaryCount(board: Board): MarkCount

    def limit: Int

    def isComplete(board: Board): Boolean = summaryCount(board) == MarkCount(limit / 2, limit / 2)

    def marks(board: Board): MarkSegment
  }

  /**
   * It is not very efficient to have to count the number of zeroes and ones time and again. This case class is used to
   * cache those counts. It also helps determining it.
   * @param zeroCount The number of marks set to 0
   * @param oneCount The number of marks set to 1
   */
  case class MarkCount(zeroCount: Int, oneCount: Int) {
    def add(mark: Mark): MarkCount = mark match {
      case ZERO => copy(zeroCount = zeroCount + 1)
      case ONE => copy(oneCount = oneCount + 1)
      case UNKNOWN => this
    }
  }

  /**
   * One row or column that holds the marks (_, 0 or 1) and caches a summary frequency count.
   * @param marks An indexed sequence of marks
   * @param count A frequency counts of the number of 0's or 1's
   */
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

  /**
   * Helps initialising segments.
   */
  object Segment {
    def empty(size: Int) = Segment(Vector.fill(size)(UNKNOWN), MarkCount(0, 0))
  }

  /**
   * Represents a Binaro board in some stage of completion. For the rules see https://en.wikipedia.org/wiki/Takuzu.
   * Since the marks are more read than written, both a view as rows and columns is stored (i.e. each mark is present
   * twice).
   * @param rows All rows that make up the board.
   * @param columns All columns that make up the board.
   * @param version The number of effective updates since the creation of the "root" board.
   */
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

    def rowView(rowIdx: Int): SegmentView = new RowView(rowIdx)

    def columnView(columnIdx: Int): SegmentView = new ColumnView(columnIdx)

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
    /**
     * Creates Binaro boards from strings that consist of one row per line. A line is terminated by either a newline or
     * a pipe character (|). A row contains either a zero (0) or a one (1) for known position or any other character
     * (but not | or \n) for an as yet undetermined mark.
     * @param graphic A visual representation of a Binaro board.
     * @return a Board.
     */
    def apply(graphic: String): Board = {
      assert(graphic.length > 3, "The board should not be trivial")
      val lines: IndexedSeq[String] = graphic.split("[\n|]")
      val lengths = lines.map(_.length).toSet

      val numColumns = lines.head.length
      val numRows = lines.length

      assert(lengths.size == 1, "All lines should be the same size")
      assert(numColumns % 2 == 0, "Rows should be of even length")
      assert(numRows % 2 == 0, "Columns should be of even length")

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
  }

}
