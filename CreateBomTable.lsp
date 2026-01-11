(defun c:CreateBomTable (/ *error* doc ms inspt title rows cols rowH hdrH colW tb)

  (vl-load-com)

  (defun *error* (msg)
    (if msg (prompt (strcat "\nError: " msg)))
    (princ)
  )

  ;; --- settings (tweak these) ---
  (setq cols 4)
  (setq rows 2)          ; row 0 = title, row 1 = headers (no data rows yet)
  (setq rowH 5)       ; title row height
  (setq hdrH 5)       ; header row height
  (setq colW 14)       ; default column width

  (setq inspt (getpoint "\nPick insertion point for BOM table: "))
  (if (not inspt) (progn (prompt "\nCancelled.") (princ) (exit)))

  (setq title (getstring T "\nEnter table title <BOM>: "))
  (if (= title "") (setq title "BOM"))

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ms  (vla-get-ModelSpace doc))

  ;; Create the table
  ;; vla-AddTable expects: InsertionPoint, Rows, Columns, RowHeight, ColumnWidth
  (setq tb (vla-AddTable ms (vlax-3D-point inspt) rows cols hdrH colW))

  ;; 2. Define row constants
  (setq acTitleRow  1
        acHeaderRow 2
        acDataRow   4)

  ;; 3. Text style & height (from your palette)
  (vla-SetTextStyle tb acTitleRow  "Standard")
  (vla-SetTextStyle tb acHeaderRow "Standard")
  (vla-SetTextStyle tb acDataRow   "Standard")

  (vla-SetTextHeight tb acTitleRow  1.839)
  (vla-SetTextHeight tb acHeaderRow 1.839)
  (vla-SetTextHeight tb acDataRow   1.839)

  ;; Merge top row across all columns for the title
  ;; MergeCells: row1 col1 row2 col2 (0-based)
  (vla-MergeCells tb 0 0 0 (1- cols))

  ;; Fill in text
  (vla-SetText tb 0 0 title)

  (vla-SetText tb 1 0 "SYMBOL")
  (vla-SetText tb 1 1 "P/N")
  (vla-SetText tb 1 2 "DESCRIPTION")
  (vla-SetText tb 1 3 "QTY.")

  ;; Optional: simple alignment (center title, center headers)
  ;; acMiddleCenter = 5
  (vla-SetCellAlignment tb 0 0 5)
  (vla-SetCellAlignment tb 1 0 5)
  (vla-SetCellAlignment tb 1 1 5)
  (vla-SetCellAlignment tb 1 2 5)
  (vla-SetCellAlignment tb 1 3 5)

  (vla-Update tb)

  (prompt "\nBOM table created.")
  (princ)
)
