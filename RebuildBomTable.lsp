;; ALL HELPER FUNCTIONS USED IN REBUILDBOMTABLE
(defun _trim (s) (if s (vl-string-trim " \t\r\n" s) ""))

(defun _get-attr (insEname tag / e d val)
  (setq val nil)
  (setq e (entnext insEname))
  (while (and e (not val))
    (setq d (entget e))
    (cond
      ((= (cdr (assoc 0 d)) "ATTRIB")
        (if (= (strcase (cdr (assoc 2 d))) (strcase tag))
          (setq val (cdr (assoc 1 d)))
        ))
      ((= (cdr (assoc 0 d)) "SEQEND")
        (setq e nil))
    )
    (if e (setq e (entnext e)))
  )
  val
)

(defun splitParentsChildren (ss / i ent sym parentSym parents children
                             TAG_SYMBOL TAG_PARENT)
  (setq parents  '()
        children '()
        i 0)

  ;; attribute tag definitions
  (setq TAG_SYMBOL *TAG_SYMBOL*)
  (setq TAG_PARENT *TAG_PARENT*)

  (while (< i (sslength ss))
    (setq ent (ssname ss i))

    (setq sym       (_trim (_get-attr ent TAG_SYMBOL))
          parentSym (_trim (_get-attr ent TAG_PARENT)))

    (cond
      ((/= sym "")       (setq parents  (cons ent parents)))
      ((/= parentSym "") (setq children (cons ent children)))
    )

    (setq i (1+ i))
  )

  (list (reverse parents) (reverse children))
)

(defun sortParentsByPN (parents / TAG_PN)
  (setq TAG_PN *TAG_PN*)
  (setq PN_PREFIX *PN_PREFIX*)

  ;; start index of numeric portion after "<PN_PREFIX>"
  (setq startIdx (+ (strlen PN_PREFIX) 1))
  
  (vl-sort
    parents
    (function
      (lambda (e1 e2)
        (<
          (atoi (substr (_get-attr e1 TAG_PN) startIdx)) ; "PN-0" → 0
          (atoi (substr (_get-attr e2 TAG_PN) startIdx))
        )
      )
    )
  )
)

(defun sortChildrenByPN (children / TAG_PN)
  (setq TAG_PN *TAG_PN*)
  (setq PN_PREFIX *PN_PREFIX*)

  ;; start index of numeric portion after "<PN_PREFIX>"
  (setq startIdx (+ (strlen PN_PREFIX) 1))
  
  (vl-sort
    children
    (function
      (lambda (e1 e2)
        (<
          (atoi (substr (_get-attr e1 TAG_PN) startIdx)) ; "PN-0" -> 0
          (atoi (substr (_get-attr e2 TAG_PN) startIdx))
        )
      )
    )
  )
)

(defun makeEmptyWorkTable ()
  '()
)

(defun collectChildrenForParent (parent children / TAG_SYMBOL TAG_PARENT
                                        pSym childSym alist rem)
  (setq TAG_SYMBOL *TAG_SYMBOL*
        TAG_PARENT *TAG_PARENT*
        alist '()
        rem   '())

  (setq pSym (_trim (_get-attr parent TAG_SYMBOL)))

  (foreach c children
    (setq childSym (_trim (_get-attr c TAG_PARENT)))
    (if (= childSym pSym)
      (setq alist (cons c alist))   ; match → collect
      (setq rem   (cons c rem))     ; no match → keep
    )
  )

  (list (reverse alist) (reverse rem))
)

(defun makePnChainFingerprint (parent children / TAG_PN seen out k)
  (setq TAG_PN *TAG_PN*
        seen '())

  ;; start with parent
  (setq out (_get-attr parent TAG_PN))
  (setq seen (list out))

  ;; children (dedupe, order preserved)
  (foreach c children
    (setq k (_get-attr c TAG_PN))
    (if (not (member k seen))
      (progn
        (setq out  (strcat out k))
        (setq seen (cons k seen))
      )
    )
  )

  out
)

(defun _setnth (lst idx val / i out)
  (setq i 0 out '())
  (foreach x lst
    (setq out (cons (if (= i idx) val x) out))
    (setq i (1+ i))
  )
  (reverse out)
)

(defun _incnth (lst idx / cur)
  (setq cur (nth idx lst))
  (_setnth lst idx (1+ cur))
)

(defun makeQtyVectorForAssy (parent children / TAG_PN keys qtys k idx)
  (setq TAG_PN *TAG_PN*)

  ;; build unique key order: parent then unique children (order preserved)
  (setq keys (list (_get-attr parent TAG_PN)))
  (foreach c children
    (setq k (_get-attr c TAG_PN))
    (if (not (member k keys))
      (setq keys (append keys (list k)))
    )
  )

  ;; init qtys to 0
  (setq qtys (mapcar '(lambda (x) 0) keys))

  ;; parent qty = 1
  (setq qtys (_setnth qtys 0 1))

  ;; count children repeats
  (foreach c children
    (setq k (_get-attr c TAG_PN))
    (setq idx (vl-position k keys))
    (setq qtys (_incnth qtys idx))
  )

  qtys
)

(defun makePnVectorForAssy (parent children / TAG_PN keys k)
  (setq TAG_PN *TAG_PN*)

  ;; parent first
  (setq keys (list (_get-attr parent TAG_PN)))

  ;; children (dedupe, preserve order)
  (foreach c children
    (setq k (_get-attr c TAG_PN))
    (if (not (member k keys))
      (setq keys (append keys (list k)))
    )
  )

  keys
)

(defun makeSymbolVectorForAssy (parent children / TAG_SYMBOL cnt out sym)
  (setq TAG_SYMBOL *TAG_SYMBOL*) ; e.g. "SYMBOL"
  (setq out '())

  (setq cnt (length (makePnVectorForAssy parent children)))
  (setq sym (_get-attr parent TAG_SYMBOL))

  (repeat cnt
    (setq out (cons sym out))
  )

  (reverse out)
)

(defun makeDescVectorForAssy (parent children / TAG_PN TAG_DESC keys descs k)
  (setq TAG_PN *TAG_PN*
        TAG_DESC *TAG_DESC*)

  ;; unique key order (parent first)
  (setq keys  (list (_get-attr parent TAG_PN)))
  (setq descs (list (_get-attr parent TAG_DESC)))

  ;; children (dedupe by RA_PN, order preserved, first seen wins)
  (foreach c children
    (setq k (_get-attr c TAG_PN))
    (if (not (member k keys))
      (progn
        (setq keys  (append keys  (list k)))
        (setq descs (append descs (list (_get-attr c TAG_DESC))))
      )
    )
  )

  descs
)

(defun findFpStart (workTable fp / i found)
  (setq i 0
        found nil)

  (while (and (< i (length workTable)) (not found))
    (if (= (nth 4 (nth i workTable)) fp)
      (setq found i)
      (setq i (1+ i))
    )
  )

  found
)

(defun _qty->str (q)
  (cond
    ((null q) "")
    ((= (type q) 'INT)  (itoa q))
    ((= (type q) 'REAL) (rtos q 2 2))
    (t (vl-princ-to-string q))
  )
)

(defun _safe-get (fn args / res)
  (setq res (vl-catch-all-apply fn args))
  (if (vl-catch-all-error-p res) nil res)
)

(defun _safe-set (fn args)
  (vl-catch-all-apply fn args)
)

(defun AppendWorkTableToBOMTable (tblEnt workTable / tbl oldRows addN rowH i r srcRow c
                                        sty hgt alg newRow)
  (vl-load-com)
  (setq tbl (vlax-ename->vla-object tblEnt))

  ;; how many new rows?
  (setq addN (length workTable))
  (if (<= addN 0)
    (progn (princ "\nNo BOM rows to add.") (exit))
  )

  ;; current row count
  (setq oldRows (vla-get-Rows tbl))

  ;; choose a template row to copy formatting from:
  ;; - if there is a previous data row, use last existing row
  ;; - else use header row 1 (common: row0 title, row1 headers)
  (setq srcRow
    (cond
      ((> oldRows 2) (1- oldRows)) ; last existing row is a data row
      ((> oldRows 1) 1)           ; header row
      ((> oldRows 0) 0)
      (t 0)
    )
  )

  ;; pick row height from template row (fallback to row0 then 3.0)
  (setq rowH
    (cond
      ((and (> oldRows 0) (_safe-get 'vla-GetRowHeight (list tbl srcRow)))
       (_safe-get 'vla-GetRowHeight (list tbl srcRow)))
      ((and (> oldRows 0) (_safe-get 'vla-GetRowHeight (list tbl 0)))
       (_safe-get 'vla-GetRowHeight (list tbl 0)))
      (t 3.0)
    )
  )

  ;; append rows at end
  (vla-InsertRows tbl oldRows rowH addN)

  ;; fill + copy formatting
  (setq i 0)
  (while (< i addN)
    (setq r (nth i workTable))
    (setq newRow (+ oldRows i))

    ;; write values (r = (SYMBOL RAPN DESC QTY ...) — fingerprint ignored if present)
    (vla-SetText tbl newRow 0 (if (nth 0 r) (nth 0 r) ""))
    (vla-SetText tbl newRow 1 (if (nth 1 r) (nth 1 r) ""))
    (vla-SetText tbl newRow 2 (if (nth 2 r) (nth 2 r) ""))
    (vla-SetText tbl newRow 3 (_qty->str (nth 3 r)))

    ;; copy formatting per column from srcRow -> newRow
    (setq c 0)
    (while (< c 4)
      (setq sty (_safe-get 'vla-GetCellTextStyle  (list tbl srcRow c)))
      (setq hgt (_safe-get 'vla-GetCellTextHeight (list tbl srcRow c)))
      (setq alg (_safe-get 'vla-GetCellAlignment (list tbl srcRow c)))

      (if sty (_safe-set 'vla-SetCellTextStyle  (list tbl newRow c sty)))
      (if hgt (_safe-set 'vla-SetCellTextHeight (list tbl newRow c hgt)))
      (if alg (_safe-set 'vla-SetCellAlignment  (list tbl newRow c alg)))

      (setq c (1+ c))
    )

    (setq i (1+ i))
  )

  (vla-Update tbl)
  tblEnt
)

;; REBUILDING THE MAIN REBUILDBOMTABLE FUNCTION BELOW
(defun c:RebuildBomTable ( / ss tblEnt tb
                          parents children result
                          parentsSorted
                          workTable
                          i pent
                          matchedChildren matchedChildrenSorted fingerprint qtyVector pnVector
                          start n row oldq
                          descVector symbolVector
                          )
  (vl-load-com)
  
  ;; 1) select blocks
  (prompt "\nBox-select ALL blocks (housings + contacts + standalone): ")
  (setq ss (ssget '((0 . "INSERT"))))
  (if (not ss) (progn (prompt "\nNo blocks selected.") (princ) (exit)))
  
  ;; 2) select table
  (prompt "\nSelect the BOM table: ")
  (setq tblEnt (car (entsel)))
  (if (not tblEnt) (progn (prompt "\nNo table selected.") (princ) (exit)))
  (setq tb (vlax-ename->vla-object tblEnt))

  ;; Split the selection set in to parents and children
  (setq result (splitParentsChildren ss))
  (setq parents  (car result))
  (setq children (cadr result))
  
  ;; Sort parents by RA P/N
  (setq parentsSorted (sortParentsByPN parents))
  
  ;; Create an empty table
  (setq workTable (makeEmptyWorkTable))
  
  ;; iterate through parents
  (setq i 0)
  (while (< i (length parents))
    (setq pent (nth i parents))     ; individual parent entity (ename)
    ;; collect children for this parent
    (setq result (collectChildrenForParent pent children))
    (setq matchedChildren (car result))
    (setq children (cadr result)) ; remedial children who didnt make the cut - story of my life
    
    ;;  sort matched children by RA P/N
    (setq matchedChildrenSorted (sortChildrenByPN matchedChildren))
    
    ;; build PNChainFingerprint
    (setq fingerprint (makePnChainFingerprint pent matchedChildrenSorted))
    
    ;; get qty vector for this parent-children assembly
    (setq qtyVector (makeQtyVectorForAssy pent matchedChildrenSorted))
    
    ;; get PN vector for this parent-children assembly
    (setq pnVector (makePnVectorForAssy pent matchedChildrenSorted))
    (setq start (findFpStart workTable fingerprint))
    
    ;; Check if this fingerprint already exists in workTable and update or add new row accordingly
    
    (if start
      (progn
        ;; Add qtyVector to existing qtys in workTable row
        (setq n (length pnVector))
        (setq newsym (nth 0 (makeSymbolVectorForAssy pent matchedChildrenSorted)))
        (setq j 0)
        (while (< j n)
          (setq row  (nth (+ start j) workTable))
          (setq oldq (nth 3 row))
          (setq oldsym (nth 0 row))
          (setq row  (list (strcat oldsym ", " newsym) (nth 1 row) (nth 2 row) (+ oldq (nth j qtyVector)) (nth 4 row)))
          (setq workTable (subst row (nth (+ start j) workTable) workTable))
          (setq j (1+ j))
        )
      )
      (progn
        ;; Get description vector for this parent-children assembly
        (setq descVector (makeDescVectorForAssy pent matchedChildrenSorted))
        ;; Get symbol vector for this parent-children assembly
        (setq symbolVector (makeSymbolVectorForAssy pent matchedChildrenSorted))

        ;; add new row to workTable
        (setq n (length pnVector))
        (setq j 0)
        (while (< j n)
          (setq workTable
            (append workTable
              (list (list (nth j symbolVector)
                          (nth j pnVector)
                          (nth j descVector)
                          (nth j qtyVector)
                          fingerprint))))
          (setq j (1+ j))
        )
      )
    )
    (setq i (1+ i))
  )
  
  ;; Add rows to the AutoCAD table
  (AppendWorkTableToBOMTable tblEnt workTable)
)