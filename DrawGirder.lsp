(defun c:DRAWGIRDER ( / ent entData p1 p2 userLen lineVec origLen angle midPt halfVec newP1 newP2)
  (prompt "\nSelect an existing line: ")
  (setq ent (car (entsel)))
  (if (not ent)
    (progn (prompt "\nNo line selected.") (exit))
  )

  (setq entData (entget ent))
  (if (/= (cdr (assoc 0 entData)) "LINE")
    (progn (prompt "\nSelected object is not a line.") (exit))
  )

  ;; Get endpoints
  (setq p1 (cdr (assoc 10 entData)))
  (setq p2 (cdr (assoc 11 entData)))

  ;; Get vector and original length
  (setq lineVec (mapcar '- p2 p1))
  (setq origLen (distance p1 p2))

  ;; Ask user for new length
  (initget 6) ;; No zero or negative
  (setq userLen (getreal (strcat "\nEnter new line length (<= " (rtos origLen 2 4) "): ")))

  (if (> userLen origLen)
    (progn
      (prompt "\nWarning: New length exceeds original line length. Command canceled.")
      (exit)
    )
  )

  ;; Midpoint of the original line
  (setq midPt (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2))

  ;; Get direction unit vector
  (defun normalize (v / len)
    (setq len (sqrt (apply '+ (mapcar '* v v))))
    (if (/= len 0) (mapcar '(lambda (x) (/ x len)) v) v)
  )

  (setq halfVec (mapcar '* (normalize lineVec) (list (/ userLen 2.0) (/ userLen 2.0) (/ userLen 2.0))))

  ;; New start and end points
  (setq newP1 (mapcar '- midPt halfVec))
  (setq newP2 (mapcar '+ midPt halfVec))

  ;; Draw new line
  (entmake
    (list
      (cons 0 "LINE")
      (cons 10 newP1)
      (cons 11 newP2)
    )
  )

  (princ "\nGirder line created.")
  (princ)
)
