(defun c:TANGENTLINE ( / arcEnt arcData pointOnArc extPoint center radius tanVector lineEnd)
  (prompt "\nSelect an arc: ")
  (setq arcEnt (car (entsel)))
  (if (not arcEnt)
    (progn (prompt "\nNo arc selected.") (exit))
  )

  (setq arcData (entget arcEnt))
  (if (/= (cdr (assoc 0 arcData)) "ARC")
    (progn (prompt "\nSelected object is not an arc.") (exit))
  )

  (prompt "\nPick a point on the arc: ")
  (setq pointOnArc (getpoint))
  (prompt "\nPick an extension point: ")
  (setq extPoint (getpoint))

  ;; Get center and radius
  (setq center (cdr (assoc 10 arcData)))
  (setq radius (cdr (assoc 40 arcData)))

  ;; Compute the normal vector from center to point on arc
  (setq normal (mapcar '- pointOnArc center))

  ;; Compute perpendicular (tangent) vector in 2D: (x, y) → (-y, x)
  (setq tanVector (list (- (cadr normal)) (car normal) 0.0))

  ;; Ensure tangent vector points toward the extension point
  (setq dirVec (mapcar '- extPoint pointOnArc))
  (if (< (apply '+ (mapcar '* tanVector dirVec)) 0)
    (setq tanVector (mapcar '* tanVector '(-1 -1 -1)))
  )

  ;; Normalize tangent vector to length 10
  (defun normalize (v / len)
    (setq len (sqrt (apply '+ (mapcar '* v v))))
    (if (/= len 0) (mapcar (function (lambda (x) (/ x len))) v) v)
  )

  (setq tanVector (mapcar '* (normalize tanVector) '(10.0 10.0 10.0)))
  (setq lineEnd (mapcar '+ pointOnArc tanVector))

  ;; Draw the tangent line
  (entmake
    (list
      (cons 0 "LINE")
      (cons 10 pointOnArc)
      (cons 11 lineEnd)
    )
  )

  (princ)
)
