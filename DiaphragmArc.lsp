(defun c:DIAPHRAGMARC ( / p1 p2 D degCurve radius chordVec chordLen midPt perpVec normPerp center arcLen angleRad endAnglePt bulge)
  (prompt "\nSelect start point of the chord: ")
  (setq p1 (getpoint))

  (prompt "\nSelect end point of the chord: ")
  (setq p2 (getpoint))

  (setq chordVec (mapcar '- p2 p1))
  (setq chordLen (distance p1 p2))

  ;; Prompt for Degree of Curvature D
  (initget 6) ;; positive real only
  (setq degCurve (getreal "\nEnter degree of curvature (in decimal degrees, e.g., 4.5): "))

  ;; Calculate radius using highway formula
  (setq radius (/ 5729.578 degCurve))

  ;; Midpoint of chord
  (setq midPt (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2))

  ;; Get perpendicular unit vector
  (setq perpVec (list (- (cadr chordVec)) (- (car chordVec))))
  (defun normalize (v / len)
    (setq len (sqrt (apply '+ (mapcar '* v v))))
    (if (/= len 0) (mapcar '(lambda (x) (/ x len)) v) v)
  )
  (setq normPerp (normalize perpVec))

  ;; Calculate sagitta to find center of arc
  ;; sagitta = R - sqrt(R^2 - (L/2)^2)
  (setq sagitta (- radius (sqrt (- (* radius radius) (/ (* chordLen chordLen) 4.0)))))
  (setq center (mapcar '+ midPt (mapcar '* normPerp (list sagitta sagitta))))

  ;; Prompt for desired arc length of the new arc
  (initget 6)
  (setq arcLen (getreal "\nEnter arc length of new arc: "))

  ;; Check if arc length is greater than the total arc's length
  (setq fullAngleRad (/ chordLen radius)) ;; approximate, not exact
  (setq maxArcLen (* radius fullAngleRad))
  (if (> arcLen maxArcLen)
    (progn
      (prompt (strcat "\nWarning: arc length exceeds implied arc length (" (rtos maxArcLen 2 4) "). Command canceled."))
      (exit)
    )
  )

  ;; Compute angle in radians subtended by arcLen
  (setq angleRad (/ arcLen radius))

  ;; Compute angle from center to start point
  (setq baseAngle (atan (- (cadr p1) (cadr center)) (- (car p1) (car center))))

  ;; Compute end point of arc
  (setq endAngle (+ baseAngle angleRad))
  (setq endAnglePt
        (list
          (+ (car center) (* radius (cos endAngle)))
          (+ (cadr center) (* radius (sin endAngle)))
          0.0
        )
  )

  ;; Draw arc from p1 to endAnglePt with center
  (entmake
    (list
      (cons 0 "ARC")
      (cons 10 center)           ; Center
      (cons 40 radius)           ; Radius
      (cons 50 baseAngle)        ; Start angle in radians
      (cons 51 endAngle)         ; End angle in radians
    )
  )

  (princ "\nNew arc drawn.")
  (princ)
)
