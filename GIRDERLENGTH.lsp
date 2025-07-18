(defun c:GIRDERLENGTH ( / midpt length halflen p1 p2 angle actual-length)

  ;; Prompt for midpoint
  (setq midpt (getpoint "\nSelect midpoint of new line: "))
  (if (not midpt)
    (progn (princ "\nNo point selected. Exiting.") (exit))
  )

  ;; Prompt for total length
  (setq length (getreal "\nEnter total length of the new line (e.g., 25.375): "))
  (if (or (not length) (<= length 0))
    (progn (princ "\nInvalid length. Exiting.") (exit))
  )

  ;; Prompt for direction
  (initget 1)
  (setq angle (getangle "\nSpecify direction of the line (in degrees): "))

  ;; Compute half length
  (setq halflen (/ length 2.0))

  ;; Calculate endpoints using polar from midpoint
  (setq p1 (polar midpt (+ angle pi) halflen))
  (setq p2 (polar midpt angle halflen))

  ;; Draw line
  (command "_.LINE" p1 p2 "")

  ;; Measure actual length
  (setq actual-length (distance p1 p2))

  ;; Display precision results
  (princ (strcat
    "\nExpected length: " (rtos length 2 3)
    "\nActual drawn length: " (rtos actual-length 2 6)
  ))

  (princ)
)
