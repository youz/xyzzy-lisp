
;;; basics

(defmacro $ (obj field &rest args)
  `(ole-method ,obj ',field ,@args))

(defmacro $$ (obj &rest fields)
  (reduce #'(lambda (form field)
	      `($ ,form ,@(if (listp field) field (list field))))
	  fields :initial-value obj))

(defmacro $to (obj &rest methods)
  `(progn
     ,@(mapcar #'(lambda (a) `($ ,obj ,@(if (listp a) a (list a))))
	       methods)))

(defmacro $fn (method &rest vars)
  (let ((obj (gensym)))
    `#'(lambda (,obj ,@vars)
	 (ole-method ,obj ',method ,@vars))))

(defmacro $new (sym)
  `(ole-create-object (symbol-name ',sym)))


;;; Utilities for Excel

(defmacro with-xl ((xl &key (show :after) quiet) &body body)
  `(let ((,xl ($new Excel.Application)))
     (setf ($ ,xl DisplayAlerts) ,(not quiet))
     ,@(case show
	 (:always `((setf ($ ,xl Visible t)) ,@body))
	 (:after  `(,@body (setf ($ ,xl Visible) t)))
	 (t `(,@body ($ ,xl Quit) (setq ,xl nil) (gc))))))

(defmacro with-book ((wb file xl &key close) &body body)
  (when close
    (nconc body `(($ ,wb Close))))
  `(let ((,wb ($$ ,xl Workbooks (Open ,file))))
     ,@body
     (setq ,wb nil)
     #+nil(gc)))

(defmacro with-new-book ((wb xl &key close) &body body)
  (when close
    (nconc body `(($ ,wb Close))))
  `(let ((,wb ($$ ,xl Workbooks Add)))
     ,@body
     (setq ,wb nil)
     #+nil(gc)))

(setf (get 'with-xl 'lisp-indent-hook) 1
      (get 'with-book 'lisp-indent-hook) 1
      (get 'with-new-book 'lisp-indent-hook) 1)


;;; some tests

(defun test-dic ()
  (let ((dic ($new Scripting.Dictionary)))
    ($to dic
      (Add 1 "foo")
      (Add 2 "bar")
      (Add 3 "baz"))
    (ole-for-each (k dic)
      (format t "~A -> ~A~%" k ($ dic Item k))))
  (gc))


(defun test-drive (path)
  (let* ((fso ($new Scripting.FileSystemObject))
	 (drv ($ fso GetDrive ($ fso GetDriveName path))))
    (format t "Drive: ~A~% Name: ~A~% Type: ~A~% SerialNumber: ~A~% FreeSpace: ~A / ~A kb"
	    ($ drv DriveLetter)
	    ($ drv VolumeName)
	    ($ drv DriveType)
	    ($ drv SerialNumber)
	    (floor ($ drv FreeSpace) 1024)
	    (floor ($ drv TotalSize) 1024)))
  (gc))


(defun test-excel ()
  (let* ((xl ($new Excel.Application))
	 (wb ($$ xl Workbooks Add))
	 (ws ($ wb Worksheets 1)))
    (setf ($ xl Visible) t
	  ($$ ws (Range "A1:E1") Value) '("" "North" "South" "East" "West")
	  ($$ ws (Range "A2:E2") Value) #("data1" 5.2 10 8 10)
	  ($$ ws (Range "A3:E3") Value) #("data2" 4 1 2 6))
    
    ($$ ws ChartObjects (Add 50 50 300 200)
	   Chart (SetSourceData ($ ws Range "A1:E3")))

    (sit-for 5)
    
    (setf ($ wb Saved) t)
    ($$ xl ActiveWorkbook (Close 0))
    ($ xl Quit))
  
  (gc))


#|
;; using ole-reader

(defun excel-test2 ()
  (let* ((xl ($new Excel.Application))
	 (wb #{xl.Workbooks.Add})
	 (ws #{wb.Worksheets[1]})
	 (rng #{ws.Range["A1:E3"]}))
    (setf #{xl.Visible} t
	  #{ws.Range["A1:E1"].Value} '("" "North" "South" "East" "West")
	  #{ws.Range["A2:E2"].Value} #("data1" 5.2 10 8 10)
	  #{ws.Range["A3:E3"].Value} #("data2" 4 1 2 6))
    #{ws.ChartObjects.Add[50 50 300 200].Chart.SetSourceData[rng]}

    (sit-for 5)
    (setf #{wb.Saved} t)
    #{xl.ActiveWorkbook.Close[0]}
    #{xl.Quit})
  
  (gc))

|#
