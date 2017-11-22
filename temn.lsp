(defun c:temn (/	 ExcelFile$	     SheetName$
	       MaxRange$ xbase	   ybase     C1	       step
	       C2	 C3	   step
	      )
  (vl-load-com)
  
  (defun GetExcel (ExcelFile$ SheetName$ MaxRange$ / Column# ColumnRow@ Data@ ExcelRange^
  ExcelValue ExcelValue ExcelVariant^ MaxColumn# MaxRow# Range$ Row# Worksheet)
  (if (= (type ExcelFile$) 'STR)
    (if (not (findfile ExcelFile$))
      (progn
        (alert (strcat "Excel file " ExcelFile$ " not found."))
        (exit)
      );progn
    );if
    (progn
      (alert "Excel file not specified.")
      (exit)
    );progn
  );if
  (gc)
  (if (setq *ExcelApp% (vlax-get-object "Excel.Application"))
    (progn
      (alert "Close all Excel spreadsheets to continue!")
      (vlax-release-object *ExcelApp%)(gc)
    );progn
  );if
  (setq ExcelFile$ (findfile ExcelFile$))
  (setq *ExcelApp% (vlax-get-or-create-object "Excel.Application"))
  (vlax-invoke-method (vlax-get-property *ExcelApp% 'WorkBooks) 'Open ExcelFile$)
  (if SheetName$
    (vlax-for Worksheet (vlax-get-property *ExcelApp% "Sheets")
      (if (= (vlax-get-property Worksheet "Name") SheetName$)
        (vlax-invoke-method Worksheet "Activate")
      );if
    );vlax-for
  );if
  (if MaxRange$
    (progn
      (setq ColumnRow@ (ColumnRow MaxRange$))
      (setq MaxColumn# (nth 0 ColumnRow@))
      (setq MaxRow# (nth 1 ColumnRow@))
    );progn
    (progn
      (setq CurRegion (vlax-get-property (vlax-get-property
        (vlax-get-property *ExcelApp% "ActiveSheet") "Range" "A1") "CurrentRegion")
      );setq
      (setq MaxRow# (vlax-get-property (vlax-get-property CurRegion "Rows") "Count"))
      (setq MaxColumn# (vlax-get-property (vlax-get-property CurRegion "Columns") "Count"))
    );progn
  );if
  (setq *ExcelData@ nil)
  (setq Row# 1)
  (repeat MaxRow#
    (setq Data@ nil)
    (setq Column# 1)
    (repeat MaxColumn#
      (setq Range$ (strcat (Number2Alpha Column#)(itoa Row#)))
      (setq ExcelRange^ (vlax-get-property *ExcelApp% "Range" Range$))
      (setq ExcelVariant^ (vlax-get-property ExcelRange^ 'Value))
      (setq ExcelValue (vlax-variant-value ExcelVariant^))
      (setq ExcelValue
        (cond
          ((= (type ExcelValue) 'INT) (itoa ExcelValue))
          ((= (type ExcelValue) 'REAL) (rtosr ExcelValue))
          ((= (type ExcelValue) 'STR) (vl-string-trim " " ExcelValue))
          ((/= (type ExcelValue) 'STR) "")
        );cond
      );setq
      (setq Data@ (append Data@ (list ExcelValue)))
      (setq Column# (1+ Column#))
    );repeat
    (setq *ExcelData@ (append *ExcelData@ (list Data@)))
    (setq Row# (1+ Row#))
  );repeat
  (vlax-invoke-method (vlax-get-property *ExcelApp% "ActiveWorkbook") 'Close :vlax-False)
  (vlax-invoke-method *ExcelApp% 'Quit)
  (vlax-release-object *ExcelApp%)(gc)
  (setq *ExcelApp% nil)
  *ExcelData@
);defun GetExcel
  
  
  
  
  (if (progn
	(setq ExcelFile$
	       (getfiled "Select Excel File:"
			 (getvar "dwgprefix")
			 "XLSX;XLS"
			 4
	       )
	)
	(setq SheetName$ "1")
	(setq MaxRange$ "L30")
	(GetExcel ExcelFile$ SheetName$ MaxRange$)
      )
    (progn
      (setq base_point (getpoint " \nPoint: ")
	    xbase      (car base_point)
	    ybase      (car (cdr base_point))
	    C1	       "start"
	    step       0
      )
      (setq i 2)
      (setq ii 1)
      (while (not (= C1 ""))
	(setq Cell1$ (strcat "B" (itoa i))) ;verhnaya nadpis'
	(setq Cell2$ (strcat "C" (itoa i))) ;nadpis' na kabele
	(setq Cell3$ (strcat "D" (itoa i))) ;tehnich oborudovanie
	(setq C1 (GetCell Cell1$))	;Or you can just use the global *ExcelData@ list
	(setq C2 (GetCell Cell2$))
	(setq C3 (GetCell Cell3$))
	(entmakex
	  (list
	    '(0 . "MTEXT")
	    '(100 . "AcDbEntity")
	    '(100 . "AcDbMText")
	    '(8 . "#TEMNIKOV_note")
	    (cons
	      10
	      (list (+ xbase step) (+ ybase 100))
	    )
	    '(71 . 5)
	    '(72 . 5)
	    '(40 . 2.5)
	    (cons 1
		  C1
	    )
	    '(7 . "STANDARD")
	    '(11 1.0 0.0 0.0)
	    (cons 50 1.5708)
	    '(73 . 1)
	    '(44 . 1.0)
	  )
	)
	(entmakex
	  (list
	    '(0 . "MTEXT")
	    '(100 . "AcDbEntity")
	    '(100 . "AcDbMText")
	    '(8 . "#TEMNIKOV_note")
	    (cons
	      10
	      (list (+ xbase step)
		    (+ ybase 50)
	      )
	    )
	    '(71 . 5)
	    '(72 . 5)
	    '(40 . 2.5)
	    (cons 1
		  C2
	    )
	    '(7 . "STANDARD")
	    '(11 1.0 0.0 0.0)
	    (cons 50 1.5708)
	    '(73 . 1)
	    '(44 . 1.0)
	  )
	)

	(entmakex
	  (list
	    '(0 . "MTEXT")
	    '(100 . "AcDbEntity")
	    '(100 . "AcDbMText")
	    '(8 . "#TEMNIKOV_note")
	    (cons
	      10
	      (list (+ xbase step)
		    (+ ybase 0)
	      )
	    )
	    '(71 . 5)
	    '(72 . 5)
	    '(40 . 2.5)
	    (cons 1
		  C3
	    )
	    '(7 . "STANDARD")
	    '(11 1.0 0.0 0.0)
	    (cons 50 0.0)
	    '(73 . 1)
	    '(44 . 1.0)
	  )
	)
	(setq step (+ step 50))
	(setq i (1+ i))
      )
    )
  )
)					;defun
