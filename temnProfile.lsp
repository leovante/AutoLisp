;;;(1)FORM													
(defun c:test (/	    ss		 sn	      ss1
	       ss2	    ss3		 elev_grud_list
	       elev_invert_list		 dl_otr_list  maxDraw
	       minDraw
	      )
  (entmakex '((0 . "LAYER")
	      (100 . "AcDbSymbolTableRecord")
	      (100 . "AcDbLayerTableRecord")
	      (2 . "#TEMNIKOV_table")
	      (70 . 0)
	      (62 . 253)
	     )
  )
  (entmakex '((0 . "LAYER")
	      (100 . "AcDbSymbolTableRecord")
	      (100 . "AcDbLayerTableRecord")
	      (2 . "#TEMNIKOV_text")
	      (70 . 0)
	      (62 . 96)
	     )
  )
  (entmakex '((0 . "LAYER")
	      (100 . "AcDbSymbolTableRecord")
	      (100 . "AcDbLayerTableRecord")
	      (2 . "#TEMNIKOV_note")
	      (70 . 0)
	      (62 . 124)
	     )
  )
  (entmakex '((0 . "LAYER")
	      (100 . "AcDbSymbolTableRecord")
	      (100 . "AcDbLayerTableRecord")
	      (2 . "#TEMNIKOV_tube")
	      (70 . 0)
	      (62 . 30)
	     )
  )
  (entmakex '((0 . "LAYER")
	      (100 . "AcDbSymbolTableRecord")
	      (100 . "AcDbLayerTableRecord")
	      (2 . "#TEMNIKOV_manhole")
	      (70 . 0)
	      (62 . 251)
	     )
  )
  (entmakex '((0 . "LAYER")
	      (100 . "AcDbSymbolTableRecord")
	      (100 . "AcDbLayerTableRecord")
	      (2 . "#TEMNIKOV_ground")
	      (70 . 0)
	      (62 . 54)
	     )
  )
  (if
    (progn
      (setq
	ss (ssget '((0 . "TEXT,LWPOLYLINE")))
					;nabor primitivov
	i  -1
	n  (sslength ss)
      )
      (initget 1)
      (setq sn (getint "Введите номер первого колодца <0>: "))
    )
     (progn
       (setq ss1 (ssadd))
       (setq ss2 (ssadd))
       (setq ss3 (ssadd))
       ;;раскладываем примитивы по трем наборам
       (while (>= n 2)
	 (ssadd (ssname ss (setq i (1+ i))) ss1)
	 (ssadd (ssname ss (setq i (1+ i))) ss2)
	 (if (ssname ss (setq i (1+ i)))
	   (ssadd (ssname ss i) ss3)
	   (not null)
	 )
	 (setq n (- n 3))
       )
       (setq i 0)
       ;;выт€гиваем из наборов значени€ и составл€ем из них списки
       (repeat (sslength ss1)
	 ;;gruond
	 (setq str1_1
		(substr
		  (cdr (assoc 1 (entget (ssname ss1 i))))
		  1
		  3
		)
	 )				;строка значений перед зап€той
	 (setq str1_2
		(substr
		  (cdr (assoc 1 (entget (ssname ss1 i))))
		  5
		  2
		)
	 )				;строка значений после зап€той
	 (setq elev_grud (list (atof (strcat str1_1 "." str1_2))))
	 (setq elev_grud_list (append elev_grud_list elev_grud))
	 ;;invert
	 (setq str2_1
		(substr
		  (cdr (assoc 1 (entget (ssname ss2 i))))
		  1
		  3
		)
	 )
	 (setq str2_2
		(substr
		  (cdr (assoc 1 (entget (ssname ss2 i))))
		  5
		  2
		)
	 )
	 (setq elev_invert (list (atof (strcat str2_1 "." str2_2))))
	 (setq elev_invert_list (append elev_invert_list elev_invert))
	 (setq i (1+ i))
       )
       (setq i -1)
       (repeat (sslength ss3)
	 ;;length
	 (setq id_prim (ssname ss3 (setq i (1+ i))))
	 (setq vert_kol_sp
		(vlax-get (vlax-ename->vla-object (ssname ss3 i))
			  'Coordinates
		)
	 )
	 (setq
	   p1 (vlax-curve-getStartPoint (ssname ss3 i))
	 )
	 (setq p2 (vlax-curve-getEndPoint (ssname ss3 i))
	 )
	 (setq dl_otr (list (distance p1 p2))) ;opredelenie rasstoyania
	 (setq dl_otr_list (append dl_otr_list dl_otr))
       )
       ;;maksimalnaya i minimalnaya tochka
       (setq maxDraw (apply 'max elev_grud_list))
       (setq minDraw (apply 'min elev_invert_list))
       (test1 sn	    ss1		  ss2		ss3
	      elev_grud_list		  elev_invert_list
	      dl_otr_list   maxDraw	  minDraw
	     )
     )
  )
)
;;;(2)FORM 													
;;; postroenie 														
(defun test1 (sn	    ss1		  ss2		ss3
	      elev_grud_list		  elev_invert_list
	      dl_otr_list   maxDraw	  minDraw	/
	      base_point    xbase	  ybase		XdrawPoint
	      YdrawPoint    YbaseMinPoint nLevel	MX
	      MY	    yVerhList	  yNizList	XdrawPointList
	     )
  ;;bazovaya tochka
  (setq	base_point    (getpoint " \nPoint: ")
	xbase	      (car base_point)
	ybase	      (car (cdr base_point))
	;;bazovaya tochka grafika
	XdrawPoint    (+ xbase 85.0)
	YdrawPoint    (+ ybase 100.0)
	;;otmetka niza lineyki
	YbaseMinPoint (fix (- minDraw 1))
	nLevel	      (fix (+ (- maxDraw YbaseMinPoint) 2))
	;;mashtabi
	MX	      4
	MY	      10
  )
;;;Rasstanovka kolodcev													
  (setq	knumb 0
	nLength
	 -1
  )
  (repeat (sslength ss1)
    (setq yverh		 (+ YdrawPoint
			    (* MY (- (nth knumb elev_grud_list) YbaseMinPoint))
			 )
	  yniz		 (+ YdrawPoint
			    (* MY (- (nth knumb elev_invert_list) YbaseMinPoint))
			 )
	  yVerhList	 (append yVerhList (list yverh))
	  yNizList	 (append yNizList (list yniz))
	  ;;coordinati tochek x & y
	  XdrawPointList (append XdrawPointList (list XdrawPoint))
    )
    ;; nadpis nad kolodcem glubina
    (entmakex
      (list
	'(0 . "MTEXT")
	'(100 . "AcDbEntity")
	'(100 . "AcDbMText")
	'(8 . "#TEMNIKOV_note")
	(cons
	  10
	  (list (- XdrawPoint 2) (+ yverh 5))
	)
	'(71 . 5)
	'(72 . 5)
	'(40 . 2.5)
	(cons 1
	      (rtos (- (nth knumb elev_grud_list)
		       (nth knumb elev_invert_list)
		    )
		    2
		    2
	      )
	)
	'(7 . "STANDARD")
	'(11 1.0 0.0 0.0)
	(cons 50 1.5708)
	'(73 . 1)
	'(44 . 1.0)
      )
    )
    ;; kolodec
    (entmakex
      (list
	(cons 0 "LWPOLYLINE")
	(cons 100 "AcDbEntity")
	(cons 100 "AcDbPolyline")
	(cons 8 "#TEMNIKOV_manhole")
	(cons 90 4)
	(cons 43 0.1)
	(cons 10 (list (+ XdrawPoint 1) (- yniz 1)))
	(cons 10 (list (+ XdrawPoint 1) yverh))
	'(42 . 0.5)
	(cons 10 (list (- XdrawPoint 1) yverh))
	(cons 10 (list (- XdrawPoint 1) (- yniz 1)))
	(cons 70 1)
	'(210 0.0 0.0 1.0)
      )
    )
    ;; vertikalnie linii
    (entmakex
      (list
	(cons 0 "LWPOLYLINE")
	(cons 100 "AcDbEntity")
	(cons 100 "AcDbPolyline")
	(cons 8 "#TEMNIKOV_table")
	(cons 90 4)
	(cons 43 0.0)
	(cons 10 (list XdrawPoint (+ ybase 55)))
	(cons 10 (list XdrawPoint (- yniz 1)))
	(cons 70 1)
      )
    )
    ;; text niz kolodca
    (entmakex
      (list
	'(0 . "MTEXT")
	'(100 . "AcDbEntity")
	'(100 . "AcDbMText")
	'(8 . "#TEMNIKOV_text")
	(cons 10 (list (- XdrawPoint 2) (+ ybase 93)))
	'(71 . 5)
	'(72 . 5)
	'(40 . 2.5)
	(cons 1 (rtos (nth knumb elev_invert_list) 2 2))
	'(7 . "STANDARD")
	'(11 1.0 0.0 0.0)
	(cons 50 1.5708)
	'(73 . 1)
	'(44 . 1.0)
      )
    )
    ;; text proektnaya otmetka zemli
    (entmakex
      (list
	'(0 . "MTEXT")
	'(100 . "AcDbEntity")
	'(100 . "AcDbMText")
	'(8 . "#TEMNIKOV_text")
	(cons 10 (list (- XdrawPoint 2) (+ ybase 78)))
	'(71 . 5)
	'(72 . 5)
	'(40 . 2.5)
	(cons 1 (rtos (nth knumb elev_grud_list) 2 2))
	'(7 . "STANDARD")
	'(11 1.0 0.0 0.0)
	(cons 50 1.5708)
	'(73 . 1)
	'(44 . 1.0)
      )
    )
    ;; text naturnaya otmetka zemli
    (entmakex
      (list
	'(0 . "MTEXT")
	'(100 . "AcDbEntity")
	'(100 . "AcDbMText")
	'(8 . "#TEMNIKOV_text")
	(cons 10 (list (- XdrawPoint 2) (+ ybase 63)))
	'(71 . 5)
	'(72 . 5)
	'(40 . 2.5)
	(cons 1 (rtos (nth knumb elev_grud_list) 2 2))
	'(7 . "STANDARD")
	'(11 1.0 0.0 0.0)
	(cons 50 1.5708)
	'(73 . 1)
	'(44 . 1.0)
      )
    )
    ;;vertikalnoe razdelenie nizhnih dvuh strok
    (entmakex
      (list
	(cons 0 "LWPOLYLINE")
	(cons 100 "AcDbEntity")
	(cons 100 "AcDbPolyline")
	(cons 8 "#TEMNIKOV_table")
	(cons 90 4)
	(cons 43 0.0)
	(cons 10 (list XdrawPoint ybase))
	(cons 10 (list XdrawPoint (+ ybase 20)))
	(cons 70 1)
      )
    )
    ;; nomer kolodca
    (entmakex
      (list
	'(0 . "MTEXT")
	'(100 . "AcDbEntity")
	'(100 . "AcDbMText")
	'(8 . "#TEMNIKOV_note")
	(cons
	  10
	  (list (- XdrawPoint 2) (+ ybase 5))
	)
	'(71 . 5)
	'(72 . 5)
	'(40 . 2.5)
	(cons 1
	      (rtos sn
		    2
		    0
	      )
	)
	'(7 . "STANDARD")
	'(11 1.0 0.0 0.0)
	(cons 50 1.5708)
	'(73 . 1)
	'(44 . 1.0)
      )
    )
    (setq sn (1+ sn))
    ;;proverka na nalichie posledneggo rasstoyaniya
    (if	(ssname ss3 (setq nLength (1+ nLength)))
      (progn
	;;stroka rasstoyanie
	(entmakex
	  (list
	    '(0 . "MTEXT")
	    '(100 . "AcDbEntity")
	    '(100 . "AcDbMText")
	    '(8 . "#TEMNIKOV_text")
	    (cons
	      10
	      (list (+ XdrawPoint (/ (nth nLength dl_otr_list) 2))
		    (+ ybase 15)
	      )
	    )
	    '(71 . 5)
	    '(72 . 5)
	    '(40 . 2.5)
	    (cons 1 (rtos (nth nLength dl_otr_list) 2 2))
	    '(7 . "STANDARD")
	    '(11 1.0 0.0 0.0)
	    (cons 50 0.0)
	    '(73 . 1)
	    '(44 . 1)
	  )
	)
	(setq XdrawPoint
	       (+ XdrawPoint (nth nLength dl_otr_list))
	)
      )
      (progn
	(entmakex
	  (list
	    (cons 0 "LWPOLYLINE")
	    (cons 100 "AcDbEntity")
	    (cons 100 "AcDbPolyline")
	    (cons 8 "#TEMNIKOV_table")
	    (cons 90 4)
	    (cons 43 0.0)
	    (cons 10 (list XdrawPoint ybase))
	    (cons 10 (list XdrawPoint (+ ybase 55)))
	    (cons 70 1)
	  )
	)
      )
    )
    (setq knumb (1+ knumb))
  )
  ;;risovanie trubi
  (setq knumb 1)
  (repeat (sslength ss3)
    (entmakex
      (list
	(cons 0 "LWPOLYLINE")
	(cons 100 "AcDbEntity")
	(cons 100 "AcDbPolyline")
	(cons 8 "#TEMNIKOV_tube")
	(cons 90 4)
	(cons 43 0.1)
	(cons
	  10
	  (list (- (nth knumb XdrawPointList) 1) (nth knumb yNizList))
	)
	(cons 10
	      (list (- (nth knumb XdrawPointList) 1)
		    (+ (nth knumb yNizList) 1.58)
	      )
	)
	(cons 10
	      (list (+ (nth (- knumb 1) XdrawPointList) 1)
		    (+ (nth (- knumb 1) yNizList) 1.58)
	      )

	)
	(cons 10
	      (list (+ (nth (- knumb 1) XdrawPointList) 1)
		    (nth (- knumb 1) yNizList)
	      )
	)
	(cons 70 1)
      )
    )
    (setq knumb (1+ knumb))
  )
  ;;risovanie urovnya grunta
  (setq knumb 1)
  (repeat (sslength ss3)
    (entmakex
      (list
	(cons 0 "LWPOLYLINE")
	(cons 100 "AcDbEntity")
	(cons 100 "AcDbPolyline")
	(cons 8 "#TEMNIKOV_ground")
	(cons 90 4)
	(cons 43 0.1)
	(cons
	  10
	  (list	(- (nth knumb XdrawPointList) 1)
		(nth knumb yVerhList)
	  )
	)
	(cons 10
	      (list (+ (nth (- knumb 1) XdrawPointList) 1)
		    (nth (- knumb 1) yVerhList)
	      )
	)
	(cons 70 1)
      )
    )
    (setq knumb (1+ knumb))
  )					;konec rasstanovki kolodcev i cikla
  (oformlenie
    ss3		    elev_grud_list  elev_invert_list
    base_point	    xbase	    ybase	    XdrawPoint
    XdrawPointList  maxDraw	    minDraw
   )
)
;;; (3)FORM													
;;; Shapka i polki														
(defun oformlenie
		  (ss3		 elev_grud_list
		   elev_invert_list	       base_point
		   xbase	 ybase	       XdrawPoint
		   XdrawPointList	       maxDraw
		   minDraw	 /	       H
		   Ymax
		  )
  (entmakex				;tolstoe obramlenie shapki
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity")
      (cons 100 "AcDbPolyline")
      (cons 8 "#TEMNIKOV_table")
      (cons 90 4)
      (cons 43 0.5)
      (cons 10 base_point)
      (cons 10 (list xbase (+ ybase 100)))
      (cons 10 (list (+ xbase 60) (+ ybase 100)))
      (cons 10 (list (+ xbase 60) ybase))
      (cons 70 1)			;замкнуть полилинию
    )
  )

  (setq	H  (list 10.0 20.0 30.0 40.0 55.0 70.0 85.0 100.0)
					;etazhi polki shapki
	Hi 0
  )
  (repeat (length H)
    (entmakex
      (list
	(cons 0 "LWPOLYLINE")
	(cons 100 "AcDbEntity")
	(cons 100 "AcDbPolyline")
	(cons 8 "#TEMNIKOV_table")
	(cons 90 4)
	(cons 39 0.15)
	(cons 70 1)
	(cons 10 (list xbase (+ ybase (nth Hi H))))
	(cons 10 (list (+ xbase 60.0) (+ ybase (nth Hi H))))
      )
    )
    (entmakex
      (list
	(cons 0 "LWPOLYLINE")
	(cons 100 "AcDbEntity")
	(cons 100 "AcDbPolyline")
	(cons 8 "#TEMNIKOV_table")
	(cons 90 4)
	(cons 39 0.15)
	(cons 70 1)
	(cons 10 (list (+ xbase 85.0) (+ ybase (nth Hi H))))
	(cons 10 (list XdrawPoint (+ ybase (nth Hi H))))
      )
    )
    (setq Hi (1+ Hi))
  )					;konec cikla polok shapki
  (entmakex				;naklonnaya poloska v shapke stroki dlina\uklon
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity")
      (cons 100 "AcDbPolyline")
      (cons 8 "#TEMNIKOV_table")
      (cons 90 4)
      (cons 10 (list xbase (+ ybase 30)))
      (cons 10 (list (+ xbase 60) (+ ybase 20)))
      (cons 70 1)
    )
  )
  ;;lineyka
  (setq
    Hi 0
  )
  (repeat (/ nLevel 2)
    (entmakex
      (list
	(cons 0 "LWPOLYLINE")
	(cons 100 "AcDbEntity")
	(cons 100 "AcDbPolyline")
	(cons 8 "#TEMNIKOV_table")
	(cons 90 4)
	(cons 43 1.5)
	(cons 39 0.15)
	(cons 70 1)
	(cons 10
	      (list (+ xbase 78.0) (+ ybase (+ 100.0 (* Hi 10))))
	)
	(cons
	  10
	  (list	(+ xbase 78.0)
		(+ ybase (+ 100.0 (+ (* Hi 10) 10)))
	  )
	)
      )
    )
    (entmakex
      (list
	(cons 0 "LWPOLYLINE")
	(cons 100 "AcDbEntity")
	(cons 100 "AcDbPolyline")
	(cons 8 "#TEMNIKOV_table")
	(cons 90 4)
	(cons 43 0.0)
	(cons 39 0.15)
	(cons 70 1)
	(cons 10
	      (list (+ xbase 85.0) (+ ybase (+ 100.0 (* Hi 10))))
	)
	(cons 10 (list XdrawPoint (+ ybase (+ 100.0 (* Hi 10)))))
      )
    )
    (entmakex
      (list
	(cons 0 "LWPOLYLINE")
	(cons 100 "AcDbEntity")
	(cons 100 "AcDbPolyline")
	(cons 8 "#TEMNIKOV_table")
	(cons 90 4)
	(cons 43 0.0)
	(cons 39 0.15)
	(cons 70 1)
	(cons 10
	      (list (+ xbase 85.0) (+ ybase (+ 110.0 (* Hi 10))))
	)
	(cons 10 (list XdrawPoint (+ ybase (+ 110.0 (* Hi 10)))))
      )
    )
    (setq Ymax (+ ybase (+ 110.0 (+ (* Hi 10) 10))))
    (setq Hi (+ Hi 2))
  )
  ;;text lineyki
  (setq Hi -1)
  (repeat nLevel
    (setq Hi (+ Hi 1))
    (entmakex
      (list
	'(0 . "MTEXT")
	'(100 . "AcDbEntity")
	'(100 . "AcDbMText")
	'(8 . "#TEMNIKOV_text")
	(cons 10 (list (+ xbase 69) (+ ybase 100 (* Hi 10))))
	'(71 . 7)
	'(72 . 5)
	'(40 . 2.5)
	(cons 1 (rtos (+ YbaseMinPoint Hi) 2 1))
	'(7 . "STANDARD")
	'(11 1.0 0.0 0.0)
	'(73 . 1)
	'(44 . 1.0)
      )
    )
  )					;konec texta lineyki
  ;;Obramlenie lineyki
  (entmakex
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity")
      (cons 100 "AcDbPolyline")
      (cons 8 "#TEMNIKOV_table")
      (cons 90 4)
      (cons 39 0.0)
      (cons 43 0.0)
      (cons 10 (list (+ xbase 77.25) (+ ybase 100)))
      (cons 10
	    (list (+ xbase 77.25) (+ ybase (+ 100.0 (* Hi 10))))
      )
      (cons 10
	    (list (+ xbase 78.75) (+ ybase (+ 100.0 (* Hi 10))))
      )
      (cons 10 (list (+ xbase 78.75) (+ ybase 100)))
      (cons 70 0)			; замкнуть полилинию
    )
  )
;;;текст в шапке
  (entmakex
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      '(100 . "AcDbMText")
      '(8 . "#TEMNIKOV_text")
      (cons 10 (list xbase (+ ybase 115.0)))
      '(71 . 7)
      '(72 . 5)
      '(40 . 2.5)
      '(1 . "Mгор. 1:1000")
      '(7 . "STANDARD")
      '(11 1.0 0.0 0.0)
      '(73 . 1)
      '(44 . 1.0)
    )
  )					; конец шапки
  (entmakex
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      '(100 . "AcDbMText")
      '(8 . "#TEMNIKOV_text")
      (cons 10 (list xbase (+ ybase 110.0)))
      '(71 . 7)
      '(72 . 5)
      '(40 . 2.5)
      '(1 . "Mвер. 1:100")
      '(7 . "STANDARD")
      '(11 1.0 0.0 0.0)
      '(73 . 1)
      '(44 . 1.0)
    )
  )
  (entmakex
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      '(100 . "AcDbMText")
      '(8 . "#TEMNIKOV_text")
      (cons 10 (list xbase (+ ybase 101.0)))
      '(71 . 7)
      '(72 . 5)
      '(40 . 2.5)
      '(1 . "Условный горизонт")
      '(7 . "STANDARD")
      '(11 1.0 0.0 0.0)
      '(73 . 1)
      '(44 . 1.0)
    )
  )
  (entmakex
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      '(100 . "AcDbMText")
      '(8 . "#TEMNIKOV_text")
      (cons 10 (list (+ xbase 30) (+ ybase 92.0)))
      '(71 . 5)
      '(72 . 5)
      '(40 . 2.5)
      '(1 . "Отметка низа или \nлотка трубы (м)")
      '(7 . "STANDARD")
      '(11 1.0 0.0 0.0)
      '(73 . 1)
      '(44 . 1.0)
    )
  )
  (entmakex
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      '(100 . "AcDbMText")
      '(8 . "#TEMNIKOV_text")
      (cons 10 (list (+ xbase 30) (+ ybase 78.0)))
      '(71 . 5)
      '(72 . 5)
      '(40 . 2.5)
      '(1 . "Проектная отметка \nземли (м)")
      '(7 . "STANDARD")
      '(11 1.0 0.0 0.0)
      '(73 . 1)
      '(44 . 1.0)
    )
  )
  (entmakex
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      '(100 . "AcDbMText")
      '(8 . "#TEMNIKOV_text")
      (cons 10 (list (+ xbase 30) (+ ybase 63.0)))
      '(71 . 5)
      '(72 . 5)
      '(40 . 2.5)
      '(1 . "Натурная отметка \nземли (м)")
      '(7 . "STANDARD")
      '(11 1.0 0.0 0.0)
      '(73 . 1)
      '(44 . 1.0)
    )
  )
  (entmakex
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      '(100 . "AcDbMText")
      '(8 . "#TEMNIKOV_text")
      (cons 10 (list (+ xbase 30) (+ ybase 47.0)))
      '(71 . 5)
      '(72 . 5)
      '(40 . 2.5)
      '(1 . "Обозначение трубы \nи изоляции")
      '(7 . "STANDARD")
      '(11 1.0 0.0 0.0)
      '(73 . 1)
      '(44 . 1.0)
    )
  )
  (entmakex
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      '(100 . "AcDbMText")
      '(8 . "#TEMNIKOV_text")
      (cons 10 (list (+ xbase 30) (+ ybase 35.0)))
      '(71 . 5)
      '(72 . 5)
      '(40 . 2.5)
      '(1 . "Основание траншеи \nи толщина основания (м)")
      '(7 . "STANDARD")
      '(11 1.0 0.0 0.0)
      '(73 . 1)
      '(44 . 1.0)
    )
  )
  (entmakex
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      '(100 . "AcDbMText")
      '(8 . "#TEMNIKOV_text")
      (cons 10 (list (+ xbase 14) (+ ybase 23.0)))
      '(71 . 5)
      '(72 . 5)
      '(40 . 2.5)
      '(1 . "Длина (м)")
      '(7 . "STANDARD")
      '(11 1.0 0.0 0.0)
      '(73 . 1)
      '(44 . 1.0)
    )
  )
  (entmakex
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      '(100 . "AcDbMText")
      '(8 . "#TEMNIKOV_text")
      (cons 10 (list (+ xbase 50) (+ ybase 27.0)))
      '(71 . 5)
      '(72 . 5)
      '(40 . 2.5)
      '(1 . "Уклон ({\\fArial|b0|i1|?2|?34;?})")
      '(7 . "STANDARD")
      '(11 1.0 0.0 0.0)
      '(73 . 1)
      '(44 . 1.0)
    )
  )
  (entmakex
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      '(100 . "AcDbMText")
      '(8 . "#TEMNIKOV_text")
      (cons 10 (list (+ xbase 30) (+ ybase 15.0)))
      '(71 . 5)
      '(72 . 5)
      '(40 . 2.5)
      '(1 . "Расстояние (м)")
      '(7 . "STANDARD")
      '(11 1.0 0.0 0.0)
      '(73 . 1)
      '(44 . 1.0)
    )
  )
  (entmakex
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      '(100 . "AcDbMText")
      '(8 . "#TEMNIKOV_text")
      (cons 10 (list (+ xbase 30) (+ ybase 5)))
      '(71 . 5)
      '(72 . 5)
      '(40 . 2.5)
      '(1 . "Номер колодца, точки \nугла поворота")
      '(7 . "STANDARD")
      '(11 1.0 0.0 0.0)
      '(73 . 1)
      '(44 . 1.0)			;коэффициент межстрочного интервала
    )
  )
  ;; vertikalnaya poloska posle lineyki
  (entmakex
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity")
      (cons 100 "AcDbPolyline")
      (cons 8 "#TEMNIKOV_table")
      (cons 90 4)
      (cons 39 0.0)
      (cons 43 0.0)
      (cons 10 (list (+ xbase 85.0) ybase))
      (cons 10
	    (list (+ xbase 85.0) (+ ybase (+ 100.0 (* Hi 10))))
      )
      (cons 70 0)
    )
  )
  ;; vertikalnaya poloska v konce tablici
  (entmakex
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity")
      (cons 100 "AcDbPolyline")
      (cons 8 "#TEMNIKOV_table")
      (cons 90 4)
      (cons 39 0.0)
      (cons 43 0.0)
      (cons 10 (list (last XdrawPointList) ybase))
      (cons 10
	    (list (last XdrawPointList) (+ ybase (+ 100.0 (* Hi 10))))
      )
      (cons 70 0)
    )
  )
  ;;stroka oboznachenie trubi i izolyacii
  (entmakex
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      '(100 . "AcDbMText")
      '(8 . "#TEMNIKOV_text")
      (cons
	10
	(list (+ (+ 85.0 xbase) (/ (- XdrawPoint (+ 85.0 xbase)) 2))
	      (+ ybase 47.5)
	)
      )
      '(71 . 5)
      '(72 . 5)
      '(40 . 2.5)
      '(1
	.
	"Труба полипропиленовая гофрированная \n'Прагма'SN8 %%c160 ГОСТ Р 54475"
       )
      '(7 . "STANDARD")
      '(11 1.0 0.0 0.0)
      (cons 50 0.0)
      '(73 . 1)
      '(44 . 1.0)
    )
  )
  (entmakex
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      '(100 . "AcDbMText")
      '(8 . "#TEMNIKOV_text")
      (cons
	10
	(list (+ (+ 85.0 xbase) (/ (- XdrawPoint (+ 85.0 xbase)) 2))
	      (+ ybase 35)
	)
      )
      '(71 . 5)
      '(72 . 5)
      '(40 . 2.5)
      '(1 . "Песчаная подготовка \n0,10")
      '(7 . "STANDARD")
      '(11 1.0 0.0 0.0)
      (cons 50 0.0)
      '(73 . 1)
      '(44 . 1.0)
    )
  )
  (entmakex				;самая нижняя полка
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity")
      (cons 100 "AcDbPolyline")
      (cons 8 "#TEMNIKOV_table")
      (cons 90 4)
      (cons 39 0.15)
      (cons 70 1)
      (cons 10 (list (+ xbase 85.0) ybase))
      (cons 10 (list XdrawPoint ybase))
    )
  )
  ;;zapolnenie stroki rasstoyanie dlina uklon
  (setq knumb 0)
  (repeat (sslength ss3)
    (entmakex
      (list
	(cons 0 "LWPOLYLINE")
	(cons 100 "AcDbEntity")
	(cons 100 "AcDbPolyline")
	(cons 8 "#TEMNIKOV_table")
	(cons 90 4)
	(cons 43 0.0)
	(cons
	  10
	  (list	(nth knumb XdrawPointList)
		(+ ybase 30)
	  )
	)
	(cons
	  10
	  (list	(nth (1+ knumb) XdrawPointList)
		(+ ybase 20)
	  )
	)
	(cons 70 1)
      )
    )
    ;;vertikalnaya poloska sprava ot yacheiki dlina\uklon
    (entmakex
      (list
	(cons 0 "LWPOLYLINE")
	(cons 100 "AcDbEntity")
	(cons 100 "AcDbPolyline")
	(cons 8 "#TEMNIKOV_table")
	(cons 90 4)
	(cons 43 0.0)
	(cons
	  10
	  (list	(+ (nth knumb XdrawPointList) (nth knumb dl_otr_list))
		(+ ybase 20)
	  )
	)
	(cons
	  10
	  (list	(+ (nth knumb XdrawPointList) (nth knumb dl_otr_list))
		(+ ybase 30)
	  )
	)
	(cons 70 1)
      )
    )					;konec vertikalnoy otmetki
    ;;dlina
    (entmakex
      (list
	'(0 . "MTEXT")
	'(100 . "AcDbEntity")
	'(100 . "AcDbMText")
	'(8 . "#TEMNIKOV_text")
	(cons
	  10
	  (list	(+ (nth knumb XdrawPointList) 6)
		(+ ybase 23)
	  )
	)
	'(71 . 5)
	'(72 . 5)
	'(40 . 2.5)
	(cons 1 (rtos (nth knumb dl_otr_list) 2 2))
	'(7 . "STANDARD")
	'(11 1.0 0.0 0.0)
	(cons 50 0.0)
	'(73 . 1)
	'(44 . 1.0)
      )
    )
    ;;uklon
    (entmakex
      (list
	'(0 . "MTEXT")
	'(100 . "AcDbEntity")
	'(100 . "AcDbMText")
	'(8 . "#TEMNIKOV_text")
	(cons
	  10
	  (list
	    (+ (nth knumb XdrawPointList) (- (nth knumb dl_otr_list) 7))
	    (+ ybase 27)
	  )
	)
	'(71 . 5)
	'(72 . 5)
	'(40 . 2.5)
	(cons 1
	      (rtos (/ (* (- (nth knumb elev_invert_list)
			     (nth (1+ knumb) elev_invert_list)
			  )
			  1000
		       )
		       (nth knumb dl_otr_list)
		    )
		    2
		    2
	      )
	)
	'(7 . "STANDARD")
	'(11 1.0 0.0 0.0)
	(cons 50 0.0)
	'(73 . 1)
	'(44 . 1.0)
      )
    )
    (setq knumb (1+ knumb))
  )					;konec cikla rasstoyaniya dlina uklon
  (princ)
)
