;;; Сделать изыскания геодезии с координатой Z
;;; Для корректной работы программы необходимо что бы полилинии были типа - 3D полилиния
;;; Далее выделяются все полилинии и запускается скрипт командой SC
;;; Скрипт копирует уже существующую Z координату в первой вершине полилинии и записывает её для всех остальных вершин
(defun C:SC (/ vert_kol id_prim) 
    (if
		(setq 
		cmd (getvar 'cmdecho)
       	i -1
		*dummy* (prompt "\nВыбери 3D полилинии для перезаписи Z координаты...")
       	ss  (ssget "_:L" '((0 . "POLYLINE")))
		)
		(progn
			(setq id_file (open "C:\\Users\\Leovante\\Desktop\\3d.scr" "w"))
			(set 'enter (chr 10))
			(repeat (sslength ss)         		                        ;(repeat <число> <выражение>...); sslength - количество примитивов в наборе ss
				(set 'name_pline '_3Dpoly)
				(princ name_pline id_file)
				(princ enter id_file)
				(setq id_prim (ssname ss (setq i (1+ i)))) 				;ssname-извлекает примитив из набора по пор¤дковому номеру. (ssname <набор> <номер>)
				(setq vert_kol (/ (length (vlax-get (vlax-ename->vla-object id_prim) 'Coordinates)) 3))
				(setq vert_kol_sp (vlax-get (vlax-ename->vla-object id_prim) 'Coordinates))
				(setq p0 (vlax-curve-getStartPoint (ssname ss i))   	;(1.73962 2.12561 0.0) ; получил координаты первой вершины полилинии
				coord_z (caddr p0))										;извлечение z координаты первой точки в виде 3.00000
				(setq ii 1)
				(while (<= ii vert_kol)                                 ;сделать цикл дл¤ количества вершин
					(setq coord_x (car vert_kol_sp)) 					;(nth 3 '(a b c d e))	возвращает	D
					(setq coord_y (cadr vert_kol_sp)) 					;assoc возвращает из списка позицию с кодом 10 (assoc <код> <список>)
					(set 'coord_xyz (strcat (rtos coord_x) "," (rtos coord_y) "," (rtos coord_z)))
					(princ coord_xyz id_file)
					(princ enter id_file)
					(setq vert_kol_sp (cdr (cdr (cdr vert_kol_sp))))
					(setq ii (+ ii 1))
				)
			(set 'enter (chr 10))
			(princ enter id_file)
			)
		(close id_file)	
		)
	)
)