;;; ������� ��������� �������� � ����������� Z
;;; ��� ���������� ������ ��������� ���������� ��� �� ��������� ���� ���� - 3D ���������
;;; ����� ���������� ��� ��������� � ����������� ������ �������� SC
;;; ������ �������� ��� ������������ Z ���������� � ������ ������� ��������� � ���������� � ��� ���� ��������� ������
(defun C:SC (/ vert_kol id_prim) 
    (if
		(setq 
		cmd (getvar 'cmdecho)
       	i -1
		*dummy* (prompt "\n������ 3D ��������� ��� ���������� Z ����������...")
       	ss  (ssget "_:L" '((0 . "POLYLINE")))
		)
		(progn
			(setq id_file (open "C:\\Users\\Leovante\\Desktop\\3d.scr" "w"))
			(set 'enter (chr 10))
			(repeat (sslength ss)         		                        ;(repeat <�����> <���������>...); sslength - ���������� ���������� � ������ ss
				(set 'name_pline '_3Dpoly)
				(princ name_pline id_file)
				(princ enter id_file)
				(setq id_prim (ssname ss (setq i (1+ i)))) 				;ssname-��������� �������� �� ������ �� ���������� ������. (ssname <�����> <�����>)
				(setq vert_kol (/ (length (vlax-get (vlax-ename->vla-object id_prim) 'Coordinates)) 3))
				(setq vert_kol_sp (vlax-get (vlax-ename->vla-object id_prim) 'Coordinates))
				(setq p0 (vlax-curve-getStartPoint (ssname ss i))   	;(1.73962 2.12561 0.0) ; ������� ���������� ������ ������� ���������
				coord_z (caddr p0))										;���������� z ���������� ������ ����� � ���� 3.00000
				(setq ii 1)
				(while (<= ii vert_kol)                                 ;������� ���� �� ���������� ������
					(setq coord_x (car vert_kol_sp)) 					;(nth 3 '(a b c d e))	����������	D
					(setq coord_y (cadr vert_kol_sp)) 					;assoc ���������� �� ������ ������� � ����� 10 (assoc <���> <������>)
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