*MESSAGEBOX("Сверление  ",0," ")

* Выбрали данные металла
SELECT * from METAL WHERE id_mat= m.cur_id_mat  INTO CURSOR Cur_metal_

* Скачаем строку Приоритетов выбора инструмента
 m.instr_OK=.f.
Select prior1, prior2, prior3, prior4, prior5, prior6  from PRIORITET where KTE= m.cur_kte AND chist=m.cur_chist INTO CURSOR ttt_

* разбор инструмента
FOR ii=1 TO 6 STEP 1

   		    DO case
   		       CASE  ii=1
    		          m.Kd_gr_rezc=ALLTRIM(ttt_.prior1)
   		       CASE  ii=2
   		               m.Kd_gr_rezc=ALLTRIM(ttt_.prior2)
   		       CASE  ii=3
   		               m.Kd_gr_rezc=ALLTRIM(ttt_.prior3)
   		       CASE  ii=4
   		               m.Kd_gr_rezc=ALLTRIM(ttt_.prior4)
   		       CASE  ii=5
   		               m.Kd_gr_rezc=ALLTRIM(ttt_.prior5)
   		       CASE  ii=6
   		               m.Kd_gr_rezc=ALLTRIM(ttt_.prior6)
   		    endcase

		IF !EMPTY(m.Kd_gr_rezc)and  m.instr_OK=.f.
		    m.sv_diam=2*m.X_max
		    SELECT * from drills WHERE tip=m.Kd_gr_rezc  AND direct= m.direction AND Dc = m.sv_diam AND Lu>=m.L_obr INTO TABLE drills_tmp ORDER BY Lu asc
		    n=_tally
		    SELECT drills_tmp

		    IF n>=1
   		    SELECT drills_tmp
   		    GO top
   		    SCATTER memvar
	         m.kod_instr=drills_tmp.instr_id
             m.name_instr=drills_tmp.name
             m.obozn_instr=drills_tmp.obozn

		       m.instr_OK=.t.
		       m.Ar=m.X_max
		       EXIT
		    endif

		endif

endfor


IF m.instr_OK=.f.
		   IF  m.instr_OK=.f.   &&& Если резец не найден
			     SELECT * from drills WHERE direct= m.direction AND Dc >= m.sv_diam  INTO CURSOR rrr_
			       n=_tally
			     IF n=0
			           MESSAGEBOX(" ВСЕ СВЕРЛА БД ИМЕЮТ ДИАМЕТР МЕНЬШЕ ЗАДАННОГО ",0," ")
	             ENDIF

			     SELECT * from drills WHERE direct= m.direction AND  Lu>=m.L_obr INTO CURSOR rrr_
			       n=_tally
			     IF n=0
			           MESSAGEBOX(" ВСЕ СВЕРЛА БД ИМЕЮТ ДЛИНУ МЕНЬШЕ, ЧЕМ ГЛУБИНА СВЕРЛЕНИЯ ",0," ")
	             ENDIF
            endif


endif





IF  m.instr_OK=.T.

	* Расчет режимов резания
	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))

	SELECT * from drilling WHERE UPPER(smg)= m.SMG_met  AND d >= m.sv_diam  INTO CURSOR regim_ order BY d
	SELECT regim_
	n1=_tally
	GO top


	m.F_tabl= regim_.F
	m.V_tabl= regim_.V


 IF m.hardness>35   &&& Корректировка от твердости

   MESSAGEBOX("Расчет для закаленных сталей пока в разработке",0," ")

 ELSE

	*корректировка  V  от прочности материала
	koef= (Cur_metal_.mpa/Cur_metal_.etal_mpa)*100
	SELECT K_mpa
	m.kmpa=1.0
	SCAN
		IF K_mpa.mpa_proc>=koef
		    m.kmpa=K_mpa.kmpa
		    exit
		endif
	ENDSCAN

	IF m.kmpa<>1.0
	    V_tabl=INT(V_tabl*m.kmpa)
	endif


endif

	 m.V=m.V_tabl
	 m.F=m.F_tabl


*!*		* проверка по мощности и усилиям резания

	   m.kc=Cur_metal_.kc


*!*		* Проверка по крутящему моменту
       diam=2*m.Ar

	   m_rasc = (diam*diam*F_tabl*m.kc/8000)


	   IF  m_rasc >m.M_tc
	      m.f2= F_tabl * m.M_tc/m_rasc
	*      m.f=ROUND(m.f2,3)
	      m.f=m.f2
	        m_rasc = (diam*diam*m.F2*m.kc/8000)

	   ENDIF


* Расчет мощности
     qq=(3.14*V_tabl*diam*diam)/4000
     P_rasc = (qq*m.kc)/(60*1000*0.85)
