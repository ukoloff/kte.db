* MESSAGEBOX("Резьба наружная ",0," ")

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

		IF !EMPTY(m.Kd_gr_rezc) and  m.instr_OK=.f.
		   IF m.ugol_rezb=55
		       SELECT * from Cutters WHERE tip=m.Kd_gr_rezc AND direct= m.direction AND TPI=m.P_pezb  INTO TABLE rezc_tmp ORDER BY prior
		   else
		       SELECT * from Cutters WHERE tip=m.Kd_gr_rezc AND direct= m.direction AND MM>=m.P_pezb  INTO TABLE rezc_tmp ORDER BY prior, mm asc
		   endif
		    n=_tally
		    SELECT rezc_tmp

		    IF n>=1

   		    SELECT rezc_tmp
   		    GO top
    		  SCATTER memvar
 	         m.kod_instr=rezc_tmp.instr_id
             m.name_instr=rezc_tmp.name
             m.obozn_instr=rezc_tmp.obozn

           * Проверка сплава
            m.cur_splav=ALLTRIM(UPPER(m.mat_name))
            m.cur_SMG= UPPER(SUBSTR(Cur_metal_.smg,1,1))

   		    SELECT * from splav WHERE ALLTRIM(UPPER(splav)) = m.cur_splav  INTO CURSOR  sss_
   		    m.splav_ok=0
   		    SELECT sss_

	   		    DO case
	   		       CASE  m.cur_SMG="P" and !EMPTY(sss_.smg_p)
	    		           m.splav_ok=1

	   		       CASE   m.cur_SMG="M" AND !EMPTY(sss_.smg_M)
	   		               m.splav_ok=1

	   		       CASE   m.cur_SMG="K" AND !EMPTY(sss_.smg_K)
	   		               m.splav_ok=1

	   		       CASE   m.cur_SMG="N" AND !EMPTY(sss_.smg_N)
	   		               m.splav_ok=1

	   		       CASE  m.cur_SMG="S"  AND !EMPTY(sss_.smg_S)
	   		               m.splav_ok=1
	   		    endcase

		        if m.splav_ok=0
		           MESSAGEBOX("МАТЕРИАЛ РЕЖУЩЕЙ ПЛАСТИНЫ НЕ РЕКОМЕДУЕТСЯ ДЛЯ ОБРАБОТКИ УКАЗАННОГО МАТЕРИАЛА ДЕТАЛИ ",0," ")
		        ENDIF

		       m.instr_OK=.t.
		       EXIT
		     thisform.Refresh

		    endif

		endif

endfor



IF  m.instr_OK=.T.

	* Расчет режимов резания

	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))

	SELECT * from V_rezb WHERE UPPER(smg)= m.SMG_met    INTO CURSOR regim_
	n=_tally

    IF n>=1
	SELECT regim_
	GO top
	m.F_tabl= m.P_pezb
	m.V_tabl= regim_.V

	else
       MESSAGEBOX("В БД  НЕТ ДАННЫХ ПО РЕЖИМАМ ДЛЯ ТАКОГО ВАРИАНТА МАТЕРИАЛ + ШИРИНА РЕЗА ",0," ")
    endif
*	SET STEP ON

* Найдем число проходов
IF m.KOD_REZB=1 OR m.KOD_REZB=3

 	SELECT * from N_metr WHERE Ph=m.P_pezb   INTO CURSOR n_proh_

    SELECT n_proh_
    m.ar_rasc=n_proh_.n1
 ELSE
    IF m.ugol_rezb=55

    	SELECT * from N_metr WHERE TPI=m.P_pezb   INTO CURSOR n_proh_

    SELECT n_proh_
        m.ar_rasc=n_proh_.n1
    endif

ENDIF

 IF m.hardness>35   &&& Корректировка от твердости

   MESSAGEBOX("Расчет для закаленных сталей пока в разработке",0," ")

 ELSE

	*корректировка  V  от прочности материала
	koef= (Cur_metal_.mpa/Cur_metal_.etal_mpa)*100
	SELECT K_mpa
	*BROWSE

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



	 m.V=V_tabl
	 m.F=m.F_tabl
	 m.Ar=m.ar_rasc
