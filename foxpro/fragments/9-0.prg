* MESSAGEBOX("Отверстие резцом  начерно   ",0," ")

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
		    SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND direct= m.direction AND dcinn<=m.d_nach AND vilet>=m.L_obr INTO TABLE rezc_tmp ORDER BY prior
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
		    endif

		endif

endfor


IF  m.instr_OK=.T.

	* Расчет режимов резания
	m.Ar_max = m.ArMax   &&&  по режушщей пластине
	m.ar_prip= m.X_max-m.X_min  &&&  по припуску
	m.ar_rasc=m.Ar_max
	IF m.ar_prip < m.ar_rasc
	    m.ar_rasc=m.ar_prip
	ENDIF


	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))

	SELECT * from boring_1 WHERE UPPER(smg)= m.SMG_met  AND ar <= m.ar_rasc  INTO CURSOR regim_ order BY ar desc, f desc

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


	* проверка по мощности и усилиям резания
	   m.min_f=0
	   m.kc=Cur_metal_.kc

	   P_rasc = (m.ar_rasc*F_tabl*V_tabl*m.kc)/(60*1000*0.85)
*	   thisform.text7.Value= P_rasc

	IF P_rasc > m.P_tc
	   m.ar_rasc= (60*1000*0.85*m.P_tc)/(F_tabl*V_tabl*m.kc)

	*   нормирование  до 0.5, 1, 1.5, 2, 2.5, 3

	 rrr=IIF(m.ar_rasc<1.0,0.5,IIF(m.ar_rasc<1.5,1.0,IIF(m.ar_rasc<2.0,1.5,IIF(m.ar_rasc<2.5,2.0,IIF(m.ar_rasc<3.0,2.5,3)))))

	 m.ar_rasc=rrr
	endif

	* Проверка по крутящему моменту

	   m_rasc = (m.ar_rasc*F_tabl*m.kc*m.x_max/1000)
*	    thisform.text8.Value= m_rasc


	   IF  m_rasc >m.M_tc
	      m.f2= F_tabl * m.M_tc/m_rasc
	      m.f2=ROUND(m.f2,3)
	   ELSE
	     m.f2=F_tabl
	   ENDIF

	*Проверка по усилию подач
	 m.F_mx = 4000
	 m.F_mz = 6000

	 m.F_rasc = m.ar_rasc*f2*m.kc*0.35
*	  thisform.text9.Value= m.F_rasc

	 IF  m.F_rasc > m.F_mx
	    m.f2= m.f2*( m.F_mx / m.F_rasc)
	 ENDIF

	 m.V=V_tabl
	 m.F=m.f2
	 m.Ar=m.ar_rasc


	 P_rasc = (m.ar*F2*V_tabl*m.kc)/(60*1000*0.85)
*	   thisform.text7.Value= P_rasc

	   m_rasc = (m.ar*F2*m.kc*m.x_max/1000)
*	    thisform.text8.Value= m_rasc
