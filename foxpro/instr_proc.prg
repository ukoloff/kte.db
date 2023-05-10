


PROCEDURE kte_var1

* MESSAGEBOX("Торец черновая ",0," ")

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
		    SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND direct= m.direction  INTO TABLE rezc_tmp ORDER BY prior
		    n=_tally
		    SELECT rezc_tmp

		    IF n>=1   
		     m.kod_instr=rezc_tmp.instr_id
            
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
	m.ar_prip= 2.0  && Для обработки торца припуск принят 2 мм 
	m.ar_obr=m.X_max/20   &&& по диаметру

	m.ar_rasc=m.Ar_max
	IF m.ar_prip < m.ar_rasc
	    m.ar_rasc=m.ar_prip
	ENDIF

	IF m.ar_obr < m.ar_rasc
	    m.ar_rasc=m.ar_obr
	ENDIF

	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))


	SELECT * from TURN_1 WHERE UPPER(smg)= m.SMG_met  AND ar <= m.ar_rasc  INTO CURSOR regim_ order BY ar desc, f desc

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
	      m.f2=  F_tabl * m.M_tc/m_rasc
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

* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  


   gnErrFile = FCREATE('Instr_rezult.txt')  && If not create it

IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file
  	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	
	
ELSE
     MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF
	

ENDFUNC 




PROCEDURE kte_var1_1

* MESSAGEBOX("Торец чистовая ",0," ")


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
		    SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND direct= m.direction  INTO TABLE rezc_tmp ORDER BY prior
		    n=_tally
		    SELECT rezc_tmp

		    
		    IF n>=1   
		     m.kod_instr=rezc_tmp.instr_id
            
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
	m.ar_rasc=0.5

	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))


	SELECT * from TURN_1 WHERE UPPER(smg)= m.SMG_met  AND ar <= m.ar_rasc  INTO CURSOR regim_ order BY ar asc, f asc

	SELECT regim_ 
	n1=_tally
	GO top


	m.F_tabl= regim_.F  
	m.V_tabl= regim_.V 

* выбор подачи от радиуса ппластинки

m.rrr=m.roughness

SELECT * from R_shift WHERE ra<=m.rrr AND r = m.re  INTO CURSOR fff_  order BY ra desc
	
	
	SELECT fff_

  IF m.F_tabl > fff_.f
	 m.F_tabl= fff_.f
  endif	

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
	    m.V_tabl=INT(m.V_tabl*m.kmpa)
	endif

endif

	 
	 m.V=m.V_tabl
	 m.F=m.F_tabl
	 m.Ar=m.ar_rasc

m.kc=Cur_metal_.kc	 
	 
	 P_rasc = (m.ar*m.F*m.V*m.kc)/(60*1000*0.85)
*	   thisform.text7.Value= P_rasc

	   m_rasc = (m.ar*F*m.kc*m.x_max/1000)
*	    thisform.text8.Value= m_rasc

* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  && If not create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file

  	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	
	
	
ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF
	

ENDFUNC 





PROCEDURE kte_var2

* MESSAGEBOX("Открытая зона наружная  черновая ",0," ")

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
		    SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND direct= m.direction  INTO TABLE rezc_tmp ORDER BY prior
		    n=_tally
		    SELECT rezc_tmp
		    
		    IF n>=1   
		     m.kod_instr=rezc_tmp.instr_id
            
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
	m.ar_obr=m.X_max/20   &&& по диаметру

	m.ar_rasc=m.Ar_max
	IF m.ar_prip < m.ar_rasc
	    m.ar_rasc=m.ar_prip
	ENDIF

	IF m.ar_obr < m.ar_rasc
	    m.ar_rasc=m.ar_obr
	ENDIF

	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))


	SELECT * from TURN_1 WHERE UPPER(smg)= m.SMG_met  AND ar <= m.ar_rasc  INTO CURSOR regim_ order BY ar desc, f desc

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

* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  && If not create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file
  	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	

ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF
	
ENDFUNC 
  




PROCEDURE kte_var2_1

* MESSAGEBOX("Открытая зона наружная чистовая ",0," ")


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
		    SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND direct= m.direction  INTO TABLE rezc_tmp ORDER BY prior
		    n=_tally
		    SELECT rezc_tmp
		    
		    IF n>=1   
		     m.kod_instr=rezc_tmp.instr_id
            
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
	m.ar_rasc=0.5

	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))
	SELECT * from TURN_1 WHERE UPPER(smg)= m.SMG_met  AND ar <= m.ar_rasc  INTO CURSOR regim_ order BY ar asc, f asc

	SELECT regim_ 
	n1=_tally
	GO top

	m.F_tabl= regim_.F  
	m.V_tabl= regim_.V 

* выбор подачи от радиуса ппластинки

m.rrr=m.roughness

SELECT * from R_shift WHERE ra<=m.rrr AND r = m.re  INTO CURSOR fff_  order BY ra desc
	
	
	SELECT fff_

  IF m.F_tabl > fff_.f
	 m.F_tabl= fff_.f
  endif	


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
	    m.V_tabl=INT(m.V_tabl*m.kmpa)
	endif
endif

	 m.V=m.V_tabl
	 m.F=m.F_tabl
	 m.Ar=m.ar_rasc

m.kc=Cur_metal_.kc	 
	 
	 P_rasc = (m.ar*m.F*m.V*m.kc)/(60*1000*0.85)
*	   thisform.text7.Value= P_rasc

	   m_rasc = (m.ar*F*m.kc*m.x_max/1000)
*	    thisform.text8.Value= m_rasc


* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it

IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file
  	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	

ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF
	

ENDFUNC  



  
PROCEDURE kte_var3 

* MESSAGEBOX("Полуоткрытая зона наружная  черновая ",0," ")


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
		    SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND direct= m.direction  INTO TABLE rezc_tmp ORDER BY prior
		    n=_tally
		    SELECT rezc_tmp
		    
		    IF n>=1   
		     m.kod_instr=rezc_tmp.instr_id
            
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
	m.ar_obr=m.X_max/20   &&& по диаметру

	m.ar_rasc=m.Ar_max
	IF m.ar_prip < m.ar_rasc
	    m.ar_rasc=m.ar_prip
	ENDIF

	IF m.ar_obr < m.ar_rasc
	    m.ar_rasc=m.ar_obr
	ENDIF

	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))


	SELECT * from TURN_1 WHERE UPPER(smg)= m.SMG_met  AND ar <= m.ar_rasc  INTO CURSOR regim_ order BY ar desc, f desc

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


* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  
   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it

IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file
  	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	
		
ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF
	

ENDFUNC  




PROCEDURE kte_var3_1 

* MESSAGEBOX("Полуоткрытая зона наружная чистовая ",0," ")

 
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
		    SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND direct= m.direction  INTO TABLE rezc_tmp ORDER BY prior
		    n=_tally
		    SELECT rezc_tmp
		    
		    IF n>=1   
		     m.kod_instr=rezc_tmp.instr_id
            
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
	m.ar_rasc=0.5

	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))


	SELECT * from TURN_1 WHERE UPPER(smg)= m.SMG_met  AND ar <= m.ar_rasc  INTO CURSOR regim_ order BY ar asc, f asc

	SELECT regim_ 
	n1=_tally
	GO top
	m.F_tabl= regim_.F  
	m.V_tabl= regim_.V 

* выбор подачи от радиуса ппластинки

m.rrr=m.roughness

SELECT * from R_shift WHERE ra<=m.rrr AND r = m.re  INTO CURSOR fff_  order BY ra desc
	
	
	SELECT fff_
  IF m.F_tabl > fff_.f
	 m.F_tabl= fff_.f
  endif	


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
	    m.V_tabl=INT(m.V_tabl*m.kmpa)
	endif

endif

 
	 m.V=m.V_tabl
	 m.F=m.F_tabl
	 m.Ar=m.ar_rasc

m.kc=Cur_metal_.kc	 
	 
	 P_rasc = (m.ar*m.F*m.V*m.kc)/(60*1000*0.85)
*	   thisform.text7.Value= P_rasc

	   m_rasc = (m.ar*F*m.kc*m.x_max/1000)
*	    thisform.text8.Value= m_rasc


* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file

  	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	
	 
	
ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF
	

ENDFUNC  



PROCEDURE kte_var4
 
* MESSAGEBOX("Выточка наружная черновая ",0," ")

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
		    SELECT * from Cutters WHERE tip=m.Kd_gr_rezc AND CW<=m.B_kan AND cdx >= m.H_kan INTO TABLE rezc_tmp ORDER BY prior, cw 
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

		   IF  m.instr_OK=.f.   &&& Если резец не найден
			     SELECT * from Cutters WHERE  CW<=m.B_kan  INTO CURSOR rrr_ 
			       n=_tally
			     IF n=0
			           MESSAGEBOX(" ВСЕ ИНСТРУМЕНТЫ БД ИМЕЮТ ШИРИНУ ПЛАСТИНКИ БОЛЬШЕ, ЧЕМ ШИРИНА ВЫТОЧКИ ",0," ")		                  
	             ENDIF
             
			     SELECT * from Cutters WHERE   cdx >= m.H_kan INTO CURSOR rrr_              
			       n=_tally
			     IF n=0
			           MESSAGEBOX(" ВСЕ ИНСТРУМЕНТЫ БД ИМЕЮТ ВЫЛЕТ ПЛАСТИНКИ МЕНЬШЕ, ЧЕМ ГЛУБИНА ВЫТОЧКИ ",0," ")		                  
	             ENDIF		     
            endif




IF  m.instr_OK=.T.

	* Расчет режимов резания
 	m.ar_rasc = rezc_tmp.cw  &&&  по режушщей пластине

	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))


	SELECT * from GROOVE WHERE UPPER(smg)= m.SMG_met  AND cw = m.ar_rasc  INTO CURSOR regim_ 
	n=_tally

    IF n>=1
	SELECT regim_ 
	GO top

	m.F_tabl= regim_.F  
	m.V_tabl= regim_.V 
	
	else
       MESSAGEBOX("В БД  НЕТ ДАННЫХ ПО РЕЖИМАМ ДЛЯ ТАКОГО ВАРИАНТА МАТЕРИАЛ + ШИРИНА РЕЗА ",0," ")	
    endif


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
	   m.F_tabl= (60*1000*0.85*m.P_tc)/(m.ar_rasc*V_tabl*m.kc) 
	   m.F_tabl= ROUND(m.F_tabl,2)
	endif

	* Проверка по крутящему моменту
	  
	   m_rasc = (m.ar_rasc*F_tabl*m.kc*m.x_max/1000)
*	    thisform.text8.Value= m_rasc
	   

	   IF  m_rasc >m.M_tc
	      m.f2= F_tabl * m.M_tc/m_rasc
	      m.f2= ROUND(m.F2,2)
	   ELSE
	     m.f2=F_tabl    
	   ENDIF  

	*Проверка по усилию подач
	 
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

* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file
  	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	

ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF
	

ENDFUNC     





PROCEDURE kte_var5
 
 * MESSAGEBOX("Канавка №1 наружная черновая ",0," ")


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
            SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND direct= m.direction  INTO TABLE rezc_tmp ORDER BY prior
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
	m.ar_obr=m.X_max/20   &&& по диаметру

	m.ar_rasc=m.Ar_max
	IF m.ar_prip < m.ar_rasc
	    m.ar_rasc=m.ar_prip
	ENDIF

	IF m.ar_obr < m.ar_rasc
	    m.ar_rasc=m.ar_obr
	ENDIF

	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))


	SELECT * from TURN_1 WHERE UPPER(smg)= m.SMG_met  AND ar <= m.ar_rasc  INTO CURSOR regim_ order BY ar desc, f desc

	n=_tally

    IF n>=1
	SELECT regim_ 
	GO top

	m.F_tabl= regim_.F  
	m.V_tabl= regim_.V 
	
	else
       MESSAGEBOX("В БД  НЕТ ДАННЫХ ПО РЕЖИМАМ ДЛЯ ТАКОГО ВАРИАНТА МАТЕРИАЛ + ШИРИНА РЕЗА ",0," ")	
    endif 

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
	   m.F_tabl= (60*1000*0.85*m.P_tc)/(m.ar_rasc*V_tabl*m.kc) 
	   m.F_tabl= ROUND(m.F_tabl,2)
	endif

	* Проверка по крутящему моменту
	  
	   m_rasc = (m.ar_rasc*F_tabl*m.kc*m.x_max/1000)
*	    thisform.text8.Value= m_rasc
	   

	   IF  m_rasc >m.M_tc
	      m.f2= F_tabl * m.M_tc/m_rasc
	      m.f2= ROUND(m.F2,2)
	   ELSE
	     m.f2=F_tabl    
	   ENDIF  

	*Проверка по усилию подач
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


* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file
  	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	
	
ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF


ENDFUNC  


PROCEDURE kte_var5_1
 
* MESSAGEBOX("Канавка №1 наружная чистовая ",0," ")


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
            SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND direct= m.direction  INTO TABLE rezc_tmp ORDER BY prior
		    n=_tally
		    SELECT rezc_tmp
		    
		    IF n>=1   
		     m.kod_instr=rezc_tmp.instr_id
            
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
	m.ar_rasc=0.5

	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))


	SELECT * from TURN_1 WHERE UPPER(smg)= m.SMG_met  AND ar <= m.ar_rasc  INTO CURSOR regim_ order BY ar asc, f asc

	SELECT regim_ 
	n1=_tally
	GO top

	m.F_tabl= regim_.F  
	m.V_tabl= regim_.V 

* выбор подачи от радиуса ппластинки

m.rrr=m.roughness

SELECT * from R_shift WHERE ra<=m.rrr AND r = m.re  INTO CURSOR fff_  order BY ra desc
	
	
	SELECT fff_

	IF m.F_tabl > fff_.f
	    m.F_tabl= fff_.f
    endif	
	

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
	    m.V_tabl=INT(m.V_tabl*m.kmpa)
	endif

endif

	 
	 m.V=m.V_tabl
	 m.F=m.F_tabl
	 m.Ar=m.ar_rasc

m.kc=Cur_metal_.kc	 
	 
	 P_rasc = (m.ar*m.F*m.V*m.kc)/(60*1000*0.85)
*	   thisform.text7.Value= P_rasc

	   m_rasc = (m.ar*F*m.kc*m.x_max/1000)
*	    thisform.text8.Value= m_rasc

* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file
 	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	
	
ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF
	

ENDFUNC  
      

PROCEDURE kte_var6
 
* MESSAGEBOX("Канавка резьб наружная черновая ",0," ")

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
		    SELECT * from Cutters WHERE tip=m.Kd_gr_rezc AND CW<=m.B_kan AND cdx >= m.H_kan INTO TABLE rezc_tmp ORDER BY prior, cw desc
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

		   IF  m.instr_OK=.f.   &&& Если резец не найден

			     SELECT * from Cutters WHERE   cdx >= m.H_kan  AND Daxx=0 AND Daxn=0INTO CURSOR rrr_              
			       n=_tally
			     IF n=0
			           MESSAGEBOX(" ВСЕ ИНСТРУМЕНТЫ БД ИМЕЮТ ВЫЛЕТ ПЛАСТИНКИ МЕНЬШЕ, ЧЕМ ГЛУБИНА КАНАВКИ ",0," ")		                  
	             ENDIF		     
            endif




IF  m.instr_OK=.T.

	* Расчет режимов резания
 	m.ar_rasc = rezc_tmp.cw  &&&  по режушщей пластине

	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))

	SELECT * from GROOVE WHERE UPPER(smg)= m.SMG_met  AND cw >= m.ar_rasc  INTO CURSOR regim_  order BY cw 
	n=_tally

    IF n>=1
	SELECT regim_ 
	GO top

	m.F_tabl= regim_.F  
	m.V_tabl= regim_.V 
	
	else
       MESSAGEBOX("В БД  НЕТ ДАННЫХ ПО РЕЖИМАМ ДЛЯ ТАКОГО ВАРИАНТА МАТЕРИАЛ + ШИРИНА РЕЗА ",0," ")	
    endif


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
	   m.F_tabl= (60*1000*0.85*m.P_tc)/(m.ar_rasc*V_tabl*m.kc) 
	   m.F_tabl= ROUND(m.F_tabl,2)
	endif

	* Проверка по крутящему моменту
	  
	   m_rasc = (m.ar_rasc*F_tabl*m.kc*m.x_max/1000)
*	    thisform.text8.Value= m_rasc
	   

	   IF  m_rasc >m.M_tc
	      m.f2= F_tabl * m.M_tc/m_rasc
	      m.f2= ROUND(m.F2,2)
	   ELSE
	     m.f2=F_tabl    
	   ENDIF  

	*Проверка по усилию подач

	 
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


* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file

* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	

ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF
	

ENDFUNC  


PROCEDURE kte_var6_1
 
* MESSAGEBOX("Канавка резьбовая  чисто ",0," ")

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
		    SELECT * from Cutters WHERE tip=m.Kd_gr_rezc AND CW<=m.B_kan AND cdx >= m.H_kan INTO TABLE rezc_tmp ORDER BY prior, cw desc
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

		   IF  m.instr_OK=.f.   &&& Если резец не найден
           
			     SELECT * from Cutters WHERE   cdx >= m.H_kan AND Daxx-0 AND Daxn=0 INTO CURSOR rrr_              
			       n=_tally
			     IF n=0
			           MESSAGEBOX(" ВСЕ ИНСТРУМЕНТЫ БД ИМЕЮТ ВЫЛЕТ ПЛАСТИНКИ МЕНЬШЕ, ЧЕМ ГЛУБИНА КАНАВКИ ",0," ")		                  
	             ENDIF		     
            endif




IF  m.instr_OK=.T.

	* Расчет режимов резания
 	m.ar_rasc = 0.5

	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))


	SELECT * from GROOVE WHERE UPPER(smg)= m.SMG_met  AND cw >= m.ar_rasc  INTO CURSOR regim_ 
	n=_tally

    IF n>=1
	SELECT regim_ 
	GO top

	m.F_tabl= regim_.F  
	m.V_tabl= regim_.V 
	
	else
       MESSAGEBOX("В БД  НЕТ ДАННЫХ ПО РЕЖИМАМ ДЛЯ ТАКОГО ВАРИАНТА МАТЕРИАЛ + ШИРИНА РЕЗА ",0," ")	
    endif
	
	* выбор подачи от радиуса ппластинки

m.rrr=m.roughness

SELECT * from R_shift WHERE ra<=m.rrr AND r = m.re  INTO CURSOR fff_  order BY ra desc
	
	
	SELECT fff_

	IF m.F_tabl > fff_.f
	    m.F_tabl= fff_.f
    endif	


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


	     m.f2=F_tabl    

	 m.V=V_tabl
	 m.F=m.f2
	 m.Ar=m.ar_rasc
	m.kc=Cur_metal_.kc	  
	 
	 P_rasc = (m.ar*F2*V_tabl*m.kc)/(60*1000*0.85)
*	   thisform.text7.Value= P_rasc

	   m_rasc = (m.ar*F2*m.kc*m.x_max/1000)
*	    thisform.text8.Value= m_rasc

* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file

  	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	

ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF
	


ENDFUNC 

     
PROCEDURE kte_var7
 
* MESSAGEBOX("Выточка аксиальная черновая ",0," ")

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
		    SELECT * from Cutters WHERE tip=m.Kd_gr_rezc AND CW<=m.B_kan AND cdx >= m.H_kan AND DAXN < m.X_min AND DAXX > m.X_max INTO TABLE rezc_tmp ORDER BY prior, cw, DAXX, DAXN 
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

		   IF  m.instr_OK=.f.   &&& Если резец не найден
			     SELECT * from Cutters WHERE  CW<=m.B_kan AND Daxx>0 AND Daxn>0 INTO CURSOR rrr_ 
			       n=_tally
			     IF n=0
			           MESSAGEBOX(" ВСЕ ИНСТРУМЕНТЫ БД ИМЕЮТ ШИРИНУ ПЛАСТИНКИ БОЛЬШЕ, ЧЕМ ШИРИНА ВЫТОЧКИ ",0," ")		                  
	             ENDIF
             
			     SELECT * from Cutters WHERE   cdx >= m.H_kan AND Daxx>0 AND Daxn>0 INTO CURSOR rrr_              
			       n=_tally
			     IF n=0
			           MESSAGEBOX(" ВСЕ ИНСТРУМЕНТЫ БД ИМЕЮТ ВЫЛЕТ ПЛАСТИНКИ МЕНЬШЕ, ЧЕМ ГЛУБИНА ВЫТОЧКИ ",0," ")		                  
	             ENDIF		     
            endif




IF  m.instr_OK=.T.

	* Расчет режимов резания
 	m.ar_rasc = rezc_tmp.cw  &&&  по режушщей пластине

	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))


	SELECT * from GROOVE WHERE UPPER(smg)= m.SMG_met  AND cw >= m.ar_rasc  INTO CURSOR regim_ 
	n=_tally

    IF n>=1
	SELECT regim_ 
	GO top



	m.F_tabl= regim_.F  
	m.V_tabl= regim_.V 
	
	else
       MESSAGEBOX("В БД  НЕТ ДАННЫХ ПО РЕЖИМАМ ДЛЯ ТАКОГО ВАРИАНТА МАТЕРИАЛ + ШИРИНА РЕЗА ",0," ")	
    endif


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
	   m.F_tabl= (60*1000*0.85*m.P_tc)/(m.ar_rasc*V_tabl*m.kc) 
*	   m.F_tabl= ROUND(m.F_tabl,2)
	endif

	* Проверка по крутящему моменту
	  
	   m_rasc = (m.ar_rasc*F_tabl*m.kc*m.x_max/1000)
*	    thisform.text8.Value= m_rasc
	   

	   IF  m_rasc >m.M_tc
	      m.f2= F_tabl * m.M_tc/m_rasc
	      m.f2= ROUND(m.F2,2)
	   ELSE
	     m.f2=F_tabl    
	   ENDIF  

	*Проверка по усилию подач
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


* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file
  	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	

ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF
	

ENDFUNC 


 
PROCEDURE kte_var8
 
* MESSAGEBOX("Канавка аксиальная   черновая ",0," ")

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
		    SELECT * from Cutters WHERE tip=m.Kd_gr_rezc AND CW<=m.B_kan AND cdx >= m.H_kan AND DAXN < m.X_min AND DAXX > m.X_max INTO TABLE rezc_tmp ORDER BY prior, cw, DAXX, DAXN 
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

		   IF  m.instr_OK=.f.   &&& Если резец не найден
			     SELECT * from Cutters WHERE  CW<=m.B_kan AND Daxx>0 AND Daxn>0 INTO CURSOR rrr_ 
			       n=_tally
			     IF n=0
			           MESSAGEBOX(" ВСЕ ИНСТРУМЕНТЫ БД ИМЕЮТ ШИРИНУ ПЛАСТИНКИ БОЛЬШЕ, ЧЕМ ШИРИНА КАНАВКИ ",0," ")		                  
	             ENDIF
             
			     SELECT * from Cutters WHERE   cdx >= m.H_kan AND Daxx>0 AND Daxn>0 INTO CURSOR rrr_              
			       n=_tally
			     IF n=0
			           MESSAGEBOX(" ВСЕ ИНСТРУМЕНТЫ БД ИМЕЮТ ВЫЛЕТ ПЛАСТИНКИ МЕНЬШЕ, ЧЕМ ГЛУБИНА КАНАВКИ ",0," ")		                  
	             ENDIF		     
            endif

IF  m.instr_OK=.T.

	* Расчет режимов резания
 	m.ar_rasc = rezc_tmp.cw  &&&  по режушщей пластине

	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))


	SELECT * from GROOVE WHERE UPPER(smg)= m.SMG_met  AND cw >= m.ar_rasc  INTO CURSOR regim_ 
	n=_tally

    IF n>=1
	SELECT regim_ 
	GO top

	m.F_tabl= regim_.F  
	m.V_tabl= regim_.V 
	
	else
       MESSAGEBOX("В БД  НЕТ ДАННЫХ ПО РЕЖИМАМ ДЛЯ ТАКОГО ВАРИАНТА МАТЕРИАЛ + ШИРИНА РЕЗА ",0," ")	
    endif


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
	   m.F_tabl= (60*1000*0.85*m.P_tc)/(m.ar_rasc*V_tabl*m.kc) 
	   m.F_tabl= ROUND(m.F_tabl,2)
	endif

	* Проверка по крутящему моменту
	  
	   m_rasc = (m.ar_rasc*F_tabl*m.kc*m.x_max/1000)
*	    thisform.text8.Value= m_rasc
	   

	   IF  m_rasc >m.M_tc
	      m.f2= F_tabl * m.M_tc/m_rasc
	      m.f2= ROUND(m.F2,2)
	   ELSE
	     m.f2=F_tabl    
	   ENDIF  

	*Проверка по усилию подач
	 
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

* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file
  	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	

ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF
	


ENDFUNC 
        

PROCEDURE kte_var8_1
 
* MESSAGEBOX("Канавка аксиальная  чисто ",0," ")


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
		    SELECT * from Cutters WHERE tip=m.Kd_gr_rezc AND CW<=m.B_kan AND cdx >= m.H_kan AND DAXN < m.X_min AND DAXX > m.X_max INTO TABLE rezc_tmp ORDER BY prior, cw, DAXX, DAXN 
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

		   IF  m.instr_OK=.f.   &&& Если резец не найден
			     SELECT * from Cutters WHERE  CW<=m.B_kan AND Daxx>0 AND Daxn>0 INTO CURSOR rrr_ 
			       n=_tally
			     IF n=0
			           MESSAGEBOX(" ВСЕ ИНСТРУМЕНТЫ БД ИМЕЮТ ШИРИНУ ПЛАСТИНКИ БОЛЬШЕ, ЧЕМ ШИРИНА канавки ",0," ")		                  
	             ENDIF
             
			     SELECT * from Cutters WHERE   cdx >= m.H_kan AND Daxx>0 AND Daxn>0 INTO CURSOR rrr_              
			       n=_tally
			     IF n=0
			           MESSAGEBOX(" ВСЕ ИНСТРУМЕНТЫ БД ИМЕЮТ ВЫЛЕТ ПЛАСТИНКИ МЕНЬШЕ, ЧЕМ ГЛУБИНА канавкт ",0," ")		                  
	             ENDIF		     
            endif

IF  m.instr_OK=.T.

	* Расчет режимов резания
 	m.ar_rasc = 0.5

	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))


	SELECT * from GROOVE WHERE UPPER(smg)= m.SMG_met  AND cw >= m.ar_rasc  INTO CURSOR regim_ 
	n=_tally

    IF n>=1
	SELECT regim_ 
	GO top

	m.F_tabl= regim_.F  
	m.V_tabl= regim_.V 
	
	else
       MESSAGEBOX("В БД  НЕТ ДАННЫХ ПО РЕЖИМАМ ДЛЯ ТАКОГО ВАРИАНТА МАТЕРИАЛ + ШИРИНА РЕЗА ",0," ")	
    endif
	
	* выбор подачи от радиуса ппластинки

  m.rrr=m.roughness

  SELECT * from R_shift WHERE ra<=m.rrr AND r = m.re  INTO CURSOR fff_  order BY ra desc
	
	
	SELECT fff_
	IF m.F_tabl > fff_.f
	    m.F_tabl= fff_.f
    endif	

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

     m.f2=F_tabl    


	 m.V=V_tabl
	 m.F=m.f2
	 m.Ar=m.ar_rasc
	m.kc=Cur_metal_.kc	  
	 
	 P_rasc = (m.ar*F2*V_tabl*m.kc)/(60*1000*0.85)
*	   thisform.text7.Value= P_rasc

	   m_rasc = (m.ar*F2*m.kc*m.x_max/1000)
*	    thisform.text8.Value= m_rasc


* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file
  	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	
	
ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF
	

ENDFUNC 
        
 

PROCEDURE kte_var9
 
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

* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file

  	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	
	
ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF
	

ENDFUNC 




PROCEDURE kte_var9_1
 
* MESSAGEBOX("Отверстие резцом  чисто   ",0," ")


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
		     m.kod_instr=rezc_tmp.instr_id
            
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
	m.ar_rasc=0.5

	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))


	SELECT * from boring_1 WHERE UPPER(smg)= m.SMG_met  AND ar <= m.ar_rasc  INTO CURSOR regim_ order BY ar desc, f desc
	
	SELECT regim_ 
	n1=_tally
	GO top

	m.F_tabl= regim_.F  
	m.V_tabl= regim_.V 

* выбор подачи от радиуса ппластинки

m.rrr=m.roughness

SELECT * from R_shift WHERE ra<=m.rrr AND r = m.re  INTO CURSOR fff_  order BY ra desc
	
	
	SELECT fff_
	IF fff_.f<m.F_tabl   &&& если подача по шероховатости больше чем расчетная, то оставляем расчетную
	     m.F_tabl= fff_.f
	endif


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
	    m.V_tabl=INT(m.V_tabl*m.kmpa)
	endif
endif

	 
	 m.V=m.V_tabl
	 m.F=m.F_tabl
	 m.Ar=m.ar_rasc

m.kc=Cur_metal_.kc	 
	 
	 P_rasc = (m.ar*m.F*m.V*m.kc)/(60*1000*0.85)
*	   thisform.text7.Value= P_rasc

	   m_rasc = (m.ar*F*m.kc*m.x_max/1000)
*	    thisform.text8.Value= m_rasc


* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file
 	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	
	 
ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF
	

ENDFUNC 



   
PROCEDURE kte_var10
 
* MESSAGEBOX("Полуоткрытая  внутр черновая ",0," ")

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

* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file
  	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	
	
ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF
	

ENDFUNC  




PROCEDURE kte_var10_1
 
* MESSAGEBOX("Полуоткрытая  внутр чисто ",0," ")

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
		    SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND direct= m.direction AND dcinn<=m.d_nach AND vilet>=m.L_obr INTO TABLE rezc_tmp ORDER BY prior
		    n=_tally
		    SELECT rezc_tmp
		    
		    IF n>=1   
		     m.kod_instr=rezc_tmp.instr_id
            
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
	m.ar_rasc=0.5

	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))


	SELECT * from boring_1 WHERE UPPER(smg)= m.SMG_met  AND ar <= m.ar_rasc  INTO CURSOR regim_ order BY ar desc, f desc
	
	SELECT regim_ 
	n1=_tally
	GO top
	m.F_tabl= regim_.F  
	m.V_tabl= regim_.V 

* выбор подачи от радиуса ппластинки

m.rrr=m.roughness

SELECT * from R_shift WHERE ra<=m.rrr AND r = m.re  INTO CURSOR fff_  order BY ra desc
	
	SELECT fff_

	IF fff_.f<m.F_tabl   &&& если подача по шероховатости больше чем расчетная, то оставляем расчетную
	     m.F_tabl= fff_.f
	endif


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
	    m.V_tabl=INT(m.V_tabl*m.kmpa)
	endif

endif

	 m.V=m.V_tabl
	 m.F=m.F_tabl
	 m.Ar=m.ar_rasc

m.kc=Cur_metal_.kc	 
	 
	 P_rasc = (m.ar*m.F*m.V*m.kc)/(60*1000*0.85)
*   thisform.text7.Value= P_rasc

	   m_rasc = (m.ar*F*m.kc*m.x_max/1000)
*    thisform.text8.Value= m_rasc


* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file
  	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	
	 

ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF
	

ENDFUNC  
  

PROCEDURE kte_var11
 
* MESSAGEBOX("Выточка внутр черновая ",0," ")

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
		    SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND direct= m.direction AND CW<=m.B_kan AND cdx >= m.H_kan  AND DCINN <= d_nach INTO TABLE rezc_tmp ORDER BY prior, cw desc
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

		   IF  m.instr_OK=.f.   &&& Если резец не найден
			     SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND  direct= m.direction AND CW<=m.B_kan  INTO CURSOR rrr_ 
			       n=_tally
			     IF n=0
			           MESSAGEBOX(" ВСЕ ИНСТРУМЕНТЫ БД ИМЕЮТ ШИРИНУ ПЛАСТИНКИ БОЛЬШЕ, ЧЕМ ШИРИНА ВЫТОЧКИ ",0," ")		                  
	             ENDIF
             
			     SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND  direct= m.direction AND  cdx >= m.H_kan INTO CURSOR rrr_              
			       n=_tally
			     IF n=0
			           MESSAGEBOX(" ВСЕ ИНСТРУМЕНТЫ БД ИМЕЮТ ВЫЛЕТ ПЛАСТИНКИ МЕНЬШЕ, ЧЕМ ГЛУБИНА ВЫТОЧКИ ",0," ")		                  
	             ENDIF		     

			     SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND  direct= m.direction AND DCINN <= d_nach INTO CURSOR rrr_              
			       n=_tally
			     IF n=0
			           MESSAGEBOX(" ВСЕ ИНСТРУМЕНТЫ БД ИМЕЮТ ДИАМЕТР НАЧАЛЬНОГО ОТВЕРСТИЯ БОЛЬШЕ ЗАДАННОГО ",0," ")		                  
	             ENDIF		
            endif




IF  m.instr_OK=.T.

	* Расчет режимов резания
 	m.ar_rasc = rezc_tmp.cw  &&&  по режушщей пластине

	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))


	SELECT * from INTGROOVE WHERE UPPER(smg)= m.SMG_met  AND cw >= m.ar_rasc  INTO CURSOR regim_ order BY cw desc
	n=_tally

    IF n>=1
	SELECT regim_ 
	GO top

	m.F_tabl= regim_.F  
	m.V_tabl= regim_.V 
	
	else
       MESSAGEBOX("В БД  НЕТ ДАННЫХ ПО РЕЖИМАМ ДЛЯ ТАКОГО ВАРИАНТА МАТЕРИАЛ + ШИРИНА РЕЗА ",0," ")	
    endif

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


	* проверка по мощности и усилиям резания   не проводится

		   m.kc=Cur_metal_.kc

	     m.f2=F_tabl    
	 
	 m.V=V_tabl
	 m.F=m.f2
	 m.Ar=m.ar_rasc
	 
	 
	 P_rasc = (m.ar*F2*V_tabl*m.kc)/(60*1000*0.85)
*	   thisform.text7.Value= P_rasc

	   m_rasc = (m.ar*F2*m.kc*m.x_max/1000)
*	    thisform.text8.Value= m_rasc



* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file
 	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	
	
ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF
	

ENDFUNC 




PROCEDURE kte_var12
* MESSAGEBOX("Канавка №1 внутр черновая ",0," ")


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
		    SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND direct= m.direction AND lh>=m.L_obr  AND DCINN <= d_nach INTO TABLE rezc_tmp ORDER BY prior
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

		   IF  m.instr_OK=.f.   &&& Если резец не найден
		     SELECT * from Cutters WHERE direct= m.direction AND lh>=m.L_obr  INTO CURSOR rrr_ 
		       n=_tally
		     IF n=0
		           MESSAGEBOX(" ВСЕ РЕЗЦЫ БД ИМЕЮТ ДЛИНУ РАСТОЧКИ  МЕНЬШЕ, ЧЕМ ЗАДАННАЯ ",0," ")		                  
             ENDIF
         
		      SELECT * from Cutters WHERE direct= m.direction AND dcinn >=m.d_nach  INTO CURSOR rrr_             
		       n=_tally
		     IF n>0
		           MESSAGEBOX(" ВСЕ РЕЗЦЫ БД ИМЕЮТ ДИАМЕТР НАЧАЛЬНОГО ОТВЕРСТИЯ ОБРАБОТКИ БОЛЬШЕ, ЧЕМ ЗАДАННЫЙ ДИАМЕТР ОТВЕРСТИЯ ",0," ")		                  
             ENDIF		     
        endif




IF  m.instr_OK=.T.

		* Расчет режимов резания
	m.Ar_max = m.ArMax   &&&  по режушщей пластине
	m.ar_prip= m.X_max-m.X_min  &&&  по припуску
*	m.ar_obr=m.X_max/20   &&& по диаметру

	m.ar_rasc=m.Ar_max
	IF m.ar_prip < m.ar_rasc
	    m.ar_rasc=m.ar_prip
	ENDIF

*!*		IF m.ar_obr < m.ar_rasc
*!*		    m.ar_rasc=m.ar_obr
*!*		ENDIF

	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))


	SELECT * from boring_1 WHERE UPPER(smg)= m.SMG_met  AND ar <= m.ar_rasc  INTO CURSOR regim_ order BY ar desc, f desc

	SELECT regim_ 
	n1=_tally
	GO top

	m.F_tabl= regim_.F  
	m.V_tabl= regim_.V 


 IF m.hardness>35   &&& Корректировка от твердости

*   MESSAGEBOX("Расчет для закаленных сталей пока в разработке",0," ")
 
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
	   thisform.text7.Value= P_rasc

	IF P_rasc > m.P_tc
	   m.ar_rasc= (60*1000*0.85*m.P_tc)/(F_tabl*V_tabl*m.kc)
	   
	*   нормирование  до 0.5, 1, 1.5, 2, 2.5, 3
	 
	 rrr=IIF(m.ar_rasc<1.0,0.5,IIF(m.ar_rasc<1.5,1.0,IIF(m.ar_rasc<2.0,1.5,IIF(m.ar_rasc<2.5,2.0,IIF(m.ar_rasc<3.0,2.5,3)))))
	  
	 m.ar_rasc=rrr 
	    
	endif

	* Проверка по крутящему моменту
	  
	   m_rasc = (m.ar_rasc*F_tabl*m.kc*m.x_max/1000)
	    thisform.text8.Value= m_rasc
	   

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
	  thisform.text9.Value= m.F_rasc
	   
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

	

* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file
 	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	
	
ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF

ENDFUNC   



PROCEDURE kte_var12_1
* MESSAGEBOX("Канавка №1 внутрен чисто ",0," ")

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
		    SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND direct= m.direction AND dcinn<=m.d_nach AND lh>=m.L_obr INTO TABLE rezc_tmp ORDER BY prior
		    n=_tally
		    SELECT rezc_tmp

		    IF n>=1   
		     m.kod_instr=rezc_tmp.instr_id
            
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


	   IF  m.instr_OK=.f.   &&& Если резец не найден
	   	    SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND direct= m.direction AND dcinn<=m.d_nach AND lh>=m.L_obr INTO TABLE rezc_tmp ORDER BY prior
		     SELECT * from Cutters WHERE direct= m.direction AND lh>=m.L_obr  INTO CURSOR rrr_ 
		       n=_tally
		     IF n=0
		           MESSAGEBOX(" ВСЕ РЕЗЦЫ БД ИМЕЮТ ДЛИНУ РАСТОЧКИ  МЕНЬШЕ, ЧЕМ ЗАДАННАЯ ",0," ")		                  
             ENDIF
         
		      SELECT * from Cutters WHERE direct= m.direction AND dcinn >=m.d_nach  INTO CURSOR rrr_             
		       n=_tally
		     IF n>0
		           MESSAGEBOX(" ВСЕ РЕЗЦЫ БД ИМЕЮТ ДИАМЕТР НАЧАЛЬНОГО ОТВЕРСТИЯ ОБРАБОТКИ БОЛЬШЕ, ЧЕМ ЗАДАННЫЙ ДИАМЕТР ОТВЕРСТИЯ ",0," ")		                  
             ENDIF		     
        endif


IF  m.instr_OK=.T.

	* Расчет режимов резания
*!*		m.Ar_max = m.ArMax   &&&  по режушщей пластине
*!*		m.ar_prip= m.X_max-m.X_min  &&&  по припуску
*!*		m.ar_obr=m.X_max/20   &&& по диаметру

	m.ar_rasc=0.5

	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))


	SELECT * from boring_1 WHERE UPPER(smg)= m.SMG_met  AND ar <= m.ar_rasc  INTO CURSOR regim_ order BY ar desc, f desc
	
	SELECT regim_ 
	n1=_tally
	GO top
*	BROWSE


	m.F_tabl= regim_.F  
	m.V_tabl= regim_.V 

* выбор подачи от радиуса ппластинки

m.rrr=VAL(m.roughness)

SELECT * from R_shift WHERE ra<=m.rrr AND r = m.re  INTO CURSOR fff_  order BY ra desc
	
	
	SELECT fff_

	IF fff_.f<m.F_tabl   &&& если подача по шероховатости больше чем расчетная, то оставляем расчетную
	     m.F_tabl= fff_.f
	endif

	

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
	    m.V_tabl=INT(m.V_tabl*m.kmpa)
	endif


endif


	 
	 m.V=m.V_tabl
	 m.F=m.F_tabl
	 m.Ar=m.ar_rasc

m.kc=Cur_metal_.kc	 
	 

* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file

  	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	
	 
	
ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF
	

ENDFUNC  


PROCEDURE kte_var13
 
* MESSAGEBOX("Канавка  резьб внутр черновая ",0," ")

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
		    SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND direct= m.direction AND CW<=m.B_kan AND cdx >= m.H_kan  AND DCINN <= d_nach INTO TABLE rezc_tmp ORDER BY prior, cw desc
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

		   IF  m.instr_OK=.f.   &&& Если резец не найден
			     SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND  direct= m.direction AND CW<=m.B_kan  INTO CURSOR rrr_ 
			       n=_tally
			     IF n=0
			           MESSAGEBOX(" ВСЕ ИНСТРУМЕНТЫ БД ИМЕЮТ ШИРИНУ ПЛАСТИНКИ БОЛЬШЕ, ЧЕМ ШИРИНА КАНАВКИ ",0," ")		                  
	             ENDIF
             
			     SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND  direct= m.direction AND  cdx >= m.H_kan INTO CURSOR rrr_              
			       n=_tally
			     IF n=0
			           MESSAGEBOX(" ВСЕ ИНСТРУМЕНТЫ БД ИМЕЮТ ВЫЛЕТ ПЛАСТИНКИ МЕНЬШЕ, ЧЕМ ГЛУБИНА КАНАВКИ ",0," ")		                  
	             ENDIF		     

			     SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND  direct= m.direction AND DCINN <= d_nach INTO CURSOR rrr_              
			       n=_tally
			     IF n=0
			           MESSAGEBOX(" ВСЕ ИНСТРУМЕНТЫ БД ИМЕЮТ ДИАМЕТР НАЧАЛЬНОГО ОТВЕРСТИЯ БОЛЬШЕ ЗАДАННОГО ",0," ")		                  
	             ENDIF		
            endif


IF  m.instr_OK=.T.

	* Расчет режимов резания
 	m.ar_rasc = rezc_tmp.cw  &&&  по режушщей пластине

	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))


	SELECT * from INTGROOVE WHERE UPPER(smg)= m.SMG_met  AND cw >= m.ar_rasc  INTO CURSOR regim_ order BY cw desc
	n=_tally

    IF n>=1
	SELECT regim_ 
	GO top

	m.F_tabl= regim_.F  
	m.V_tabl= regim_.V 
	
	else
       MESSAGEBOX("В БД  НЕТ ДАННЫХ ПО РЕЖИМАМ ДЛЯ ТАКОГО ВАРИАНТА МАТЕРИАЛ + ШИРИНА РЕЗА ",0," ")	
    endif

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


	* проверка по мощности и усилиям резания   не проводится

		   m.kc=Cur_metal_.kc

	     m.f2=F_tabl    

	 
	 m.V=V_tabl
	 m.F=m.f2
	 m.Ar=m.ar_rasc
	 
	 
	 P_rasc = (m.ar*F2*V_tabl*m.kc)/(60*1000*0.85)
*	   thisform.text7.Value= P_rasc

	   m_rasc = (m.ar*F2*m.kc*m.x_max/1000)
*	    thisform.text8.Value= m_rasc


* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file
 	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	
	
ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF

ENDFUNC  
 


PROCEDURE kte_var13_1
 
* MESSAGEBOX("Канавка резьбовая внутр чисто ",0," ")

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
		    SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND direct= m.direction AND CW<=m.B_kan AND cdx >= m.H_kan  AND DCINN <= d_nach INTO TABLE rezc_tmp ORDER BY prior, cw desc
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

		   IF  m.instr_OK=.f.   &&& Если резец не найден

             
			     SELECT * from Cutters WHERE   cdx >= m.H_kan AND Daxx-0 AND Daxn=0 INTO CURSOR rrr_              
			       n=_tally
			     IF n=0
			           MESSAGEBOX(" ВСЕ ИНСТРУМЕНТЫ БД ИМЕЮТ ВЫЛЕТ ПЛАСТИНКИ МЕНЬШЕ, ЧЕМ ГЛУБИНА КАНАВКИ ",0," ")		                  
	             ENDIF		     
            endif




IF  m.instr_OK=.T.

	* Расчет режимов резания
 	m.ar_rasc = 0.5

	 m.SMG_met= ALLTRIM(UPPER(Cur_metal_.smg))


	SELECT * from INTGROOVE WHERE UPPER(smg)= m.SMG_met  AND cw >= m.ar_rasc  INTO CURSOR regim_ order BY cw desc
	n=_tally

    IF n>=1
	SELECT regim_ 
	GO top

	m.F_tabl= regim_.F  
	m.V_tabl= regim_.V 
	
	else
       MESSAGEBOX("В БД  НЕТ ДАННЫХ ПО РЕЖИМАМ ДЛЯ ТАКОГО ВАРИАНТА МАТЕРИАЛ + ШИРИНА РЕЗА ",0," ")	
    endif
	

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


	     m.f2=F_tabl    

 
	 m.V=V_tabl
	 m.F=m.f2
	 m.Ar=m.ar_rasc
	m.kc=Cur_metal_.kc	  
	 
	 P_rasc = (m.ar*F2*V_tabl*m.kc)/(60*1000*0.85)
*	   thisform.text7.Value= P_rasc

	   m_rasc = (m.ar*F2*m.kc*m.x_max/1000)
*	    thisform.text8.Value= m_rasc



* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file

  	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	
	
	
	
ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF
	

ENDFUNC         


PROCEDURE kte_var14

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
	 
	 
* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file
 	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	

ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF


ENDFUNC  

 
PROCEDURE kte_var15

* MESSAGEBOX("Резьба внутренняя ",0," ")

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
		       SELECT * from Cutters WHERE tip=m.Kd_gr_rezc AND direct= m.direction AND TPI=m.P_pezb AND DCINN <= d_nach INTO TABLE rezc_tmp ORDER BY prior		   
		   else
		       SELECT * from Cutters WHERE tip=m.Kd_gr_rezc AND direct= m.direction AND MM>=m.P_pezb AND DCINN <= d_nach INTO TABLE rezc_tmp ORDER BY prior, mm asc
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
	 

* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file

  	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	
	
ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF

ENDFUNC 


PROCEDURE kte_var16
 
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


* Запись в выходной файл	
m.OUT_inst_info=STR(m.kod_instr,4)+","+m.obozn_instr+","+STR(m.Ar,4,1)+","+STR(m.F,4,2)+","+STR(m.V,4)
	  

   gnErrFile = FCREATE('Instr_rezult.txt')  &&  create it


IF gnErrFile < 0     && Check for opening file
   WAIT 'Не могу открыть выходной файл для записи' WINDOW NOWAIT
ELSE  &&  write to file
   =FWRITE(gnErrFile , m.OUT_inst_info)
ENDIF
=FCLOSE(gnErrFile )     && Close file

  	
* MESSAGEBOX(m.OUT_inst_info,0,"Запись в выходной файл  Instr_rezult.txt ")	
	
	
	
ELSE
   MESSAGEBOX("В СПРАВОЧНИКЕ СВЕРЛ ИНСТРУМЕНТ НЕ НАЙДЕН",0," ")
ENDIF
	

ENDFUNC   
  


