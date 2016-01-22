SELECT COUNT(ROWNUM) NB,CASE WHEN H.TRJT_TYP = 'O' THEN TO_CHAR(DAT_SOR_TRAJET1,'DD/MM/YYYY') ELSE TO_CHAR(DAT_ENTR_TRAJET1,'DD/MM/YYYY') END DAT_ENTR_TRAJET1,TRAJET1.CLAS_TABU,TRAJET1.MOD_RGLT,TRAJET1.COD_GARE_SOR,TRAJET2.COD_GARE_ENTR
FROM 
 (SELECT
  NVL("IDT_TOK_PAN","NUM_MOY_PAIM") NUM_MOY_PAIM,
  TO_DATE(DAT_SOR,'YYYYMMDDHH24MISS') DAT_SOR_TRAJET2,
  TO_DATE(DAT_ENTR,'YYYYMMDDHH24MISS') DAT_ENTR_TRAJET2,
  COD_GARE_SOR,cod_gare_entr,
  CLAS_TABU,
  COD_PAY_ENTR,
  COD_PAY_SOR,
  COD_STE_ENTR,
  COD_STE_SOR,
  CASE WHEN MOD_RGLT in ('COF','DKV','DYN','ESS','EUR','GR','RES','SHE','UTA') THEN 'ACCREDITIVES' ELSE MOD_RGLT END MOD_RGLT
FROM
  TRAN_RAMP
WHERE
  ( nvl(TYP_FIC,'T')='T'  )
  AND  (COD_GARE_ENTR in @Prompt('07 - Gare(s) d''entrée second passage ?','A',,multi,free))
  AND  DAT_JOU_PEA between @Prompt('01 - Date début journée péage (DD/MM/AAAA) ?','A',,mono,free) and @Prompt('02 - Date fin journée péage (DD/MM/AAAA) ?','A',,mono,free)
  AND  COD_DRE  in  @Prompt('08 - Dre(s) entrée second passage ?','A',,multi,free)
  AND  MOD_RGLT  in  ('TPE','CB','COF','DKV','DYN','ESS','EUR','GR','RES','SHE','UTA')
  AND NVL("IDT_TOK_PAN","NUM_MOY_PAIM") IS NOT NULL
  AND FLG_TRAN_VALID = '0') TRAJET2,
  (SELECT
  NVL("IDT_TOK_PAN","NUM_MOY_PAIM") NUM_MOY_PAIM,
  TO_DATE(DAT_SOR,'YYYYMMDDHH24MISS') DAT_SOR_TRAJET1,
  TO_DATE(DAT_ENTR,'YYYYMMDDHH24MISS') DAT_ENTR_TRAJET1,
  COD_GARE_SOR,cod_gare_entr,
  CLAS_TABU,
  COD_PAY_ENTR,
  COD_PAY_SOR,
  COD_STE_ENTR,
  COD_STE_SOR,
  CASE WHEN MOD_RGLT in ('COF','DKV','DYN','ESS','EUR','GR','RES','SHE','UTA') THEN 'ACCREDITIVES' ELSE MOD_RGLT END MOD_RGLT
FROM
  TRAN_RAMP
WHERE
  nvl(TYP_FIC,'T')='T' 
  AND  COD_GARE_ENTR  in @Prompt('03 - Gare(s) d''entrée premier passage ?','A',,multi,free)
  --and ((cod_gare_entr between '201' and '223') OR COD_GARE_SOR in ('261','262'))
  AND  DAT_JOU_PEA  between @Prompt('01 - Date début journée péage (DD/MM/AAAA) ?','A',,mono,free) and @Prompt('02 - Date fin journée péage (DD/MM/AAAA) ?','A',,mono,free)
  AND  COD_DRE  in  @Prompt('05 - Dre(s) entrée premier passage ?','A',,multi,free)
  AND  MOD_RGLT  in  ('TPE','CB','COF','DKV','DYN','ESS','EUR','GR','RES','SHE','UTA')
  AND NVL("IDT_TOK_PAN","NUM_MOY_PAIM") IS NOT NULL
  AND FLG_TRAN_VALID = '0') TRAJET1, HIS_TRJT_IFCT H
WHERE TRAJET1.NUM_MOY_PAIM = TRAJET2.NUM_MOY_PAIM 
AND TRAJET2.COD_PAY_ENTR = H.COD_PAY_ENTR
AND TRAJET2.COD_STE_ENTR = H.COD_STE_ENTR
AND TRAJET2.COD_GARE_ENTR = H.COD_GAR_ENTR
AND TRAJET2.COD_PAY_SOR = H.COD_PAY_SOR
AND TRAJET2.COD_STE_SOR = H.COD_STE_SOR
AND TRAJET2.COD_GARE_SOR = H.COD_GAR_SOR
--AND TRAJET2.COD_PAY_ENTR = H.COD_PAY_ENTR
--AND TRAJET2.COD_STE_ENTR = H.COD_STE_ENTR
--AND TRAJET2.COD_GARE_ENTR = H.COD_GAR_ENTR
--AND TRAJET2.COD_PAY_SOR = H.COD_PAY_SOR
--AND TRAJET2.COD_STE_SOR = H.COD_STE_SOR
--AND TRAJET2.COD_GARE_SOR = H.COD_GAR_SOR
AND H.DAT_FIN_EFF IS NULL
AND CASE WHEN H.TRJT_TYP = 'O' THEN TO_DATE(DAT_SOR_TRAJET2,'DD-MM-YYYY HH24:MI:SS') ELSE TO_DATE(DAT_ENTR_TRAJET2,'DD-MM-YYYY HH24:MI:SS') END
    BETWEEN TO_DATE(DAT_SOR_TRAJET1,'DD-MM-YYYY HH24:MI:SS')+ @Prompt('09 - Délai début second passage (heures) ?','A',,mono,free)/24 AND TO_DATE(DAT_SOR_TRAJET1,'DD-MM-YYYY HH24:MI:SS')+ @Prompt('10 - Délai fin second passage (heures) ?','A',,mono,free)/24
    GROUP BY CASE WHEN H.TRJT_TYP = 'O' THEN TO_CHAR(DAT_SOR_TRAJET1,'DD/MM/YYYY') ELSE TO_CHAR(DAT_ENTR_TRAJET1,'DD/MM/YYYY') END,TRAJET1.CLAS_TABU,TRAJET1.MOD_RGLT,TRAJET1.COD_GARE_SOR,TRAJET2.COD_GARE_ENTR