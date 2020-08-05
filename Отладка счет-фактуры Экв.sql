SET SERVEROUTPUT ON;
EXEC CU.set_org_id(10);
declare
  fid INTEGER;
  bid INTEGER := 227539;
  cnt INTEGER;
  fnum INTEGER := 6003;
begin
  SELECT count(*) INTO cnt FROM FACT_LIST WHERE BASE_ID=bid AND FNUMBER=fnum;
  IF (cnt > 0) THEN
    SELECT ID INTO fid FROM FACT_LIST WHERE BASE_ID=bid AND FNUMBER=fnum;
    FACTF.clear(fid);
  END IF;
  CU.set_org_id(10);
  FACTF.MAKE_FACT(bid, 'N', 30, 2, null, fnum, to_date('31.07.2020', 'dd.mm.yyyy'), '', null);
  /*PROCEDURE make_fact(baseid, is_nal, type_id, prc_prec, opts, fnumber, fdate, waybids, contract);*/
  --round(a.PRICE * /*$S.*/RELF.get_rate(/*$S.*/RELF.get_base_rel(:BASE_ID, :ACT_ID), a.VAL_ID, /*$S.*/SYSPC.ROUBLE, NVL(b.DATE_GOODS, SYSDATE)) / (1 + 0.01*v.NDS), 2)
end;
/

--price_nonds := to_number(SUBSTR(to_char(wlc.price),1,4)) / (1 + 0.01 * nds);
--price_nonds := round(round((1 + 0.01*NVL(wlc.CONV_PCT,0)) * wlc.PRICE, 2) * (1 + 0.01*NVL(nds,0)), 2);
--dbms_output.put_line('round(round((1 + 0.01*NVL('||wlc.CONV_PCT||',0)) * '||wlc.PRICE||', 2) * (1 + 0.01*NVL('||nds||',0)), 2)');

--SELECT ID FROM FACT_LIST WHERE BASE_ID=227539 AND FNUMBER=6003;
--SELECT * FROM FACT_GOODS WHERE FACT_ID = 52014;
/*
SELECT wg.price, ABS(nvl(SUM(wg.num_ * wl.MOVE), 0)) num_, 
      nvl(SUM(wg.num2_ * wl.MOVE), 0) num2_, wg.goods_id, wl.cust_id, MAX(wl.date_) wbdate, 
      wl.base_id, bl.conv_pct,
      round((100 * (bg.price0 - bg.price)) / bg.price0, 2) disc_pct,
      CASE MAX(al.defect_pos) WHEN 'Y' THEN 'Y' END AS defect_pos
      FROM waybill_list wl, waybill_goods wg, base_list bl, base_goods bg, act_list al
     WHERE wg.wayb_id = wl.ID
       AND wl.base_id = bl.ID
       AND bg.base_id = bl.ID
       AND bg.goods_id = wg.goods_id
       --and wl.id = wlid
       AND TRUNC(wl.date_) = :wdt
       AND wl.act_id = al.ID
       AND bl.btype_id = 40
       AND bg.act_id != 431
       AND al.IS_RETURN = 'N'
       AND bl.ORG_ID = 10
     GROUP BY wg.price, wg.goods_id, wg.goods_id, wl.cust_id,
       wl.base_id, bl.conv_pct, (100 * (bg.price0 - bg.price)) / bg.price0, 10;

*/