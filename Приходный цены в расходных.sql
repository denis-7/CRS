  SET SERVEROUTPUT ON;
  declare
    CURSOR gtd_cur(gid INTEGER, gnum NUMBER, wb INTEGER) IS
      SELECT pq.GTD
        FROM PRT_HIST ph, PRT_QUEUE pq
       WHERE ph.QUEUE_ID = pq.ID
         AND ph.GOODS_ID = gid
         AND ph.NUM_ = gnum
         AND ph.WAYB_ID = wb;
    --
    save_ii  CHAR(1);
    curr_num NUMBER;
    curr_sum NUMBER;
    curr_prc NUMBER;
    num      NUMBER;
    i        BINARY_INTEGER := 0;
    j        BINARY_INTEGER := 0;
    old_goods_id BINARY_INTEGER := 0;
    ch_price VARCHAR2(1);
    --do_dbg   BOOLEAN := (NVL(dbg,'N') = 'Y') AND (NVL(goodsid,0) > 0);
    goodsid  INTEGER := 49316;
    orgid    INTEGER := 11;
    rgtd     gtd_cur%ROWTYPE;
    wayb_in  INTEGER;
  BEGIN
    save_ii := SECUR.is_internal_import;
    SECUR.set_internal_import('Y');
    FOR c IN (
      SELECT wg.ID, wl.MOVE, wg.WAYB_ID, wl.DATE_, wg.GOODS_ID, wg.PRICE, wg.NUM_, 
             wg.PRICE_IN, wg.PRICE*wg.RATE as PRICE_YE, wg.ROWID as ROW_ID, wl.ID OUT_WAYB,
             WAYBILLP.part_orig_inwayb2(wg.GOODS_ID, null, wg.ID, null, wg.NUM_, 'Y') IN_WAYB
        FROM WAYBILL_GOODS wg, WAYBILL_LIST wl, STORE_LIST sl, ACT_LIST al
       WHERE wg.WAYB_ID = wl.ID AND wl.STORE_ID = sl.ID AND wl.ACT_ID = al.ID 
         AND sl.ORG_ID = NVL(orgid, CU.org_id) AND al.IS_INTERNAL = 'N'
         AND ((wg.GOODS_ID = goodsid) OR (goodsid IS NULL))
       ORDER BY wg.GOODS_ID, wl.DATE_
    ) LOOP
      IF c.move = -1 THEN
        --UPDATE WAYBILL_GOODS SET PRICE_IN = curr_prc WHERE ROWID = c.row_id;
        i := 0;
        OPEN gtd_cur(c.GOODS_ID, c.NUM_, c.OUT_WAYB);
        LOOP
          FETCH gtd_cur INTO rgtd;
          EXIT WHEN  gtd_cur%NOTFOUND;
          wayb_in := WAYBILLP.part_orig_inwayb2(c.GOODS_ID, null, c.OUT_WAYB, rgtd.GTD, c.NUM_,  'Y');
          dbms_output.put_line(i||') '||'gtd: ' || rgtd.GTD || ', gid: ' || c.GOODS_ID || ', NUM_: ' || c.NUM_ || ', IN_WAYB: ' || wayb_id);
          i := i + 1;
        END LOOP;
        CLOSE gtd_cur;
      ELSIF c.move = 1 THEN
        --curr_sum := curr_sum + c.num_ * c.price_ye;
        --curr_num := curr_num + c.num_;
        null;
      END IF;
    END LOOP;    
    COMMIT;
    SECUR.set_internal_import(save_ii);
  END;