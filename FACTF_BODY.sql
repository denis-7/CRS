--------------------------------------------------------
--  File created - понедельник-августа-03-2020   
--------------------------------------------------------
--------------------------------------------------------
--  DDL for Package Body FACTF
--------------------------------------------------------

  CREATE OR REPLACE PACKAGE BODY "CRS"."FACTF" AS
--
  SUBTYPE FactListRowType  IS FACT_LIST%ROWTYPE;
  SUBTYPE FactGoodsRowType IS FACT_GOODS%ROWTYPE;
--
  FUNCTION get_supported_types RETURN VARCHAR2 IS
    r VARCHAR2(2000);
  BEGIN
    FOR c IN (SELECT id FROM FACT_TYPES ORDER BY ORD) LOOP
      r := r || ',';
    END LOOP;
    RETURN substr(r, 1, length(r) - 1);
  END;
--
  FUNCTION get_supp_type_name(typeid IN INTEGER) RETURN VARCHAR2 IS
    r VARCHAR2(80) := '???';
  BEGIN
    FOR c IN (SELECT * FROM FACT_TYPES WHERE ID = typeid) LOOP
      r := c.name;
    END LOOP;
    RETURN r;
  END;
--
  FUNCTION get_val_by_type(typeid IN INTEGER) RETURN INTEGER IS
    r INTEGER := 5;
  BEGIN
    FOR c IN (SELECT * FROM FACT_TYPES WHERE ID = typeid) LOOP
      r := c.val_id;
    END LOOP;
    RETURN r;
  END;
--
  FUNCTION get_base_id(factid IN INTEGER) RETURN INTEGER IS
    CURSOR cur IS
      SELECT base_id FROM FACT_LIST WHERE id = factid;
    r INTEGER;
  BEGIN
    OPEN cur;
    FETCH cur INTO r;
    CLOSE cur;
    RETURN r;
  END;
--
  FUNCTION get_cust_nds(custid IN INTEGER) RETURN NUMBER IS
    r NUMBER;
  BEGIN
    SELECT to_number(NVL(MAX(oa.VALUE), SYSPC.NDS))
      INTO r
      FROM OBJ_ATTRIBUTES oa, ATTR_LIST al
     WHERE al.ID = oa.ATTR_ID and al.KEY = 'SPEC_NDS' and oa.OBJ_ID = custid;
    RETURN r;    
  END;
--
  PROCEDURE reord_fact(factid IN INTEGER) IS
    CURSOR factreord_cur IS
      SELECT ord FROM FACT_GOODS WHERE FACT_ID = factid
      ORDER BY ord FOR UPDATE;
    factno INTEGER;
  BEGIN
    factno := 1;
    FOR t IN factreord_cur LOOP
      UPDATE FACT_GOODS SET ord = factno
        WHERE CURRENT OF factreord_cur;
      factno := factno + 1;
    END LOOP;
  END;
--
  PROCEDURE adjust_fact(factid IN INTEGER, delt_summ IN NUMBER,
    delt_summnp IN NUMBER, prc_prec IN NUMBER DEFAULT 2)
  IS
    CURSOR factcorr_cur(goodsid IN INTEGER) IS
      SELECT FACT_ID, GOODS_ID, NUM_, PRICE, SUM_NDS, SUM_NP
      FROM FACT_GOODS WHERE FACT_ID = factid AND GOODS_ID = goodsid;
    t1 factcorr_cur%ROWTYPE;
    t2 factcorr_cur%ROWTYPE;
    t3 factcorr_cur%ROWTYPE;
    n          INTEGER;
    min_goods  INTEGER;
    max_goods  INTEGER;
    max_loop   BINARY_INTEGER := 5;
    nds        NUMBER;
    sumost     NUMBER;
    sumostnp   NUMBER;
    diffp      NUMBER;
  BEGIN
    SELECT count(*) INTO n FROM FACT_GOODS WHERE FACT_ID = factid;
    nds := get_cust_nds(BASEF.get_cust_id(get_base_id(factid))); --SYSPC.NDS;
    sumost := NVL(delt_summ, 0);
    sumostnp := NVL(delt_summnp, 0);
    IF (n = 0) OR (sumost = 0 AND sumostnp = 0) THEN RETURN; END IF;
    SELECT max(goods_id) INTO min_goods FROM FACT_GOODS
      WHERE FACT_ID = factid AND NUM_ =
        (SELECT min(NUM_) FROM FACT_GOODS WHERE FACT_ID = factid);
    SELECT max(goods_id) INTO max_goods FROM FACT_GOODS
      WHERE FACT_ID = factid AND NUM_ =
        (SELECT max(NUM_) FROM FACT_GOODS WHERE FACT_ID = factid);
    OPEN factcorr_cur(min_goods);
    FETCH factcorr_cur INTO t1;
    IF factcorr_cur%FOUND THEN
      CLOSE factcorr_cur;
      t3.PRICE  := t1.PRICE + ((100 * sumost) / (t1.NUM_ * (100 + nds)));
      t3.SUM_NDS := t1.SUM_NDS + (nds * sumost / (100 + nds));
      t3.SUM_NP  := t1.SUM_NP + sumostnp;
      WHILE NVL(t3.PRICE,0) <= 0 AND max_loop > 0 LOOP
        -- Погрешность "уводит" цену мин.позиции меньше нуля,
        -- корректируем позицию с макс. кол-вом
        OPEN factcorr_cur(max_goods);
        FETCH factcorr_cur INTO t2;
        IF factcorr_cur%FOUND THEN
          diffp := t2.NUM_ * t2.PRICE + t2.SUM_NDS;
          sumost := sumost + (t2.NUM_ * t2.PRICE + t2.SUM_NDS);
          sumostnp := sumostnp + t2.SUM_NP;
          t2.PRICE := t2.PRICE - power(10, -prc_prec);
          t2.SUM_NDS := ROUND(t2.NUM_ * t2.PRICE * nds / 100, 2);
          IF NVL(diffp,0) != 0 THEN
            t2.SUM_NP := (t2.SUM_NP * (t2.NUM_ * t2.PRICE + t2.SUM_NDS)) / diffp;
          END IF;
          sumost := sumost - (t2.NUM_ * t2.PRICE + t2.SUM_NDS);
          sumostnp := sumostnp - t2.SUM_NP;
          UPDATE FACT_GOODS
            SET PRICE = t2.PRICE, SUM_NDS = t2.SUM_NDS, SUM_NP = t2.SUM_NP
            WHERE FACT_ID = factid AND GOODS_ID = max_goods;
          t3.PRICE  := t1.PRICE + ((100 * sumost) / (t1.NUM_ * (100 + nds)));
          t3.SUM_NDS := t1.SUM_NDS + (nds * sumost / (100 + nds));
          t3.SUM_NP  := t1.SUM_NP + sumostnp;
        ELSE
          t3.PRICE := 0.01;
          t3.SUM_NDS := 0;
          t3.SUM_NP  := 0;
        END IF;
        max_loop := max_loop - 1;
        CLOSE factcorr_cur;
      END LOOP;
      -- Коррекция позиции с мин.кол-вом (списание погрешностей)
      UPDATE FACT_GOODS
        SET PRICE = t3.PRICE, SUM_NDS = t3.SUM_NDS, SUM_NP = t3.SUM_NP -- debug -- , CODE_GTD='*'
        WHERE FACT_ID = factid AND GOODS_ID = min_goods;
    ELSE
      CLOSE factcorr_cur;
    END IF;
  END;
--
  PROCEDURE adjust_fact2(factid IN INTEGER, delt_summ IN NUMBER,
    delt_summnp IN NUMBER, prc_prec IN NUMBER DEFAULT 2)
  IS
    CURSOR fcorr_cur IS
      SELECT GOODS_ID, NUM_, PRICE, SUM_NDS, SUM_NP
      FROM FACT_GOODS WHERE FACT_ID = factid
      ORDER BY NUM_ DESC, PRICE DESC FOR UPDATE;
    c fcorr_cur%ROWTYPE;
    goodsid   BINARY_INTEGER := 0;
    num_      NUMBER;
    delt      NUMBER;
    delt_np   NUMBER;
    max_mist  NUMBER;
    d1        NUMBER;
    k1        NUMBER;
    m1        NUMBER;
    n1        NUMBER;
    nds1      NUMBER;
    nds2      NUMBER;
  BEGIN
    nds1 := NVL(0.01 * get_cust_nds(BASEF.get_cust_id(get_base_id(factid))) /*SYSPC.NDS */, 0);
    nds2 := 1 + nds1;
    delt := delt_summ;
    delt_np := delt_summnp;
    max_mist := power(10, -prc_prec);
    OPEN fcorr_cur;
    FETCH fcorr_cur INTO c;
    WHILE fcorr_cur%FOUND AND ABS(delt) >= max_mist LOOP
      goodsid := c.goods_id;
      num_ := c.num_;
      d1 := c.num_ * max_mist * nds2;
      IF ABS(delt) >= d1 AND ((delt > 0) OR (d1 > (c.num_ * c.price + c.sum_nds))) THEN
        k1 := round(ABS(delt) / d1);
        m1 := sign(delt) * k1 * max_mist;
        n1 := round(m1 * c.num_ * nds1, prc_prec);
        UPDATE FACT_GOODS SET
          PRICE   = PRICE + m1,
          SUM_NDS = SUM_NDS + n1
          --SUM_NP  = SUM_NP
          -- debug -- , CODE_GTD='*'
        WHERE CURRENT OF fcorr_cur;
        delt := delt - (m1 * c.num_ + n1);
      END IF;
      FETCH fcorr_cur INTO c;
    END LOOP;
    CLOSE fcorr_cur;
    IF ABS(delt) >= max_mist AND goodsid > 0 THEN
      m1 := delt / (nds2 * num_);
      n1 := m1 * num_ * nds1;
      UPDATE FACT_GOODS SET
        PRICE   = PRICE + m1,
        SUM_NDS = SUM_NDS + n1
        --SUM_NP  = SUM_NP
        -- debug -- , CODE_GTD='*'
      WHERE FACT_ID = factid AND GOODS_ID = goodsid;
    END IF;
  END;
--
  PROCEDURE adjust_fact_debug(factid IN INTEGER, delt_summ IN NUMBER,
    delt_summnp IN NUMBER, prc_prec IN NUMBER DEFAULT 2)
  IS
    CURSOR factcorr_cur(goodsid IN INTEGER) IS
      SELECT FACT_ID, GOODS_ID, NUM_, PRICE, SUM_NDS, SUM_NP
      FROM FACT_GOODS WHERE FACT_ID = factid AND GOODS_ID = goodsid;
    t1 factcorr_cur%ROWTYPE;
    t2 factcorr_cur%ROWTYPE;
    t3 factcorr_cur%ROWTYPE;
    n          INTEGER;
    min_goods  INTEGER;
    max_goods  INTEGER;
    max_loop   BINARY_INTEGER := 5;
    nds        NUMBER;
    sumost     NUMBER;
    sumostnp   NUMBER;
    diffp      NUMBER;
  BEGIN
    SELECT count(*) INTO n FROM FACT_GOODS WHERE FACT_ID = factid;
    nds := get_cust_nds(BASEF.get_cust_id(get_base_id(factid))); --SYSPC.NDS;
    sumost := NVL(delt_summ, 0);
    sumostnp := NVL(delt_summnp, 0);
    IF (n = 0) OR (sumost = 0 AND sumostnp = 0) THEN RETURN; END IF;
    SELECT max(goods_id) INTO min_goods FROM FACT_GOODS
      WHERE FACT_ID = factid AND NUM_ =
        (SELECT min(NUM_) FROM FACT_GOODS WHERE FACT_ID = factid);
    SELECT max(goods_id) INTO max_goods FROM FACT_GOODS
      WHERE FACT_ID = factid AND NUM_ =
        (SELECT max(NUM_) FROM FACT_GOODS WHERE FACT_ID = factid);
    OPEN factcorr_cur(min_goods);
    FETCH factcorr_cur INTO t1;
    IF factcorr_cur%FOUND THEN
      CLOSE factcorr_cur;
      t3.PRICE  := t1.PRICE + ((100 * sumost) / (t1.NUM_ * (100 + nds)));
      t3.SUM_NDS := t1.SUM_NDS + (nds * sumost / (100 + nds));
      t3.SUM_NP  := t1.SUM_NP + sumostnp;
      WHILE NVL(t3.PRICE,0) <= 0 AND max_loop > 0 LOOP
        -- Погрешность "уводит" цену мин.позиции меньше нуля,
        -- корректируем позицию с макс. кол-вом
        OPEN factcorr_cur(max_goods);
        FETCH factcorr_cur INTO t2;
        IF factcorr_cur%FOUND THEN
          diffp := t2.NUM_ * t2.PRICE + t2.SUM_NDS;
          sumost := sumost + (t2.NUM_ * t2.PRICE + t2.SUM_NDS);
          sumostnp := sumostnp + t2.SUM_NP;
          t2.PRICE := t2.PRICE - power(10, -prc_prec);
          t2.SUM_NDS := ROUND(t2.NUM_ * t2.PRICE * nds / 100, 2);
          IF NVL(diffp,0) != 0 THEN
            t2.SUM_NP := (t2.SUM_NP * (t2.NUM_ * t2.PRICE + t2.SUM_NDS)) / diffp;
          END IF;
          sumost := sumost - (t2.NUM_ * t2.PRICE + t2.SUM_NDS);
          sumostnp := sumostnp - t2.SUM_NP;
          --UPDATE FACT_GOODS
          --  SET PRICE = t2.PRICE, SUM_NDS = t2.SUM_NDS, SUM_NP = t2.SUM_NP
          --  WHERE FACT_ID = factid AND GOODS_ID = max_goods;
          dbms_output.put_line('=== Ajust fact. SET: ' || 'PRICE = '||t2.PRICE||', SUM_NDS = '||t2.SUM_NDS||', SUM_NP = '||t2.SUM_NP);
          t3.PRICE  := t1.PRICE + ((100 * sumost) / (t1.NUM_ * (100 + nds)));
          t3.SUM_NDS := t1.SUM_NDS + (nds * sumost / (100 + nds));
          t3.SUM_NP  := t1.SUM_NP + sumostnp;
        ELSE
          t3.PRICE := 0.01;
          t3.SUM_NDS := 0;
          t3.SUM_NP  := 0;
        END IF;
        max_loop := max_loop - 1;
        CLOSE factcorr_cur;
      END LOOP;
      -- Коррекция позиции с мин.кол-вом (списание погрешностей)
      --UPDATE FACT_GOODS
      --  SET PRICE = t3.PRICE, SUM_NDS = t3.SUM_NDS, SUM_NP = t3.SUM_NP -- debug -- , CODE_GTD='*'
      --  WHERE FACT_ID = factid AND GOODS_ID = min_goods;
      dbms_output.put_line('=== Ajust fact. Correct pos with min count: SET: PRICE = '||t3.PRICE||', || SUM_NDS = '||t3.SUM_NDS||', SUM_NP = '||t3.SUM_NP);
    ELSE
      CLOSE factcorr_cur;
    END IF;
  END;
--
  PROCEDURE subtract_fact(factid IN INTEGER, prc_prec IN NUMBER DEFAULT 2, opts IN VARCHAR2 DEFAULT NULL) IS
    CURSOR goods1_cur IS
      SELECT GOODS_ID, NUM_, SUM_NDS, SUM_NP
      FROM FACT_GOODS WHERE FACT_ID = factid FOR UPDATE;
    CURSOR goods2_cur(goodsid IN INTEGER) IS
      SELECT NVL(sum(a.NUM_),0) NUM_,
        NVL(sum(a.SUM_NDS*NVL(b.RATE,1)/NVL(c.RATE,1)),0) SUM_NDS,
        NVL(sum(a.SUM_NP*NVL(b.RATE,1)/NVL(c.RATE,1)),0) SUM_NP
      FROM FACT_GOODS a, FACT_LIST b, FACT_LIST c
      WHERE a.GOODS_ID = goodsid AND a.FACT_ID = b.ID AND b.ID != c.ID
        AND b.BASE_ID = c.BASE_ID AND b.ACT_ID = c.ACT_ID AND c.ID = factid;
    CURSOR payms1_cur IS
      SELECT SUMM FROM FACT_PAYMENTS WHERE FACT_ID = factid
      ORDER BY DATE_ FOR UPDATE;
    c2     goods2_cur%ROWTYPE;
    ps     NUMBER;
    sum1   NUMBER;
    sumnp1 NUMBER;
    sum2   NUMBER;
    sumnp2 NUMBER;
  BEGIN
    SELECT NVL(sum(SUM_NP),0), NVL(sum(NUM_ * PRICE + NVL(SUM_NDS,0)),0)
      INTO sumnp1, sum1 FROM FACT_GOODS WHERE FACT_ID = factid;
    SELECT NVL(sum((NVL(a.SUM_NP,0) * NVL(b.RATE,1)) / NVL(c.RATE,1)),0),
        NVL(sum(((a.NUM_ * a.PRICE + NVL(a.SUM_NDS,0)) * NVL(b.RATE,1)) / NVL(c.RATE,1)),0)
      INTO sumnp2, sum2
      FROM FACT_GOODS a, FACT_LIST b, FACT_LIST c
      WHERE a.FACT_ID = b.ID AND b.ID != c.ID AND c.ID = factid
        AND b.BASE_ID = c.BASE_ID AND b.ACT_ID = c.ACT_ID;
    -- sum2/sumnp2: на какую сумму должна выйти фактура
    sum2 := sum1 - sum2;
    sumnp2 := sumnp1 - sumnp2;
    -- Вычитание уже проведенных товаров
    FOR c1 IN goods1_cur LOOP
      OPEN goods2_cur(c1.GOODS_ID);
      FETCH goods2_cur INTO c2;
      IF goods2_cur%FOUND THEN
        IF c2.NUM_ >= c1.NUM_ THEN
          DELETE FACT_GOODS
            WHERE CURRENT OF goods1_cur;
        ELSIF c2.NUM_ != 0 THEN
          UPDATE FACT_GOODS
            SET NUM_ = NUM_ - c2.NUM_,
                SUM_NDS = SUM_NDS - c2.SUM_NDS,
                SUM_NP = SUM_NP - c2.SUM_NP
            WHERE CURRENT OF goods1_cur;
        END IF;
      END IF;
      CLOSE goods2_cur;
    END LOOP;
    -- Вычитание уже проведенных платежей
    SELECT NVL(sum(a.SUMM*NVL(b.RATE,1)/NVL(c.RATE,1)),0) INTO ps
     FROM FACT_PAYMENTS a, FACT_LIST b, FACT_LIST c
     WHERE a.FACT_ID = b.ID AND b.ID != c.ID AND b.BASE_ID = c.BASE_ID
       AND b.ACT_ID = c.ACT_ID AND c.ID = factid;
    FOR p1 IN payms1_cur LOOP
      IF ps > 0 THEN
        IF ps >= p1.SUMM THEN
          DELETE FACT_PAYMENTS
            WHERE CURRENT OF payms1_cur;
          ps := ps - p1.SUMM;
        ELSE
          UPDATE FACT_PAYMENTS
            SET SUMM = SUMM - ps
            WHERE CURRENT OF payms1_cur;
          ps := 0;
        END IF;
      END IF;
    END LOOP;
    reord_fact(factid);
    -- подгонка по сумме
    IF NVL(INSTR(opts, 'A'), 0) = 0 THEN 
      SELECT NVL(sum(SUM_NP),0), NVL(sum(NUM_ * PRICE + NVL(SUM_NDS,0)),0)
        INTO sumnp1, sum1 FROM FACT_GOODS WHERE FACT_ID = factid;
      adjust_fact(factid, sum2-sum1, sumnp2-sumnp1, prc_prec);
    END IF;
  END;
--
  PROCEDURE subtract_fact_debug(factid IN INTEGER, prc_prec IN NUMBER DEFAULT 2, opts IN VARCHAR2 DEFAULT NULL) IS
    CURSOR goods1_cur IS
      SELECT GOODS_ID, NUM_, SUM_NDS, SUM_NP
      FROM FACT_GOODS WHERE FACT_ID = factid FOR UPDATE;
    CURSOR goods2_cur(goodsid IN INTEGER) IS
      SELECT NVL(sum(a.NUM_),0) NUM_,
        NVL(sum(a.SUM_NDS*NVL(b.RATE,1)/NVL(c.RATE,1)),0) SUM_NDS,
        NVL(sum(a.SUM_NP*NVL(b.RATE,1)/NVL(c.RATE,1)),0) SUM_NP
      FROM FACT_GOODS a, FACT_LIST b, FACT_LIST c
      WHERE a.GOODS_ID = goodsid AND a.FACT_ID = b.ID AND b.ID != c.ID
        AND b.BASE_ID = c.BASE_ID AND b.ACT_ID = c.ACT_ID AND c.ID = factid;
    CURSOR payms1_cur IS
      SELECT SUMM FROM FACT_PAYMENTS WHERE FACT_ID = factid
      ORDER BY DATE_ FOR UPDATE;
    c2     goods2_cur%ROWTYPE;
    ps     NUMBER;
    sum1   NUMBER;
    sumnp1 NUMBER;
    sum2   NUMBER;
    sumnp2 NUMBER;
  BEGIN
    SELECT NVL(sum(SUM_NP),0), NVL(sum(NUM_ * PRICE + NVL(SUM_NDS,0)),0)
      INTO sumnp1, sum1 FROM FACT_GOODS WHERE FACT_ID = factid;
    SELECT NVL(sum((NVL(a.SUM_NP,0) * NVL(b.RATE,1)) / NVL(c.RATE,1)),0),
        NVL(sum(((a.NUM_ * a.PRICE + NVL(a.SUM_NDS,0)) * NVL(b.RATE,1)) / NVL(c.RATE,1)),0)
      INTO sumnp2, sum2
      FROM FACT_GOODS a, FACT_LIST b, FACT_LIST c
      WHERE a.FACT_ID = b.ID AND b.ID != c.ID AND c.ID = factid
        AND b.BASE_ID = c.BASE_ID AND b.ACT_ID = c.ACT_ID;
    -- sum2/sumnp2: на какую сумму должна выйти фактура
    sum2 := sum1 - sum2;
    sumnp2 := sumnp1 - sumnp2;
    -- Вычитание уже проведенных товаров
    FOR c1 IN goods1_cur LOOP
      OPEN goods2_cur(c1.GOODS_ID);
      FETCH goods2_cur INTO c2;
      IF goods2_cur%FOUND THEN
        IF c2.NUM_ >= c1.NUM_ THEN
          --DELETE FACT_GOODS
          --  WHERE CURRENT OF goods1_cur;
          null;
        ELSIF c2.NUM_ != 0 THEN
          --UPDATE FACT_GOODS
          --  SET NUM_ = NUM_ - c2.NUM_,
          --      SUM_NDS = SUM_NDS - c2.SUM_NDS,
          --      SUM_NP = SUM_NP - c2.SUM_NP
          --  WHERE CURRENT OF goods1_cur;
          DBMS_OUTPUT.put_line('Subtract. SET: NUM_ = ' || (c1.NUM_ - c2.NUM_) || ', SUM_NDS = ' || (c1.SUM_NDS - c2.SUM_NDS) || ', SUM_NP = ' || (c1.SUM_NP - c2.SUM_NP));
        END IF;
      END IF;
      CLOSE goods2_cur;
    END LOOP;
    -- Вычитание уже проведенных платежей
    SELECT NVL(sum(a.SUMM*NVL(b.RATE,1)/NVL(c.RATE,1)),0) INTO ps
     FROM FACT_PAYMENTS a, FACT_LIST b, FACT_LIST c
     WHERE a.FACT_ID = b.ID AND b.ID != c.ID AND b.BASE_ID = c.BASE_ID
       AND b.ACT_ID = c.ACT_ID AND c.ID = factid;
    FOR p1 IN payms1_cur LOOP
      IF ps > 0 THEN
        IF ps >= p1.SUMM THEN
          --DELETE FACT_PAYMENTS
          --  WHERE CURRENT OF payms1_cur;
          ps := ps - p1.SUMM;
        ELSE
          --UPDATE FACT_PAYMENTS
          --  SET SUMM = SUMM - ps
          --  WHERE CURRENT OF payms1_cur;
          ps := 0;
        END IF;
      END IF;
    END LOOP;
    --reord_fact_debug(factid);
    -- подгонка по сумме
    IF NVL(INSTR(opts, 'A'), 0) = 0 THEN 
      SELECT NVL(sum(SUM_NP),0), NVL(sum(NUM_ * PRICE + NVL(SUM_NDS,0)),0)
        INTO sumnp1, sum1 FROM FACT_GOODS WHERE FACT_ID = factid;
      adjust_fact_debug(factid, sum2-sum1, sumnp2-sumnp1, prc_prec);
    END IF;
  END;
--
  PROCEDURE def_fact_check(baseid IN INTEGER) IS
    flag CHAR(1);
    errm VARCHAR2(4001);
  BEGIN
    errm := NVL(BASEF.get_is_allowed(baseid, 'DOCS'), 'N');
    IF errm != 'Y' THEN
      raise_application_error(-20500, 'Состояние основания №' || to_char(baseid) ||
        ' не допускает проведения счетов-фактур. (' || substr(errm, 3) || ')');
    END IF;
    -- Проверка клиента
    SELECT is_firm INTO flag
      FROM OBJ_LIST WHERE id = BASEF.get_cust_id(baseid);
    IF flag != 'Y' THEN
      raise_application_error(-20500, OBJF.get_name(BASEF.get_cust_id(baseid)) ||
       ' не является юридическим лицом. Провести счет-фактуру невозможно.');
    END IF;
  END;
--
  FUNCTION def_fact_check2(fnumber IN INTEGER DEFAULT NULL, 
    fdate IN DATE DEFAULT NULL, orgid IN INTEGER DEFAULT NULL) RETURN VARCHAR2 
  IS
  --
  CURSOR mc(fn IN INTEGER, fd IN DATE, pfx IN VARCHAR2) IS
    SELECT count(*) cnt, a.base_id
    FROM FACT_LIST a, BASE_LIST bl
    WHERE pfx || a.FNUMBER = pfx || fn
    AND a.base_id = bl.id
    AND (CASE WHEN ORGF.is_sub(NVL(orgid, CU.org_id)) = 'Y' AND a.PREFIX = pfx THEN 1
         WHEN ORGF.is_sub(NVL(orgid, CU.org_id)) = 'N' AND a.PREFIX IS NULL THEN 1 END) = 1
    AND bl.ORG_ID = NVL(orgid, CU.org_id)
    AND extract(year from SYSDATE) = extract(year from a.FDATE)
    group by a.base_id;
  --
  ret VARCHAR2(100);
  mcr mc%ROWTYPE;
  pfx VARCHAR2(1) := '';
  org INTEGER;
  BEGIN
    org := NVL(orgid, CU.org_id);
    IF (ORGF.is_sub(org) = 'Y') THEN
      pfx := ORGF.fact_prefix(org);
    END IF;
    open mc(fnumber, fdate, pfx);
    fetch mc into mcr;
    close mc;
    if (mcr.cnt > 0 ) then
      ret := 'Этот номер счет-фактуры уже существует в основании ' || mcr.base_id || '.';
    end if;
    return ret; 
  END;
--
  PROCEDURE def_fact_attribs(baseid IN INTEGER, type_id_ IN NUMBER,
    fnumber_ IN INTEGER DEFAULT NULL, fdate IN DATE DEFAULT NULL,
    fl IN OUT FactListRowType, orgid IN INTEGER DEFAULT NULL) 
  IS
    oid_ INTEGER;
    sn INTEGER;
  BEGIN
    oid_ := NVL(orgid, CU.org_id);
    IF fdate IS NULL THEN
      fl.FDATE := SYSDATE;
    ELSE
      fl.FDATE := fdate;
    END IF;
    fl.ACT_ID := SYSPC.FACT_ID;
    fl.MOVE := -1;
    fl.BASE_ID := baseid;
    fl.TYPE_ID := type_id_;
    --fl.EXP_DATE := SYSDATE + SYSPC.FACTNEW_LIFE;
    fl.EXP_DATE := to_date(to_char(SYSDATE, 'dd.mm.yyyy') || ' 23:59:59', 'dd.mm.yyyy HH24:MI:SS');
    fl.PAYM_NP := NULL;
    fl.VAL_ID := get_val_by_type(type_id_);
    fl.PREFIX := ORGF.FACT_PREFIX(oid_);
    SELECT s_factl_id.NEXTVAL INTO fl.ID FROM dual;
    IF fnumber_ IS NULL THEN
      --SELECT nvl(max(FNUMBER), 0) + 1 INTO fl.FNUMBER FROM
      --(SELECT f.FNUMBER FROM FACT_LIST f, FACT_TYPES t
      SELECT NVL(max(a.FNUMBER), 0) + 1 INTO fl.FNUMBER FROM 
      (SELECT f.FNUMBER
         FROM FACT_LIST f, FACT_TYPES t
        WHERE f.ACT_ID = SYSPC.FACT_ID AND trunc(SYSDATE, 'YEAR') = trunc(f.FDATE, 'YEAR')
          AND f.BASE_ID IN (select ID from BASE_LIST where ORG_ID = oid_ AND VISIBLE_ = 'Y')
          AND f.PREFIX || f.FNUMBER = fl.PREFIX || f.FNUMBER
          AND f.TYPE_ID = t.ID 
          AND CASE WHEN type_id_ IN (SELECT ID FROM FACT_TYPES WHERE USED_IN_NUM = 'N') AND f.TYPE_ID = type_id_ AND t.USED_IN_NUM = 'N' THEN 1
                   WHEN type_id_ IN (SELECT ID FROM FACT_TYPES WHERE USED_IN_NUM = 'Y') AND t.USED_IN_NUM = 'Y' THEN 1 END = 1
        UNION ALL
       SELECT FNUMBER FROM FACT_JOURNAL
        WHERE ORG_ID = oid_ AND FYEAR = to_number(to_char(SYSDATE, 'YY'))
          AND PREFIX || FNUMBER = fl.PREFIX || FNUMBER
       ) a;
       -- Так сделал, потому что выражение NVL(max(FNUMBER), 0) не срабатывает, если
       -- вместо нуля поставить START_NUM
       SELECT t.START_NUM INTO sn FROM FACT_TYPES t WHERE t.ID = type_id_;
       IF (fl.FNUMBER < sn) AND (fl.FNUMBER = 0) THEN
        fl.FNUMBER := fl.FNUMBER + sn;
       END IF;
    ELSE
      fl.FNUMBER := fnumber_;
    END IF;
    fl.FIDX := fl.FNUMBER;
    fl.NOTE2 := '';
    FOR n2 IN (SELECT a.note2 FROM PAYMENTS a, VAL_LIST b
      WHERE a.BASE_ID = baseid AND a.VAL_ID = b.ID AND NOT (a.NOTE2 IS NULL)
       --AND (b.IS_CASH = 'N' or a.IS_MARKED = 'Y')
      ORDER BY a.DATE_IN)
    LOOP
      IF LENGTH(fl.NOTE2) > 0 THEN
        fl.NOTE2 :=  rtrim(ltrim(rpad(fl.NOTE2 || ', ', 250)));
      END IF;
      fl.NOTE2 := rtrim(ltrim(rpad(fl.NOTE2 || n2.note2, 250)));
    END LOOP;
  END;
--
  FUNCTION def_fact_nexnum(type_id IN NUMBER, orgid IN INTEGER DEFAULT NULL) RETURN INTEGER
  IS
    fl FactListRowType;
  BEGIN
    def_fact_attribs(0, type_id, null, null, fl, orgid);
    RETURN fl.FNUMBER;
  END;
--
  FUNCTION reserv_fact_nextnum(baseid IN INTEGER, orgid IN INTEGER DEFAULT NULL) RETURN INTEGER
  IS
    fl FactListRowType;
    fnid INTEGER;
    oid INTEGER;
    bimport VARCHAR2(10);
  BEGIN
    oid := NVL(orgid, CU.org_id);
    SELECT s_factl_id.NEXTVAL INTO fl.ID FROM dual;
    SELECT FNUM INTO fl.FNUMBER FROM (
      SELECT o.FNUM + 1 as FNUM
        FROM FACT_NUMS o,
             FACT_NUMS o1
       WHERE o1.FNUM(+) = o.FNUM + 1
         AND o1.FNUM IS NULL
         AND o.ORG_ID = oid
         AND o1.ORG_ID(+) = o.ORG_ID
         AND o.YR = to_char(SYSDATE, 'yyyy')
         AND o1.YR(+) = o.YR
       ORDER BY o.id
    ) WHERE ROWNUM = 1;
    bimport := BASEF.get_import_id(baseid);
    fl.FDATE := SYSDATE;
    INSERT INTO FACT_NUMS (FACT_ID, FNUM, YR, ORG_ID, BIMPORT_ID) 
    VALUES (fl.ID, fl.FNUMBER, to_char(fl.FDATE, 'yyyy'), oid, bimport)
    RETURNING ID INTO fnid;
    COMMIT;
    RETURN fnid;
  END;
--
  FUNCTION get_reserved_fnum(reservid IN INTEGER) RETURN INTEGER
  IS
  CURSOR cur(rid INTEGER) IS
    SELECT FNUM
    FROM FACT_NUMS
    WHERE ID = rid;
  --
  rcur cur%ROWTYPE;
  BEGIN
    OPEN cur(reservid);
    FETCH cur INTO rcur;
    CLOSE cur;
    RETURN rcur.FNUM;
  END;
--
  FUNCTION get_reserveid_by(fnum_ IN INTEGER, yr_ IN INTEGER, orgid IN INTEGER) RETURN INTEGER
  IS
  CURSOR cur IS
    SELECT ID
    FROM FACT_NUMS
    WHERE fnum = fnum_ AND yr = yr_ AND org_id = orgid;
  --
  rcur cur%ROWTYPE;
  BEGIN
    OPEN cur;
    FETCH cur INTO rcur;
    CLOSE cur;
    RETURN rcur.ID;
  END;
--
  PROCEDURE remove_reserv_nextnum(reservid IN INTEGER)
  IS
  BEGIN
    IF (reservid > 0) THEN
      DELETE FROM FACT_NUMS WHERE ID = reservid;
      COMMIT;
    END IF;
  END;
--
  FUNCTION check_reserv_nextnum(cur_baseid IN INTEGER, fnum IN INTEGER, mess OUT VARCHAR2) RETURN INTEGER
  IS
  CURSOR cur(num_ INTEGER, bid INTEGER) IS
    SELECT * 
    FROM FACT_NUMS
    WHERE fnum = num_
    AND ORG_ID = CU.org_id
    AND YR = to_char(SYSDATE, 'yyyy');
  --
  cust1 INTEGER;
  cust2 INTEGER;
  curr_importid VARCHAR2(10);
  rcur cur%ROWTYPE;
  ret INTEGER := 0;
  baseid INTEGER;
  ffid INTEGER;
  BEGIN
    OPEN cur(fnum, cur_baseid);
    FETCH cur INTO rcur;
    IF (cur%FOUND) THEN
      SELECT IMPORT_ID INTO curr_importid 
      FROM BASE_LIST WHERE ID = cur_baseid;
      SELECT count(*) INTO ffid 
      FROM FACT_NUMS WHERE EXISTS (SELECT 1 FROM FACT_LIST WHERE ID = rcur.FACT_ID);
      IF (ffid > 0) THEN
        ret := 1; -- Номер уже занят
        mess := 'Номер уже занят';
        cust1 := BASEF.get_cust_id(cur_baseid);
        SELECT ID INTO baseid
        FROM BASE_LIST WHERE IMPORT_ID = rcur.BIMPORT_ID;
        cust2 := BASEF.get_cust_id(baseid);
        IF (cust1 = cust2) THEN
          --ret := 2; -- Можно заменить счет-фактуру из основания rcur.BIMPORT_ID на 
          --mess := 'Произойдет замена счет-фактуры №' || rcur.FNUM || ' (' || rcur.FACT_ID || ') ' || chr(13)||chr(10)|| 'в основании '|| '№' || rcur.BIMPORT_ID;
          ret := 1; -- Номер уже занят
          mess := 'Номер уже занят';
        END IF;
      ELSE
        IF (curr_importid = rcur.BIMPORT_ID) THEN
          ret := 0; -- Номер свободен
          mess := 'Номер свободен';
        ELSE
          ret := 1; -- Номер свободен
          mess := 'Номер уже занят';
        END IF;
      END IF;
    ELSE
      ret := 0; -- Номер свободен
      mess := 'Номер свободен';
      null;
    END IF;
    CLOSE cur;
    RETURN ret;
  END;
--
  FUNCTION update_reserv_nextnum(current_reserv IN INTEGER, fnum_ IN INTEGER) RETURN INTEGER
  IS
  CURSOR cur(num_ INTEGER) IS
    SELECT * 
    FROM FACT_NUMS
    WHERE fnum = num_
    AND ORG_ID = CU.org_id
    AND YR = to_char(SYSDATE, 'yyyy');
  --
  reserv_id INTEGER;
  BEGIN
    UPDATE FACT_NUMS SET FNUM = fnum_ WHERE ID = current_reserv;
    COMMIT;
  END;
--
  FUNCTION get_fact_nextnum(reserv_id IN INTEGER) RETURN INTEGER
  IS
  CURSOR cur(ID_ INTEGER) IS
    SELECT FNUM
    FROM FACT_NUMS
    WHERE ID = ID_;
  --
  rcur cur%ROWTYPE;
  ret INTEGER;
  BEGIN
    OPEN cur(reserv_id);
    FETCH cur INTO rcur;
    IF (cur%FOUND) THEN
      ret := rcur.FNUM;
    END IF;
    CLOSE cur;
    RETURN ret;
  END;
--
PROCEDURE div_goods_fact_debug(baseid_ IN INTEGER, fid IN INTEGER, wbdate IN DATE DEFAULT SYSDATE) IS
    --
    CURSOR cur2(bid IN INTEGER, gid IN INTEGER, wdate IN DATE) IS
      SELECT wl.ID as wlid, wg.goods_id, wg.id as wgid
      FROM WAYBILL_LIST wl, WAYBILL_GOODS wg 
      WHERE wl.base_id = bid
      and wg.goods_id = gid
      and wg.WAYB_ID = wl.ID
      and wl.DATE_ = wdate;
    --
    CURSOR cur3(fact_id IN INTEGER, goods_id IN INTEGER) IS
      SELECT FACT_ID, GOODS_ID, NUM_, PRICE, SUM_NDS, SUM_NP, ORD, DISC_PCT, CNTR_ID, CODE_GTD, DEFECT_POS
      FROM FACT_GOODS WHERE FACT_ID = fact_id AND GOODS_ID = goods_id;
    --
    r  TResDivGoods;
    rcur2  cur2%ROWTYPE;
    f      cur3%ROWTYPE;
    i BINARY_INTEGER := 0;
    j BINARY_INTEGER := 0;
    ii CHAR;
    ord_ INTEGER := 1;
    wgid INTEGER;
    price NUMBER;
    sum_nds_ NUMBER;
    sum_np_ NUMBER;
  BEGIN
    FOR g IN (SELECT fl.ID FLID, fg.fact_id, fg.goods_id, fg.num_, fg.price, fg.sum_nds, fg.sum_np,
      fg.ORD, fg.DISC_PCT, fg.CNTR_ID, fg.DEFECT_POS
      FROM FACT_LIST fl, FACT_GOODS fg
      WHERE fl.id = fg.fact_id
      AND fl.base_id = baseid_ AND fl.ID = fid) LOOP
        /*
        OPEN cur2(baseid_, g.GOODS_ID);
        LOOP
          FETCH cur2 INTO rcur2;
          EXIT WHEN cur2%NOTFOUND;
        END LOOP;
        CLOSE cur2;
        */
        --
        OPEN cur2(baseid_, g.GOODS_ID, wbdate);
        FETCH cur2 INTO rcur2;
        CLOSE cur2;
        IF (GOODSP.IS_STRICT_GOODS(g.GOODS_ID) = 'Y') THEN
          r.DELETE;
          r := div_prod_sales(g.GOODS_ID, rcur2.WLID, rcur2.WGID, baseid_);
          IF r.COUNT > 0 THEN
            OPEN cur3(g.fact_id, g.goods_id);
            FETCH cur3 INTO f;
            CLOSE cur3;
            --ii := SECUR.is_internal_import;
            --SECUR.set_internal_import('Y');
            --DELETE FROM FACT_GOODS WHERE fact_id = g.fact_id AND goods_id = g.goods_id;
            --COMMIT;
            --SECUR.set_internal_import(ii);
            FOR j IN 0..r.LAST LOOP
              sum_nds_ := r(j).num * g.sum_nds/g.num_;
              --sum_nds_ := ((r(j).num * g.PRICE) * (1 + SYSPC.NDS/100)) - (r(j).num * g.PRICE);
              sum_np_ := r(j).num * g.sum_np/g.num_;
              --INSERT INTO FACT_GOODS (FACT_ID, GOODS_ID, NUM_, PRICE, SUM_NDS, SUM_NP, ORD, DISC_PCT, CNTR_ID, CODE_GTD, DEFECT_POS)
              --  VALUES (g.FACT_ID, g.GOODS_ID, r(j).NUM, g.PRICE, sum_nds_, sum_np_, ord_, g.DISC_PCT, g.CNTR_ID, r(j).GTD, g.DEFECT_POS);
              DBMS_OUTPUT.PUT_LINE('=== Div goods fact. SET: PRICE = ' || g.PRICE || ', NUM: ' || r(j).NUM || ', GTD: ' || r(j).GTD);
              ord_ := ord_ + 1;
            END LOOP;
          END IF;
        END IF;
    END LOOP;
  END;
--
  PROCEDURE def_fact_gen_payments(factid IN INTEGER, sumr IN NUMBER) IS
    sumost_r NUMBER;
    max_nalr NUMBER;
    paymdate DATE;
  BEGIN
    sumost_r := ROUND(sumr, 2);
    max_nalr := SYSPC.FACTPAY_MAX;
    paymdate := TRUNC(SYSDATE);
    WHILE sumost_r > max_nalr LOOP
      INSERT INTO FACT_PAYMENTS (DATE_, FACT_ID, SUMM)
        VALUES (paymdate, factid, max_nalr);
      SELECT paymdate - decode(to_number(to_char(paymdate - 1, 'D')), 1, 3, 7, 2, 1)
        INTO paymdate FROM dual;  -- предыдущий раб.день
      sumost_r := sumost_r - max_nalr;
    END LOOP;
    IF sumost_r > 0 THEN
      INSERT INTO FACT_PAYMENTS (DATE_, FACT_ID, SUMM)
        VALUES (paymdate, factid, sumost_r);
    END IF;
  END;
--
  PROCEDURE make_fact_2000(baseid IN INTEGER, is_nal IN CHAR DEFAULT 'Y')
  IS
    CURSOR goods_cur IS
      SELECT a.GOODS_ID, SUM(-b.MOVE * a.NUM_) NUM_,
        SUM(-b.MOVE * a.NUM_ * a.PRICE * a.RATE) SUMMD
      FROM WAYBILL_GOODS a, WAYBILL_LIST b
      WHERE a.wayb_id = b.id AND b.base_id = baseid -- AND b.move = -1
      GROUP BY a.GOODS_ID
      HAVING SUM(-b.MOVE*a.NUM_) > 0;
    CURSOR goods_info_cur(goodsid IN INTEGER) IS
      SELECT a.ORD, (100 * (a.PRICE0 - a.PRICE)) / a.PRICE0 DISC_PCT,
       --decode(GOODSP.get_in_group(a.GOODS_ID, 12), 'Y', NULL, 171) CNTR_ID
       to_number(NULL) CNTR_ID
      FROM ACT_LIST c, BASE_GOODS a, BASE_LIST b
      WHERE a.base_id = b.id AND a.act_id = c.id
       AND b.id = baseid AND a.goods_id = goodsid AND c.move = -1;
    CURSOR factcorr_cur(factid IN INTEGER, goodsid IN INTEGER) IS
      SELECT FACT_ID, GOODS_ID, NUM_, PRICE, SUM_NDS
      FROM FACT_GOODS WHERE FACT_ID = factid AND GOODS_ID = goodsid
      FOR UPDATE;
    c goods_cur%ROWTYPE;
    p goods_info_cur%ROWTYPE;
    t factcorr_cur%ROWTYPE;
    g FactGoodsRowType;
    flag CHAR(1);
    factid   INTEGER;
    factno   INTEGER;
    sumt_d   NUMBER;
    sump_d   NUMBER;
    sump_r   NUMBER;
    sumpn_r  NUMBER;
    sumnp_r  NUMBER;
    sumost_r NUMBER;
    max_nalr NUMBER;
    min_num  NUMBER;
    min_goods INTEGER;
    nds      NUMBER;
    diffp    NUMBER;
    factdate DATE;
    errm     VARCHAR2(4001);
  BEGIN
    errm := NVL(BASEF.get_is_allowed(baseid, 'DOCS'), 'N');
    IF errm != 'Y' THEN
      raise_application_error(-20500, 'Состояние основания №' || to_char(baseid) ||
        ' не допускает проведения счетов-фактур. (' || substr(errm, 3) || ')');
    END IF;
    -- Проверка клиента
    SELECT is_firm INTO flag FROM OBJ_LIST WHERE id = BASEF.get_cust_id(baseid);
    IF flag != 'Y' THEN
      raise_application_error(-20500, OBJF.get_name(BASEF.get_cust_id(baseid)) ||
       ' не является юридическим лицом. Провести счет-фактуру невозможно.');
    END IF;
    -- Выделить сумму отпущенного товара
    SELECT NVL(SUM(-b.MOVE * a.NUM_ * a.PRICE * a.RATE), 0) SUMMD
      INTO sumt_d
      FROM WAYBILL_GOODS a, WAYBILL_LIST b
      WHERE a.wayb_id = b.id AND b.base_id = baseid;
    -- Выделить сумму принятых денег
    SELECT NVL(SUM(decode(a.val_id, SYSPC.ROUBLE, 1, SYSPC.ROUBLEBN, 1,
        RELF.get_rate(RELF.get_base_rel(baseid, a.act_id), a.val_id, SYSPC.ROUBLE, a.DATE_IN))
         * MONEY * MOVE), 0) SUMMR,
       NVL(SUM(decode(b.is_cash, 'Y', 1, 0) * decode(a.val_id, SYSPC.ROUBLE, 1,
        RELF.get_rate(RELF.get_base_rel(baseid, a.act_id), a.val_id, SYSPC.ROUBLE, a.DATE_IN))
         * MONEY * MOVE), 0) SUMMNPR,
       NVL(SUM(MONEY * MOVE * RATE), 0) SUMMD
      INTO sump_r, sumpn_r, sump_d
      FROM PAYMENTS a, VAL_LIST b WHERE a.base_id = baseid AND a.val_id = b.id;
    -- Проверка соответствия платежей отпущенному товару
    IF ABS(sumt_d) > 0 THEN
      diffp := ROUND(ABS(100 * (sumt_d - sump_d) / sumt_d), 2);
    ELSE
      diffp := 100;
    END IF;
    IF sump_d < sumt_d AND NVL(diffp, 100) > NVL(SYSPC.FACTDIFF_MAX, 0) THEN
      raise_application_error(-20500, 'Разница сумм между накладными и оплатами превышает '
       || to_char(SYSPC.FACTDIFF_MAX) || '% [' || to_char(diffp) ||
       '%]. Провести счет-фактуру невозможно.');
    END IF;
    -- Атрибуты фактуры: номер, налог с продаж
    nds := get_cust_nds(BASEF.get_cust_id(baseid)); --SYSPC.NDS;
    factdate := SYSDATE;
    SELECT s_factl_id.NEXTVAL INTO factid FROM dual;
    SELECT nvl(max(FNUMBER), 0) + 1 INTO factno FROM
      (SELECT FNUMBER FROM FACT_LIST
       WHERE ACT_ID = SYSPC.FACT_ID AND trunc(SYSDATE, 'YEAR') = trunc(FDATE, 'YEAR')
        AND BASE_ID IN (select ID from BASE_LIST where ORG_ID = CU.org_id)
       UNION ALL
       SELECT FNUMBER FROM FACT_JOURNAL
       WHERE ORG_ID = CU.org_id AND FYEAR = to_number(to_char(SYSDATE, 'YY'))
       );
    -- Для ч.п. налог с продаж берется со всей суммы, ост - только с нал.оплат
    SELECT ROUND((decode(rtrim(b.REGNO), NULL, sumpn_r, sump_r) *
        SYSPC.OLDFACT_NP / (100 + SYSPC.OLDFACT_NP)), 2) *
        decode(is_nal, 'N', decode(rtrim(b.REGNO), NULL, 0, 1), 1)
      INTO sumnp_r
      FROM BASE_LIST a, OBJ_LIST b
      WHERE a.id = baseid AND b.id = a.cust_id;
    sump_r := sump_r - sumnp_r;
    sumost_r := sump_r;
    -- Завести фактуру
    -- max_nal  := SYSPC.FACT_MAX; -- разбивка на неск.фактур - устарело
    INSERT INTO FACT_LIST (ID, ACT_ID, MOVE, BASE_ID, FDATE, FNUMBER, PAYM_NP, EXP_DATE, VAL_ID, NDS_PCT)
       VALUES (factid, SYSPC.FACT_ID, -1, baseid, factdate, factno, sumnp_r, SYSDATE + SYSPC.FACTNEW_LIFE, SYSPC.ROUBLEBN, nds);
    g.FACT_ID := factid;
    FOR c IN goods_cur LOOP
      IF (c.NUM_ < NVL(min_num,0)) OR (min_goods IS NULL) THEN
        min_num := c.NUM_;
        min_goods := c.GOODS_ID;
      END IF;
      OPEN goods_info_cur(c.goods_id);
      FETCH goods_info_cur INTO p;
      g.DISC_PCT := NULL;
      IF goods_info_cur%FOUND THEN
        g.ORD := p.ORD;
        g.CNTR_ID := p.CNTR_ID;
        IF p.DISC_PCT > 0 AND p.DISC_PCT <= 100 THEN
          g.DISC_PCT := p.DISC_PCT;
        END IF;
      ELSE
        g.ORD := 0;
        g.CNTR_ID := NULL;
      END IF;
      CLOSE goods_info_cur;
      g.GOODS_ID := c.goods_id;
      g.NUM_ := c.num_;
      g.PRICE := ROUND((100 * sump_r * c.summd) / (sumt_d * c.num_ * (100 + nds)), 2);
      g.SUM_NDS := ROUND(g.NUM_ * g.PRICE * nds / 100, 2);
      sumost_r := sumost_r - (g.NUM_ * g.PRICE + g.SUM_NDS);
      INSERT INTO FACT_GOODS (FACT_ID, GOODS_ID, NUM_, PRICE, SUM_NDS, SUM_NP, ORD, DISC_PCT, CNTR_ID)
        VALUES (g.FACT_ID, g.GOODS_ID, g.NUM_, g.PRICE, g.SUM_NDS, 0, g.ORD, g.DISC_PCT, g.CNTR_ID);
    END LOOP;
    -- debug -- update fact_list set note=to_char(sumost_r) where id = factid;
    IF min_goods > 0 AND ABS(sumost_r) > 0.00009 THEN
      OPEN factcorr_cur(factid, min_goods);
      FETCH factcorr_cur INTO t;
      IF factcorr_cur%FOUND THEN
        t.PRICE := t.PRICE + ((100 * sumost_r) / (t.NUM_ * (100 + nds)));
        t.SUM_NDS := t.SUM_NDS + (nds * sumost_r / (100 + nds));
        UPDATE FACT_GOODS
          SET PRICE = t.PRICE, SUM_NDS = t.SUM_NDS -- debug -- , CODE_GTD='*'
          WHERE CURRENT OF factcorr_cur;
      END IF;
      CLOSE factcorr_cur;
    END IF;
    IF is_nal = 'Y' THEN
      sumost_r := sumpn_r;
      max_nalr := SYSPC.FACTPAY_MAX;
      factdate := TRUNC(factdate);
      WHILE sumost_r > max_nalr LOOP
        INSERT INTO FACT_PAYMENTS (DATE_, FACT_ID, SUMM)
          VALUES (factdate, factid, max_nalr);
        SELECT factdate - decode(to_number(to_char(factdate - 1, 'D')), 1, 3, 7, 2, 1)
          INTO factdate FROM dual;  -- предыдущий раб.день
        sumost_r := sumost_r - max_nalr;
      END LOOP;
      IF sumost_r > 0 THEN
        INSERT INTO FACT_PAYMENTS (DATE_, FACT_ID, SUMM)
          VALUES (factdate, factid, sumost_r);
      END IF;
    END IF;
    COMMIT;
  END;
--
  PROCEDURE make_fact_2001(baseid IN INTEGER, is_nal IN CHAR DEFAULT 'Y',
    type_id IN NUMBER DEFAULT 1, prc_prec IN NUMBER DEFAULT 2,
    opts IN VARCHAR2 DEFAULT NULL)
  IS
    CURSOR goods_cur IS
      SELECT a.GOODS_ID, SUM(-b.MOVE * a.NUM_) NUM_,
        SUM(-b.MOVE * a.NUM_ * a.PRICE * a.RATE) SUMMD,
        CASE max(c.DEFECT_POS) WHEN 'Y' THEN 'Y' END as DEFECT_POS
      FROM WAYBILL_GOODS a, WAYBILL_LIST b, ACT_LIST c
      WHERE a.wayb_id = b.id AND b.base_id = baseid AND b.act_id = c.id -- AND b.move = -1
      GROUP BY a.GOODS_ID
      HAVING SUM(-b.MOVE*a.NUM_) > 0
      ORDER BY 2 DESC;
    CURSOR goods_info_cur(goodsid IN INTEGER) IS
      SELECT a.ORD, (100 * (a.PRICE0 - a.PRICE)) / a.PRICE0 DISC_PCT,
       --decode(GOODSP.get_in_group(a.GOODS_ID, 12), 'Y', NULL, 171) CNTR_ID
       to_number(NULL) CNTR_ID
      FROM ACT_LIST c, BASE_GOODS a, BASE_LIST b
      WHERE a.base_id = b.id AND a.act_id = c.id
       AND b.id = baseid AND a.goods_id = goodsid AND c.move = -1;
    c  goods_cur%ROWTYPE;
    p  goods_info_cur%ROWTYPE;
    g  FactGoodsRowType;
    flag       CHAR(1);
    prc_round  INTEGER;
    factid     INTEGER;
    val_id     INTEGER;
    factno     INTEGER;
    sumt_d     NUMBER;
    sump_d     NUMBER;
    sump_r     NUMBER;
    sumpn_r    NUMBER;
    sumnp_r    NUMBER;
    sumost_r   NUMBER;
    sumostnp_r NUMBER;
    rate_r     NUMBER;
    max_nalr   NUMBER;
    nds        NUMBER;
    diffp      NUMBER;
    prc        NUMBER;
    delt       NUMBER;
    factdate   DATE;
    wblastdate DATE;
    errm       VARCHAR2(4001);
    is_exprt   BOOLEAN;
    tnved_id   INTEGER;
  BEGIN
    is_exprt := NVL(INSTR(opts, 'E'), 0) > 0;
    errm := NVL(BASEF.get_is_allowed(baseid, 'DOCS'), 'N');
    IF errm != 'Y' THEN
      raise_application_error(-20500, 'Состояние основания №' || to_char(baseid) ||
        ' не допускает проведения счетов-фактур. (' || substr(errm, 3) || ')');
    END IF;
    -- Проверка клиента
    SELECT is_firm INTO flag FROM OBJ_LIST WHERE id = BASEF.get_cust_id(baseid);
    IF flag != 'Y' THEN
      raise_application_error(-20500, OBJF.get_name(BASEF.get_cust_id(baseid)) ||
       ' не является юридическим лицом. Провести счет-фактуру невозможно.');
    END IF;
    -- Выделить сумму отпущенного товара
    SELECT NVL(SUM(-b.MOVE * a.NUM_ * a.PRICE * a.RATE), 0), MAX(b.DATE_)
      INTO sumt_d, wblastdate
      FROM WAYBILL_GOODS a, WAYBILL_LIST b
      WHERE a.wayb_id = b.id AND b.base_id = baseid;
    -- Выделить сумму принятых денег
    IF type_id >= 10 THEN
      SELECT ROUND(sumt_d * (1 + 0.01*NVL(a.conv_pct,0)) *
        (1 + 0.01*NVL(a.nsp_pct,0)*decode(is_nal, 'N', decode(rtrim(b.REGNO), NULL, 0, 1), 1)), 2)
       INTO sumt_d FROM BASE_LIST a, OBJ_LIST b
       WHERE a.id = baseid AND b.id = a.cust_id;
      rate_r := RELF.get_rate(SYSPC.DEF_REL, SYSPC.DEF_VAL, SYSPC.ROUBLEBN, wblastdate);
      sump_r := sumt_d;
      sump_d := sumt_d;
      prc_round := prc_prec;
      IF is_nal = 'Y' THEN sumpn_r := sump_r; ELSE sumpn_r := 0; END IF;
      IF sumt_d = 0 THEN
        raise_application_error(-20500, 'Сумма по накладным=0. Провести счет-фактуру невозможно.');
      END IF;
    ELSE
      rate_r := 1;
      SELECT NVL(SUM(decode(a.val_id, SYSPC.ROUBLE, 1, SYSPC.ROUBLEBN, 1,
          RELF.get_rate(RELF.get_base_rel(baseid, SYSPC.fact_id), a.val_id, SYSPC.ROUBLE, a.DATE_IN))
           * MONEY * MOVE), 0) SUMMR,
         NVL(SUM(decode(b.is_cash, 'Y', 1, 0) * decode(a.val_id, SYSPC.ROUBLE, 1,
          RELF.get_rate(RELF.get_base_rel(baseid, SYSPC.fact_id), a.val_id, SYSPC.ROUBLE, a.DATE_IN))
           * MONEY * MOVE), 0) SUMMNPR,
         NVL(SUM(MONEY * MOVE * RATE), 0) SUMMD
        INTO sump_r, sumpn_r, sump_d
        FROM PAYMENTS a, VAL_LIST b WHERE a.base_id = baseid AND a.val_id = b.id;
      -- Проверка соответствия платежей отпущенному товару
      IF ABS(sumt_d) > 0 THEN
        diffp := ROUND(ABS(100 * (sumt_d - sump_d) / sumt_d), 2);
      ELSE
        diffp := 100;
      END IF;
      IF sump_d < sumt_d AND NVL(diffp, 100) > NVL(SYSPC.FACTDIFF_MAX, 0) THEN
        -- директору все можно!
        IF CU.own_role_enabled('DIRECTOR') != 'Y' THEN
          raise_application_error(-20500, 'Разница сумм между накладными и оплатами превышает '
           || to_char(SYSPC.FACTDIFF_MAX) || '% [' || to_char(diffp) ||
           '%]. Провести счет-фактуру невозможно.');
        END IF;
      END IF;
      prc_round := 2;
    END IF;
    -- Атрибуты фактуры: номер, налог с продаж
    nds := get_cust_nds(BASEF.get_cust_id(baseid)); --SYSPC.NDS;
    factdate := SYSDATE;
    SELECT s_factl_id.NEXTVAL INTO factid FROM dual;
    SELECT nvl(max(FNUMBER), 0) + 1 INTO factno FROM
      (SELECT FNUMBER FROM FACT_LIST
       WHERE ACT_ID = SYSPC.FACT_ID AND trunc(SYSDATE, 'YEAR') = trunc(FDATE, 'YEAR')
        AND BASE_ID IN (select ID from BASE_LIST where ORG_ID = CU.org_id)
       UNION ALL
       SELECT FNUMBER FROM FACT_JOURNAL
       WHERE ORG_ID = CU.org_id AND FYEAR = to_number(to_char(SYSDATE, 'YY'))
       );
    -- Для ч.п. налог с продаж берется со всей суммы, ост - только с нал.оплат
    SELECT ROUND(TOOLS.npt(decode(rtrim(b.REGNO), NULL, sumpn_r, sump_r)), 2) *
        decode(is_nal, 'N', decode(rtrim(b.REGNO), NULL, 0, 1), 1)
      INTO sumnp_r
      FROM BASE_LIST a, OBJ_LIST b
      WHERE a.id = baseid AND b.id = a.cust_id;
    sump_r := sump_r - sumnp_r;
    sumost_r := sump_r;
    sumostnp_r := sumnp_r;
    -- Завести фактуру
    -- max_nal  := SYSPC.FACT_MAX; -- разбивка на неск.фактур - устарело
    val_id := get_val_by_type(type_id);
    INSERT INTO FACT_NUMS(FACT_ID, BIMPORT_ID, FNUM, YR, ORG_ID, PREFIX) 
    VALUES (factid, BASEF.get_import_id(baseid), factno, to_char(factdate, 'yyyy'), CU.org_id, ORGF.fact_prefix(CU.org_id));
    --
    INSERT INTO FACT_LIST (ID, ACT_ID, MOVE, BASE_ID, TYPE_ID, FDATE, 
      FNUMBER, PAYM_NP, RATE, EXP_DATE, VAL_ID, NDS_PCT)
    VALUES (factid, SYSPC.FACT_ID, -1, baseid, type_id, factdate, factno, NULL/*sumnp_r*/,
      rate_r, SYSDATE + SYSPC.FACTNEW_LIFE, val_id, nds);
    g.FACT_ID := factid;
    delt := 0;
    FOR c IN goods_cur LOOP
      OPEN goods_info_cur(c.goods_id);
      FETCH goods_info_cur INTO p;
      g.DISC_PCT := NULL;
      IF goods_info_cur%FOUND THEN
        g.ORD := p.ORD;
        g.CNTR_ID := p.CNTR_ID;
        IF p.DISC_PCT > 0 AND p.DISC_PCT <= 100 THEN
          g.DISC_PCT := p.DISC_PCT;
        END IF;
      ELSE
        g.ORD := 0;
        g.CNTR_ID := NULL;
      END IF;
      CLOSE goods_info_cur;
      g.GOODS_ID := c.goods_id;
      g.NUM_ := c.num_;
      prc := (delt + (100 * sump_r * c.summd) / (sumt_d * (100 + nds))) / c.num_;
      IF prc <= 0 THEN
        prc := (100 * sump_r * c.summd) / (sumt_d * (100 + nds) * c.num_);
      END IF;
      g.PRICE  := ROUND(prc, prc_round);
      delt := c.num_ * (prc - g.PRICE);
      g.SUM_NDS := ROUND(g.NUM_ * g.PRICE * nds / 100, 2);
      g.SUM_NP  := ROUND((sumnp_r * c.summd) / sumt_d, 2);
      sumost_r := sumost_r - (g.NUM_ * g.PRICE + g.SUM_NDS);
      sumostnp_r := sumostnp_r - g.SUM_NP;
      tnved_id := null;
      IF (is_exprt) THEN
        tnved_id := GOODSP.FIRST_TNVED(g.GOODS_ID);
      END IF;
      INSERT INTO FACT_GOODS (FACT_ID, GOODS_ID, NUM_, PRICE, SUM_NDS, SUM_NP, 
        ORD, DISC_PCT, CNTR_ID, DEFECT_POS, TNVED_ID, WEIGHT)
      VALUES (g.FACT_ID, g.GOODS_ID, g.NUM_, g.PRICE, g.SUM_NDS, g.SUM_NP, 
        g.ORD, g.DISC_PCT, g.CNTR_ID, c.DEFECT_POS, tnved_id, g.WEIGHT);
    END LOOP;
    -- debug -- update fact_list set note=to_char(sumost_r) where id = factid;
    IF NVL(INSTR(opts, 'A'), 0) = 0 AND (ABS(sumost_r) > 0.00009 OR ABS(sumostnp_r) > 0.00009) THEN
      adjust_fact(factid, sumost_r, sumostnp_r, prc_round);
    END IF;
    reord_fact(factid);
    IF (is_nal = 'Y') AND (type_id < 10) THEN
      sumost_r := sumpn_r;
      max_nalr := SYSPC.FACTPAY_MAX;
      factdate := TRUNC(factdate);
      WHILE sumost_r > max_nalr LOOP
        INSERT INTO FACT_PAYMENTS (DATE_, FACT_ID, SUMM)
          VALUES (factdate, factid, max_nalr);
        SELECT factdate - decode(to_number(to_char(factdate - 1, 'D')), 1, 3, 7, 2, 1)
          INTO factdate FROM dual;  -- предыдущий раб.день
        sumost_r := sumost_r - max_nalr;
      END LOOP;
      IF sumost_r > 0 THEN
        INSERT INTO FACT_PAYMENTS (DATE_, FACT_ID, SUMM)
          VALUES (factdate, factid, sumost_r);
      END IF;
    END IF;
    COMMIT;
  EXCEPTION
    WHEN DUP_VAL_ON_INDEX
      THEN raise_application_error(-20500, 'Такой номер счет-фактуры уже существует.');
      ROLLBACK;
  END;
--
  PROCEDURE make_fact_2002(baseid IN INTEGER, is_nal IN CHAR DEFAULT 'Y',
    type_id IN NUMBER DEFAULT 1, prc_prec IN NUMBER DEFAULT 2,
    opts IN VARCHAR2 DEFAULT NULL, filt_goods_valid IN INTEGER DEFAULT NULL,
    fnumber IN INTEGER DEFAULT NULL, fdate IN DATE DEFAULT NULL, contract IN INTEGER DEFAULT NULL)
  IS
    CURSOR goods_info_cur(goodsid IN INTEGER) IS
      SELECT a.ORD, (100 * (a.PRICE0 - a.PRICE)) / a.PRICE0 DISC_PCT,
       decode(GOODSP.get_in_group(a.GOODS_ID, 12), 'Y', NULL, 171) CNTR_ID
       --to_number(NULL) CNTR_ID
      FROM ACT_LIST c, BASE_GOODS a, BASE_LIST b
      WHERE a.base_id = b.id AND a.act_id = c.id
       AND b.id = baseid AND a.goods_id = goodsid AND c.move = -1;
    --
    TYPE FactGoodsRecType IS RECORD (
      GOODS_ID NUMBER(6),
      NUM_     NUMBER,
      NUM2_    NUMBER,
      SUMMD    NUMBER,
      ORD      NUMBER(6),
      DEFECT_POS VARCHAR2(1),
      ID  NUMBER
    );
    --
    TYPE FactGoodsCurType IS REF CURSOR RETURN FactGoodsRecType;
    --
    goodscur FactGoodsCurType;
    c  FactGoodsRecType;
    p  goods_info_cur%ROWTYPE;
    g  FactGoodsRowType;
    fl FactListRowType;
    r TResDivGoods;
    --
    j BINARY_INTEGER := 0;
    notes2     FACT_LIST.NOTE2%TYPE;
    flag       CHAR(1);
    is_chp     CHAR(1);
    copy_fact  CHAR(1) := 'N';
    prc_round  INTEGER;
    factid     INTEGER;
    factid_to  INTEGER;
    factno     INTEGER;
    fact_count_to INTEGER := 0;
    org_to     INTEGER;
    actid      INTEGER;
    val_id     INTEGER;
    base_to    INTEGER := 0; -- в какое основание копировать накладную
    sumt_d     NUMBER;
    sumt_r     NUMBER;
    sumt_x     NUMBER;
    sump_d     NUMBER;
    sump_r     NUMBER;
    sumpn_r    NUMBER;
    sumnp_r    NUMBER;
    sumost_r   NUMBER;
    sumostnp_r NUMBER;
    sumorder_r NUMBER;
    ostp_d     NUMBER;
    ostp_r     NUMBER;
    ostp_rel   NUMBER;
    val_id_count BINARY_INTEGER := 0;
    rate_r     NUMBER;
    max_nalr   NUMBER;
    nds        NUMBER;
    diffp      NUMBER;
    prc        NUMBER;
    delt       NUMBER;
    wblastdate DATE;
    is_partial BOOLEAN;
    relid      NUMBER;
    factdate   DATE;
    min_val_nk BINARY_INTEGER;
    max_val_nk BINARY_INTEGER;
    errm       VARCHAR2(4001);
    is_exprt   BOOLEAN;
    tnved_id   INTEGER;
  BEGIN
    errm := NVL(BASEF.get_is_allowed(baseid, 'DOCS'), 'N');
    IF errm != 'Y' THEN
      raise_application_error(-20500, 'Состояние основания №' || to_char(baseid) ||
        ' не допускает проведения счетов-фактур. (' || substr(errm, 3) || ')');
    END IF;
    -- Для межорганизационного перемещения копируем фактуру в организацию "приемник"
    actid := BASEF.get_act_id;
    IF (actid = 352) THEN
      dbms_output.put_line('=== Вкл. копирование фактуры');
      copy_fact := 'Y';
    END IF;    
    is_partial := NVL(INSTR(opts, 'P'), 0) > 0;
    is_exprt := NVL(INSTR(opts, 'E'), 0) > 0;
    relid := BASEF.get_rel_id(baseid);
    dbms_output.put_line('=== relid: ' || relid);
    -- Проверка клиента
    SELECT is_firm, decode(rtrim(REGNO), NULL, 'N', 'Y')
      INTO flag, is_chp FROM OBJ_LIST WHERE id = BASEF.get_cust_id(baseid);
    IF flag != 'Y' THEN
      raise_application_error(-20500, OBJF.get_name(BASEF.get_cust_id(baseid)) ||
       ' не является юридическим лицом. Провести счет-фактуру невозможно.');
    END IF;
    -- Выделить сумму отпущенного товара (для частичн.сч-ф исходя из принятых денег - предоплата)
    IF is_partial THEN
      SELECT NVL(SUM(MOVE * MONEY * RATE), 0), MAX(DATE_IN)
        INTO sumt_d, wblastdate
        FROM PAYMENTS WHERE base_id = baseid;
      dbms_output.put_line('=== Частичная сф');
    ELSE
      /*
      SELECT NVL(SUM(-b.MOVE * round(a.NUM_ * a.PRICE, prc_prec) * a.RATE), 0),
             NVL(SUM(-b.MOVE * round(a.NUM_ * a.PRICE, prc_prec) *
              RELF.get_rate(RELF.get_base_rel(baseid, SYSPC.fact_id), a.val_id, SYSPC.ROUBLE, b.DATE_IN)), 0),
             NVL(SUM(-b.MOVE * round(a.NUM_ * a.PRICE, prc_prec)), 0),
             MAX(b.DATE_), MIN(a.VAL_ID), MAX(a.VAL_ID)
        INTO sumt_d, sumt_r, sumt_x, wblastdate, min_val_nk, max_val_nk
        FROM WAYBILL_GOODS a, WAYBILL_LIST b
        WHERE a.wayb_id = b.id AND b.base_id = baseid
          AND ((filt_goods_valid IS NULL) OR (a.val_id != filt_goods_valid));
      */
      SELECT NVL(SUM(-b.MOVE * a.NUM_ * a.PRICE * a.RATE), 0),
             NVL(SUM(-b.MOVE * a.NUM_ * a.PRICE *
              RELF.get_rate(RELF.get_base_rel(baseid, SYSPC.fact_id), a.val_id, SYSPC.ROUBLE, b.DATE_IN)), 0),
             NVL(SUM(-b.MOVE * a.NUM_ * a.PRICE), 0),
             MAX(b.DATE_), MIN(a.VAL_ID), MAX(a.VAL_ID)
        INTO sumt_d, sumt_r, sumt_x, wblastdate, min_val_nk, max_val_nk
        FROM WAYBILL_GOODS a, WAYBILL_LIST b
        WHERE a.wayb_id = b.id AND b.base_id = baseid
          AND ((filt_goods_valid IS NULL) OR (a.val_id != filt_goods_valid));
      dbms_output.put_line('=== Не частичная сф');
      dbms_output.put_line('=== sumt_d: '||sumt_d||', sumt_r: '||sumt_r||', sumt_x: '||sumt_x||', wblastdate: '||wblastdate||', min_val_nk: '||min_val_nk||', max_val_nk: ' || max_val_nk);
      -- Запрос Антона от 13.1.15 
      -- отгрузка товара в долларах с выписыванием сч.фактуры в рублях.
      -- Надо: Курс Магазина - Рубли б/н (!) на дату, указываемую при создании сч.фактуры.
      -- доп.от 21.1.15 если дата фактуры сегодняшняя, время для расчета курса берется текущее
      IF min_val_nk = max_val_nk AND min_val_nk = SYSPC.DEF_VAL THEN
        dbms_output.put_line('=== Отгрузка товаров в доллрах с выписывание сф в руб.:');
        SELECT NVL(SUM(-b.MOVE * round(a.NUM_ * a.PRICE, prc_prec) * a.RATE), 0),
               NVL( round(SUM(-b.MOVE * a.NUM_ * a.PRICE), prc_prec) * 
               MAX(RELF.get_rate(RELF.get_base_rel(baseid, SYSPC.fact_id), a.val_id, SYSPC.ROUBLEBN,
                  case when trunc(NVL(fdate, SYSDATE)) = trunc(SYSDATE)
                   then SYSDATE else fdate end)), 0),
               NVL(SUM(-b.MOVE * round(a.NUM_ * a.PRICE, prc_prec
               )), 0),
               MAX(b.DATE_), MIN(a.VAL_ID), MAX(a.VAL_ID)
          INTO sumt_d, sumt_r, sumt_x, wblastdate, min_val_nk, max_val_nk
          FROM WAYBILL_GOODS a, WAYBILL_LIST b
          WHERE a.wayb_id = b.id AND b.base_id = baseid
            AND ((filt_goods_valid IS NULL) OR (a.val_id != filt_goods_valid));
          dbms_output.put_line('=== sumt_d: '||sumt_d||', sumt_r: '||sumt_r||', sumt_x: '||sumt_x||', wblastdate: '||wblastdate||', min_val_nk: '||min_val_nk||', max_val_nk: ' || max_val_nk);
      END IF;
    END IF;
    -- Выделить сумму принятых денег
    IF type_id >= 10 THEN
      dbms_output.put_line('=== type_id >= 10');
      SELECT ROUND(sumt_d * (1 + 0.01*NVL(conv_pct,0)) * (1 + 0.01*NVL(nsp_pct,0)), 2)
        INTO sumt_d FROM BASE_LIST WHERE id = baseid;
      rate_r := RELF.get_rate(SYSPC.DEF_REL, SYSPC.DEF_VAL, SYSPC.ROUBLEBN, wblastdate);
      sump_r := sumt_d;
      sump_d := sumt_d;
      prc_round := prc_prec;
      dbms_output.put_line('=== 1. (type_id >= 10) sumt_d: '||sumt_d||', sumt_r: '||sumt_r||', sumt_x: '||sumt_x||', wblastdate: '||wblastdate||', min_val_nk: '||min_val_nk||', max_val_nk: ' || max_val_nk);
      IF is_nal = 'Y' and is_chp = 'Y' THEN sumpn_r := sump_r; ELSE sumpn_r := 0; END IF;
      dbms_output.put_line('=== 2. (type_id >= 10) sumt_d: '||sumt_d||', sumt_r: '||sumt_r||', sumt_x: '||sumt_x||', wblastdate: '||wblastdate||', min_val_nk: '||min_val_nk||', max_val_nk: ' || max_val_nk);
      IF (NVL(sumt_d,0) = 0) AND (NVL(INSTR(opts, 'M'), 0) > 0) THEN
        RETURN;
      END IF;
      dbms_output.put_line('=== 3. (type_id >= 10) sumt_d: '||sumt_d||', sumt_r: '||sumt_r||', sumt_x: '||sumt_x||', wblastdate: '||wblastdate||', min_val_nk: '||min_val_nk||', max_val_nk: ' || max_val_nk);
      IF NVL(sumt_d,0) = 0 THEN
        raise_application_error(-20500, 'Сумма по накладным=0. Провести счет-фактуру невозможно.');
      END IF;
      sumorder_r := 0;
    ELSE
      dbms_output.put_line('=== type_id < 10');
      rate_r := 1;
      SELECT
         NVL(SUM(decode(a.val_id, SYSPC.ROUBLE, 1, SYSPC.ROUBLEBN, 1, 
          RELF.get_rate(RELF.get_base_rel(baseid, SYSPC.fact_id), a.val_id, SYSPC.ROUBLE, a.DATE_IN)) 
          * MONEY * MOVE ), 0) SUMMR,
         NVL(SUM(decode(b.is_cash, 'Y', 1, 0) * decode(a.val_id, SYSPC.ROUBLE, 1, 
          RELF.get_rate(RELF.get_base_rel(baseid, SYSPC.fact_id), a.val_id, SYSPC.ROUBLE, a.DATE_IN)) 
          * MONEY * MOVE), 0) SUMMNPR,
         NVL(SUM(MONEY * RATE * MOVE), 0) SUMMD
        INTO sump_r, sumorder_r, sump_d
        FROM PAYMENTS a, VAL_LIST b WHERE a.base_id = baseid AND a.val_id = b.id;
      dbms_output.put_line('=== 1. (type_id < 10) sumt_d: '||sumt_d||', sumt_r: '||sumt_r||', sumt_x: '||sumt_x||', wblastdate: '||wblastdate||', min_val_nk: '||min_val_nk||', max_val_nk: ' || max_val_nk);
      --
      IF (NOT is_partial) AND sump_r = 0 AND min_val_nk = max_val_nk AND min_val_nk IN (SYSPC.ROUBLE, SYSPC.ROUBLEBN) THEN
        sump_r := sumt_x;
        sumorder_r := sumt_x;
        sump_d := sumt_d;
        dbms_output.put_line('=== 2. (type_id < 10) sumt_d: '||sumt_d||', sumt_r: '||sumt_r||', sumt_x: '||sumt_x||', wblastdate: '||wblastdate||', min_val_nk: '||min_val_nk||', max_val_nk: ' || max_val_nk);
      END IF;
      -- Новое требование: создавать сч-фактуру по сумме в накладных
      sump_r := sumt_r;
      sumorder_r := sumt_r;
      sump_d := sumt_d;
      dbms_output.put_line('=== 3. (type_id < 10) sumt_d: '||sumt_d||', sumt_r: '||sumt_r||', sumt_x: '||sumt_x||', wblastdate: '||wblastdate||', min_val_nk: '||min_val_nk||', max_val_nk: ' || max_val_nk);
      --
      IF is_chp = 'Y' THEN sumpn_r := sumorder_r; ELSE sumpn_r := 0; END IF;
      -- Проверка соответствия платежей отпущенному товару
      IF ABS(sumt_d) > 0 THEN
        diffp := ROUND(ABS(100 * (sumt_d - sump_d) / sumt_d), 2);
      ELSE
        diffp := 100;
      END IF;
      dbms_output.put_line('=== 1. (type_id < 10), diffp: '||diffp||' sumt_d: '||sumt_d||', sumt_r: '||sumt_r||', sumt_x: '||sumt_x||', wblastdate: '||wblastdate||', min_val_nk: '||min_val_nk||', max_val_nk: ' || max_val_nk);
      /* Откл.проверка соответствия накладных платежам
      IF sump_d < sumt_d AND NVL(diffp, 100) > NVL(SYSPC.FACTDIFF_MAX, 0) THEN
        -- директору все можно!
        IF CU.own_role_enabled('DIRECTOR') != 'Y' THEN
          raise_application_error(-20500, 'Разница сумм между накладными и оплатами превышает '
           || to_char(SYSPC.FACTDIFF_MAX) || '% [' || to_char(diffp) ||
           '%].'||Chr(10)||'Провести счет-фактуру невозможно. Уплачено: $'||
           to_char(round(sump_d,2))||'; отпущено: $'||to_char(round(sumt_d,2)));
        END IF;
      END IF;
      */
      --prc_round := prc_prec;
      prc_round := 2;
    END IF;
    -- Атрибуты фактуры: номер, налог с продаж
    def_fact_attribs(baseid, type_id, fnumber, fdate, fl);
    nds := get_cust_nds(BASEF.get_cust_id(baseid)); --SYSPC.NDS;
    dbms_output.put_line('Cust NDS: ' || nds);
    factid := fl.ID;
    factno := fl.FNUMBER;
    -- Для ч.п. налог с продаж берется с нал.оплат
    dbms_output.put_line('=== Для ЧП');
    IF is_chp = 'Y' THEN
      sumnp_r := ROUND(TOOLS.npt(sumpn_r), 2);
    ELSE
      sumnp_r := 0;
    END IF;
    sump_r := sump_r - sumnp_r;
    sumost_r := sump_r;
    sumostnp_r := sumnp_r;
    dbms_output.put_line('=== sumost_r: '||sumost_r||', sumostnp_r: '
      || sumostnp_r || ',sumt_d: '||sumt_d||', sumt_r: '||sumt_r
      ||', sumt_x: '||sumt_x||', wblastdate: '||wblastdate||', min_val_nk: '
      ||min_val_nk||', max_val_nk: ' || max_val_nk);
    -- Завести фактуру
    -- max_nal  := SYSPC.FACT_MAX; -- разбивка на неск.фактур - устарело
    notes2 := fl.NOTE2;
    val_id := fl.VAL_ID;
    dbms_output.put_line('=== val_id: ' || fl.VAL_ID);
    INSERT INTO FACT_NUMS(FACT_ID, BIMPORT_ID, FNUM, YR, ORG_ID, PREFIX) 
      VALUES (factid, BASEF.get_import_id(baseid), factno, to_char(fl.FDATE, 'yyyy'), CU.org_id, ORGF.fact_prefix(CU.org_id));
    --
    INSERT INTO FACT_LIST (ID, ACT_ID, MOVE, BASE_ID, TYPE_ID, FDATE, FNUMBER,
      PAYM_NP, RATE, EXP_DATE, NOTE2, VAL_ID, NDS_PCT, ID_CONTRACT, FIDX, PREFIX)
    VALUES (factid, SYSPC.FACT_ID, -1, baseid, type_id, fl.FDATE, factno,
      NULL, rate_r, fl.EXP_DATE, notes2, val_id, nds, contract, fl.FIDX, fl.PREFIX);
    g.FACT_ID := factid;
    delt := 0;
    IF is_partial THEN
      OPEN goodscur FOR
      /*
        SELECT a.GOODS_ID, SUM(-c.MOVE * a.NUM_) NUM_,
          SUM(-c.MOVE * a.NUM_ * a.PRICE *
            RELF.get_rate(relid, a.VAL_ID, SYSPC.DEF_VAL, wblastdate)) SUMMD,
          MAX(a.ORD) ORD
        FROM BASE_GOODS a, BASE_LIST b, ACT_LIST c
        WHERE a.base_id = b.id AND a.act_id = c.id AND b.id = baseid -- AND b.move = -1
          AND ((filt_goods_valid IS NULL) OR (a.val_id != filt_goods_valid))
        GROUP BY a.GOODS_ID
        HAVING SUM(-c.MOVE*a.NUM_) > 0
        ORDER BY 4;
      */
        SELECT a.GOODS_ID, SUM(x.NUM_) NUM_, SUM(a.NUM2_) NUM2_,
          SUM(x.NUM_) * (SUM(-b.MOVE * round(a.NUM_ * a.PRICE, prc_round) * a.RATE) / SUM(-b.MOVE * a.NUM_)) SUMMD,
          MAX(a.ORD) ORD,
          CASE max(c.DEFECT_POS) WHEN 'Y' THEN 'Y' END as DEFECT_POS,
          MAX(a.ID)
        FROM WAYBILL_GOODS a, WAYBILL_LIST b, ACT_LIST c,
             (SELECT pg.GOODS_ID, sum(pg.NUM_ * p.MOVE) NUM_
                FROM PAYMENTS p, PAYMENT_GOODS pg
               WHERE p.id = pg.paym_id AND p.base_id = baseid
               GROUP BY pg.GOODS_ID) x
        WHERE a.WAYB_ID = b.ID AND b.BASE_ID = baseid
          AND ((filt_goods_valid IS NULL) OR (a.val_id != filt_goods_valid))
          AND b.ACT_ID = c.ID AND a.GOODS_ID = x.GOODS_ID
        GROUP BY a.GOODS_ID
        HAVING SUM(-b.MOVE*a.NUM_) > 0 AND sum(x.NUM_) > 0
        ORDER BY 2 DESC;
    ELSE
      dbms_output.put_line('=== GOODS. Не частичная');
      OPEN goodscur FOR
        SELECT a.GOODS_ID, SUM(-b.MOVE * a.NUM_) NUM_, SUM(-b.MOVE * a.NUM2_) NUM2_,
          SUM(-b.MOVE * round(a.NUM_ * a.PRICE, prc_round) * a.RATE) SUMMD, MAX(a.ORD) ORD,
          CASE max(c.DEFECT_POS) WHEN 'Y' THEN 'Y' END as DEFECT_POS,
          MAX(a.ID)
        FROM WAYBILL_GOODS a, WAYBILL_LIST b, ACT_LIST c
        WHERE a.WAYB_ID = b.ID AND b.BASE_ID = baseid -- AND b.move = -1
          AND b.ACT_ID = c.ID AND ((filt_goods_valid IS NULL) OR (a.val_id != filt_goods_valid))
        GROUP BY a.GOODS_ID
        HAVING SUM(-b.MOVE*a.NUM_) > 0
        ORDER BY 2 DESC;
    END IF;
    BEGIN
      FETCH goodscur INTO c;
      WHILE goodscur%FOUND LOOP
        dbms_output.put_line('=== goods_id: '||c.goods_id||', num_: '||c.num_||', summd: ' || c.summd);
        OPEN goods_info_cur(c.goods_id);
        FETCH goods_info_cur INTO p;
        g.DISC_PCT := NULL;
        IF goods_info_cur%FOUND THEN
          dbms_output.put_line('=== goods info: goods_id: '||c.goods_id||', disc_pct: '||p.disc_pct||', cntr_id: ' || p.CNTR_ID);
          g.ORD := p.ORD;
          g.CNTR_ID := p.CNTR_ID;
          IF p.DISC_PCT > 0 AND p.DISC_PCT <= 100 THEN
            g.DISC_PCT := p.DISC_PCT;
          END IF;
        ELSE
          g.ORD := 0;
          g.CNTR_ID := NULL;
        END IF;
        CLOSE goods_info_cur;
        g.GOODS_ID := c.goods_id;
        g.NUM2_ := c.NUM2_;
        --
        -- 20.12.2015 - требование создавать рублевую фактуру, если платежи в у.е.,
        -- на сумму предоплаты (+ остаток до полной стоимости, если была частичная предоплата)        
        SELECT COUNT(REG_VAL_ID) 
          INTO val_id_count 
          FROM (SELECT p.REG_VAL_ID FROM PAYMENTS p WHERE base_id = baseid GROUP BY REG_VAL_ID);
        IF (val_id_count = 1 AND (type_id = 6 OR type_id = 3)) THEN
          SELECT p.REG_VAL_ID INTO val_id FROM PAYMENTS p WHERE base_id = baseid AND ROWNUM = 1 GROUP BY REG_VAL_ID;
          IF (val_id NOT IN (SYSPC.ROUBLE, SYSPC.ROUBLEBN) ) THEN
            dbms_output.put_line('=== Рублевая сф при платежах в уе');
            SELECT SUM(p.reg_rate * p.money * p.move), SUM( p.money * p.move)
            INTO sump_d, sump_r
            FROM PAYMENTS p
            WHERE p.base_id = baseid;
            dbms_output.put_line('=== rub_sf. sump_d: '||sump_d||', sump_r: '||sump_r||', sumt_d: '||sumt_d||', sumt_r: '||sumt_r||', sumt_x: '||sumt_x||', wblastdate: '||wblastdate||', min_val_nk: '||min_val_nk||', max_val_nk: ' || max_val_nk);
            --
            SELECT NVL(decode(p.reg_val_id, SYSPC.ROUBLE, 1, SYSPC.ROUBLEBN, 1,
              RELF.get_rate(RELF.get_base_rel(baseid, SYSPC.fact_id), p.reg_val_id, SYSPC.ROUBLEBN, wblastdate)), 0)
              INTO ostp_rel
              FROM payments p 
            WHERE p.base_id = baseid;
            dbms_output.put_line('=== rub_sf. ostp_rel: '||ostp_rel);
            --
            ostp_d := sumt_d - sump_d;
            dbms_output.put_line('=== rub_sf. ostp_d: '||ostp_d);
            IF ostp_d > 0 THEN
              -- фактура создается на сумму предоплаты в рублях + остаток * курс у.е на дату отгрузки, т.е. на дату создания накладной.
              sump_r := sump_r + ostp_d * ostp_rel;
              dbms_output.put_line('=== rub_fs. sump_r: '||sump_r);
            END IF;
          END IF;
        END IF;
        --
        prc := (delt + (100 * sump_r * c.summd) / (sumt_d * (100 + nds))) / c.num_;
        dbms_output.put_line('=== prc := (delt + (100 * sump_r * c.summd) / (sumt_d * (100 + nds))) / c.num_;');
        dbms_output.put_line('=== prc: ' || prc || ', sump_d: '|| sump_d ||', sump_r: '||sump_r||', sumt_d: '||sumt_d||', sumt_r: '||sumt_r||', sumt_x: '||sumt_x||', wblastdate: '||wblastdate||', min_val_nk: '||min_val_nk||', max_val_nk: ' || max_val_nk);
        IF prc <= 0 THEN
          prc := (100 * sump_r * c.summd) / (sumt_d * (100 + nds) * c.num_);
          dbms_output.put_line('=== prc (prc <= 0): '||prc);
        END IF;
        IF is_partial THEN
          IF ABS(prc) > 0.000001 THEN
            -- sumost_r - сумма платежей в рублях sump_r
            -- g.NUM_ - может быть дробным числом, если количество в граммах
            g.NUM_ := ROUND(sumost_r / (prc * 0.01 * (nds + 100)), 2);
          ELSE
            g.NUM_ := 0;
          END IF;
          IF g.NUM_ > c.num_ THEN
            g.NUM_ := c.num_;
          END IF;
        ELSE
          g.NUM_ := c.num_;
        END IF;
        g.PRICE  := ROUND(prc, prc_round);
        delt := g.NUM_ * (prc - g.PRICE);
        g.SUM_NDS := ROUND(g.NUM_ * g.PRICE * nds / 100, prc_round);
        g.SUM_NP  := ROUND((sumnp_r * c.summd) / sumt_d, prc_round);
        IF g.NUM_ > 0 THEN
          sumost_r := sumost_r - (g.NUM_ * g.PRICE + g.SUM_NDS);
          sumostnp_r := sumostnp_r - g.SUM_NP;
          IF is_gtd_enabled = 'Y' THEN
            g.CODE_GTD := get_gtd(g.GOODS_ID, g.NUM_, fl.FDATE, g.CNTR_ID);
            IF (g.CODE_GTD IS NULL) THEN
              g.CODE_GTD := get_rand_gtd(g.GOODS_ID, fl.FDATE, g.CNTR_ID);
            END IF;
          ELSE
            g.CODE_GTD := NULL;
          END IF;
          tnved_id := null;
          IF (is_exprt) THEN
            tnved_id := GOODSP.FIRST_TNVED(g.GOODS_ID);
          END IF;
          INSERT INTO FACT_GOODS (FACT_ID, GOODS_ID, NUM_, PRICE, SUM_NDS, 
            SUM_NP, ORD, DISC_PCT, CNTR_ID, CODE_GTD, DEFECT_POS, TNVED_ID, WEIGHT, NUM2_)
          VALUES (g.FACT_ID, g.GOODS_ID, g.NUM_, g.PRICE, g.SUM_NDS, g.SUM_NP, 
            g.ORD, g.DISC_PCT, g.CNTR_ID, g.CODE_GTD, c.DEFECT_POS, tnved_id, g.WEIGHT, g.NUM2_);
        END IF;
        FETCH goodscur INTO c;
        -- EXIT WHEN g.NUM_ <= 0;
      END LOOP;
    EXCEPTION WHEN OTHERS THEN
      CLOSE goodscur;
      raise;
    END;
    CLOSE goodscur;
--    raise_application_error(-20500, 'delt=' || to_char(sumost_r));
    
    IF NVL(INSTR(opts, 'A'), 0) = 0 AND (ABS(sumost_r) > 0.00009 OR ABS(sumostnp_r) > 0.00009) THEN
      adjust_fact(factid, sumost_r, sumostnp_r, prc_round);
    END IF;
    reord_fact(factid);
    IF (is_nal = 'Y') AND (type_id < 10) THEN
      sumost_r := ROUND(sumorder_r, 2);
      max_nalr := SYSPC.FACTPAY_MAX;
      factdate := TRUNC(fl.FDATE);
      WHILE sumost_r > max_nalr LOOP
        INSERT INTO FACT_PAYMENTS (DATE_, FACT_ID, SUMM)
          VALUES (factdate, factid, max_nalr);
        SELECT factdate - decode(to_number(to_char(factdate - 1, 'D')), 1, 3, 7, 2, 1)
          INTO factdate FROM dual;  -- предыдущий раб.день
        sumost_r := sumost_r - max_nalr;
      END LOOP;
      IF sumost_r > 0 THEN
        INSERT INTO FACT_PAYMENTS (DATE_, FACT_ID, SUMM)
          VALUES (factdate, factid, sumost_r);
      END IF;
    END IF;
    subtract_fact(factid, prc_round, opts);
    COMMIT;
    -- Делим товары строгой отчетности по разным приходным ГТД
    --div_goods_fact(baseid, factid, wblastdate);
    COMMIT;
    -- Копируем фактуру в основание-приемник при межорге
    SELECT COUNT(*) INTO fact_count_to FROM FACT_LIST WHERE BASE_ID = base_to;
    IF (copy_fact = 'Y' AND fact_count_to = 0) THEN
      SELECT MAX(BASE_ID) INTO base_to FROM (
        SELECT c.BASE_ID FROM WAYBILL_LIST c CONNECT BY PRIOR c.id = c.up_id 
        START WITH c.id = (SELECT b.ID FROM WAYBILL_LIST b WHERE b.BASE_ID = baseid) ORDER BY LEVEL DESC
      ) WHERE BASE_ID != baseid;
      SELECT ORG_ID INTO org_to FROM BASE_LIST WHERE id = base_to;
      -- Пока копируем только в "Фасет Кострома"
      IF (base_to > 0 AND org_to = 11) THEN
        SELECT s_factl_id.NEXTVAL INTO factid_to FROM dual;
        INSERT INTO FACT_LIST (ID, ACT_ID, MOVE, BASE_ID, TYPE_ID, FDATE, FNUMBER,
          PAYM_NP, RATE, EXP_DATE, NOTE2, VAL_ID, NDS_PCT, FIDX)
        VALUES (factid_to, SYSPC.FACT_ID, 1, base_to, type_id, fl.FDATE, 0,
          NULL, rate_r, fl.EXP_DATE, notes2, val_id, nds, factno);
        INSERT INTO FACT_GOODS (FACT_ID, GOODS_ID, NUM_, PRICE, SUM_NDS, SUM_NP, ORD, DISC_PCT, CNTR_ID, CODE_GTD, DEFECT_POS)
        (SELECT factid_to, GOODS_ID, NUM_, PRICE, SUM_NDS, SUM_NP, ORD, DISC_PCT, CNTR_ID, CODE_GTD, DEFECT_POS
          FROM FACT_GOODS WHERE FACT_ID = factid);
      END IF;
    END IF;
  EXCEPTION
    WHEN DUP_VAL_ON_INDEX
      THEN raise_application_error(-20500, 'Такой номер счет-фактуры уже существует.');
      ROLLBACK;
  END;
--
  PROCEDURE make_fact_2016(baseid IN INTEGER, is_nal IN CHAR DEFAULT 'Y',
    type_id IN NUMBER DEFAULT 1, prc_prec IN NUMBER DEFAULT 2,
    opts IN VARCHAR2 DEFAULT NULL, filt_goods_valid IN INTEGER DEFAULT NULL,
    fnumber IN INTEGER DEFAULT NULL, waybid_ IN VARCHAR DEFAULT '', 
    fdate IN DATE DEFAULT NULL, contract IN INTEGER DEFAULT NULL)
  IS
    CURSOR goods_info_cur(goodsid IN INTEGER) IS
      SELECT a.ORD, (100 * (a.PRICE0 - a.PRICE)) / a.PRICE0 DISC_PCT,
       decode(GOODSP.get_in_group(a.GOODS_ID, 12), 'Y', NULL, 171) CNTR_ID
      FROM ACT_LIST c, BASE_GOODS a, BASE_LIST b
      WHERE a.base_id = b.id AND a.act_id = c.id
       AND b.id = baseid AND a.goods_id = goodsid AND c.move = -1;
    --
    CURSOR service_sum IS
      SELECT SUM(-b.MOVE * a.NUM_) NUM_, SUM(-b.MOVE * round(a.NUM_ * a.PRICE, 4) * a.RATE) SUMMD
        FROM WAYBILL_GOODS a, WAYBILL_LIST b, ACT_LIST c
        WHERE a.WAYB_ID = b.ID AND b.BASE_ID = baseid
          AND b.ACT_ID = c.ID 
          AND b.ID IN (select regexp_substr(waybid_,'[^,]+', 1, level) 
                         from dual 
                      connect by regexp_substr(waybid_,'[^,]+', 1, level) is not null) 
          AND ((filt_goods_valid IS NULL) OR (a.val_id != filt_goods_valid))
          AND GOODSP.get_is_service(a.GOODS_ID) = 'Y'
          --AND GOODSP.allow_inc_ivoice(a.GOODS_ID) = 'Y'
          ;
    --
    TYPE FactGoodsRecType IS RECORD (
      GOODS_ID NUMBER(6),
      NUM_     NUMBER,
      NUM2_    NUMBER,
      SUMMD    NUMBER,
      ORD      NUMBER(6),
      DEFECT_POS VARCHAR2(1),
      ID  NUMBER,
      WEIGHT NUMBER
    );
    --
    TYPE FactGoodsCurType IS REF CURSOR RETURN FactGoodsRecType;
    --
    goodscur FactGoodsCurType;
    c  FactGoodsRecType;
    p  goods_info_cur%ROWTYPE;
    g  FactGoodsRowType;
    fl FactListRowType;
    flc FactListRowType;
    r TResDivGoods;
    --
    j BINARY_INTEGER := 0;
    notes2     FACT_LIST.NOTE2%TYPE;
    flag       CHAR(1);
    is_chp     CHAR(1);
    copy_fact  CHAR(1) := 'N';
    prc_round  INTEGER;
    factid     INTEGER;
    factid_to  INTEGER;
    factno     INTEGER;
    fact_count_to INTEGER := 0;
    org_to     INTEGER;
    actid      INTEGER;
    val_id     INTEGER;
    base_to    INTEGER := 0; -- в какое основание копировать накладную
    sumt_d     NUMBER;
    sumt_r     NUMBER;
    sumt_x     NUMBER;
    sump_d     NUMBER;
    sump_r     NUMBER;
    sumpn_r    NUMBER;
    sumnp_r    NUMBER;
    sumost_r   NUMBER;
    sumostnp_r NUMBER;
    sumorder_r NUMBER;
    ostp_d     NUMBER;
    ostp_r     NUMBER;
    ostp_rel   NUMBER;
    val_id_count BINARY_INTEGER := 0;
    rate_r     NUMBER;
    max_nalr   NUMBER;
    nds        NUMBER;
    diffp      NUMBER;
    prc        NUMBER;
    delt       NUMBER;
    wblastdate DATE;
    is_partial BOOLEAN;
    relid      NUMBER;
    factdate   DATE;
    min_val_nk BINARY_INTEGER;
    max_val_nk BINARY_INTEGER;
    errm       VARCHAR2(4001);
    vis_fact   CHAR(1) := 'Y';
    waybidf    INTEGER;
    exprt      BOOLEAN;
    tnved_id   INTEGER;
  BEGIN
    errm := NVL(BASEF.get_is_allowed(baseid, 'DOCS'), 'N');
    IF errm != 'Y' THEN
      raise_application_error(-20500, 'Состояние основания №' || to_char(baseid) ||
        ' не допускает проведения счетов-фактур. (' || substr(errm, 3) || ')');
    END IF;
    -- Для межорганизационного перемещения копируем фактуру в организацию "приемник"
    actid := BASEF.get_act_id;
    IF (actid = 352 OR actid = 378 OR actid = 475) THEN
      vis_fact := 'N';
      copy_fact := 'Y';
      IF (actid = 352) THEN
        vis_fact := 'N';
      END IF;
    END IF;
    is_partial := NVL(INSTR(opts, 'P'), 0) > 0;
    exprt := NVL(INSTR(opts, 'E'), 0) > 0;
    relid := BASEF.get_rel_id(baseid);
    -- Проверка клиента
    SELECT is_firm, decode(rtrim(REGNO), NULL, 'N', 'Y')
      INTO flag, is_chp FROM OBJ_LIST WHERE id = BASEF.get_cust_id(baseid);
    IF flag != 'Y' THEN
      raise_application_error(-20500, OBJF.get_name(BASEF.get_cust_id(baseid)) ||
       ' не является юридическим лицом. Провести счет-фактуру невозможно.');
    END IF;
    -- Выделить сумму отпущенного товара (для частичн.сч-ф исходя из принятых денег - предоплата)
    IF is_partial THEN
      SELECT NVL(SUM(MOVE * MONEY * RATE), 0), MAX(DATE_IN)
        INTO sumt_d, wblastdate
        FROM PAYMENTS WHERE base_id = baseid;
    ELSE
      SELECT NVL(SUM(-b.MOVE * a.NUM_ * a.PRICE * a.RATE), 0),
             NVL(SUM(-b.MOVE * a.NUM_ * a.PRICE *
              RELF.get_rate(RELF.get_base_rel(baseid, SYSPC.fact_id), a.val_id, SYSPC.ROUBLE, b.DATE_IN)), 0),
             NVL(SUM(-b.MOVE * a.NUM_ * a.PRICE), 0),
             MAX(b.DATE_), MIN(a.VAL_ID), MAX(a.VAL_ID)
        INTO sumt_d, sumt_r, sumt_x, wblastdate, min_val_nk, max_val_nk
        FROM WAYBILL_GOODS a, WAYBILL_LIST b
        WHERE a.wayb_id = b.id AND b.base_id = baseid
          AND b.ID IN (select regexp_substr(waybid_,'[^,]+', 1, level) 
                         from dual 
                      connect by regexp_substr(waybid_,'[^,]+', 1, level) is not null)
          AND ((filt_goods_valid IS NULL) OR (a.val_id != filt_goods_valid));
      -- Запрос Антона от 13.1.15 
      -- отгрузка товара в долларах с выписыванием сч.фактуры в рублях.
      -- Надо: Курс Магазина - Рубли б/н (!) на дату, указываемую при создании сч.фактуры.
      -- доп.от 21.1.15 если дата фактуры сегодняшняя, время для расчета курса берется текущее
      IF min_val_nk = max_val_nk AND min_val_nk = SYSPC.DEF_VAL THEN
        SELECT NVL(SUM(-b.MOVE * round(a.NUM_ * a.PRICE, prc_prec) * a.RATE), 0),
               -- Исправление от 18.06.2018
               NVL(round(round(SUM(-b.MOVE * a.NUM_ * a.PRICE), prc_prec+1), prc_prec) *
               MAX(RELF.get_rate(RELF.get_base_rel(baseid, SYSPC.fact_id), a.val_id, SYSPC.ROUBLEBN,
                  case when trunc(NVL(fdate, SYSDATE)) = trunc(SYSDATE)
                   then SYSDATE else fdate end)), 0),
               NVL(SUM(-b.MOVE * round(a.NUM_ * a.PRICE, prc_prec
               )), 0),
               MAX(b.DATE_), MIN(a.VAL_ID), MAX(a.VAL_ID)
          INTO sumt_d, sumt_r, sumt_x, wblastdate, min_val_nk, max_val_nk
          FROM WAYBILL_GOODS a, WAYBILL_LIST b
          WHERE a.wayb_id = b.id AND b.base_id = baseid 
            AND b.ID IN (select regexp_substr(waybid_,'[^,]+', 1, level) from dual connect by regexp_substr(waybid_,'[^,]+', 1, level) is not null)
            AND ((filt_goods_valid IS NULL) OR (a.val_id != filt_goods_valid));
      END IF;
    END IF;
    -- Выделить сумму принятых денег
    IF type_id >= 10 AND type_id < 30 THEN
      SELECT ROUND(sumt_d * (1 + 0.01*NVL(conv_pct,0)) * (1 + 0.01*NVL(nsp_pct,0)), 2)
        INTO sumt_d FROM BASE_LIST WHERE id = baseid;
      rate_r := RELF.get_rate(SYSPC.DEF_REL, SYSPC.DEF_VAL, SYSPC.ROUBLEBN, wblastdate);
      sump_r := sumt_d;
      sump_d := sumt_d;
      prc_round := prc_prec;
      IF is_nal = 'Y' and is_chp = 'Y' THEN sumpn_r := sump_r; ELSE sumpn_r := 0; END IF;
      IF (NVL(sumt_d,0) = 0) AND (NVL(INSTR(opts, 'M'), 0) > 0) THEN
        RETURN;
      END IF;
      IF NVL(sumt_d,0) = 0 THEN
        raise_application_error(-20500, 'Сумма по накладным=0. Провести счет-фактуру невозможно.');
      END IF;
      sumorder_r := 0;
    ELSE
      rate_r := 1;
      SELECT
         NVL(SUM(decode(a.val_id, SYSPC.ROUBLE, 1, SYSPC.ROUBLEBN, 1, 
          RELF.get_rate(RELF.get_base_rel(baseid, SYSPC.fact_id), a.val_id, SYSPC.ROUBLE, a.DATE_IN)) 
          * MONEY * MOVE ), 0) SUMMR,
         NVL(SUM(decode(b.is_cash, 'Y', 1, 0) * decode(a.val_id, SYSPC.ROUBLE, 1, 
          RELF.get_rate(RELF.get_base_rel(baseid, SYSPC.fact_id), a.val_id, SYSPC.ROUBLE, a.DATE_IN)) 
          * MONEY * MOVE), 0) SUMMNPR,
         NVL(SUM(MONEY * RATE * MOVE), 0) SUMMD
        INTO sump_r, sumorder_r, sump_d
        FROM PAYMENTS a, VAL_LIST b WHERE a.base_id = baseid AND a.val_id = b.id;
      --
      IF (NOT is_partial) AND sump_r = 0 AND min_val_nk = max_val_nk AND min_val_nk IN (SYSPC.ROUBLE, SYSPC.ROUBLEBN) THEN
        sump_r := sumt_x;
        sumorder_r := sumt_x;
        sump_d := sumt_d;
      END IF;
      -- Новое требование: создавать сч-фактуру по сумме в накладных
      sump_r := sumt_r;
      sumorder_r := sumt_r;
      sump_d := sumt_d;
      --
      IF is_chp = 'Y' THEN sumpn_r := sumorder_r; ELSE sumpn_r := 0; END IF;
      -- Проверка соответствия платежей отпущенному товару
      IF ABS(sumt_d) > 0 THEN
        diffp := ROUND(ABS(100 * (sumt_d - sump_d) / sumt_d), 2);
      ELSE
        diffp := 100;
      END IF;
      /* Откл.проверка соответствия накладных платежам
      IF sump_d < sumt_d AND NVL(diffp, 100) > NVL(SYSPC.FACTDIFF_MAX, 0) THEN
        -- директору все можно!
        IF CU.own_role_enabled('DIRECTOR') != 'Y' THEN
          raise_application_error(-20500, 'Разница сумм между накладными и оплатами превышает '
           || to_char(SYSPC.FACTDIFF_MAX) || '% [' || to_char(diffp) ||
           '%].'||Chr(10)||'Провести счет-фактуру невозможно. Уплачено: $'||
           to_char(round(sump_d,2))||'; отпущено: $'||to_char(round(sumt_d,2)));
        END IF;
      END IF;
      */
      prc_round := prc_prec;
      --prc_round := 2;
    END IF;
    -- Атрибуты фактуры: номер, налог с продаж
    def_fact_attribs(baseid, type_id, fnumber, fdate, fl);
    nds := get_cust_nds(BASEF.get_cust_id(baseid)); --SYSPC.NDS;
    factid := fl.ID;
    factno := fl.FNUMBER;
    -- Для ч.п. налог с продаж берется с нал.оплат
    IF is_chp = 'Y' THEN
      sumnp_r := ROUND(TOOLS.npt(sumpn_r), 2);
    ELSE
      sumnp_r := 0;
    END IF;
    sump_r := sump_r - sumnp_r;
    sumost_r := sump_r;
    sumostnp_r := sumnp_r;
    -- Завести фактуру
    -- max_nal  := SYSPC.FACT_MAX; -- разбивка на неск.фактур - устарело
    notes2 := fl.NOTE2;
    val_id := fl.VAL_ID;
    IF (factno > 0) THEN
      INSERT INTO FACT_NUMS(FACT_ID, BIMPORT_ID, FNUM, YR, ORG_ID, PREFIX) 
        VALUES (factid, BASEF.get_import_id(baseid), factno, to_char(fl.FDATE, 'yyyy'), CU.org_id, ORGF.fact_prefix(CU.org_id));
    END IF;
    --
    INSERT INTO FACT_LIST (ID, ACT_ID, MOVE, BASE_ID, TYPE_ID, FDATE, FNUMBER,
      PAYM_NP, RATE, EXP_DATE, NOTE2, VAL_ID, NDS_PCT, ID_CONTRACT, FIDX, PREFIX)
    VALUES (factid, SYSPC.FACT_ID, -1, baseid, type_id, fl.FDATE, factno,
      NULL, rate_r, fl.EXP_DATE, notes2, val_id, nds, contract, fl.FIDX, 
      fl.PREFIX);
    INSERT INTO FACT_WAYB_REFS (WAYB_ID, FACT_ID)
    (SELECT regexp_substr(waybid_,'[^,]+', 1, level), factid
       FROM dual CONNECT BY regexp_substr(waybid_,'[^,]+', 1, level) IS NOT NULL);
    g.FACT_ID := factid;
    delt := 0;
    IF is_partial THEN
      OPEN goodscur FOR
        SELECT a.GOODS_ID, SUM(x.NUM_) NUM_, SUM(a.NUM2_) NUM2_,
          SUM(x.NUM_) * (SUM(-b.MOVE * round(a.NUM_ * a.PRICE, prc_round) * a.RATE) / SUM(-b.MOVE * a.NUM_)) SUMMD,
          MAX(a.ORD) ORD,
          CASE max(c.DEFECT_POS) WHEN 'Y' THEN 'Y' END as DEFECT_POS,
          MAX(a.ID), MAX(a.WEIGHT) WEIGHT
        FROM WAYBILL_GOODS a, WAYBILL_LIST b, ACT_LIST c,
             (SELECT pg.GOODS_ID, sum(pg.NUM_ * p.MOVE) NUM_
                FROM PAYMENTS p, PAYMENT_GOODS pg
               WHERE p.id = pg.paym_id AND p.base_id = baseid
               GROUP BY pg.GOODS_ID) x
        WHERE a.WAYB_ID = b.ID AND b.BASE_ID = baseid
          AND ((filt_goods_valid IS NULL) OR (a.val_id != filt_goods_valid))
          AND b.ACT_ID = c.ID AND a.GOODS_ID = x.GOODS_ID 
          AND b.ID IN (select regexp_substr(waybid_,'[^,]+', 1, level) from dual connect by regexp_substr(waybid_,'[^,]+', 1, level) is not null)
          --AND GOODSP.allow_inc_ivoice(a.GOODS_ID) = 'N'
        GROUP BY a.GOODS_ID
        HAVING SUM(-b.MOVE*a.NUM_) > 0 AND sum(x.NUM_) > 0
        ORDER BY 2 DESC;
    ELSE
      OPEN goodscur FOR
        SELECT a.GOODS_ID, SUM(-b.MOVE * a.NUM_) NUM_, SUM(-b.MOVE * a.NUM2_) NUM2_,
          SUM(-b.MOVE * round(a.NUM_ * a.PRICE, prc_round) * a.RATE) SUMMD, MAX(a.ORD) ORD,
          CASE max(c.DEFECT_POS) WHEN 'Y' THEN 'Y' END as DEFECT_POS,
          MAX(a.ID), MAX(a.WEIGHT) WEIGHT
        FROM WAYBILL_GOODS a, WAYBILL_LIST b, ACT_LIST c
        WHERE a.WAYB_ID = b.ID AND b.BASE_ID = baseid
          AND b.ACT_ID = c.ID AND b.ID IN (select regexp_substr(waybid_,'[^,]+', 1, level) from dual connect by regexp_substr(waybid_,'[^,]+', 1, level) is not null) 
          AND ((filt_goods_valid IS NULL) OR (a.val_id != filt_goods_valid))
          --AND GOODSP.allow_inc_ivoice(a.GOODS_ID) = 'N'
        GROUP BY a.GOODS_ID
        HAVING SUM(-b.MOVE*a.NUM_) > 0
        ORDER BY 2 DESC;
    END IF;
    BEGIN
      FETCH goodscur INTO c;
      WHILE goodscur%FOUND LOOP
        OPEN goods_info_cur(c.goods_id);
        FETCH goods_info_cur INTO p;
        CORRECTIONS.prt_goods_recalc(c.goods_id);
        g.DISC_PCT := NULL;
        IF goods_info_cur%FOUND THEN
          g.ORD := p.ORD;
          g.CNTR_ID := p.CNTR_ID;
          IF p.DISC_PCT > 0 AND p.DISC_PCT <= 100 THEN
            g.DISC_PCT := p.DISC_PCT;
          END IF;
        ELSE
          g.ORD := 0;
          g.CNTR_ID := NULL;
        END IF;
        CLOSE goods_info_cur;
        g.GOODS_ID := c.goods_id;
        g.WEIGHT := c.WEIGHT;
        --
        -- 20.12.2015 - требование создавать рублевую фактуру, если платежи в у.е.,
        -- на сумму предоплаты (+ остаток до полной стоимости, если была частичная предоплата)        
        -- группируем платежи по валютам и считаем количество групп
        SELECT COUNT(REG_VAL_ID) 
          INTO val_id_count 
          FROM (SELECT MIN(p.REG_VAL_ID) REG_VAL_ID, max(p.DATE_), max(wl.DATE_) FROM PAYMENTS p, WAYBILL_LIST wl 
                 WHERE p.base_id = baseid AND p.DATE_ < wl.DATE_ AND p.base_id = wl.base_id GROUP BY REG_VAL_ID);
        -- Если все платежи в одной валюте
        IF (actid = 21000 AND val_id_count = 1 AND (type_id = 6 OR type_id = 3 OR type_id = 7)) THEN
          SELECT p.REG_VAL_ID INTO val_id FROM PAYMENTS p WHERE base_id = baseid AND ROWNUM = 1 GROUP BY REG_VAL_ID;
          -- Если все платежи в одной валюте - не рубли
          IF (val_id NOT IN (SYSPC.ROUBLE, SYSPC.ROUBLEBN) ) THEN
            SELECT SUM( p.money * p.move), SUM(RELF.get_rate(RELF.get_base_rel(baseid, actid), p.reg_val_id, SYSPC.ROUBLEBN, p.DATE_) * p.money * p.move)
            INTO sump_d, sump_r
            FROM PAYMENTS p
            WHERE p.base_id = baseid;
            --
            SELECT NVL(decode(p.reg_val_id, SYSPC.ROUBLE, 1, SYSPC.ROUBLEBN, 1,
              RELF.get_rate(RELF.get_base_rel(baseid, actid), p.val_id, SYSPC.ROUBLEBN, wblastdate)), 0)
              INTO ostp_rel
              FROM payments p 
            WHERE p.base_id = baseid;
            --
            ostp_d := sumt_d - sump_d;
            IF ostp_d > 0 THEN
              -- фактура создается на сумму предоплаты в рублях + остаток * курс у.е на дату отгрузки, т.е. на дату создания накладной.
              sump_r := sump_r + ostp_d * ostp_rel;
            END IF;
          END IF;
        END IF;
        --
        prc := (delt + (100 * sump_r * c.summd) / (sumt_d * (100 + nds))) / c.num_;
        IF prc <= 0 THEN
          prc := (100 * sump_r * c.summd) / (sumt_d * (100 + nds) * c.num_);
        END IF;
        IF is_partial THEN
          IF ABS(prc) > 0.000001 THEN
            -- sumost_r - сумма платежей в рублях sump_r
            -- g.NUM_ - может быть дробным числом, если количество товара в 
            -- граммах
            g.NUM_ := ROUND(sumost_r / (prc * 0.01 * (nds + 100)));
          ELSE
            g.NUM_ := 0;
          END IF;
          IF g.NUM_ > c.num_ THEN
            g.NUM_ := c.num_;
          END IF;
        ELSE
          g.NUM_ := c.num_;
        END IF;
        g.NUM2_ := c.NUM2_;
        g.PRICE  := ROUND(prc, prc_round);
        delt := g.NUM_ * (prc - g.PRICE);
        g.SUM_NDS := ROUND(g.NUM_ * g.PRICE * nds / 100, prc_round);
        --g.SUM_NDS := round(g.PRICE, prc_round) * (1 + 0.01 * nds) * g.NUM_ - round(g.PRICE, prc_round) * g.NUM_;
        g.SUM_NP  := ROUND((sumnp_r * c.summd) / sumt_d, prc_round);
        IF g.NUM_ > 0 THEN
          sumost_r := sumost_r - (g.NUM_ * g.PRICE + g.SUM_NDS);
          --sumost_r := sumost_r - round(g.PRICE, prc_round) * (1 + 0.01 * nds) * g.NUM_;
          sumostnp_r := sumostnp_r - g.SUM_NP;
          IF is_gtd_enabled = 'Y' THEN
            g.CODE_GTD := get_gtd(g.GOODS_ID, g.NUM_, fl.FDATE, g.CNTR_ID);
            IF (g.CODE_GTD IS NULL) THEN
              g.CODE_GTD := get_rand_gtd(g.GOODS_ID, fl.FDATE, g.CNTR_ID);
            END IF;
          ELSE
            g.CODE_GTD := NULL;
          END IF;
          IF (exprt) THEN
            tnved_id := GOODSP.FIRST_TNVED(g.GOODS_ID);
          END IF;
          INSERT INTO FACT_GOODS (FACT_ID, GOODS_ID, NUM_, PRICE, SUM_NDS, 
            SUM_NP, ORD, DISC_PCT, CNTR_ID, CODE_GTD, DEFECT_POS, TNVED_ID, WEIGHT, NUM2_)
          VALUES (g.FACT_ID, g.GOODS_ID, g.NUM_, g.PRICE, g.SUM_NDS, g.SUM_NP, 
            g.ORD, g.DISC_PCT, g.CNTR_ID, g.CODE_GTD, c.DEFECT_POS, tnved_id, g.WEIGHT, g.NUM2_);
        END IF;
        FETCH goodscur INTO c;
        -- EXIT WHEN g.NUM_ <= 0;
      END LOOP;
    EXCEPTION WHEN OTHERS THEN
      CLOSE goodscur;
      raise;
    END;
    CLOSE goodscur;
--    raise_application_error(-20500, 'delt=' || to_char(sumost_r));
    
    IF NVL(INSTR(opts, 'A'), 0) = 0 AND (ABS(sumost_r) > 0.00009 OR ABS(sumostnp_r) > 0.00009) THEN
      adjust_fact(factid, sumost_r, sumostnp_r, prc_round);
    END IF;
    reord_fact(factid);
    IF (is_nal = 'Y') AND (type_id < 10) THEN
      sumost_r := ROUND(sumorder_r, 2);
      max_nalr := SYSPC.FACTPAY_MAX;
      factdate := TRUNC(fl.FDATE);
      WHILE sumost_r > max_nalr LOOP
        INSERT INTO FACT_PAYMENTS (DATE_, FACT_ID, SUMM)
          VALUES (factdate, factid, max_nalr);
        SELECT factdate - decode(to_number(to_char(factdate - 1, 'D')), 1, 3, 7, 2, 1)
          INTO factdate FROM dual;  -- предыдущий раб.день
        sumost_r := sumost_r - max_nalr;
      END LOOP;
      IF sumost_r > 0 THEN
        INSERT INTO FACT_PAYMENTS (DATE_, FACT_ID, SUMM)
          VALUES (factdate, factid, sumost_r);
      END IF;
    END IF;
    --subtract_fact(factid, prc_round, opts);
    --COMMIT;
    -- Подгоняем товары по цене к накладной
    div_goods_fact_prt(baseid, factid, wblastdate, prc_prec);
    -- Делим товары строгой отчетности по разным приходным ГТД
    --div_goods_fact(baseid, factid, wblastdate);
    reord_fact(factid);
    COMMIT;
    -- Копируем фактуру в основание-приемник при межорге
    IF (copy_fact = 'Y' AND fact_count_to = 0) THEN
      SELECT max(base_id) INTO base_to from (
        SELECT trim(c.base_id) base_id FROM WAYBILL_LIST c 
        WHERE c.up_id IN (select regexp_substr(waybid_,'[^,]+', 1, level) FROM dual connect by regexp_substr(waybid_,'[^,]+', 1, level) is not null)
      );
      IF (base_to IS NULL) THEN
        SELECT max(count(*)), max(id)
          INTO fact_count_to, waybidf
          FROM waybill_list
         WHERE base_id = baseid
           AND id IN
               (SELECT REGEXP_SUBSTR(waybid_,'[^,]+', 1, LEVEL) r FROM DUAL 
                 CONNECT BY REGEXP_SUBSTR(waybid_,'[^,]+', 1, LEVEL) IS NOT NULL)
         GROUP BY ID;
        actid := WAYBILLP.GET_ACT_ID(waybidf);
        -- Если основание не найдено, значит выделяем номер основания из поля NOTE.
        IF (actid = 350 or actid = 352) THEN
          -- Привязываем значения id накладных из строки к номеру основаня
          SELECT ID INTO waybidf FROM WAYBILL_LIST WHERE BASE_ID = baseid AND id = 
            (SELECT trim(c.base_id) base_id FROM WAYBILL_LIST c 
            WHERE c.up_id IN (select regexp_substr(waybid_,'[^,]+', 1, level) FROM dual connect by regexp_substr(waybid_,'[^,]+', 1, level) is not null));
          FOR a IN (SELECT R FROM (select regexp_substr((SELECT a.NOTE FROM WAYBILL_LIST a WHERE a.ID = to_number(waybidf)),'[^ ]+', 1, level) as R from dual
            CONNECT BY regexp_substr( (SELECT a.NOTE FROM WAYBILL_LIST a WHERE a.ID = to_number(waybidf)), '[^ ]+', 1, level) is not null) where regexp_like(R, '[0-9]{6,7}')) LOOP
            SELECT TRIM(REGEXP_REPLACE(a.r, '№','')) INTO base_to FROM DUAL;
          END LOOP;
        ELSIF (actid = 378) THEN
          -- Возврат из филиалов. В этом случае поле up_id указывает не понятно куда
          -- Привязываем значения id накладных из строки к номеру основаня
          FOR A IN (SELECT R FROM (SELECT REGEXP_SUBSTR((SELECT a.NOTE FROM WAYBILL_LIST A WHERE a.ID = TO_NUMBER(waybidf)),'[^ ]+', 1, LEVEL) AS R FROM DUAL
            CONNECT BY regexp_substr( (SELECT a.NOTE FROM WAYBILL_LIST a WHERE a.ID = to_number(waybidf)), '[^ ]+', 1, level) is not null) where regexp_like(R, '[0-9]{6,7}')) 
          LOOP
            SELECT TRIM(REGEXP_REPLACE(a.r, '№','')) INTO base_to FROM DUAL;
          END LOOP;
        END IF;
        
        IF (base_to IS NULL) THEN
          raise_application_error(-20500, 'Не удается скопировать фактуру в основание-приемник. Это основание не найдено.');
          RETURN;
        END IF;
      END IF;
      org_to := BASEF.GET_ORG_ID(base_to);
      IF (org_to IS NOT NULL) THEN
        IF (base_to > 0) THEN
          def_fact_attribs(base_to, type_id, null, fdate, flc, org_to);
          INSERT INTO FACT_LIST (ID, ACT_ID, MOVE, BASE_ID, TYPE_ID, FDATE, FNUMBER,
            PAYM_NP, RATE, EXP_DATE, NOTE2, VAL_ID, NDS_PCT, VISIBLE_, FIDX, PREFIX)
          VALUES (flc.ID, SYSPC.FACT_ID, 1, base_to, type_id, flc.FDATE, 
            CASE vis_fact WHEN 'Y' THEN flc.FNUMBER WHEN 'N' THEN 0 END,
            NULL, rate_r, CASE vis_fact WHEN 'Y' THEN flc.EXP_DATE WHEN 'N' THEN fl.EXP_DATE END, 
            DECODE(actid, 378, 'Автофактура при возврате', 'Автофактура при отгрузе в филиалы'), 
            val_id, nds, vis_fact, fl.FIDX, fl.PREFIX);
          INSERT INTO FACT_GOODS (FACT_ID, GOODS_ID, NUM_, PRICE, SUM_NDS, 
            SUM_NP, ORD, DISC_PCT, CNTR_ID, CODE_GTD, DEFECT_POS, TNVED_ID)
          (SELECT flc.ID, GOODS_ID, NUM_, PRICE, SUM_NDS, SUM_NP, ORD, DISC_PCT, 
                  CNTR_ID, CODE_GTD, DEFECT_POS, TNVED_ID
             FROM FACT_GOODS WHERE FACT_ID = factid);
          END IF;
      END IF;
    END IF;
  EXCEPTION
    WHEN DUP_VAL_ON_INDEX
      THEN raise_application_error(-20500, 'Такой номер счет-фактуры уже существует.');
      ROLLBACK;
  END;
--
  PROCEDURE make_fact_2017k(baseid IN INTEGER, is_nal IN CHAR DEFAULT 'Y',
    type_id IN NUMBER DEFAULT 1, prc_prec IN NUMBER DEFAULT 2,
    opts IN VARCHAR2 DEFAULT NULL, filt_goods_valid IN INTEGER DEFAULT NULL,
    fnumber IN INTEGER DEFAULT NULL, waybid_ IN VARCHAR DEFAULT '', 
    fdate IN DATE DEFAULT NULL, contract IN INTEGER DEFAULT NULL)
  IS
  CURSOR info(wlid_ IN VARCHAR2) IS
    select wg.price, abs(nvl(sum(wg.num_ * wl.move), 0)) num_, 
      nvl(sum(wg.num2_ * wl.move), 0) num2_, wg.goods_id, wl.cust_id, max(wl.date_) wbdate, 
      wl.base_id, bl.CONV_PCT,
      round((100 * (bg.PRICE0 - bg.PRICE)) / bg.PRICE0, 2) DISC_PCT,
      CASE max(al.DEFECT_POS) WHEN 'Y' THEN 'Y' END as DEFECT_POS,
      wg.cntr_id 
      from waybill_list wl, waybill_goods wg, base_list bl, base_goods bg, act_list al
     where wg.wayb_id = wl.id
       and wl.base_id = bl.id
       and bg.base_id = bl.id
       and bg.goods_id = wg.goods_id
       and wl.id IN (select regexp_substr(wlid_,'[^,]+', 1, level) from dual connect by regexp_substr(wlid_,'[^,]+', 1, level) is not null)
       and bg.act_id != 431
       and wl.act_id = al.id
     group by wg.price, wg.goods_id, wg.goods_id, wl.cust_id, /*wl.date_,*/ 
       wl.base_id, bl.conv_pct, (100 * (bg.PRICE0 - bg.PRICE)) / bg.PRICE0, 10,
       wg.cntr_id;
  --
  wlc info%ROWTYPE;
  nds NUMBER;
  price_nonds NUMBER;
  price_nds NUMBER;
  summ_nds NUMBER;
  summ_wnds NUMBER;
  summ_np NUMBER := 0;
  rate_r     NUMBER := 1;
  ord INTEGER;
  disc_pct NUMBER;
  wblastdate DATE;
  fl FactListRowType;
  is_exprt BOOLEAN;
  tnved_id INTEGER;
  wb_stmt VARCHAR2(1500);
  BEGIN
    def_fact_attribs(baseid, type_id, fnumber, fdate, fl);
    is_exprt := NVL(INSTR(opts, 'E'), 0) > 0;
    nds := get_cust_nds(BASEF.get_cust_id(baseid));
    INSERT INTO FACT_NUMS(FACT_ID, BIMPORT_ID, FNUM, YR, ORG_ID, PREFIX) 
      VALUES (fl.ID, BASEF.get_import_id(baseid), fl.fnumber, to_char(fl.FDATE, 'yyyy'), CU.org_id, ORGF.fact_prefix(CU.org_id));
    --
    INSERT INTO FACT_LIST (ID, ACT_ID, MOVE, BASE_ID, TYPE_ID, FDATE, FNUMBER,
      PAYM_NP, RATE, EXP_DATE, NOTE2, VAL_ID, NDS_PCT, ID_CONTRACT, FIDX, PREFIX)
    VALUES (fl.ID, SYSPC.FACT_ID, -1, baseid, type_id, fl.FDATE, fl.fnumber,
      NULL, rate_r, fl.EXP_DATE, fl.note2, fl.val_id, nds, contract, fl.fnumber, ORGF.fact_prefix(CU.org_id));
    --
    INSERT INTO FACT_WAYB_REFS (WAYB_ID, FACT_ID)
    (SELECT regexp_substr(waybid_,'[^,]+', 1, level), fl.ID
       FROM dual CONNECT BY regexp_substr(waybid_,'[^,]+', 1, level) IS NOT NULL);
    --
    OPEN info(waybid_);
    ord := 1;
      LOOP
        FETCH info INTO wlc;
        EXIT WHEN info%NOTFOUND;
        price_nds := round(round(wlc.price / (1 + 0.01 * nds), 2) * (1 + 0.01 * nds), 2);
        price_nonds := ( round(price_nds * wlc.num_, 2) / (1 + 0.01 * nds)) / wlc.num_;
        summ_nds := price_nonds * wlc.num_ * (0.01 * nds);
        IF (wlc.disc_pct >= 0) THEN
          disc_pct := wlc.disc_pct;
        ELSE
          disc_pct := 0;
        END IF;
        tnved_id := null;
        IF (is_exprt) THEN
          tnved_id := GOODSP.FIRST_TNVED(wlc.GOODS_ID);
        END IF;
        INSERT INTO FACT_GOODS (FACT_ID, GOODS_ID, NUM_, PRICE, SUM_NDS, SUM_NP, 
          ORD, DISC_PCT, CNTR_ID, CODE_GTD, DEFECT_POS, TNVED_ID)
        VALUES (fl.ID, wlc.GOODS_ID, wlc.NUM_, price_nonds, summ_nds, summ_np, 
          ord, disc_pct, wlc.cntr_id, '', wlc.DEFECT_POS, tnved_id);
        ord := ord + 1;
        wblastdate := wlc.wbdate;
      END LOOP;
    CLOSE info;
    div_goods_fact_prt(baseid, fl.ID, wblastdate, prc_prec);
    reord_fact(fl.ID);
    COMMIT;
  EXCEPTION
    WHEN DUP_VAL_ON_INDEX
      THEN raise_application_error(-20500, 'Такой номер счет-фактуры уже существует.');
      ROLLBACK;
  END;
--
  PROCEDURE make_fact_2003(baseid IN INTEGER, is_nal IN CHAR DEFAULT 'Y',
    type_id IN NUMBER DEFAULT 1, prc_prec IN NUMBER DEFAULT 2,
    opts IN VARCHAR2 DEFAULT NULL, fnumber IN INTEGER DEFAULT NULL,
    fdate IN DATE DEFAULT NULL)
  IS
    CURSOR goods_info_cur(goodsid IN INTEGER) IS
      SELECT a.ORD, (100 * (a.PRICE0 - a.PRICE)) / a.PRICE0 DISC_PCT,
       --decode(GOODSP.get_in_group(a.GOODS_ID, 12), 'Y', NULL, 171) CNTR_ID
       to_number(NULL) CNTR_ID
      FROM ACT_LIST c, BASE_GOODS a, BASE_LIST b
      WHERE a.base_id = b.id AND a.act_id = c.id
       AND b.id = baseid AND a.goods_id = goodsid AND c.move = -1;
    p   goods_info_cur%ROWTYPE;
    g   FactGoodsRowType;
    fl  FactListRowType;
    sum_nk NUMBER;
    sum_pm NUMBER;
    diffp  NUMBER;
    nds    NUMBER;
    min_val_nk BINARY_INTEGER;
    max_val_nk BINARY_INTEGER;
  BEGIN
    def_fact_check(baseid);
    PARAM.setp(baseid);

    SELECT SUM(round(PRICE*NUM, 2)+round(0.2*PRICE*NUM, 2)), MIN(MIN_VAL_ID), MAX(MAX_VAL_ID)
      INTO sum_nk, min_val_nk, max_val_nk
      FROM UR_BASEFACT_PREVIEW;
    IF NVL(sum_nk,0) <= 0 THEN
      raise_application_error(-20500, 'Сумма по накладным=0. Провести счет-фактуру невозможно.');
    END IF;

    SELECT NVL(SUM(decode(a.val_id, SYSPC.ROUBLE, 1, SYSPC.ROUBLEBN, 1,
      RELF.get_rate(RELF.get_base_rel(baseid, SYSPC.fact_id), a.val_id, SYSPC.ROUBLE, a.DATE_IN)) * MONEY * MOVE), 0)
      INTO sum_pm
      FROM PAYMENTS a, VAL_LIST b WHERE a.base_id = baseid AND a.val_id = b.id;

    IF sum_pm = 0 AND min_val_nk = max_val_nk AND min_val_nk IN (SYSPC.ROUBLE, SYSPC.ROUBLEBN) THEN
      sum_pm := sum_nk;
    END IF;

    IF ABS(sum_nk) > 0 THEN
      diffp := ROUND(ABS(100 * (sum_nk - sum_pm) / sum_nk), 2);
    ELSE
      diffp := 100;
    END IF;
    IF NVL(diffp, 100) > NVL(SYSPC.FACTDIFF_MAX, 0) AND CU.own_role_enabled('DIRECTOR') != 'Y' THEN
      raise_application_error(-20500, 'Разница сумм между накладными и оплатами превышает '
       || to_char(SYSPC.FACTDIFF_MAX) || '% [' || to_char(diffp) || '%]. Провести счет-фактуру невозможно.');
    END IF;

    def_fact_attribs(baseid, type_id, fnumber, fdate, fl);
    fl.RATE := 1;
    nds := get_cust_nds(BASEF.get_cust_id(baseid)); --SYSPC.NDS;
    INSERT INTO FACT_NUMS(FACT_ID, BIMPORT_ID, FNUM, YR, ORG_ID, PREFIX) 
      VALUES (fl.ID, BASEF.get_import_id(baseid), fl.FNUMBER, to_char(fl.FDATE, 'yyyy'), CU.org_id, ORGF.fact_prefix(CU.org_id));
    --
    INSERT INTO FACT_LIST (ID, ACT_ID, MOVE, BASE_ID, TYPE_ID, FDATE, FNUMBER, 
      PAYM_NP, RATE, EXP_DATE, NOTE2, VAL_ID, NDS_PCT, PREFIX)
    VALUES (fl.ID, fl.ACT_ID, fl.MOVE, fl.BASE_ID, fl.TYPE_ID, fl.FDATE, 
      fl.FNUMBER, fl.PAYM_NP, fl.RATE, fl.EXP_DATE, fl.NOTE2, fl.VAL_ID, nds,
      fl.PREFIX);
    g.FACT_ID := fl.ID;
    FOR c IN (SELECT goods_id, num, price, defect_pos FROM UR_BASEFACT_PREVIEW) LOOP
      OPEN goods_info_cur(c.goods_id);
      FETCH goods_info_cur INTO p;
      g.DISC_PCT := NULL;
      IF goods_info_cur%FOUND THEN
        g.ORD := p.ORD;
        g.CNTR_ID := p.CNTR_ID;
        IF p.DISC_PCT > 0 AND p.DISC_PCT <= 100 THEN
          g.DISC_PCT := p.DISC_PCT;
        END IF;
      ELSE
        g.ORD := 0;
        g.CNTR_ID := NULL;
      END IF;
      CLOSE goods_info_cur;
      g.GOODS_ID := c.goods_id;
      g.NUM_ := c.num;
      g.PRICE  := ROUND(c.price, 2);
      g.SUM_NDS := ROUND(g.NUM_ * g.PRICE * nds / 100, 2);
      g.SUM_NP  := 0;
		  IF is_gtd_enabled = 'Y' THEN
        g.CODE_GTD := get_gtd(g.GOODS_ID, g.NUM_, fl.FDATE, g.CNTR_ID);
        IF (g.CODE_GTD IS NULL) THEN
          g.CODE_GTD := get_rand_gtd(g.GOODS_ID, fl.FDATE, g.CNTR_ID);
        END IF;
      ELSE
        g.CODE_GTD := NULL;
      END IF;
      INSERT INTO FACT_GOODS (FACT_ID, GOODS_ID, NUM_, PRICE, SUM_NDS, SUM_NP, ORD, DISC_PCT, CNTR_ID, CODE_GTD, DEFECT_POS)
        VALUES (g.FACT_ID, g.GOODS_ID, g.NUM_, g.PRICE, g.SUM_NDS, g.SUM_NP, g.ORD, g.DISC_PCT, g.CNTR_ID, g.CODE_GTD, c.DEFECT_POS);
    END LOOP;

    reord_fact(g.FACT_ID);
    IF (is_nal = 'Y') AND (type_id < 10) THEN
      def_fact_gen_payments(g.FACT_ID, sum_nk);
    END IF;
    subtract_fact(g.FACT_ID, 2, opts);
    COMMIT;
    EXCEPTION
    WHEN DUP_VAL_ON_INDEX
      THEN raise_application_error(-20500, 'Такой номер счет-фактуры уже существует.');
      ROLLBACK;
  END;
--
  PROCEDURE make_fact_euro_2003(baseid IN INTEGER, is_nal IN CHAR DEFAULT 'Y',
    type_id IN NUMBER DEFAULT 1, prc_prec IN NUMBER DEFAULT 2,
    opts IN VARCHAR2 DEFAULT NULL, filt_goods_valid IN INTEGER DEFAULT NULL,
    fnumber IN INTEGER DEFAULT NULL, fdate IN DATE DEFAULT NULL)
  IS
    CURSOR goods_info_cur(goodsid IN INTEGER) IS
      SELECT a.ORD, (100 * (a.PRICE0 - a.PRICE)) / a.PRICE0 DISC_PCT,
       --decode(GOODSP.get_in_group(a.GOODS_ID, 12), 'Y', NULL, 171) CNTR_ID
       to_number(NULL) CNTR_ID
      FROM ACT_LIST c, BASE_GOODS a, BASE_LIST b
      WHERE a.base_id = b.id AND a.act_id = c.id
       AND b.id = baseid AND a.goods_id = goodsid AND c.move = -1;
    p   goods_info_cur%ROWTYPE;
    g   FactGoodsRowType;
    fl  FactListRowType;
    nds        NUMBER;
    relid      INTEGER;
    valid      INTEGER;
    wblastdate DATE;
    k_price    NUMBER := 1;
    sum_e      NUMBER;
    sum_e2     NUMBER;
  BEGIN
    def_fact_check(baseid);
    PARAM.setp(baseid);
    nds := get_cust_nds(BASEF.get_cust_id(baseid)); --SYSPC.NDS;
    relid := RELF.get_base_rel(baseid, SYSPC.fact_id);
    IF type_id = 11 THEN
      valid := SYSPC.DEF_VAL;
    ELSE
      valid := SYSPC.EURO;
    END IF;
    --SELECT (1 + 0.01*NVL(nsp_pct,0)) INTO k_price FROM BASE_LIST WHERE id = baseid;

    SELECT MAX(b.DATE_), NVL(SUM(-b.MOVE * a.NUM_ * a.PRICE *
      RELF.get_rate(relid, a.VAL_ID, valid, b.DATE_)), 0)
    INTO wblastdate, sum_e
    FROM WAYBILL_GOODS a, WAYBILL_LIST b
    WHERE a.wayb_id = b.id AND b.base_id = baseid
      AND ((filt_goods_valid IS NULL) OR (a.val_id = filt_goods_valid));

    IF NVL(sum_e,0) <= 0 THEN
      --raise_application_error(-20500, 'Сумма по накладным=0. Провести счет-фактуру невозможно.');
      RETURN;
    END IF;

    def_fact_attribs(baseid, type_id, fnumber, fdate, fl);
    fl.RATE := RELF.get_rate(relid, valid, SYSPC.ROUBLEBN, wblastdate);
    IF fl.RATE IS NULL AND valid = SYSPC.EURO THEN
      fl.RATE := RELF.get_rate(SYSPC.DEF_REL, valid, SYSPC.ROUBLEBN, wblastdate);
    ELSIF fl.RATE IS NULL AND valid = SYSPC.DEF_VAL THEN
      fl.RATE := RELF.get_rate(SYSPC.DEF_REL, valid, SYSPC.ROUBLEBN, wblastdate);
    END IF;
    INSERT INTO FACT_NUMS(FACT_ID, BIMPORT_ID, FNUM, YR, ORG_ID, PREFIX) 
      VALUES (fl.ID, BASEF.get_import_id(baseid), fl.FNUMBER, to_char(fl.FDATE, 'yyyy'), CU.org_id, ORGF.fact_prefix(CU.org_id));
    --
    INSERT INTO FACT_LIST (ID, ACT_ID, MOVE, BASE_ID, TYPE_ID, FDATE, FNUMBER, 
      PAYM_NP, RATE, EXP_DATE, NOTE2, VAL_ID, NDS_PCT, PREFIX)
    VALUES (fl.ID, fl.ACT_ID, fl.MOVE, fl.BASE_ID, fl.TYPE_ID, fl.FDATE, 
      fl.FNUMBER, fl.PAYM_NP, fl.RATE, fl.EXP_DATE, fl.NOTE2, fl.VAL_ID, nds,
      fl.PREFIX);
    INSERT INTO FACT_GOODS (FACT_ID, GOODS_ID, NUM_, PRICE, SUM_NDS, SUM_NP, DEFECT_POS)
    (SELECT fl.ID, a.GOODS_ID, SUM(-b.MOVE * a.NUM_) NUM_,
       NVL(SUM(-b.MOVE * a.NUM_ * a.PRICE *
         RELF.get_rate(relid, a.VAL_ID, valid, b.DATE_)), 0), 0, 0,
       CASE max(c.DEFECT_POS) WHEN 'Y' THEN 'Y' END
     FROM WAYBILL_GOODS a, WAYBILL_LIST b, ACT_LIST c
     WHERE a.wayb_id = b.id AND b.base_id = baseid AND b.act_id = c.id  -- AND b.move = -1
       AND ((filt_goods_valid IS NULL) OR (a.val_id = filt_goods_valid))
     GROUP BY a.GOODS_ID
     HAVING SUM(-b.MOVE * a.NUM_) > 0);

    UPDATE FACT_GOODS SET price = k_price * price / num_ WHERE fact_id = fl.ID;

    FOR p IN (
      SELECT g.GOODS_ID, NVL(g.NUM_, 0) - NVL(d.NUMM, 0) NEW_NUM
      FROM FACT_GOODS g,
       (SELECT a.GOODS_ID, SUM(a.NUM_) NUMM
        FROM FACT_GOODS a, FACT_LIST b
        WHERE a.fact_id = b.id AND b.id != fl.ID
          AND b.base_id = baseid AND b.ACT_ID = fl.ACT_ID
        GROUP BY a.GOODS_ID) d
      WHERE g.GOODS_ID = d.GOODS_ID AND g.FACT_ID = fl.ID)
    LOOP
      IF p.NEW_NUM > 0 THEN
        UPDATE FACT_GOODS SET num_ = p.NEW_NUM
          WHERE fact_id = fl.ID AND goods_id = p.GOODS_ID;
      ELSE
        DELETE FACT_GOODS WHERE fact_id = fl.ID AND goods_id = p.GOODS_ID;
      END IF;
    END LOOP;
    SELECT NVL(SUM(price * num_),0) INTO sum_e
      FROM FACT_GOODS WHERE fact_id = fl.ID;
    g.FACT_ID := fl.ID;
    FOR c IN (SELECT * FROM FACT_GOODS WHERE fact_id = fl.ID) LOOP
      OPEN goods_info_cur(c.goods_id);
      FETCH goods_info_cur INTO p;
      g.DISC_PCT := NULL;
      IF goods_info_cur%FOUND THEN
        g.ORD := p.ORD;
        g.CNTR_ID := p.CNTR_ID;
        IF p.DISC_PCT > 0 AND p.DISC_PCT <= 100 THEN
          g.DISC_PCT := p.DISC_PCT;
        END IF;
      ELSE
        g.ORD := 0;
        g.CNTR_ID := NULL;
      END IF;
      CLOSE goods_info_cur;
      g.PRICE  := ROUND(c.PRICE * 100 / (100 + nds), prc_prec);
      g.SUM_NDS := ROUND(c.NUM_ * g.PRICE * nds / 100, prc_prec);
		  IF is_gtd_enabled = 'Y' THEN
        g.CODE_GTD := get_gtd(g.GOODS_ID, g.NUM_, fl.FDATE, g.CNTR_ID);
        IF (g.CODE_GTD IS NULL) THEN
          g.CODE_GTD := get_rand_gtd(g.GOODS_ID, fl.FDATE, g.CNTR_ID);
        END IF;
      ELSE
        g.CODE_GTD := NULL;
      END IF;
      UPDATE FACT_GOODS SET
        price = g.PRICE,
        sum_nds = g.SUM_NDS,
        ord = g.ORD,
        disc_pct = g.DISC_PCT,
        cntr_id = g.CNTR_ID,
        code_gtd = g.CODE_GTD
      WHERE fact_id = fl.ID AND goods_id = c.GOODS_ID;
    END LOOP;
    SELECT NVL(SUM(price * num_ + sum_nds),0) INTO sum_e2
      FROM FACT_GOODS WHERE fact_id = fl.ID;
    IF NVL(INSTR(opts, 'A'), 0) = 0 AND ABS(sum_e - sum_e2) > 0.00009 THEN
      adjust_fact2(fl.ID, sum_e - sum_e2, 0, prc_prec);
    END IF;
    reord_fact(g.FACT_ID);
    COMMIT;
  EXCEPTION
    WHEN DUP_VAL_ON_INDEX
      THEN raise_application_error(-20500, 'Такой номер счет-фактуры уже существует.');
      ROLLBACK;
  END;
--
  PROCEDURE make_agg_fact_2008(baseid IN INTEGER, is_nal IN CHAR DEFAULT 'Y',
    type_id IN NUMBER DEFAULT 1, prc_prec IN NUMBER DEFAULT 2,
    opts IN VARCHAR2 DEFAULT NULL, filt_goods_valid IN INTEGER DEFAULT NULL,
    fnumber IN INTEGER DEFAULT NULL, fdate IN DATE DEFAULT NULL)
  IS
    TYPE FactGoodsRecType IS RECORD (
      GOODS_ID NUMBER(6),
      NUM_     NUMBER,
      SUMMD    NUMBER,
      ORD      NUMBER(6),
      DEFECT_POS VARCHAR2(1));
    TYPE FactGoodsCurType IS REF CURSOR RETURN FactGoodsRecType;
    goodscur FactGoodsCurType;
    c  FactGoodsRecType;
    g  FactGoodsRowType;
    fl FactListRowType;
    notes2     FACT_LIST.NOTE2%TYPE;
    flag       CHAR(1);
    is_chp     CHAR(1);
    prc_round  INTEGER;
    factid     INTEGER;
    factno     INTEGER;
    val_id     INTEGER;
    sumt_d     NUMBER;
    sumt_x     NUMBER;
    sump_d     NUMBER;
    sump_r     NUMBER;
    sumpn_r    NUMBER;
    sumnp_r    NUMBER;
    sumost_r   NUMBER;
    sumostnp_r NUMBER;
    sumorder_r NUMBER;
    rate_r     NUMBER;
    max_nalr   NUMBER;
    nds        NUMBER;
    diffp      NUMBER;
    prc        NUMBER;
    delt       NUMBER;
    wblastdate DATE;
    is_partial BOOLEAN;
    relid      NUMBER;
    factdate   DATE;
    min_val_nk BINARY_INTEGER;
    max_val_nk BINARY_INTEGER;
    errm     VARCHAR2(4001);
  BEGIN
    errm := NVL(BASEF.get_is_allowed(baseid, 'DOCS'), 'N');
    IF errm != 'Y' THEN
      raise_application_error(-20500, 'Состояние основания №' || to_char(baseid) ||
        ' не допускает проведения счетов-фактур. (' || substr(errm, 3) || ')');
    END IF;
    is_partial := NVL(INSTR(opts, 'P'), 0) > 0;
    relid := BASEF.get_rel_id(baseid);
    -- Проверка клиента
    SELECT is_firm, decode(rtrim(REGNO), NULL, 'N', 'Y')
      INTO flag, is_chp FROM OBJ_LIST WHERE id = BASEF.get_cust_id(baseid);
    IF flag != 'Y' THEN
      raise_application_error(-20500, OBJF.get_name(BASEF.get_cust_id(baseid)) ||
       ' не является юридическим лицом. Провести счет-фактуру невозможно.');
    END IF;
    -- Выделить сумму отпущенного товара (для частичн.сч-ф исходя из принятых денег - предоплата)
    IF is_partial THEN
      SELECT NVL(SUM(p.MOVE * p.MONEY * p.RATE), 0), MAX(p.DATE_IN)
        INTO sumt_d, wblastdate
        FROM PAYMENTS p, BASE_LIST bl
       WHERE trunc(p.DATE_IN) = trunc(fdate) AND p.IS_MARKED = 'Y'
         AND p.BASE_ID = bl.ID AND bl.ORG_ID = CU.org_id;
    ELSE
      SELECT NVL(SUM(-wl.MOVE * wg.NUM_ * wg.PRICE * wg.RATE), 0),
             NVL(SUM(-wl.MOVE * wg.NUM_ * wg.PRICE), 0),
             MAX(wl.DATE_), MIN(wg.VAL_ID), MAX(wg.VAL_ID)
        INTO sumt_d, sumt_x, wblastdate, min_val_nk, max_val_nk
        FROM WAYBILL_GOODS wg, WAYBILL_LIST wl, BASE_LIST bl
        WHERE wg.WAYB_ID = wl.ID AND wl.BASE_ID = bl.ID
          AND bl.ORG_ID = CU.org_id AND trunc(wl.DATE_) = trunc(fdate)
          AND ((filt_goods_valid IS NULL) OR (wg.VAL_ID != filt_goods_valid))
          AND EXISTS (SELECT 1 FROM PAYMENTS p
            WHERE bl.ID = p.BASE_ID AND trunc(p.DATE_IN) = trunc(fdate) AND p.IS_MARKED = 'Y')
          --AND NOT EXISTS (SELECT 1 FROM ACT_LIST al WHERE al.ID = wl.ACT_ID AND al.IS_RETURN = 'Y');
          AND wl.ACT_ID != 20;
    END IF;
    -- Выделить сумму принятых денег
    IF type_id >= 10 AND type_id < 30 THEN
      SELECT ROUND(sumt_d * (1 + 0.01*NVL(conv_pct,0)) * (1 + 0.01*NVL(nsp_pct,0)), 2)
        INTO sumt_d FROM BASE_LIST WHERE id = baseid;
      rate_r := RELF.get_rate(SYSPC.DEF_REL, SYSPC.DEF_VAL, SYSPC.ROUBLEBN, wblastdate);
      sump_r := sumt_d;
      sump_d := sumt_d;
      prc_round := prc_prec;
      IF is_nal = 'Y' and is_chp = 'Y' THEN sumpn_r := sump_r; ELSE sumpn_r := 0; END IF;
      IF (NVL(sumt_d,0) = 0) AND (NVL(INSTR(opts, 'M'), 0) > 0) THEN
        RETURN;
      END IF;
      IF NVL(sumt_d,0) = 0 THEN
        raise_application_error(-20500, 'Сумма по накладным=0. Провести счет-фактуру невозможно.');
      END IF;
      sumorder_r := 0;
    ELSE
      rate_r := 1;
      SELECT
         NVL(SUM(decode(p.val_id, SYSPC.ROUBLE, 1, SYSPC.ROUBLEBN, 1,
          RELF.get_rate(RELF.get_base_rel(baseid, SYSPC.fact_id), p.val_id, SYSPC.ROUBLE, p.DATE_IN))
           * MONEY * MOVE), 0) SUMMR,
         NVL(SUM(decode(v.is_cash, 'Y', 1, 0) * decode(p.val_id, SYSPC.ROUBLE, 1,
          RELF.get_rate(RELF.get_base_rel(baseid, SYSPC.fact_id), p.val_id, SYSPC.ROUBLE, p.DATE_IN))
           * MONEY * MOVE), 0) SUMMNPR,
         NVL(SUM(MONEY * MOVE * RATE), 0) SUMMD
        INTO sump_r, sumorder_r, sump_d
        FROM PAYMENTS p, BASE_LIST bl, VAL_LIST v
       WHERE p.BASE_ID = bl.ID AND p.val_id = v.id AND bl.ORG_ID = CU.org_id
         AND trunc(p.DATE_IN) = trunc(fdate) AND p.IS_MARKED = 'Y';
      --
      IF (NOT is_partial) AND sump_r = 0 AND min_val_nk = max_val_nk AND min_val_nk IN (SYSPC.ROUBLE, SYSPC.ROUBLEBN) THEN
        sump_r := sumt_x;
        sumorder_r := sumt_x;
        sump_d := sumt_d;
      END IF;
      --
      IF is_chp = 'Y' THEN sumpn_r := sumorder_r; ELSE sumpn_r := 0; END IF;
      -- Проверка соответствия платежей отпущенному товару
      IF ABS(sumt_d) > 0 THEN
        diffp := ROUND(ABS(100 * (sumt_d - sump_d) / sumt_d), 2);
      ELSE
        diffp := 100;
      END IF;
      IF sump_d < sumt_d AND NVL(diffp, 100) > NVL(SYSPC.FACTDIFF_MAX, 0) THEN
        -- директору все можно!
        IF CU.own_role_enabled('DIRECTOR') != 'Y' THEN
          raise_application_error(-20500, 'Разница сумм между накладными и оплатами превышает '
           || to_char(SYSPC.FACTDIFF_MAX) || '% [' || to_char(diffp) ||
           '%].'||Chr(10)||'Провести счет-фактуру невозможно. Уплачено: $'||
           to_char(round(sump_d,2))||'; отпущено: $'||to_char(round(sumt_d,2)));
        END IF;
      END IF;
      prc_round := 2;
    END IF;
    -- Атрибуты фактуры: номер, налог с продаж
    def_fact_attribs(baseid, type_id, fnumber, fdate, fl);
    --nds := get_cust_nds(BASEF.get_cust_id(baseid)); --SYSPC.NDS;
    nds := SYSPC.NDS;
    factid := fl.ID;
    factno := fl.FNUMBER;
    -- Для ч.п. налог с продаж берется с нал.оплат
    IF is_chp = 'Y' THEN
      sumnp_r := ROUND(TOOLS.npt(sumpn_r), 2);
    ELSE
      sumnp_r := 0;
    END IF;
    sump_r := sump_r - sumnp_r;
    sumost_r := sump_r;
    sumostnp_r := sumnp_r;
    -- Завести фактуру
    -- max_nal  := SYSPC.FACT_MAX; -- разбивка на неск.фактур - устарело
    notes2 := fl.NOTE2;
    val_id := fl.VAL_ID;
    INSERT INTO FACT_NUMS(FACT_ID, BIMPORT_ID, FNUM, YR, ORG_ID, PREFIX) 
      VALUES (factid, BASEF.get_import_id(baseid), factno, to_char(fl.FDATE, 'yyyy'), CU.org_id, ORGF.fact_prefix(CU.org_id));
    --
    INSERT INTO FACT_LIST (ID, ACT_ID, MOVE, BASE_ID, TYPE_ID, FDATE, FNUMBER,
      PAYM_NP, RATE, EXP_DATE, NOTE2, VAL_ID, NDS_PCT, PREFIX)
    VALUES (factid, SYSPC.FACT_ID, -1, baseid, type_id, fl.FDATE, factno,
      NULL/*sumnp_r*/, rate_r, fl.EXP_DATE, notes2, val_id, nds, fl.PREFIX);
    --
    g.FACT_ID := factid;
    delt := 0;
    IF is_partial THEN
      OPEN goodscur FOR
       SELECT GOODS_ID, SUM(NUM_) as NUM_, SUM(SUMMD) as SUMMD, MAX(ORD) as ORD, MAX(DEFECT_POS) as DEFECT_POS
         FROM (
          SELECT wg.GOODS_ID, -wl.MOVE * wg.NUM_ as NUM_,
            -wl.MOVE * wg.NUM_ * wg.PRICE * wg.RATE as SUMMD, wg.ORD, al.DEFECT_POS
          FROM WAYBILL_GOODS wg, WAYBILL_LIST wl, BASE_LIST bl, ACT_LIST al
          WHERE wg.WAYB_ID = wl.ID AND wl.BASE_ID = bl.ID AND bl.ORG_ID = CU.org_id
            AND wl.MOVE = -1 AND wl.ACT_ID = al.ID AND trunc(wl.DATE_) = trunc(fdate)
            AND ((filt_goods_valid IS NULL) OR (wg.VAL_ID != filt_goods_valid))
            AND EXISTS (SELECT 1 FROM PAYMENTS p
              WHERE bl.ID = p.BASE_ID AND trunc(p.DATE_IN) = trunc(fdate) AND p.IS_MARKED = 'Y')
          UNION ALL
          SELECT fg.GOODS_ID, -fg.NUM_, NULL, NULL, fg.DEFECT_POS
            FROM FACT_GOODS fg, FACT_LIST fl
           WHERE fg.FACT_ID = fl.ID AND fl.BASE_ID = baseid
             AND fl.TYPE_ID = type_id AND trunc(fl.FDATE) = trunc(fdate)
        ) x
        GROUP BY x.GOODS_ID
        HAVING SUM(x.NUM_) > 0
        ORDER BY 2 DESC;
    ELSE
      OPEN goodscur FOR
        SELECT wg.GOODS_ID, SUM(-wl.MOVE * wg.NUM_) NUM_,
          SUM(-wl.MOVE * wg.NUM_ * wg.PRICE * wg.RATE) SUMMD, MAX(wg.ORD) ORD,
          CASE max(al.DEFECT_POS) WHEN 'Y' THEN 'Y' END as DEFECT_POS
        FROM WAYBILL_GOODS wg, WAYBILL_LIST wl, BASE_LIST bl, ACT_LIST al
        WHERE wg.WAYB_ID = wl.ID AND wl.BASE_ID = bl.ID AND bl.ORG_ID = CU.org_id
          AND wl.MOVE = -1 AND wl.ACT_ID = al.ID AND trunc(wl.DATE_) = trunc(fdate)
          AND ((filt_goods_valid IS NULL) OR (wg.VAL_ID != filt_goods_valid))
          AND EXISTS (SELECT 1 FROM PAYMENTS p
            WHERE bl.ID = p.BASE_ID AND trunc(p.DATE_IN) = trunc(fdate) AND p.IS_MARKED = 'Y')
          AND NOT EXISTS (SELECT 1 FROM ACT_LIST al WHERE al.ID = wl.ACT_ID AND al.IS_RETURN = 'Y')
        GROUP BY wg.GOODS_ID
        HAVING SUM(-wl.MOVE * wg.NUM_) > 0
        ORDER BY 2 DESC;
    END IF;
    BEGIN
      FETCH goodscur INTO c;
      WHILE goodscur%FOUND LOOP
        SELECT decode(GOODSP.get_in_group(c.goods_id, 12), 'Y', NULL, 171)
          INTO g.CNTR_ID
          FROM dual;
        g.DISC_PCT := NULL;
        g.GOODS_ID := c.goods_id;
        prc := (delt + (100 * sump_r * c.summd) / (sumt_d * (100 + nds))) / c.num_;
        IF prc <= 0 THEN
          prc := (100 * sump_r * c.summd) / (sumt_d * (100 + nds) * c.num_);
        END IF;
        IF is_partial THEN
          IF ABS(prc) > 0.000001 THEN
            g.NUM_ := ROUND(sumost_r / (prc * 0.01 * (nds + 100)));
          ELSE
            g.NUM_ := 0;
          END IF;
          IF g.NUM_ > c.num_ THEN
            g.NUM_ := c.num_;
          END IF;
        ELSE
          g.NUM_ := c.num_;
        END IF;
        g.PRICE  := ROUND(prc, prc_round);
        delt := g.NUM_ * (prc - g.PRICE);
        g.SUM_NDS := ROUND(g.NUM_ * g.PRICE * nds / 100, 2);
        g.SUM_NP  := ROUND((sumnp_r * c.summd) / sumt_d, 2);
        IF g.NUM_ > 0 THEN
          sumost_r := sumost_r - (g.NUM_ * g.PRICE + g.SUM_NDS);
          sumostnp_r := sumostnp_r - g.SUM_NP;
          IF is_gtd_enabled = 'Y' THEN
            g.CODE_GTD := get_gtd(g.GOODS_ID, g.NUM_, fl.FDATE, g.CNTR_ID);
            IF (g.CODE_GTD IS NULL) THEN
              g.CODE_GTD := get_rand_gtd(g.GOODS_ID, fl.FDATE, g.CNTR_ID);
            END IF;
          ELSE
            g.CODE_GTD := NULL;
          END IF;
          INSERT INTO FACT_GOODS (FACT_ID, GOODS_ID, NUM_, PRICE, SUM_NDS, SUM_NP, ORD, DISC_PCT, CNTR_ID, CODE_GTD, DEFECT_POS)
            VALUES (g.FACT_ID, g.GOODS_ID, g.NUM_, g.PRICE, g.SUM_NDS, g.SUM_NP, g.ORD, g.DISC_PCT, g.CNTR_ID, g.CODE_GTD, c.DEFECT_POS);
        END IF;
        FETCH goodscur INTO c;
        -- EXIT WHEN g.NUM_ <= 0;
      END LOOP;
    EXCEPTION WHEN OTHERS THEN
      CLOSE goodscur;
      raise;
    END;
    CLOSE goodscur;
--    raise_application_error(-20500, 'delt=' || to_char(sumost_r));
    IF NVL(INSTR(opts, 'A'), 0) = 0 AND (ABS(sumost_r) > 0.00009 OR ABS(sumostnp_r) > 0.00009) THEN
      adjust_fact(factid, sumost_r, sumostnp_r, prc_round);
    END IF;
    reord_fact(factid);
    COMMIT;
  EXCEPTION
    WHEN DUP_VAL_ON_INDEX
      THEN raise_application_error(-20500, 'Такой номер счет-фактуры уже существует.');
      ROLLBACK;
  END;
--
  PROCEDURE make_agg_fact_2018ekv(baseid IN INTEGER, is_nal IN CHAR DEFAULT 'Y',
    type_id IN NUMBER DEFAULT 1, prc_prec IN NUMBER DEFAULT 2,
    opts IN VARCHAR2 DEFAULT NULL, filt_goods_valid IN INTEGER DEFAULT NULL,
    fnumber IN INTEGER DEFAULT NULL, fdate IN DATE DEFAULT NULL)
  IS
  CURSOR info(wdt IN DATE) IS
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
       AND TRUNC(wl.date_) = TRUNC(wdt)
       AND wl.act_id = al.ID
       AND bl.btype_id = 40
       AND bg.act_id != 431
       AND al.IS_RETURN = 'N'
       AND bl.ORG_ID = CU.org_id
     GROUP BY wg.price, wg.goods_id, wg.goods_id, wl.cust_id, /*wl.date_,*/ 
       wl.base_id, bl.conv_pct, (100 * (bg.price0 - bg.price)) / bg.price0, 10;
  --
  wlc info%ROWTYPE;
  nds NUMBER;
  price_nonds NUMBER;
  price_nds NUMBER;
  summ_nds NUMBER;
  summ_wnds NUMBER;
  summ_np NUMBER := 0;
  rate_r     NUMBER := 1;
  ord INTEGER;
  disc_pct NUMBER;
  wblastdate DATE;
  tnved_id INTEGER;
  fl FactListRowType;
  is_exprt BOOLEAN;
  gtd_ VARCHAR2(40);
  cntrid_ INTEGER;
  BEGIN
    def_fact_attribs(baseid, type_id, fnumber, fdate, fl);
    is_exprt := NVL(INSTR(opts, 'E'), 0) > 0;
    nds := get_cust_nds(BASEF.get_cust_id(baseid));
    --
    INSERT INTO FACT_NUMS(FACT_ID, BIMPORT_ID, FNUM, YR, ORG_ID, PREFIX) 
      VALUES (fl.ID, BASEF.get_import_id(baseid), fl.FNUMBER, to_char(fl.FDATE, 'yyyy'), CU.org_id, ORGF.fact_prefix(CU.org_id));
    --
    INSERT INTO FACT_LIST (ID, ACT_ID, MOVE, BASE_ID, TYPE_ID, FDATE, FNUMBER,
      PAYM_NP, RATE, EXP_DATE, NOTE2, VAL_ID, NDS_PCT, ID_CONTRACT, FIDX, PREFIX)
    VALUES (fl.ID, SYSPC.FACT_ID, -1, baseid, type_id, fl.FDATE, fl.fnumber,
      NULL, rate_r, fl.EXP_DATE, fl.note2, fl.val_id, nds, null, fl.fnumber, fl.PREFIX);
    --
    OPEN info(fdate);
    ord := 1;
    LOOP
      FETCH info INTO wlc;
      EXIT WHEN info%NOTFOUND;
      -- При откате заменить prc_prec на 2
      price_nonds := round(round(round((1 + 0.01 * NVL(wlc.conv_pct,0)) * wlc.price, prc_prec) / 
        (1 + 0.01 * nds), 3), prc_prec);
      price_nds := round(round(wlc.price / (1 + 0.01 * nds), prc_prec) * (1 + 0.01 * nds), prc_prec);
      price_nonds := round(wlc.price / (1 + 0.01 * nds), prc_prec);
      summ_wnds := round(price_nds * wlc.num_, prc_prec);
      summ_nds := round(summ_wnds - (price_nonds * wlc.num_), prc_prec);
      IF (wlc.disc_pct >= 0) THEN
        disc_pct := wlc.disc_pct;
      ELSE
        disc_pct := 0;
      END IF;
      IF (is_exprt) THEN
        tnved_id := GOODSP.FIRST_TNVED(wlc.GOODS_ID);
      END IF;
      IF is_gtd_enabled = 'Y' THEN
        gtd_ := get_gtd(wlc.GOODS_ID, wlc.NUM_, SYSDATE, cntrid_);
        IF (gtd_ IS NULL) THEN
          gtd_ := get_rand_gtd(wlc.GOODS_ID, SYSDATE, cntrid_);
        END IF;
      END IF;
      INSERT INTO FACT_GOODS (FACT_ID, GOODS_ID, NUM_, PRICE, SUM_NDS, SUM_NP, 
        ORD, DISC_PCT, CNTR_ID, CODE_GTD, DEFECT_POS, TNVED_ID)
      VALUES (fl.ID, wlc.GOODS_ID, wlc.NUM_, price_nonds, summ_nds, summ_np, 
        ord, disc_pct, cntrid_, gtd_, wlc.DEFECT_POS, tnved_id);
      ord := ord + 1;
      wblastdate := wlc.wbdate;
    END LOOP;
    CLOSE info;
    reord_fact(fl.ID);
    COMMIT;
  EXCEPTION
    WHEN DUP_VAL_ON_INDEX
      THEN raise_application_error(-20500, 'Такой номер счет-фактуры уже существует.');
      ROLLBACK;
  END;
--
  PROCEDURE make_fact(baseid IN INTEGER, is_nal IN CHAR DEFAULT 'Y',
    type_id IN NUMBER DEFAULT 1, prc_prec IN NUMBER DEFAULT 2,
    opts IN VARCHAR2 DEFAULT NULL, fnumber IN INTEGER DEFAULT NULL,
    fdate IN DATE DEFAULT NULL, waybids IN VARCHAR DEFAULT '', 
    contract IN INTEGER DEFAULT NULL)
  IS
  BEGIN
    IF type_id = 1 THEN
      make_fact_2000(baseid, is_nal);
    ELSIF type_id = 30 THEN
      make_agg_fact_2018ekv(baseid, is_nal, type_id, prc_prec, opts, NULL, fnumber, fdate);
    ELSIF type_id IN (2, 10) THEN
      make_fact_2001(baseid, is_nal, type_id, prc_prec, opts);
    ELSIF type_id IN (3, 6, 7, 8, 9, 11, 12, 13, 14) THEN
      IF (type_id IN (11, 12)) and (NVL(INSTR(opts, 'M'), 0) > 0) THEN
        --make_fact_2002(baseid, is_nal, 11, prc_prec, opts, SYSPC.EURO);
        make_fact_euro_2003(baseid, is_nal, 11, prc_prec, opts, SYSPC.DEF_VAL, fnumber, fdate);
        make_fact_euro_2003(baseid, is_nal, 12, prc_prec, opts, SYSPC.EURO, fnumber, fdate);
      ELSE
        IF (type_id = 7 or type_id = 9 or type_id = 14) THEN
          make_fact_2016(baseid, is_nal, type_id, prc_prec, opts, NULL, fnumber, 
            waybids, fdate, contract);
        ELSIF (type_id = 8) THEN
          make_fact_2017k(baseid, is_nal, type_id, prc_prec, opts, NULL, fnumber, 
            waybids, fdate, contract);
        ELSE
          make_fact_2002(baseid, is_nal, type_id, prc_prec, opts, NULL, fnumber, 
            fdate, contract);
        END IF;
      END IF;
    ELSIF type_id = 4 THEN
      make_fact_2003(baseid, is_nal, type_id, prc_prec, opts, fnumber, fdate);
    ELSIF type_id IN (5, 31) THEN
      make_agg_fact_2008(baseid, is_nal, type_id, prc_prec, opts, NULL, fnumber, fdate);
    END IF;
  END;
--
  PROCEDURE recalc_fact(factid IN INTEGER, sum_total IN NUMBER, sum_np IN NUMBER,
    type_id IN NUMBER DEFAULT 1, prc_prec IN NUMBER DEFAULT 2, opts IN VARCHAR2 DEFAULT NULL)
  IS
    CURSOR factcorr_cur IS
      SELECT FACT_ID, GOODS_ID, NUM_, PRICE, SUM_NDS, SUM_NP
      FROM FACT_GOODS WHERE FACT_ID = factid ORDER BY NUM_ desc
      FOR UPDATE;
    c factcorr_cur%ROWTYPE;
    nds       NUMBER;
    sum_prop  NUMBER;
    sum_inds  NUMBER;
    ost_sum   NUMBER;
    ost_np    NUMBER;
    baseid    BINARY_INTEGER;
    prc_round BINARY_INTEGER := prc_prec;
    last_goods_id  BINARY_INTEGER;
    errm     VARCHAR2(4001);
  BEGIN
    baseid := FACTF.get_base_id(factid);
    errm := NVL(BASEF.get_is_allowed(baseid, 'DOCS'), 'N');
    IF errm != 'Y' THEN
      raise_application_error(-20500, 'Состояние основания №' || to_char(baseid) ||
        ' не допускает проведения счетов-фактур. (' || substr(errm, 3) || ')');
    END IF;
    -- Выделить сумму отпущенного товара для расчета пропорций
    nds := get_cust_nds(BASEF.get_cust_id(baseid)); --SYSPC.NDS;
    sum_inds := (sum_total - sum_np) / (1 + 0.01 * nds);
    SELECT SUM(NUM_ * PRICE) INTO sum_prop FROM FACT_GOODS WHERE fact_id = factid;
    ost_sum := sum_total - sum_np;
    ost_np  := sum_np;
    FOR c IN factcorr_cur LOOP
      last_goods_id := c.GOODS_ID;
      c.SUM_NP  := ROUND((c.PRICE * c.NUM_ * sum_np) / sum_prop, 2);
      c.PRICE  := ROUND((c.PRICE * sum_inds) / sum_prop, prc_round);
      c.SUM_NDS := ROUND(c.NUM_ * c.PRICE * nds / 100, 2);
      ost_sum := ost_sum - (c.NUM_ * c.PRICE + c.SUM_NDS);
      ost_np := ost_np - c.SUM_NP;
      UPDATE FACT_GOODS
        SET PRICE = c.PRICE, SUM_NDS = c.SUM_NDS, SUM_NP = c.SUM_NP
        WHERE CURRENT OF factcorr_cur;
    END LOOP;
    IF last_goods_id > 0 AND (ABS(ost_sum) > 0.00009 OR ABS(ost_np) > 0.00009) THEN
      UPDATE FACT_GOODS
        SET PRICE = PRICE + ((100 * ost_sum) / (NUM_ * (100 + nds))),
            SUM_NDS = SUM_NDS + (nds * ost_sum / (100 + nds)),
            SUM_NP = SUM_NP + ost_np
        WHERE fact_id = factid AND goods_id = last_goods_id;
    END IF;
    COMMIT;
  END;
--
  PROCEDURE make_ret_doc(baseid IN INTEGER, actid IN INTEGER, opts IN VARCHAR2 DEFAULT NULL) IS
    factid BINARY_INTEGER;
    factno BINARY_INTEGER;
    val_id BINARY_INTEGER;
    i      BINARY_INTEGER;
    nds    NUMBER;
  BEGIN
    SELECT s_factl_id.NEXTVAL INTO factid FROM dual;
    SELECT nvl(max(FNUMBER), 0) + 1 INTO factno FROM FACT_LIST
      WHERE act_id = actid AND trunc(SYSDATE, 'YEAR') = trunc(FDATE, 'YEAR');
    val_id := get_val_by_type(3);
    nds := get_cust_nds(BASEF.get_cust_id(baseid));  --SYSPC.nds
    INSERT INTO FACT_LIST (ID, ACT_ID, MOVE, BASE_ID, TYPE_ID, FDATE, FNUMBER, RATE, EXP_DATE, VAL_ID, NDS_PCT)
      VALUES (factid, actid, 1, baseid, 3, SYSDATE, factno, 1, SYSDATE + SYSPC.FACTNEW_LIFE, val_id, nds);
    i := 1;
    FOR c IN (
      SELECT a.GOODS_ID, NVL(sum(a.NUM_),0) NUM_,
        round(NVL(sum(a.NUM_*a.PRICE*NVL(b.RATE,1)) / sum(a.NUM_), 0), 4) PRICE,
        round(NVL(sum(a.SUM_NDS*NVL(b.RATE,1)),0), 4) SUM_NDS,
        round(NVL(sum(a.SUM_NP*NVL(b.RATE,1)),0), 4) SUM_NP,
        max(a.CNTR_ID) CNTR_ID
      FROM FACT_GOODS a, FACT_LIST b
      WHERE a.FACT_ID = b.ID AND b.BASE_ID = baseid AND b.ACT_ID = SYSPC.FACT_ID
      GROUP BY a.GOODS_ID
      HAVING NVL(sum(a.NUM_),0) != 0)
    LOOP
      --begin
      INSERT INTO FACT_GOODS (FACT_ID, GOODS_ID, NUM_, PRICE, ORD, SUM_NDS, SUM_NP, CNTR_ID)
        VALUES (factid, c.GOODS_ID, c.NUM_, c.PRICE, i, c.SUM_NDS, c.SUM_NP, c.CNTR_ID);
      --exception when others then
      --  raise_application_error(-20112, factid||'/'||c.GOODS_ID||'/'||c.NUM_||'/'||c.PRICE||'/'||i||'/'||c.SUM_NDS||'/'||c.SUM_NP||'/'||c.CNTR_ID);
      --end;
      i := i + 1;
    END LOOP;
    COMMIT;
  END;
--
  PROCEDURE make_doc(baseid IN INTEGER, actid IN INTEGER, opts IN VARCHAR2 DEFAULT NULL) IS
    CURSOR act_cur (act IN INTEGER) IS
      SELECT a.NAME, a.VAL_ID, a.MOVE, a.IS_VIRTUAL,
        d.IS_GOODS, d.IS_MONEY, d.IS_CASH
      FROM VAL_LIST d, ACT_LIST a, BTYPE_ACTS b, BASE_LIST c
      WHERE c.id = baseid AND c.btype_id = b.btype_id
       AND b.act_id = act AND a.id = act AND d.id = a.val_id;
    c act_cur%ROWTYPE;
    factid   INTEGER;
    val_id   INTEGER;
    f_actid  NUMBER;
    n_actid  NUMBER;
    nds      NUMBER;
    errm     VARCHAR2(4001);
    fact_flag INTEGER;
  BEGIN
    errm := NVL(BASEF.get_is_allowed(baseid, 'DOCS'), 'N');
    IF errm != 'Y' THEN
      raise_application_error(-20500, 'Состояние основания №' || to_char(baseid) ||
        ' не допускает проведения счетов-фактур. (' || substr(errm, 3) || ')');
    END IF;
    SELECT sign(count(*)) INTO fact_flag 
      FROM BTYPE_ACTS b, BASE_LIST c
     WHERE c.id = baseid AND c.btype_id = b.btype_id AND b.act_id = SYSPC.FACT_ID;
    -- Если в типе осн. предусмотрены фактуры
    IF actid = SYSPC.BUXDOC_RET and fact_flag = 1 THEN
      make_ret_doc(baseid, actid, opts);
      RETURN;
    END IF;
    --------------------------------------------------------------------------
    f_actid := actid;
    IF actid IS NULL THEN f_actid := SYSPC.FACT_ID; END IF;
    n_actid := BTCONV.get_base_act_conv_from(3, baseid, baseid, f_actid);
    IF n_actid IS NULL THEN
      raise_application_error(-20112, 'Нет х/о, по которой проводится документ!');
    END IF;
    OPEN act_cur(n_actid);
    FETCH act_cur INTO c;
    IF act_cur%NOTFOUND THEN
      CLOSE act_cur;
      raise_application_error(-20112, 'Не определены данные по хозоперации!');
    END IF;
    CLOSE act_cur;
    SELECT s_factl_id.NEXTVAL INTO factid FROM dual;
    val_id := get_val_by_type(1);
    nds := get_cust_nds(BASEF.get_cust_id(baseid));  --SYSPC.nds
    INSERT INTO FACT_LIST (ID, ACT_ID, TYPE_ID, MOVE, BASE_ID, FDATE, FNUMBER, EXP_DATE, VAL_ID, NDS_PCT)
      VALUES (factid, f_actid, 1, c.move, baseid, SYSDATE, 0, SYSDATE + SYSPC.FACTNEW_LIFE, val_id, nds);
    IF c.IS_GOODS = 'Y' THEN
      FOR c1 IN (SELECT GOODS_ID, NUM_, PRICE, ORD FROM BASE_GOODS
        WHERE BASE_ID = baseid AND ACT_ID = n_actid)
      LOOP
        INSERT INTO FACT_GOODS (FACT_ID, GOODS_ID, NUM_, PRICE, ORD, SUM_NDS, SUM_NP)
          VALUES (factid, c1.GOODS_ID, c1.NUM_, c1.PRICE, c1.ORD, 0, 0);
      END LOOP;
    ELSIF c.IS_MONEY = 'Y' THEN
      FOR c2 IN (SELECT DATE_, MONEY FROM BASE_PAYMENTS
        WHERE BASE_ID = baseid AND ACT_ID = n_actid)
      LOOP
        INSERT INTO FACT_PAYMENTS (DATE_, FACT_ID, SUMM)
          VALUES (c2.DATE_, factid, c2.MONEY);
      END LOOP;
    END IF;
    COMMIT;
  END;
--
  PROCEDURE clear(factid IN INTEGER DEFAULT NULL) IS
  BEGIN
    DELETE FACT_GOODS WHERE FACT_ID = factid;
    DELETE FACT_PAYMENTS WHERE FACT_ID = factid;
    DELETE FACT_LIST WHERE ID = factid;
    DELETE FACT_WAYB_REFS a WHERE a.FACT_ID = factid;
    DELETE FACT_NUMS a WHERE a.FACT_ID = factid;
    COMMIT;
  END;
--
  FUNCTION fsumr(factid IN INTEGER) RETURN NUMBER IS
    r1 NUMBER;
    r2 NUMBER;
  BEGIN
    SELECT NVL(SUM(num_ * price), 0) + NVL(SUM(sum_nds), 0) + NVL(SUM(sum_np), 0)
     INTO r1 FROM FACT_GOODS WHERE FACT_ID = factid;
    SELECT NVL(paym_np, 0)
     INTO r2 FROM FACT_LIST WHERE ID = factid;
    RETURN r1 + r2;
  END;
--
  FUNCTION fsum_npr(factid IN INTEGER) RETURN NUMBER IS
    r NUMBER;
  BEGIN
    SELECT SUM(sum_np) INTO r
     FROM FACT_GOODS WHERE FACT_ID = factid;
    RETURN r;
  END;
--
  FUNCTION fsum_ndsr(factid IN INTEGER) RETURN NUMBER IS
    r NUMBER;
  BEGIN
    SELECT SUM(sum_nds) INTO r
     FROM FACT_GOODS WHERE FACT_ID = factid;
    RETURN r;
  END;
--
  FUNCTION fnum(factid IN INTEGER) RETURN VARCHAR2 IS
    s VARCHAR2(500);
  BEGIN
    FOR c IN (SELECT to_char(sum(NUM_)) || ' ' || MSR STR FROM (
       SELECT a.NUM_, rtrim(rpad(GOODSP.msr_name(a.GOODS_ID), 30)) MSR
        FROM FACT_GOODS a, GOODS b
        WHERE a.GOODS_ID = b.ID AND b.IS_SERVICE != 'Y' AND a.FACT_ID = factid)
      GROUP BY MSR)
    LOOP
      IF s IS NULL THEN
        s := c.STR;
      ELSE
        s := s || ', ' || c.STR;
      END IF;
    END LOOP;
    RETURN s;
  END;
--
  FUNCTION fnum2(factid IN INTEGER) RETURN VARCHAR2 IS
    s VARCHAR2(500);
  BEGIN
    FOR c IN (SELECT to_char(sum(NUM2_)) || ' шт.' STR FROM (
       SELECT a.NUM2_
        FROM FACT_GOODS a, GOODS b
        WHERE a.GOODS_ID = b.ID AND b.IS_SERVICE != 'Y' AND a.FACT_ID = factid)
      )
    LOOP
      IF s IS NULL THEN
        s := c.STR;
      ELSE
        s := s || ', ' || c.STR;
      END IF;
    END LOOP;
    RETURN s;
  END;
--
  FUNCTION fnum_prop(factid IN INTEGER) RETURN VARCHAR2 IS
    s VARCHAR2(500);
  BEGIN
    FOR c IN (SELECT TOOLS.fget_num_str(sum(NUM_),0,1,0,0) || ' ' || MSR STR FROM (
       SELECT a.NUM_, rtrim(rpad(GOODSP.msr_name(a.GOODS_ID), 30)) MSR
        FROM FACT_GOODS a, GOODS b
        WHERE a.GOODS_ID = b.ID AND b.IS_SERVICE != 'Y' AND a.FACT_ID = factid)
      GROUP BY MSR)
    LOOP
      IF s IS NULL THEN
        s := c.STR;
      ELSE
        s := s || ', ' || c.STR;
      END IF;
    END LOOP;
    RETURN s;
  END;
--
  FUNCTION get_cust_name(factid IN INTEGER) RETURN VARCHAR2 IS
    CURSOR cur IS
      SELECT c.NAME
      FROM FACT_LIST a, BASE_LIST b, OBJ_LIST c
      WHERE a.id = factid AND a.base_id = b.id AND b.cust_id = c.id;
    r VARCHAR2(100);
  BEGIN
    OPEN cur;
    FETCH cur INTO r;
    CLOSE cur;
    RETURN r;
  END;
--
  FUNCTION get_type_id(factid IN INTEGER) RETURN INTEGER IS
    CURSOR cur IS
      SELECT type_id FROM FACT_LIST WHERE id = factid;
    r INTEGER;
  BEGIN
    OPEN cur;
    FETCH cur INTO r;
    CLOSE cur;
    RETURN r;
  END;
--
  FUNCTION get_val_id(factid IN INTEGER) RETURN INTEGER IS
  BEGIN
    RETURN get_val_by_type(get_type_id(factid));
  END;
--
  FUNCTION get_fnumber(baseid IN INTEGER) RETURN INTEGER IS
    CURSOR cur IS
      SELECT fnumber FROM FACT_LIST WHERE base_id = baseid ORDER BY 1;
    r INTEGER;
  BEGIN
    OPEN cur;
    FETCH cur INTO r;
    CLOSE cur;
    RETURN r;
  END;
--
  FUNCTION is_gtd_enabled RETURN VARCHAR2 IS
  BEGIN
    RETURN 'Y';
  END;
--
  FUNCTION get_rand_gtd(goodsid IN INTEGER, fdate_ IN DATE, cntrid OUT INTEGER) RETURN VARCHAR2
  IS
  CURSOR cur IS
    SELECT fnumber, CNTR_ID from (
      SELECT a.GOODS_ID, REGEXP_REPLACE(a.CODE_GTD,'\/\d+$','') FNUMBER, a.NUM_, a.CNTR_ID
      FROM BASE_LIST c, FACT_LIST b, FACT_GOODS a
      WHERE REGEXP_LIKE(a.CODE_GTD, '^\d{8,}\/\d{6,}\/\d{5,}(\/\d{1,})?$') 
      AND c.ORG_ID = CU.org_id
      AND a.goods_id = goodsid
      AND b.FDATE < fdate_
      AND c.id = b.base_id
      AND a.fact_id = b.id
    );
  --
  i INTEGER;
  j INTEGER;
  rc INTEGER;
  r VARCHAR(50);
  type arr is table of cur%ROWTYPE index by binary_integer;
  a arr;
  c cur%ROWTYPE;
begin
  IF (GOODSP.is_strict_goods(goodsid) = 'N') THEN
    RETURN NULL;
  END IF;
  i := 0;
  OPEN cur;
  LOOP
    FETCH cur INTO c;
    EXIT WHEN cur%NOTFOUND;
    a(i).FNUMBER := c.FNUMBER;
    a(i).CNTR_ID := c.CNTR_ID;
    i := i + 1;
  END LOOP;
  rc := cur%ROWCOUNT;
  dbms_output.put_line('rc = ' || rc);
  IF (rc > 0) THEN
    SELECT trunc(DBMS_RANDOM.VALUE(1, rc + 1)) INTO j FROM DUAL;
    IF (j = rc + 1) THEN j := trunc(rc + 0.9); END IF;
    CLOSE cur;
    cntrid := a(j - 1).CNTR_ID;
    return a(j - 1).FNUMBER;
  END IF;
  return NULL;
end;
--
  FUNCTION get_gtd(goodsid IN INTEGER, gnum IN NUMBER,
    ondate IN DATE DEFAULT NULL, cntr OUT INTEGER) RETURN VARCHAR2
  IS
    CURSOR cur IS
      SELECT fnumber, cntr_id
        FROM UF_WB_FREE_FNUMS
       WHERE goods_id = goodsid
         AND num_ >= gnum
         AND date_ <= NVL(ondate, sysdate)
         AND REGEXP_LIKE(FNUMBER, '\d{7,}\/\d{6,}\/\d{5,}(\/\d{1,})?')
       ORDER BY date_ DESC;
    CURSOR cur2 IS
      SELECT fnumber, cntr_id
        FROM UF_WB_FREE_FNUMS
       WHERE goods_id = goodsid
         AND ly_avail = 'Y'
         AND REGEXP_LIKE(FNUMBER, '\d{7,}\/\d{6,}\/\d{5,}(\/\d{1,})?')
         AND date_ <= NVL(ondate, sysdate);
    c cur%ROWTYPE;
    b BOOLEAN;
    nat BOOLEAN;
  BEGIN
    IF (GOODSP.is_strict_goods(goodsid) = 'N') THEN
      RETURN NULL;
    END IF;
    IF GOODSP.get_in_groups(goodsid, SYSP.get_char('FACT_NGTDGRP')) = 'Y' THEN
      c.fnumber := NULL;
      cntr := NULL;
    ELSE
      OPEN cur;
      FETCH cur INTO c;
      b := cur%FOUND;
      CLOSE cur;
      IF NOT b THEN
        OPEN cur2;
        FETCH cur2 INTO c;
        b := cur2%FOUND;
        CLOSE cur2;
      END IF;
      IF b THEN
        cntr := c.cntr_id;
      END IF;
      IF (c.fnumber IS NOT NULL) AND (substr(trim(c.fnumber), -2, 1) != '/') THEN
        nat := GOODSP.get_in_groups(goodsid, SYSP.get_char('FACT_NAT_GRP')) = 'Y';
        IF nat THEN
          c.fnumber := c.fnumber || '/2';
        ELSE
          -- Для товаров строгого учета запрещено какое-либо изменение номера ГТД
          IF (GOODSP.is_strict_goods(goodsid) = 'N') THEN
            c.fnumber := c.fnumber || '/1';
          END IF;
        END IF;
      END IF;
    END IF;
    RETURN c.fnumber;
  END;
--
  FUNCTION chk_fnumber_exists(new_fnumber IN INTEGER, new_fyear IN INTEGER,
    excl_factid IN INTEGER DEFAULT NULL, excl_fjourid IN INTEGER DEFAULT NULL) RETURN VARCHAR2
  IS
    fyr INTEGER;
    -- Используется функция ORGF.fact_prefix вместо a.PREFIX, потому что функция chk_fnumber_exists
    -- используется при вставке новой записи и заранее не определно какой префикс использовать при 
    -- проверке, потому что запись в таблицу еще не занесена
    CURSOR cur IS
      SELECT 'Y' FROM FACT_LIST a, BASE_LIST b
       WHERE a.BASE_ID = b.ID AND b.ORG_ID = CU.org_id AND a.ID != NVL(excl_factid,0)
         AND a.PREFIX || a.FNUMBER = ORGF.fact_prefix(CU.org_id) || new_fnumber 
         AND to_number(to_char(a.FDATE,'YYYY')) = fyr
      UNION ALL
      SELECT 'Y' FROM FACT_JOURNAL a
       WHERE a.PREFIX || a.FNUMBER = ORGF.fact_prefix(CU.org_id) || new_fnumber AND a.FYEAR = (fyr - 2000)
         AND a.ORG_ID = CU.org_id AND a.ID != NVL(excl_fjourid,0);
    r CHAR(1);
  BEGIN
    fyr := new_fyear;
    IF fyr < 1000 THEN fyr := fyr + 2000; END IF;
    OPEN cur;
    FETCH cur INTO r;
    IF cur%NOTFOUND THEN r := 'N'; END IF;
    CLOSE cur;
    RETURN r;
  END;
--
  PROCEDURE fill_journal(date_from IN DATE DEFAULT NULL, date_to IN DATE DEFAULT NULL) IS
    df DATE;
    dt DATE;
  BEGIN
    df := NVL(date_from, to_date('1.1.'||to_char(SYSDATE,'YYYY'), 'DD.MM.YYYY'));
    dt := NVL(date_to, trunc(SYSDATE-1));
    -- Update existing
    UPDATE FACT_JOURNAL fj SET (FDATE, OBJ_NAME, SUMM, SUMM_NDS, VAL_ID) =
     (SELECT trunc(fl.FDATE), ol.NAME, round(FACTF.fsumr(fl.ID), 2),
       round(FACTF.fsum_ndsr(fl.ID), 2), fl.VAL_ID
      FROM FACT_LIST fl, BASE_LIST bl, OBJ_LIST ol, UH_FACTS_FOR_JOURNAL fll
      WHERE fl.BASE_ID = bl.ID AND bl.CUST_ID = ol.ID
        AND fll.ID = fl.ID AND fj.FNUMBER = fll.FNUMBER AND fj.FYEAR = fll.FYEAR2)
    WHERE fj.ORG_ID = CU.org_id AND fj.FDATE BETWEEN df AND dt;
    -- Insert new ones
    INSERT INTO FACT_JOURNAL (FNUMBER, FDATE, OBJ_NAME, SUMM, SUMM_NDS, VAL_ID, PREFIX)
    SELECT fll.FNUMBER, trunc(fl.FDATE), ol.NAME,
      round(FACTF.fsumr(fl.ID), 2), round(FACTF.fsum_ndsr(fl.ID), 2), fl.VAL_ID, fl.PREFIX
    FROM FACT_LIST fl, BASE_LIST bl, OBJ_LIST ol, UH_FACTS_FOR_JOURNAL fll
    WHERE fl.BASE_ID = bl.ID AND bl.CUST_ID = ol.ID
      AND fll.ID = fl.ID AND fl.FDATE BETWEEN df AND dt
      AND NOT EXISTS (SELECT 1 FROM FACT_JOURNAL fj
        WHERE fj.ORG_ID = CU.org_id AND trim(fj.PREFIX) || fj.FNUMBER = trim(fj.PREFIX) || fll.FNUMBER 
          AND fj.FYEAR = fll.FYEAR2);
  END;
--
  FUNCTION fnakl_custom_footer_txt(factid IN INTEGER) RETURN VARCHAR2 IS
    r VARCHAR2(1) := 'N';
    grps VARCHAR2(80);
  BEGIN
    grps := ',' || SYSP.get_char('FACT_PRIMGRP') || ',';
    SELECT case sign(count(*)) when 1 then 'Y' else 'N' end
      INTO r
      FROM goods g, fact_goods fg,
           (SELECT id FROM goods_groups
             CONNECT BY PRIOR id = up_id
             START WITH id IN
             (select id from goods_groups where grps like '%,' || id || ',%')) a
     WHERE a.ID = g.GRP_ID and g.ID = fg.GOODS_ID and fg.FACT_ID = factid;
    RETURN r;
  END;
--
  FUNCTION get_wb_price_usd(factid IN INTEGER, goodsid IN INTEGER) RETURN NUMBER IS
    r NUMBER := NULL;
  BEGIN
    FOR c IN (
      SELECT wg.PRICE * wg.RATE as PRICE
        FROM FACT_LIST fl, WAYBILL_LIST wl, WAYBILL_GOODS wg
       WHERE fl.BASE_ID = wl.BASE_ID AND wl.ID = wg.WAYB_ID AND 
             fl.ID = factid AND wg.GOODS_ID = goodsid AND wl.MOVE = -1
       ORDER BY wl.DATE_IN
    ) LOOP
      r := c.PRICE;
    END LOOP;
    RETURN r;    
  END;
--
  FUNCTION get_wb_price_rur(factid IN INTEGER, goodsid IN INTEGER) RETURN NUMBER IS
    r NUMBER := NULL;
  BEGIN
    FOR c IN (
      SELECT wg.PRICE * RELF.get_rate(RELF.get_base_rel(fl.BASE_ID, SYSPC.fact_id), 
               wg.VAL_ID, SYSPC.ROUBLE, wl.DATE_IN) as PRICE
        FROM FACT_LIST fl, WAYBILL_LIST wl, WAYBILL_GOODS wg
       WHERE fl.BASE_ID = wl.BASE_ID AND wl.ID = wg.WAYB_ID AND 
             fl.ID = factid AND wg.GOODS_ID = goodsid AND wl.MOVE = -1
       ORDER BY wl.DATE_IN
    ) LOOP
      r := c.PRICE;
    END LOOP;
    RETURN r;    
  END;
--
  PROCEDURE div_goods_fact(baseid_ IN INTEGER, fid IN INTEGER, wbdate IN DATE DEFAULT SYSDATE) IS
    --
    CURSOR cur1(bid IN INTEGER, gid IN INTEGER, n IN NUMBER) IS
      SELECT wl.ID as wlid, wg.goods_id, wg.id as wgid
      FROM WAYBILL_LIST wl, WAYBILL_GOODS wg 
      WHERE wl.base_id = bid
      and wg.goods_id = gid
      and wg.WAYB_ID = wl.ID
      and wg.num_ = n
      order by wl.date_ desc;
    --
    CURSOR cur2(bid IN INTEGER, gid IN INTEGER, wdate IN DATE) IS
      SELECT wl.ID as wlid, wg.goods_id, wg.id as wgid
      FROM WAYBILL_LIST wl, WAYBILL_GOODS wg 
      WHERE wl.base_id = bid
      and wg.goods_id = gid
      and wg.WAYB_ID = wl.ID
      and trunc(wl.DATE_) = wdate;
    --
    CURSOR cur3(fact_id IN INTEGER, goods_id IN INTEGER) IS
      SELECT FACT_ID, GOODS_ID, NUM_, PRICE, SUM_NDS, SUM_NP, ORD, DISC_PCT, CNTR_ID, CODE_GTD, DEFECT_POS
      FROM FACT_GOODS WHERE FACT_ID = fact_id AND GOODS_ID = goods_id;
    --
    r  TResDivGoods;
    rcur1  cur1%ROWTYPE;
    rcur2  cur2%ROWTYPE;
    f      cur3%ROWTYPE;
    i BINARY_INTEGER := 0;
    j BINARY_INTEGER := 0;
    ii CHAR;
    ord_ INTEGER := 1;
    wgid INTEGER;
    price NUMBER;
    p NUMBER;
    sum_nds_ NUMBER;
    sum_np_ NUMBER;
    wgt NUMBER;
  BEGIN
    FOR g IN (SELECT fl.ID FLID, fg.fact_id, fg.goods_id, fg.num_, fg.price, fg.sum_nds, fg.sum_np,
      fg.ORD, fg.DISC_PCT, fg.CNTR_ID, fg.DEFECT_POS, fg.CODE_GTD, fg.TNVED_ID, fg.WEIGHT
      FROM FACT_LIST fl, FACT_GOODS fg
      WHERE fl.id = fg.fact_id
      AND fl.base_id = baseid_ AND fl.ID = fid ORDER BY fg.ORD) LOOP
        OPEN cur1(baseid_, g.GOODS_ID, g.num_);
        FETCH cur1 INTO rcur1;
        CLOSE cur1;
        IF (GOODSP.IS_STRICT_GOODS(g.GOODS_ID) = 'Y') THEN
          r.DELETE;
          r := div_prod_sales(g.GOODS_ID, rcur1.WLID, rcur1.WGID, baseid_);
          IF r.COUNT > 0 THEN
            OPEN cur3(g.fact_id, g.goods_id);
            FETCH cur3 INTO f;
            CLOSE cur3;
            DELETE FROM FACT_GOODS WHERE fact_id = g.fact_id AND goods_id = g.goods_id;
            FOR j IN 0..r.LAST LOOP
              sum_nds_ := r(j).num * g.sum_nds/g.num_;
              sum_np_ := r(j).num * g.sum_np/g.num_;
              wgt := round(r(j).num * g.weight/g.num_, 6);
              IF (r(j).GTD IS NULL OR r(j).GTD = '') THEN
                r(j).GTD := get_gtd(g.GOODS_ID, r(j).NUM, SYSDATE, r(j).cntrid);
                IF (r(j).GTD IS NULL) THEN
                  r(j).GTD := get_rand_gtd(g.GOODS_ID, SYSDATE, r(j).cntrid);
                END IF;
              END IF;
              INSERT INTO FACT_GOODS (FACT_ID, GOODS_ID, NUM_, PRICE, SUM_NDS, SUM_NP, ORD, DISC_PCT, CNTR_ID, CODE_GTD, DEFECT_POS, TNVED_ID, WEIGHT)
                VALUES (g.FACT_ID, g.GOODS_ID, r(j).NUM, g.PRICE, sum_nds_, sum_np_, ord_, g.DISC_PCT, r(j).cntrid, r(j).GTD, g.DEFECT_POS, g.TNVED_ID, wgt);
              ord_ := ord_ + 1;
            END LOOP;
          END IF;
        END IF;
    END LOOP;
  END;
--
  PROCEDURE div_goods_fact_prt(baseid_ IN INTEGER, fid IN INTEGER, wbdate IN DATE DEFAULT SYSDATE, prec INTEGER DEFAULT 2) 
  IS
    CURSOR cur(wb IN INTEGER, gid IN INTEGER) IS
      -- Убрал wayb_id, потому что может давать одинаковые ГТД, если расход по разным приходным
      SELECT goods_id, sum(num_) num_, sum(num2_) num2_, gtd, /*wayb_id,*/ cntr_id FROM (
        SELECT goods_id, sum(num_) num_, sum(num2_) num2_, gtd, /*wayb_id,*/ cntr_id FROM (
          -- Если вдруг есть дубликаты
          SELECT ph.goods_id, ph.num_, ph.num2_, pq.gtd, pq.cntr_id--, pq.wayb_id--CASE WHEN pq.gtd is null then 0 else pq.wayb_id end wayb_id
          FROM prt_hist ph, prt_queue pq 
          WHERE ph.wayb_id = wb
          AND ph.queue_id = pq.id
          AND ph.goods_id = gid
          ORDER BY ph.goods_id
        ) GROUP BY goods_id, gtd, /*wayb_id,*/ cntr_id
      ) GROUP BY goods_id, gtd, /*wayb_id,*/ cntr_id;
    --
    CURSOR fref(f_ IN INTEGER) IS
      SELECT fl.id, fl.nds_pct, fl.val_id, fg.*, fr.wayb_id, fg.ROWID as RID
      FROM fact_list fl, fact_goods fg, fact_wayb_refs fr
      WHERE fg.fact_id = fl.id
      AND fr.fact_id = fl.id
      AND fl.id = f_
      ORDER BY fg.ORD;
    --
    CURSOR wb(wbid IN INTEGER, gid IN INTEGER) IS
      SELECT wg.*, wl.date_in, wl.act_id 
      FROM WAYBILL_GOODS wg, WAYBILL_LIST wl 
      WHERE wg.wayb_id = wl.ID 
      AND wg.wayb_id = wbid
      AND wg.goods_id = gid;
    --
    CURSOR flupd(f_ IN INTEGER) IS
      SELECT fg.* 
      FROM FACT_GOODS fg
      WHERE fg.fact_id = f_
      FOR UPDATE;
    --
    rcur cur%ROWTYPE;
    rfr fref%ROWTYPE;
    rwb wb%ROWTYPE;
    rfl flupd%ROWTYPE;
    --
    ord_ INTEGER := 1;
    sum_nds_ NUMBER;
    sum_np_ NUMBER;
    num2 NUMBER;
    wgt NUMBER;
    prc NUMBER;
    prc1 NUMBER;
    a NUMBER;
    b NUMBER;
    c NUMBER;
  BEGIN
    OPEN fref(fid);
    LOOP
      FETCH fref INTO rfr;
      EXIT WHEN fref%NOTFOUND;
      OPEN wb(rfr.wayb_id, rfr.goods_id);
      FETCH wb INTO rwb;
      CLOSE wb;
      /*
      prc := rwb.PRICE * round(RELF.get_rate(RELF.get_base_rel(baseid_, rwb.ACT_ID), rwb.val_id, rfr.val_id, rwb.DATE_IN), prec);
      prc1 := (prc * rfr.NUM_ / (1 + 0.01 * rfr.NDS_PCT))/rfr.NUM_;
      rfr.PRICE := round((prc * rfr.NUM_ / (1 + 0.01 * rfr.NDS_PCT))/rfr.NUM_, prec);
      rfr.SUM_NDS := round(prc * rfr.NUM_ - rfr.PRICE * rfr.NUM_, 3);
      a := rwb.PRICE * rwb.NUM_;
      b := rfr.PRICE * rfr.NUM_ + rfr.SUM_NDS;
      */
      OPEN cur(rfr.wayb_id, rfr.goods_id);
      FETCH cur INTO rcur;
      IF (cur%FOUND) THEN
        DELETE FROM FACT_GOODS 
         WHERE fact_id = rfr.ID 
           AND goods_id = rfr.goods_id
           AND ROWID = rfr.RID;
      END IF;
      CLOSE cur;
      OPEN cur(rfr.wayb_id, rfr.goods_id);
      LOOP
        FETCH cur INTO rcur;
        EXIT WHEN cur%NOTFOUND;
        sum_nds_ := rcur.num_ * rfr.sum_nds/rfr.num_;
        sum_np_ := rcur.num_ * rfr.sum_np/rfr.num_;
        wgt := round(rcur.num_ * rfr.weight/rfr.num_, 6);
        num2 := round(rcur.num_ * rfr.num2_/rfr.num_, 6);
        IF (rcur.GTD IS NULL AND GOODSP.is_strict_goods(rfr.GOODS_ID) = 'Y') THEN
          FOR a IN (
            SELECT FNUMBER, CNTR_ID 
            FROM WAYBILL_LIST 
            WHERE ID = WAYBILLP.part_orig_inwayb(rfr.GOODS_ID, null, rfr.wayb_id, 'Y')) 
          LOOP
            rcur.GTD := a.FNUMBER;
            rcur.cntr_id := a.CNTR_ID;
          END LOOP;
        END IF;
        INSERT INTO FACT_GOODS (FACT_ID, GOODS_ID, NUM_, PRICE, SUM_NDS, SUM_NP, ORD, DISC_PCT, CNTR_ID, CODE_GTD, DEFECT_POS, TNVED_ID, WEIGHT, NUM2_)
        VALUES (rfr.ID, rfr.GOODS_ID, rcur.NUM_, rfr.PRICE, sum_nds_, sum_np_, ord_, rfr.DISC_PCT, rcur.cntr_id, rcur.GTD, rfr.DEFECT_POS, rfr.TNVED_ID, wgt, num2);
        COMMIT;
        ord_ := ord_ + 1;
      END LOOP;
      CLOSE cur;
    END LOOP;
    CLOSE fref;
    /*
    OPEN flupd(fid);
    LOOP
      FETCH flupd INTO rfl;
      EXIT WHEN flupd%NOTFOUND;
      a := round(rfl.PRICE * rfl.NUM_, prec) + round(rfl.SUM_NDS, 2);
      b := round(rfl.PRICE * rfl.NUM_ + rfl.SUM_NDS, 2);
      c := a - b;
      IF (c > 0) THEN
        UPDATE FACT_GOODS fg SET fg.SUM_NDS = round(fg.SUM_NDS, 2) - c 
        WHERE CURRENT OF flupd;
      END IF;
    END LOOP;
    CLOSE flupd;
    */
  END;
--
FUNCTION div_prod_sales(goodsid IN INTEGER, wayb_id IN INTEGER DEFAULT null, 
  wg_id IN INTEGER DEFAULT null, baseid_ IN INTEGER) RETURN TResDivGoods 
IS
  CURSOR p_cur(gid IN INTEGER, wlid IN INTEGER) IS
   --SELECT * FROM (
    SELECT DISTINCT gid as GOODS_ID, 'NUM_BTYPE1' AS WG_ID, 'NUM_OUT' AS NUM_,
      wg.PRICE_IN as WG_PRICE_IN, wg.VAL_ID AS WG_VAL_ID, wl.ID as WL_ID, 
      wl.DATE_ AS WL_DATE, wl.MOVE as WL_MOVE, wl.STORE_ID, wl.IMPORT_ID,
      gc.FULL_NAME as GNAME, bl.ID AS BASE_ID, sl.ORG_ID, sc.NUM_ NUM_CURRENT,
      al.NAME AS AL_NAME, wl.REF1_IID, 
      (SELECT fl.ID FROM fact_list fl WHERE fl.base_id = bl.id AND wl.base_id = bl.id AND ROWNUM = 1) FL_ID,
      (SELECT fl.ACT_ID FROM fact_list fl WHERE fl.base_id = bl.id  AND wl.base_id = bl.id AND ROWNUM = 1) FL_ACT_ID,
      NVL((SELECT fg.SUM_NDS 
         FROM fact_list fl, fact_goods fg 
        WHERE fl.base_id = bl.id 
          AND wl.base_id = bl.id 
          AND fg.FACT_ID = fl.ID 
          AND fg.GOODS_ID = gid 
          AND ROWNUM = 1
      ), 0) FG_NDS,
      NVL(
        (SELECT fg.PRICE 
           FROM fact_list fl, fact_goods fg 
          WHERE fl.base_id = bl.id 
            AND wl.base_id = bl.id AND fg.FACT_ID = fl.ID 
            AND fg.GOODS_ID = gid AND ROWNUM = 1
        ), round( wg.PRICE * RELF.get_rate(RELF.get_base_rel(wl.BASE_ID, wl.ACT_ID), wg.VAL_ID, SYSPC.ROUBLE, wl.DATE_) / (1 + 0.01*NVL(SYSPC.nds,0)), 4)
      ) WG_PRICE,
      --NVL(round( wg.PRICE * RELF.get_rate(RELF.get_base_rel(wl.BASE_ID, wl.ACT_ID), wg.VAL_ID, SYSPC.ROUBLE, wl.DATE_) / (1 + 0.01*NVL(SYSPC.nds,0)), 4), 0) as WG_PRICE,
      (SELECT p.IS_MARKED FROM payments p WHERE p.base_id = bl.id AND bl.id = wl.base_id AND p.IS_MARKED = 'Y' GROUP BY p.IS_MARKED) IS_MARKED,
      ol.NAME OL_NAME
      FROM waybill_goods wg
      LEFT JOIN waybill_list wl ON (wg.wayb_id = wl.id)
      LEFT JOIN goods_cash gc ON (gc.goods_id = gid AND gc.goods_id = wg.goods_id)
      LEFT JOIN base_list bl ON (wl.base_id = bl.id)
      LEFT JOIN STORE_LIST sl ON (wl.STORE_ID = sl.ID AND sl.ORG_ID = CU.ORG_ID)
      LEFT JOIN STORE_CURRENT sc ON (sc.STORE_ID = wl.STORE_ID AND sc.GOODS_ID = gid)
      LEFT JOIN act_list al ON (wl.ACT_ID = al.ID)
      LEFT JOIN obj_list ol ON (wl.CUST_ID = ol.ID)
      INNER JOIN (SELECT 1+0.01*/*$S.*/SYSPC.nds as K_NDS from dual) k on 1=1
      --WHERE g.goods_id = 46263
      WHERE bl.org_id = CU.org_id 
      AND wg.ID = wlid
      ORDER BY wl.date_
  -- ) WHERE IS_MARKED = 'Y' OR FL_ACT_ID IS NOT NULL AND FL_ID IS NOT NULL
  ;
  --
  --
  CURSOR r_cur(goodsid IN INTEGER, dateto IN DATE, towlid IN INTEGER) IS
    SELECT wl.ID as WL_ID, wg.ID as WG_ID, wg.PRICE WG_PRICE, wl.date_ as WL_DATE, 
           al.NAME AS AL_NAME, bl.ID AS BASE_ID, wl.REF1_IID, wl.CNTR_ID WCNTR_ID, 
           wg.PRICE_IN as WG_PRICE_IN, wg.RATE as WG_RATE, wg.NUM_, wl.ACT_ID as ACT_ID, 
           wl.MOVE, al.VAL_ID, wg.VAL_ID as WG_VAL_ID, wl.FNUMBER, wg.GOODS_ID,
           wl.IMPORT_ID, al.ID as AL_ACT_ID, al.TYPE_ID as AL_TYPE_ID, 
           al.UP_ID as AL_UP_ID, al.MOVE as AL_ACT_MOVE,
           (SELECT fl1.ACT_ID 
              FROM fact_list fl1, fact_goods fg1 
             WHERE fl1.base_id = wl.base_id 
               AND fg1.fact_id=fl1.id 
               AND fg1.goods_id = goodsid 
               AND ROWNUM = 1) AS FL_ACT_ID,
           (SELECT p.IS_MARKED 
              FROM payments p 
             WHERE p.base_id = bl.id 
               AND bl.id = wl.base_id 
               AND p.IS_MARKED = 'Y' 
             GROUP BY p.IS_MARKED) IS_MARKED,
           (SELECT fg1.CODE_GTD 
              FROM fact_list fl1, fact_goods fg1 
             WHERE fl1.base_id = wl.base_id 
               AND fg1.fact_id=fl1.id 
               AND fg1.goods_id = goodsid 
               AND ROWNUM = 1) as CODE_GTD,
           (SELECT fg1.CNTR_ID 
              FROM fact_list fl1, fact_goods fg1 
             WHERE fl1.base_id = wl.base_id 
               AND fg1.fact_id = fl1.id 
               AND fg1.goods_id = goodsid 
               AND ROWNUM=1) as FCNTR_ID,
           (SELECT count(*) 
              FROM fact_list fl 
             WHERE fl.base_id = wl.base_id) FACT_COUNT,
           ol.NAME OL_NAME, count(*) OVER() CNT,
           ROW_NUMBER() OVER(ORDER BY wl.date_) RN
      FROM waybill_list wl
      LEFT JOIN waybill_goods wg ON (wg.wayb_id = wl.id)
      LEFT JOIN act_list al ON (wl.ACT_ID = al.ID)
      LEFT JOIN base_list bl ON (wl.base_id = bl.id)
      LEFT JOIN obj_list ol ON (wl.cust_id = ol.id)
     --WHERE wl.date_ <= dateto
     WHERE wl.ID <= towlid
       AND wg.goods_id = goodsid
       AND al.ID != 141 AND al.ID != 142
       AND bl.ORG_ID = CU.ORG_ID
     ORDER BY WL_DATE ASC;
  --
  CURSOR w_cur(wgid INTEGER) IS
    SELECT wg.id WG_ID, wg.num_ WG_NUM, wl.id WL_ID, wg.price wg_price, 
           wl.import_id, wg.val_id WG_VALID, 
           wl.tax, wl.addcost, wl.sumnds, wl.tax_rate, wl.ktax, wl.date_, 
           wl.base_id as BASE_ID_, wl.act_id, wl.date_in, wl.taxkor, 
           wl.date_ as WL_DATE, bl.cust_id CUSTID
      FROM waybill_goods wg, waybill_list wl, base_list bl
     WHERE wg.wayb_id = wl.id
       AND wl.base_id = bl.id
       AND wg.id = wgid;
  --
  -- Приходная накладная при х/о возврата по ссылке из ref1_iid. При изменении
  -- также изменить w_cur
  CURSOR ref_cur(ref1_ varchar2, gdid_ INTEGER) is
    SELECT wg.id WG_ID, wg.num_ WG_NUM, wl.id WL_ID, wg.price wg_price, 
           wl.import_id, wg.val_id WG_VALID, 
           wl.tax, wl.addcost, wl.sumnds, wl.tax_rate, wl.ktax, wl.date_, 
           wl.base_id as BASE_ID_, wl.act_id, wl.date_in, wl.taxkor, 
           wl.date_ as WL_DATE, bl.cust_id CUSTID
      FROM waybill_goods wg, waybill_list wl, base_list bl
     WHERE wg.wayb_id = wl.id
       AND wl.base_id = bl.id
       AND wg.goods_id = gdid_
       AND wl.import_id = ref1_;
  --
  -- Код ГТД из фактуры приходной накладной по ссылке из ref1_iid
  CURSOR ref_fact(iid varchar2, gdid INTEGER) is
    select code_gtd, cntr_id 
      from fact_list fl, fact_goods fg
     where FL.BASE_ID = (select base_id from waybill_list where import_id = iid)
       and FG.GOODS_ID = gdid
       and FG.FACT_ID = fl.id
       and rownum = 1;
  --
  -- Код ГТД из приходной накладной по ссылке ref1_iid
  CURSOR ref_wayb(iid varchar2) is
    select fnumber
      from waybill_list
     where import_id = iid;
  --
  CURSOR scur(bid_ INTEGER, gid_ INTEGER) IS
    select wg.goods_id, sum(-wl.move * wg.num_) num_
    from waybill_goods wg, waybill_list wl
    where WG.WAYB_ID = wl.id
    and wl.base_id = bid_
    and wg.goods_id = gid_
    group by wg.goods_id;
  --
  rec p_cur%ROWTYPE;
  ras r_cur%ROWTYPE;
  pri w_cur%ROWTYPE;
  str VARCHAR(2000);
  fct ref_fact%ROWTYPE;
  wct ref_wayb%ROWTYPE;
  sr scur%ROWTYPE;  
  --
  TYPE TPrice IS RECORD (
    price NUMBER,
    --nonds INTEGER,
    quant NUMBER,
    val_id INTEGER,
    quant_cur NUMBER,
    p_nakl INTEGER,
    fnumber VARCHAR2(100 CHAR),
    pimpid VARCHAR2(40 CHAR),
    goodsid INTEGER,
    ref1_iid VARCHAR2(10 CHAR),
    cntrid INTEGER
  );
  TYPE TRec IS TABLE OF TPrice INDEX BY BINARY_INTEGER;
  prc TRec;
  pro TRec;
  res TIncWayb;
  ar TFactGoods;
  rr TResDivGoods;
  dr TResDivGoods;
  divgoods TResDivGoods;
  --  
  i BINARY_INTEGER;
  b BINARY_INTEGER;
  k BINARY_INTEGER;
  c BINARY_INTEGER;
  m BINARY_INTEGER;
  n INTEGER;
  --
  q_oct NUMBER;
  d NUMBER;
  wp NUMBER;
  cp NUMBER;
  apply BOOLEAN;
  allow BOOLEAN := true;
  by_fact BOOLEAN := true;
  --
  in_rel INTEGER := 3;
  --
  FUNCTION fnd(arr IN TRec, val IN NUMBER) RETURN BINARY_INTEGER IS  
    j BINARY_INTEGER := 0;
  BEGIN
    IF arr.COUNT IS NULL THEN
      RETURN -1;
    END IF;
    IF arr.COUNT = 0 THEN
      RETURN -1;
    END IF;
    FOR j IN 0..arr.LAST LOOP
      IF (arr(j).price IS NOT NULL AND arr(j).quant IS NOT NULL) THEN
        IF ( round(arr(j).price, in_rel) = round(val, in_rel) AND arr(j).quant > 0) THEN
          RETURN j;
        END IF;      
      END IF;
    END LOOP;
    RETURN 0;
  END;  
  --
BEGIN
  n := 0;
  rr.DELETE;
  OPEN p_cur(goodsid, wg_id);
  LOOP
    FETCH p_cur INTO rec;
    EXIT WHEN p_cur%NOTFOUND;
    i := 0;
    b := 0;
    
    prc.DELETE;
    OPEN r_cur(rec.GOODS_ID, rec.WL_DATE, rec.WL_ID);
    LOOP
      FETCH r_cur INTO ras;
      EXIT WHEN r_cur%NOTFOUND;
      allow := false;
      IF (ras.AL_ACT_MOVE = 1) THEN
        -- приход
        -- Все типы х.о. кроме внутренних товарных перемещений
        IF (ras.AL_TYPE_ID != 57 OR ras.ACT_ID = 476) THEN
        --IF (ras.FL_ACT_ID > 0 OR ras.IS_MARKED = 'Y' OR ras.FNUMBER IS NOT NULL) THEN
          IF (ras.FACT_COUNT > 0 AND by_fact) THEN
            FOR b IN (SELECT fg.NUM_, fg.CODE_GTD, fg.CNTR_ID 
                        FROM FACT_GOODS fg, FACT_LIST fl 
                       WHERE fg.FACT_ID = fl.ID 
                         AND fl.BASE_ID = ras.BASE_ID
                         AND fg.GOODS_ID = rec.GOODS_ID
                    ORDER BY ORD)
            LOOP
              k := fnd(prc, ras.WG_PRICE);
              prc(i).price := ras.WG_PRICE;
              prc(i).quant := b.NUM_;
              prc(i).quant_cur := b.NUM_;
              prc(i).val_id := ras.WG_VAL_ID;
              prc(i).p_nakl := ras.WG_ID;
              prc(i).fnumber := b.CODE_GTD;
              prc(i).cntrid := b.CNTR_ID;
              prc(i).pimpid := ras.IMPORT_ID;
              prc(i).goodsid := rec.GOODS_ID;
              prc(i).ref1_iid := ras.REF1_IID;
              i := i + 1;
            END LOOP;
          ELSE
            k := fnd(prc, ras.WG_PRICE);
            prc(i).price := ras.WG_PRICE;
            prc(i).quant := ras.NUM_;
            prc(i).quant_cur := ras.NUM_;
            prc(i).val_id := ras.WG_VAL_ID;
            prc(i).p_nakl := ras.WG_ID;
            IF (ras.CODE_GTD IS NOT NULL) THEN
              prc(i).fnumber := ras.CODE_GTD;
              prc(i).cntrid := ras.FCNTR_ID;
            ELSE
              IF (REGEXP_LIKE(ras.FNUMBER,'\d{8}\/\d{6}\/')) THEN
                prc(i).fnumber := ras.FNUMBER;
                prc(i).cntrid := ras.WCNTR_ID;
              END IF;
            END IF;
            prc(i).pimpid := ras.IMPORT_ID;
            prc(i).goodsid := rec.GOODS_ID;
            prc(i).ref1_iid := ras.REF1_IID;
            -- Если все же ГТД не найден, то скорее всего ref1_iid указывает
            -- на расходную накладную, например для ИП; и в ней нет
            -- ГТД. Тогда "раскручиваем" продажи дальше.
            if (ras.ref1_iid is not null) then
              open ref_fact(ras.ref1_iid, rec.goods_id);
              fetch ref_fact into fct;
              if (fct.code_gtd is not null) then
                prc(i).fnumber := fct.code_gtd;
                prc(i).cntrid := fct.cntr_id;
              end if;
              close ref_fact;
            end if;
            i := i + 1;
          END IF;
          --i := i + 1;
        END IF;
      ELSE
        -- расход
        q_oct := 0;
        apply := true;
        --
        --IF (ras.FL_ACT_ID > 0 OR ras.IS_MARKED = 'Y' OR ras.WL_ID = wayb_id) THEN
        IF (ras.FL_ACT_ID > 0 OR ras.IS_MARKED = 'Y') THEN
        IF (prc.LAST IS NOT NULL) THEN
          FOR c IN 0..prc.LAST LOOP
            IF (prc(c).quant > 0) THEN
              d := prc(c).quant - ras.NUM_;
              IF (d >= 0 AND apply) THEN
                res.price := prc(c).price;
                res.quant := prc(fnd(prc, prc(c).price)).quant;
                res.quant_cur := prc(fnd(prc, prc(c).price)).quant_cur;
                res.nnakl := prc(fnd(prc, prc(c).price)).p_nakl;                  
                res.val_id := prc(fnd(prc, prc(c).price)).val_id;
                res.fnumber := prc(fnd(prc, prc(c).price)).fnumber;
                res.cntrid := prc(fnd(prc, prc(c).price)).cntrid;
                res.rnakl := rec.WL_ID;
                res.rimpid := rec.IMPORT_ID;
                res.pimpid := prc(fnd(prc, prc(c).price)).pimpid;
                res.goodsid := rec.GOODS_ID;
                res.ref1_iid := prc(fnd(prc, prc(c).price)).ref1_iid;
                --IF (ras.CNT = ras.RN) THEN
                IF (ras.WL_ID = rec.WL_ID) THEN
                  ar.goods_id := rec.GOODS_ID;
                  ar.num := ras.NUM_;
                  IF (GOODSP.is_strict_goods(ar.goods_id) = 'N') THEN
                    ar.gtd := NULL;
                  ELSE
                    ar.gtd := res.fnumber;
                  END IF;
                  ar.cntrid := res.cntrid;
                  ar.inprice := res.price;
                  ar.inwayb := res.nnakl;
                  rr(b) := ar;
                  b := b + 1;
                END IF;
                prc(c).quant := d;
                --
                apply := false;
              ELSE
                IF (apply) THEN
                  q_oct := ras.NUM_ - prc(c).quant;
                  res.price := prc(c).price;
                  res.quant := prc(fnd(prc, prc(c).price)).quant;
                  --res.quant_cur := prc(fnd(prc, prc(c).price)).quant_cur;
                  res.quant_cur := prc(c).quant;
                  res.nnakl := prc(fnd(prc, prc(c).price)).p_nakl;                  
                  res.val_id := prc(fnd(prc, prc(c).price)).val_id;
                  res.fnumber := prc(fnd(prc, prc(c).price)).fnumber;
                  res.cntrid := prc(fnd(prc, prc(c).price)).cntrid;
                  res.rnakl := rec.WL_ID;
                  res.rimpid := rec.IMPORT_ID;
                  res.pimpid := prc(fnd(prc, prc(c).price)).pimpid;
                  res.goodsid := rec.GOODS_ID;
                  res.ref1_iid := prc(fnd(prc, prc(c).price)).ref1_iid;
                  ras.NUM_ := ras.NUM_ - prc(c).quant;
                  IF (ras.WL_ID = rec.WL_ID) THEN
                  --IF (ras.CNT = ras.RN) THEN
                    ar.goods_id := rec.GOODS_ID;
                    ar.num := prc(c).quant;
                    IF (GOODSP.is_strict_goods(ar.goods_id) = 'N') THEN
                      ar.gtd := NULL;
                    ELSE
                      ar.gtd := res.fnumber;
                    END IF;
                    ar.cntrid := res.cntrid;
                    ar.inprice := res.price;
                    ar.inwayb := res.nnakl;
                    rr(b) := ar;
                    b := b + 1;
                  END IF;
                  prc(c).quant := 0;                    
                  --apply := false;
                END IF;
              END IF;
            END IF;
          END LOOP;
        END IF;
        END IF;
      END IF;
    END LOOP;
    CLOSE r_cur;
    /* here */
    n := n + 1;
  END LOOP;
  CLOSE p_cur;
  b := 0;
  k := 0;
  -- Удаляем двойные ГТД
  dr.delete;
  if (rr.count > 0) then
    for i in 0..rr.last loop
      if (dr.count > 0) then
        for k in 0..dr.last loop
          if ( (dr(k).gtd = rr(i).gtd or 
               (dr(k).gtd is null and rr(i).gtd is null) ) and 
               dr(k).goods_id = rr(i).goods_id) then
            dr(k).num := dr(k).num + rr(i).num;
            rr(i).goods_id := null;
           end if;
        end loop;
        if (rr(i).goods_id is not null) then
          dr(dr.count) := rr(i);
          rr(i).goods_id := null;
        end if;
      else
        dr(dr.count) := rr(i);
      end if;
    end loop;
  end if;
  /*
  if (dr.count > 0) then
    for k in 0..dr.last loop
      OPEN scur(baseid_, dr(k).goods_id);
      fetch scur into sr;
      CLOSE scur;
      if (dr(k).num != sr.num_) then
        dr(k).num := sr.num_;
      end if;
    end loop;
  end if;
  */
  RETURN dr;
END;
--
END FACTF;

/

  GRANT EXECUTE ON "CRS"."FACTF" TO "CRS$MANAGER";
