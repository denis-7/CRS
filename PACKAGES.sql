--------------------------------------------------------
--  DDL for Package FACTF
--------------------------------------------------------

  CREATE OR REPLACE PACKAGE "CRS"."FACTF" AS
--
  TYPE TIncWayb IS RECORD (
    price NUMBER, 
    quant NUMBER, 
    nnakl VARCHAR2(10), 
    rnakl VARCHAR2(10), 
    val_id INTEGER, 
    quant_cur NUMBER,
    fnumber VARCHAR2(100 CHAR),
    rimpid VARCHAR2(40 CHAR),
    pimpid VARCHAR2(40 CHAR),
    goodsid INTEGER,
    ref1_iid VARCHAR2(10 CHAR),
    cntrid INTEGER
  );
--
  TYPE TFactGoods IS RECORD (
    goods_id INTEGER,
    num NUMBER,
    gtd VARCHAR2(100),
    inprice NUMBER,
    inwayb INTEGER,
    cntrid INTEGER
  );
--
  TYPE TResDivGoods IS TABLE OF TFactGoods INDEX BY BINARY_INTEGER;
--
  FUNCTION get_supported_types RETURN VARCHAR2;
  PRAGMA RESTRICT_REFERENCES (get_supported_types, WNPS, WNDS, RNPS);
--
  FUNCTION get_supp_type_name(typeid IN INTEGER) RETURN VARCHAR2;
  PRAGMA RESTRICT_REFERENCES (get_supp_type_name, WNPS, WNDS, RNPS);
--
  PROCEDURE make_fact(baseid IN INTEGER, is_nal IN CHAR DEFAULT 'Y',
    type_id IN NUMBER DEFAULT 1, prc_prec IN NUMBER DEFAULT 2,
    opts IN VARCHAR2 DEFAULT NULL, fnumber IN INTEGER DEFAULT NULL,
    fdate IN DATE DEFAULT NULL, waybids IN VARCHAR DEFAULT '', 
    contract IN INTEGER DEFAULT NULL);
--
  PROCEDURE recalc_fact(factid IN INTEGER, sum_total IN NUMBER, sum_np IN NUMBER,
    type_id IN NUMBER DEFAULT 1, prc_prec IN NUMBER DEFAULT 2, opts IN VARCHAR2 DEFAULT NULL);
--
  PROCEDURE make_doc(baseid IN INTEGER, actid IN INTEGER, opts IN VARCHAR2 DEFAULT NULL);
--
  PROCEDURE clear(factid IN INTEGER DEFAULT NULL);
--
  FUNCTION def_fact_check2(fnumber IN INTEGER DEFAULT NULL, 
    fdate IN DATE DEFAULT NULL, orgid IN INTEGER DEFAULT NULL) RETURN VARCHAR2;
--
  FUNCTION fsumr(factid IN INTEGER) RETURN NUMBER;
  PRAGMA RESTRICT_REFERENCES (fsumr, WNPS, WNDS, RNPS);
--
  FUNCTION fsum_npr(factid IN INTEGER) RETURN NUMBER;
  PRAGMA RESTRICT_REFERENCES (fsum_npr, WNPS, WNDS, RNPS);
--
  FUNCTION fsum_ndsr(factid IN INTEGER) RETURN NUMBER;
  PRAGMA RESTRICT_REFERENCES (fsum_ndsr, WNPS, WNDS, RNPS);
--
  FUNCTION fnum(factid IN INTEGER) RETURN VARCHAR2;
  PRAGMA RESTRICT_REFERENCES (fnum, WNPS, WNDS, RNPS);
--
  FUNCTION fnum2(factid IN INTEGER) RETURN VARCHAR2;
  PRAGMA RESTRICT_REFERENCES (fnum2, WNPS, WNDS, RNPS);
--
  FUNCTION fnum_prop(factid IN INTEGER) RETURN VARCHAR2;
  PRAGMA RESTRICT_REFERENCES (fnum_prop, WNPS, WNDS, RNPS);
--
  FUNCTION get_base_id(factid IN INTEGER) RETURN INTEGER;
  PRAGMA RESTRICT_REFERENCES (get_base_id, WNPS, WNDS, RNPS);
--
  FUNCTION get_cust_nds(custid IN INTEGER) RETURN NUMBER;
--
  FUNCTION get_cust_name(factid IN INTEGER) RETURN VARCHAR2;
  PRAGMA RESTRICT_REFERENCES (get_cust_name, WNPS, WNDS, RNPS);
--
  FUNCTION get_type_id(factid IN INTEGER) RETURN INTEGER;
  PRAGMA RESTRICT_REFERENCES (get_type_id, WNPS, WNDS, RNPS);
--
  FUNCTION get_val_id(factid IN INTEGER) RETURN INTEGER;
  PRAGMA RESTRICT_REFERENCES (get_val_id, WNPS, WNDS, RNPS);
--
  FUNCTION get_fnumber(baseid IN INTEGER) RETURN INTEGER;
  PRAGMA RESTRICT_REFERENCES (get_fnumber, WNPS, WNDS, RNPS);
--
  FUNCTION is_gtd_enabled RETURN VARCHAR2;
  PRAGMA RESTRICT_REFERENCES (is_gtd_enabled, WNPS, WNDS, RNPS);
--
  FUNCTION get_gtd(goodsid IN INTEGER, gnum IN NUMBER,
    ondate IN DATE DEFAULT NULL, cntr OUT INTEGER) RETURN VARCHAR2;
  PRAGMA RESTRICT_REFERENCES (get_gtd, WNPS, WNDS);
--
  FUNCTION get_rand_gtd(goodsid IN INTEGER, fdate_ IN DATE, cntrid OUT INTEGER) RETURN VARCHAR2;
--
  FUNCTION chk_fnumber_exists(new_fnumber IN INTEGER, new_fyear IN INTEGER,
    excl_factid IN INTEGER DEFAULT NULL, excl_fjourid IN INTEGER DEFAULT NULL) RETURN VARCHAR2;
  --PRAGMA RESTRICT_REFERENCES (chk_fnumber_exists, WNPS, WNDS);
--
  PROCEDURE fill_journal(date_from IN DATE DEFAULT NULL, date_to IN DATE DEFAULT NULL);
--
  FUNCTION fnakl_custom_footer_txt(factid IN INTEGER) RETURN VARCHAR2;
  PRAGMA RESTRICT_REFERENCES (fnakl_custom_footer_txt, WNPS, WNDS);
--
  FUNCTION get_wb_price_usd(factid IN INTEGER, goodsid IN INTEGER) RETURN NUMBER;
  PRAGMA RESTRICT_REFERENCES (get_wb_price_usd, WNPS, WNDS);
--
  FUNCTION get_wb_price_rur(factid IN INTEGER, goodsid IN INTEGER) RETURN NUMBER;
  PRAGMA RESTRICT_REFERENCES (get_wb_price_rur, WNPS, WNDS);
--
  PROCEDURE div_goods_fact(baseid_ IN INTEGER, fid IN INTEGER, 
    wbdate IN DATE DEFAULT SYSDATE);
--
  PROCEDURE div_goods_fact_prt(baseid_ IN INTEGER, fid IN INTEGER, 
    wbdate IN DATE DEFAULT SYSDATE, prec INTEGER DEFAULT 2);
--
  FUNCTION div_prod_sales(goodsid IN INTEGER, wayb_id IN INTEGER DEFAULT null, 
    wg_id IN INTEGER DEFAULT null, baseid_ IN INTEGER) RETURN TResDivGoods;
--
  FUNCTION def_fact_nexnum(type_id IN NUMBER, orgid IN INTEGER DEFAULT NULL) RETURN INTEGER;
--
  FUNCTION reserv_fact_nextnum(baseid IN INTEGER, orgid IN INTEGER DEFAULT NULL) RETURN INTEGER;
--
  FUNCTION get_fact_nextnum(reserv_id IN INTEGER) RETURN INTEGER;
--
  FUNCTION check_reserv_nextnum(cur_baseid IN INTEGER, fnum IN INTEGER, mess OUT VARCHAR2) RETURN INTEGER;
--
  PROCEDURE remove_reserv_nextnum(reservid IN INTEGER);
--
  FUNCTION get_reserved_fnum(reservid IN INTEGER) RETURN INTEGER;
--
  FUNCTION get_reserveid_by(fnum_ IN INTEGER, yr_ IN INTEGER, orgid IN INTEGER) RETURN INTEGER;
--
  FUNCTION update_reserv_nextnum(current_reserv IN INTEGER, fnum_ IN INTEGER) RETURN INTEGER;
--
END FACTF;

/

  GRANT EXECUTE ON "CRS"."FACTF" TO "CRS$MANAGER";
