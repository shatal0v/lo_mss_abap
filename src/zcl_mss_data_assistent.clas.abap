class ZCL_MSS_DATA_ASSISTENT definition
  public
  create public .

public section.

  types:
    BEGIN OF gtys_input_data,
        main_user     TYPE string,
        entrance_user TYPE string,
      END OF gtys_input_data .
  types:
    BEGIN OF ts_emp_event,
        main_user     TYPE string,
        entrance_user TYPE string,
        begda         TYPE string,
        endda         TYPE string,
        pernr         TYPE string,
        seqnr         TYPE pstring,
        att_date      TYPE string,
        att_res       TYPE string,
        exam_date     TYPE string,
        exam_res      TYPE string,
        decl_need     TYPE string,
        decl_date     TYPE string,
        decl_stat     TYPE string,
        pen_begda     TYPE string,
        pen_endda     TYPE string,
      END OF ts_emp_event .
  types:
    tt_emp_event TYPE  TABLE OF ts_emp_event .
  types:
    BEGIN OF ts_mainuserdata,
        main_user TYPE text100,
        ess       TYPE string,
        mss       TYPE string,
        fio       TYPE string,
        pernr     TYPE string,
        sign      TYPE string,
      END OF ts_mainuserdata .
  types:
    tt_mainuserdata TYPE STANDARD TABLE OF ts_mainuserdata .
  types:
    BEGIN OF ty_users
           , usrid TYPE p0105-usrid
           , ename TYPE p0001-ename
           , pernr TYPE p0001-pernr
           , END OF ty_users .
  types:
    tt_users TYPE TABLE OF ty_users .
  types:
    BEGIN OF ty_schedule
             , fname   TYPE string
             , content TYPE xstring
             , END OF ty_schedule .
  types:
    tt_schedule TYPE TABLE OF ty_schedule .
  types:
    BEGIN OF ty_entrance
               , entrance_user TYPE string
               , fio           TYPE string
               , orgeh       TYPE string
               , desc          TYPE string
               , END OF ty_entrance .
  types:
    tt_entrance TYPE TABLE OF ty_entrance .
  types:
    BEGIN OF ty_struc
               , object TYPE hrobject
               , parent TYPE hrobject
               , descr  TYPE string
               , rdate  TYPE datum
               , sdate  TYPE datum
               , stat   TYPE c1               "12.11.2019 22:03:17 Skorobagtov A.N.
               , END OF ty_struc .
  types:
    tt_struc TYPE TABLE OF ty_struc .
  types:
    BEGIN OF ts_ipr_list,
        main_user     TYPE string,
        entrance_user TYPE string,
        req_id        TYPE string,
        edu_type      TYPE string,
        edu_spec      TYPE string,
        edu_frm       TYPE string,
        edu_goal      TYPE string,
        edu_length    TYPE string,
        edu_year      TYPE string,
        edu_result    TYPE string,
        mess_type     TYPE string,
        mess_text     TYPE string,
      END OF ts_ipr_list .
  types:
    tt_ipr_list TYPE  TABLE OF ts_ipr_list .
  types:
    BEGIN OF ts_compmaindata,
        main_user     TYPE string,
        entrance_user TYPE string,
        objid         TYPE string,
        status        TYPE string,
        create        TYPE string,
        withdraw      TYPE string,
        view          TYPE string,
        bkper         TYPE string,
        kadrz         TYPE string,
        begda         TYPE string,
        ibegd         TYPE string,
        iendd         TYPE string,
        pkond         TYPE string,
        kondt         TYPE string,
        knitg         TYPE string,
      END OF ts_compmaindata .
  types:
    tt_compmaindata TYPE  TABLE OF ts_compmaindata .
  types:
    gtytd_list_ipr TYPE STANDARD TABLE OF zshr_mss_ws_list_ipr .
  types:
    tt_sort_struc TYPE SORTED TABLE OF struc WITH NON-UNIQUE KEY seqnr .        "12.11.2019 22:24:47 Skorobagtov A.N.

  constants GC_KSTATUS_CREATE type ZE_KSTATUS value '1' ##NO_TEXT.
  constants GC_KSTATUS_WITHDRAW type ZE_KSTATUS value '2' ##NO_TEXT.
  constants GC_KSTATUS_PROCESS type ZE_KSTATUS value '3' ##NO_TEXT.
  constants GC_KSTATUS_CLOSE type ZE_KSTATUS value '4' ##NO_TEXT.

  methods IS_ARGUS
    importing
      !IV_PERNR type PERNR_D
    returning
      value(RV_IS) type FLAG .
  class-methods CLASS_CONSTRUCTOR .
  class-methods GET_INN_ORGEH
    importing
      !I_PERNR type PERSNO
      !I_DATE type DATS default SY-DATUM
    returning
      value(R_INN) type STRING .
  class-methods GET_INSTANCE
    returning
      value(RV_ASSISTENT) type ref to ZCL_MSS_DATA_ASSISTENT .
  class-methods GET_SNILS
    importing
      !I_PERNR type PERSNO
      !I_DATE type DATS default SY-DATUM
    returning
      value(R_SNILS) type STRING .
  methods CHANGE_IPR_STATUS
    importing
      !IV_STATUS type ZHR_XSS_IPRREQ_STATUS
      !IV_STATUS_OLD type ZHR_XSS_IPRREQ_STATUS
    exporting
      value(EV_STATUS) type ZHR_XSS_IPRREQ_STATUS .
  methods CHECK_IS_SIT_CEN_FOR_GSS
    importing
      !IV_PERNR type PERNR_D
    returning
      value(RV_IS_SIT_CEN) type BOOLE_D .
  methods CHECK_PERNR_IS_TOP_HEAD
    importing
      !IV_PERNR type PERNR_D
      !IV_BEGDA type BEGDA
      !IV_ENDDA type ENDDA
    exporting
      !EV_PERNR_GOV type PERNR_D
      !EV_IS_GOV type BOOLE_D .
  methods CONSTRUCTOR .
  methods FILE_DELETE
    importing
      !IV_OBJECT type ZTHR_MSS_AT_ATTR-OBJECT
      !IV_FTYPE type ZTHR_MSS_AT_ATTR-FTYPE
      !IV_SEQNR type ZTHR_MSS_AT_ATTR-SEQNR
      !IV_COMMIT type FLAG default ABAP_TRUE
    returning
      value(RV_SUBRC) type SYSUBRC .
  methods FILE_DOWNLOAD
    importing
      !IV_OBJECT type ZTHR_MSS_AT_ATTR-OBJECT
      !IV_FTYPE type ZTHR_MSS_AT_ATTR-FTYPE
      !IV_SEQNR type ZTHR_MSS_AT_ATTR-SEQNR optional
      !IV_OPEN type FLAG default ABAP_TRUE
    exporting
      !ES_ATTR type ZTHR_MSS_AT_ATTR
      !ES_CONTENT type XSTRING
    returning
      value(RV_SUBRC) type SYSUBRC .
  methods FILE_SAVE
    importing
      !IV_FTYPE type ZTHR_MSS_AT_ATTR-FTYPE
      !IV_OBJECT type ZTHR_MSS_AT_ATTR-OBJECT
      !IV_NAME type ZTHR_MSS_AT_ATTR-FNAME
      !IV_DATUM type SY-DATUM default SY-DATUM
      !IV_UZEIT type SY-UZEIT default SY-UZEIT
      !IV_UNAME type SY-UNAME default SY-UNAME
      !IV_FILELENGTH type ZTHR_MSS_AT_ATTR-LENGTH
      !IV_UNSEQ type FLAG optional
      !IV_CONTENT type XSTRING
      !IV_DTYPE type ZTHR_MSS_AT_ATTR-DTYPE
    returning
      value(RV_SUBRC) type SY-SUBRC
    raising
      CX_UUID_ERROR .
  methods FILE_UPLOAD
    importing
      !IV_OBJECT type ZTHR_MSS_AT_ATTR-OBJECT
      !IV_FTYPE type ZTHR_MSS_AT_ATTR-FTYPE optional
      !IV_UNSEQ type FLAG default ABAP_TRUE
    returning
      value(RV_SUBRC) type SYSUBRC
    raising
      CX_UUID_ERROR .
  methods GET_ALL_USERS
    importing
      !IV_BEGDA type BEGDA default SY-DATUM
    exporting
      !ET_USERS type TT_USERS .
  methods GET_APPROVER_NUMBER
    importing
      !IV_PERNR type PERNR_D
      !IV_STATUS type ZHR_XSS_IPRREQ-STATUS optional
      !IV_STATUS_OLD type ZHR_XSS_IPRREQ-STATUS optional
      !IV_DATUM type SYDATUM default SY-DATUM
    exporting
      !EV_APPROVER type PERNR_D
      !EV_STATUS type ZHR_XSS_IPRREQ-STATUS
      !EV_STATUS_LOG type ZHR_XSS_IPRREQ-STATUS .
  methods GET_INPUT_FILTER
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
    exporting
      !ES_INPUT_DATA type ANY
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  methods GET_NEXT_STATUS
    importing
      !IV_STATUS type ZHR_XSS_IPRREQ-STATUS
    returning
      value(RV_STATUS) type ZHR_XSS_IPRREQ-STATUS .
  methods GET_PERNR
    importing
      !IV_USRID type P0105-USRID optional
      !IV_BEGDA type BEGDA default SY-DATUM
      !IV_SUBTY type SUBTY default '9001'
      !IV_SNILS type P0290-NOMER optional
      !IV_SUBTY2 type SUBTY default '802'
    exporting
      !EV_STAT2 type P0000-STAT2
    returning
      value(RV_PERNR) type PERNR_D .
  methods GET_WERKS_BY_PERNR
    importing
      !IV_PERNR type PERNR_D
    exporting
      !EV_WERKS type PERSA .
  methods GET_PERNR_0105
    importing
      !IV_PERNR type PERNR_D
      !IV_BEGDA type DATUM default SY-DATUM
      !IV_SUBTY type SUBTY default 'MAIL'
    returning
      value(RV_USRID) type LTEXT .
  methods GET_PERNR_BY_PLANS
    importing
      !IV_PLANS type HROBJID
      !IV_BEGDA type BEGDA default SY-DATUM
    returning
      value(RV_PERNR) type PERNR_D .
  methods GET_PERNR_FIO
    importing
      !IV_PERNR type PERNR_D
      !IV_BEGDA type BEGDA default SY-DATUM
    returning
      value(RV_FIO) type STRING .
  methods GET_PERNR_RUK
    importing
      !IV_PERNR type PERNR_D
      !IV_BEGDA type BEGDA default SY-DATUM
      !IV_LEVEL type INT1 default 2
    returning
      value(RV_PERNR) type PERNR_D .
  methods GET_PERNR_RUK0
    importing
      !IV_PERNR type PERNR_D
      !IV_BEGDA type BEGDA default SY-DATUM
    returning
      value(RV_PERNR) type PERNR_D .
  methods GET_STRUC
    importing
      !IV_OBJID type HROBJID
      !IV_OTYPE type OTYPE
      !IV_BEGDA type BEGDA default SY-DATUM
      !IV_ENDDA type ENDDA default SY-DATUM
      !IV_WEGID type WEGID
      !IV_TFLAG type FLAG optional
      !IV_VFLAG type FLAG optional
      !IV_ACHECK type FLAG default ABAP_TRUE
    exporting
      !ET_STRUC type STRUC_T
      !ET_OBJEC type OBJEC_T .
  methods GET_USER_FIO
    importing
      !IV_USRID type STRING
      !IV_BEGDA type BEGDA default SY-DATUM
    returning
      value(RV_NAME) type STRING .
  methods GET_USER_ORGEH
    importing
      !IV_USRID type STRING
      !IV_BEGDA type BEGDA default SY-DATUM
    returning
      value(RV_NAME) type STRING .
  methods GET_ZAMESTITEL_LIST
    importing
      !IV_MAIN_USER type P0105-USRID
    exporting
      !ET_PERNRS_TAB type ZCL_ZHR_READ_STRUCTURE_MPC=>TT_SUBPERNRS .
  methods WRITE_IPR_STA
    importing
      !IV_REQ_ID type ZHR_XSS_IPRREQ_REQID
      !IV_DATUM type SYDATUM default SY-DATUM
      !IV_UZEIT type SYUZEIT default SY-UZEIT
      !IV_PERNR type PERNR_D
      !IV_STATUS type ZHR_XSS_IPRREQ_STATUS
      !IV_COMMIT type FLAG optional .
  methods WS_COMPETITION_REQUEST_CREATE
    importing
      !IS_INPUT_DATA type ZSHR_MSS_WS_COMPETITION
    exporting
      !ES_OUTPUT_DATA type ZSHR_MSS_WS_COMPETITION .
  methods WS_COMPETITION_REQUEST_WITHDR
    importing
      !IS_INPUT_DATA type ZSHR_MSS_WS_COMPETITION
    exporting
      !ES_OUTPUT_DATA type ZSHR_MSS_WS_COMPETITION .
  methods WS_GET_ATTEST_XML
    importing
      !IV_EUSER type STRING
      !IV_BEGDA type BEGDA default SY-DATUM
      !IV_ENDDA type ENDDA default SY-DATUM
    exporting
      !E_XSTRING type XSTRING
    exceptions
      NO_PERNR
      NO_SEC_LEV_RUK .
  methods WS_GET_COMPMAINDATA
    importing
      !IV_EUSER type STRING
    exporting
      !ET_COMPMAINDATA type ZCL_MSS_COMPMAINDATA_MPC=>TT_COMPMAINDATA
    exceptions
      NO_PERNR
      NO_SEC_LEV_RUK .
  methods WS_GET_COMPMAINDESC
    importing
      !IT_OBJID type HROBJECT_T
    exporting
      !ET_COMPMAINDESC type ZCL_MSS_COMPMAINDATA_MPC=>TT_COMPMAINDESC
    exceptions
      NO_PERNR
      NO_SEC_LEV_RUK .
  methods WS_GET_DOC
    importing
      !IV_OBJID type STRING
      !IV_BEGDA type BEGDA default SY-DATUM .
  methods WS_GET_EMP_EVENTS
    importing
      !IV_EUSER type STRING
      !IV_BEGDA type BEGDA default SY-DATUM
      !IV_ENDDA type ENDDA default SY-DATUM
      !IV_XSS type STRING optional
    exporting
      !ET_EMP_EVENTS type TT_EMP_EVENT
    exceptions
      NO_PERNR
      NO_SEC_LEV_RUK .
  methods WS_GET_STAFF_GRAPH
    importing
      !IV_EUSER type STRING
      !IV_BEGDA type BEGDA default SY-DATUM
      !IV_ENDDA type ENDDA default SY-DATUM
    exporting
      !E_XSTRING type XSTRING
    exceptions
      NO_PERNR
      NO_SEC_LEV_RUK .
  methods WS_GET_EMP_RESERV_XML
    importing
      !IV_EUSER type STRING
      !IV_BEGDA type BEGDA default SY-DATUM
      !IV_ENDDA type ENDDA default SY-DATUM
    exporting
      !E_XSTRING type XSTRING
    exceptions
      NO_PERNR
      NO_SEC_LEV_RUK .
  methods WS_GET_ENTRANCE_USER
    importing
      !IV_MUSER type STRING
      !IV_BEGDA type BEGDA default SY-DATUM
      !IV_EUSER type STRING optional
    exporting
      !ET_ENTRANCE type TT_ENTRANCE .
  methods WS_GET_F1GS_XML
    importing
      !IV_EUSER type STRING
      !IV_BEGDA type BEGDA default SY-DATUM
      !IV_ENDDA type ENDDA default SY-DATUM
    exporting
      !E_XSTRING type XSTRING
    exceptions
      NO_PERNR
      NO_SEC_LEV_RUK .
  methods WS_GET_IPR_LIST
    importing
      !IV_EUSER type STRING
      !IV_REQ_ID type STRING optional
    exporting
      !ET_IPR_LIST type TT_IPR_LIST
    exceptions
      NO_PERNR
      NO_SEC_LEV_RUK .
  methods WS_GET_LIST_IPR
    importing
      !IS_INPUT_DATA type GTYS_INPUT_DATA
    exporting
      !ET_OUTPUT_DATA type GTYTD_LIST_IPR
    exceptions
      NO_PERNR
      NO_SEC_LEV_RUK
      PERNR_INACTIVE .
  methods WS_GET_MAIN_USER_DATA
    importing
      !IV_EUSER type STRING optional
      !IV_BEGDA type BEGDA default SY-DATUM
      !IV_ENDDA type ENDDA default SY-DATUM
      !IV_SNILS type STRING optional
    exporting
      !ET_MAINUSERDATA type ZCL_Z_MSS_MAIN_USER_DA_MPC=>TT_MAINUSERDATA
    exceptions
      NO_PERNR
      NO_PERNR_SUB
      NO_SEC_LEV_RUK
      PERNR_INACTIVE .
  methods WS_GET_SCHEDULE
    importing
      !IV_EUSER type STRING
      !IV_BEGDA type BEGDA default SY-DATUM
    exporting
      !ET_SCHEDULE type TT_SCHEDULE .
  methods WS_GET_SHTAT_CHISL_XML
    importing
      !IV_EUSER type STRING
      !IV_BEGDA type BEGDA default SY-DATUM
      !IV_ENDDA type ENDDA default SY-DATUM
    exporting
      !E_XSTRING type XSTRING
    exceptions
      NO_PERNR
      NO_SEC_LEV_RUK .
  methods WS_GET_STAFFLIST_XML
    importing
      !IV_EUSER type STRING
      !IV_BEGDA type BEGDA default SY-DATUM
      !IV_ENDDA type ENDDA default SY-DATUM
    exporting
      !E_XSTRING type XSTRING
    exceptions
      NO_PERNR
      NO_SEC_LEV_RUK .
  methods WS_GET_STRUCTURE
    importing
      !IV_MUSER type STRING optional
      !IV_BEGDA type BEGDA default SY-DATUM
      !IV_EUSER type STRING
      !IV_XSS type STRING optional
    exporting
      value(ET_STRUC) type TT_STRUC
    exceptions
      NO_PERNR
      NO_SEC_LEV_RUK
      PERNR_INACTIVE .
  methods WS_GET_T13_XML
    importing
      !IV_EUSER type STRING
      !IV_BEGDA type BEGDA default SY-DATUM
      !IV_ENDDA type ENDDA default SY-DATUM
    exporting
      !E_XSTRING type XSTRING
    exceptions
      NO_PERNR
      NO_SEC_LEV_RUK .
  methods GET_ATTACHMENT
    importing
      !IS_ATTABS type PTREQ_ATTABSDATA
    exporting
      !EV_ATT_HEX type SOLI_TAB .
protected section.

  class-data MV_ASSISTENT type ref to ZCL_MSS_DATA_ASSISTENT .
private section.

  class-data MV_PLVAR type PLVAR .
  constants GC_GSS_ATT type OM_ATTRIB value 'ZGSS_VIEW' ##NO_TEXT.
  constants GC_FOR_VIEW_TC type RVARI_VNAM value 'ZHR_GSS_OBJ_C_FOR_VIEW_TC' ##NO_TEXT.
  class-data:
    GR_C_OBJEC  type range of p1000-objid .
  data GV_GSS type ABAP_BOOL .
  constants GC_GSS_GOV type STRING value 'GSS' ##NO_TEXT.
  constants GC_GSS_SCN type OM_ATTRSCN value 'Z_GSS' ##NO_TEXT.
  constants GC_OBJ_TYPE_S type OTYPE value 'S' ##NO_TEXT.
  constants GC_OBJ_TYPE_P type OTYPE value 'P' ##NO_TEXT.
  data GC_TVARV_GOV_OBJ_C type RVARI_VNAM value 'ZHR_GSS_GOV_OBJ_C' ##NO_TEXT.
  constants GC_SUBTY_9001 type SUBTY value '9001' ##NO_TEXT.
  constants GC_OBJ_TYPE_O type OTYPE value 'O' ##NO_TEXT.
  constants GC_WEGID_HEAD_ALL type WEGID value 'Z_RUK_O' ##NO_TEXT.
  constants GC_VIEW_GSS_FOR_SIT_CEN type RVARI_VNAM value 'ZHR_GSS_PERNR_SIT_CEN' ##NO_TEXT.

  methods CHECK_GSS_VALUE
    importing
      !IT_STRUC type STRUC_T
      !IV_BEGDA type BEGDA
    returning
      value(RV_GSS) type ABAP_BOOL .
  methods CHECK_HEAD_IS_SECOND_LVL
    importing
      !IV_PERNR type PERNR_D
      !IV_BEGDA type BEGDA
      !IV_ENDDA type ENDDA
    exporting
      !EV_IS_SECOND_LVL type BOOLE_D
      !ET_STRUC type STRUC_T .
  methods CHECK_NO_VIEW
    importing
      !IS_STRUC type STRUC
      !IT_P1001 type P1001TAB
    returning
      value(RV_VIEW) type ABAP_BOOL .
  methods CHECK_RANGE
    changing
      !CT_OBJEC like GR_C_OBJEC .
  methods GET_EN_USER
    importing
      !IV_USRID type P0105-USRID
      !IV_BEGDA type BEGDA
    returning
      value(RV_EUSER) type P0105-USRID .
  methods GET_KVARTAL
    importing
      !IV_DATE type DATUM
    returning
      value(RV_KVART) type ZEHR_PA_KVART .
  methods GET_PENALTY
    importing
      !IT_PERNR type HROBJECT_T
      !IV_BEGDA type BEGDA
      !IV_ENDDA type ENDDA
    exporting
      !ET_PEN type ZHR_TT_PENALTY .
  methods HIDE_ORG_STRUC_EMPTY_ELEM
    importing
      !IT_SORT_STRUC_ALL type TT_SORT_STRUC
    changing
      !CT_ORG_STRUC type TT_STRUC .
  methods IS_REDUNDANT
    importing
      !IV_OTYPE type OTYPE
      !IV_OBJID type OBJEKTID
      !IV_BEGDA type BEGDA
      !IV_ENDDA type ENDDA
    returning
      value(RV_OK) type ABAP_BOOL .
  methods _FILL_COMP_MAIL_FROM
    returning
      value(RV_MAIL_FROM) type STRING .
  methods _FILL_COMP_MAIL_TO
    importing
      !IT_PERNR type PERNR_TAB optional
    exporting
      !ET_TO type BCSY_SMTPA .
  methods _FILL_COMP_MAPPING
    importing
      !IS_INPUT_DATA type ZSHR_MSS_WS_COMPETITION
      !IS_P9103 type P9103
      !IV_PERNR type PERNR_D
    exporting
      !ET_MAPPING type ZTTMAIL_MAPPING .
ENDCLASS.



CLASS ZCL_MSS_DATA_ASSISTENT IMPLEMENTATION.


  METHOD change_ipr_status.

  ENDMETHOD.


  METHOD check_gss_value.
    DATA: lt_attrib TYPE STANDARD TABLE OF pt1222.

    READ TABLE it_struc ASSIGNING FIELD-SYMBOL(<ls_struc>) WITH KEY otype = 'S'.
    IF sy-subrc = 0.

      CALL FUNCTION 'RH_OM_ATTRIBUTES_READ'
        EXPORTING
          plvar            = mv_plvar
          otype            = <ls_struc>-otype
          objid            = <ls_struc>-objid
          scenario         = gc_gss_scn
          seldate          = iv_begda
        TABLES
          attrib           = lt_attrib[]
        EXCEPTIONS
          no_active_plvar  = 1
          no_attributes    = 2
          no_values        = 3
          object_not_found = 4
          OTHERS           = 5.

      LOOP AT lt_attrib ASSIGNING FIELD-SYMBOL(<ls_attrib>) WHERE attrib = gc_gss_att.
        IF <ls_attrib>-low IS INITIAL.
          rv_gss = abap_false.
          EXIT.
        ELSE.
          rv_gss = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF sy-subrc <> 0.
        rv_gss = abap_false.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD check_head_is_second_lvl.

    DATA:
      lv_i        TYPE i,
      lv_head_lvl TYPE i.

    me->get_werks_by_pernr(
          EXPORTING
            iv_pernr = iv_pernr
          IMPORTING
            ev_werks = DATA(lv_werks) ).

    me->get_struc(
          EXPORTING
             iv_objid  = iv_pernr
             iv_otype  = me->gc_obj_type_p
             iv_begda  = iv_begda
             iv_endda  = iv_begda
             iv_wegid  = me->gc_wegid_head_all
             iv_acheck = abap_false
           IMPORTING
             et_struc  = DATA(lt_struc) ).

    DATA(lt_all_struc) = lt_struc.

    DELETE lt_struc
        WHERE otype <> me->gc_obj_type_o.

    LOOP AT lt_struc TRANSPORTING NO FIELDS "Определяем уровень вложения, отсчитывая кол-во орг.ед. "сверху внизу"
      WHERE pdown = '0'.

      lv_head_lvl = lv_i = 1.

      WHILE lv_i <> 0.

        READ TABLE lt_struc                     "Считываем О на уровень ниже и т.д. пока не придем к той, которой руководим
            ASSIGNING FIELD-SYMBOL(<fs_struc>)
            INDEX sy-tabix - 1.

        IF sy-subrc = 0.

          IF <fs_struc>-pdown > 0.    "Проверка на руководство несколькими О, если = 0, то новая иерархия О-шек, проверим их в с след. итерации
            ADD 1 TO lv_head_lvl.
          ELSE.
            CLEAR lv_i.
          ENDIF.

        ELSE.
          CLEAR lv_i.
        ENDIF.
      ENDWHILE.

      CASE lv_werks.
        WHEN '0161'
          OR '0162'
          OR '0165'.

          IF lv_head_lvl = 1.
            ev_is_second_lvl = 'X'.
            et_struc = lt_all_struc.
            EXIT.
          ENDIF.

        WHEN OTHERS.

          IF lv_head_lvl = 2.             "Если есть хотя бы одна О второго уровня
            ev_is_second_lvl = 'X'.
            et_struc = lt_all_struc.
            EXIT.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_is_sit_cen_for_gss.

    DATA:
      lr_p_object TYPE RANGE OF pernr_d,
      lv_pernr    TYPE          pernr_d.

    lv_pernr = iv_pernr.
    SHIFT lv_pernr LEFT DELETING LEADING '0'.

    zcl_hr_get_data=>read_stvarv_ran(
        EXPORTING
          iv_name  = gc_view_gss_for_sit_cen
        IMPORTING
          et_range = lr_p_object ).

    LOOP AT lr_p_object ASSIGNING FIELD-SYMBOL(<fs_p_object>).

      SHIFT <fs_p_object>-low LEFT DELETING LEADING '0'.

      IF <fs_p_object>-low = lv_pernr.
        rv_is_sit_cen = abap_true.
        EXIT.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD check_no_view.
    READ TABLE it_p1001 ASSIGNING FIELD-SYMBOL(<ls_p1001>) WITH KEY objid = is_struc-objid BINARY SEARCH.
    IF sy-subrc = 0.
      DATA(lv_c_objid) = <ls_p1001>-sobid.
      IF lv_c_objid NOT IN gr_c_objec.
        rv_view = abap_false.
      ELSE.
        rv_view = abap_true.
      ENDIF.
    ELSE.
      rv_view = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD check_pernr_is_top_head.

    DATA:
           lv_objid_c     TYPE objektid

         , lt_object_c    TYPE hrobject_t
         , lt_object_s    TYPE hrobject_t
         , lt_p1001_s     TYPE TABLE OF p1001
         , lt_p1001_p     TYPE TABLE OF p1001

         , ls_object_tmp  TYPE hrobject
         .

    DATA(lo_get_governor) = NEW zcl_hr_get_data( ).

    lo_get_governor->read_stvarv_par(
        EXPORTING
          i_name = 'ZHR_GSS_GOV_OBJ_C'
        IMPORTING
          e_value = lv_objid_c ).

    IF lv_objid_c IS NOT INITIAL.
      ls_object_tmp-objid = lv_objid_c.
      ls_object_tmp-plvar = '01'.
      ls_object_tmp-otype = 'C'.
    ENDIF.

    APPEND ls_object_tmp TO lt_object_c.

    lo_get_governor->read_om_infty(
        EXPORTING
          i_infty  = '1001'
          i_begda  = iv_begda
          i_endda  = iv_begda
          i_object = lt_object_c
        IMPORTING
          e_pnnnn  = lt_p1001_s ).

    DELETE lt_p1001_s
       WHERE rsign <> 'A'
         AND relat <> '007'.

    SORT lt_p1001_s BY objid.

    LOOP AT lt_p1001_s INTO DATA(ls_p1001_s).
      CLEAR ls_object_tmp.

      ls_object_tmp-plvar = ls_p1001_s-plvar.
      ls_object_tmp-objid = ls_p1001_s-sobid.
      ls_object_tmp-otype = 'S'.

      APPEND ls_object_tmp TO lt_object_s.
    ENDLOOP.

    lo_get_governor->read_om_infty(
       EXPORTING
         i_infty  = '1001'
         i_begda  = iv_begda
         i_endda  = iv_begda
         i_object = lt_object_s
       IMPORTING
         e_pnnnn  = lt_p1001_p ).

    SORT lt_p1001_p BY objid.

    DELETE lt_p1001_p
       WHERE rsign <> 'A'
         AND relat <> '008'
          OR prozt <> '100'.

    READ TABLE lt_p1001_p
        INTO DATA(ls_p1001_p)
        INDEX 1 .

    IF ls_p1001_p-sobid <> iv_pernr.
      ev_pernr_gov = ls_p1001_p-sobid.
    ELSE.
      ev_pernr_gov = iv_pernr.
      ev_is_gov    = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD check_range.
    " проверка настройки
    delete ct_objec WHERE sign is initial.

  ENDMETHOD.


  METHOD class_constructor.
    mv_plvar = zcl_hr_get_data=>a_plvar.
  ENDMETHOD.


  method CONSTRUCTOR.
  endmethod.


  METHOD file_delete.
    DATA: ls_at_cont TYPE zthr_mss_at_cont
        .
    SELECT SINGLE * INTO @DATA(ls_attr) FROM zthr_mss_at_attr WHERE object = @iv_object
                                                                AND ftype  = @iv_ftype
                                                                AND seqnr  = @iv_seqnr.

     CHECK sy-subrc = 0.

     DELETE zthr_mss_at_attr FROM ls_attr.

     ls_at_cont-srtfd = ls_attr-guid.

     DELETE FROM DATABASE zthr_mss_at_cont(zz) ID ls_at_cont-srtfd.

     CHECK iv_commit is NOT INITIAL.
     COMMIT WORK AND WAIT.
  ENDMETHOD.


  METHOD file_download.
    rv_subrc = 8.

    SELECT SINGLE * FROM zthr_mss_at_attr INTO @es_attr WHERE object = @iv_object
                                                          AND ftype  = @iv_ftype
                                                          AND seqnr  = @iv_seqnr.

    IF sy-subrc = 0.

    ELSE.
      rv_subrc = 4.
      RETURN.
    ENDIF.

    DATA: ls_cont TYPE zthr_mss_at_cont
        .
    ls_cont-srtfd =  es_attr-guid.

    IMPORT content = es_content FROM DATABASE zthr_mss_at_cont(zz) ID ls_cont-srtfd.

    IF es_content IS REQUESTED.
      RETURN.
    ENDIF.

    DATA: lv_folder  TYPE string
        , lt_data    TYPE solix_tab
        .

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = es_content
      TABLES
        binary_tab = lt_data.
    .
    DO 1 TIMES.
      IF iv_open = abap_true.
        CALL METHOD cl_gui_frontend_services=>get_sapgui_workdir
          CHANGING
            sapworkdir            = lv_folder
          EXCEPTIONS
            get_sapworkdir_failed = 1
            cntl_error            = 2
            error_no_gui          = 3
            not_supported_by_gui  = 4
            OTHERS                = 5.

        CHECK sy-subrc = 0.

        CALL METHOD cl_gui_cfw=>flush
          EXCEPTIONS
            OTHERS = 1.

        DATA(lv_fname) = es_attr-fname.

        DATA(lv_shift1) = strlen( lv_fname ).
        DATA(lv_shift2) = strlen( es_attr-dtype ).

        lv_shift1 = lv_shift1 - lv_shift2 - 1.

        IF lv_shift1 > 0.
          lv_fname = lv_fname+0(lv_shift1) && `_` && sy-datum && `_` &&  sy-uzeit && lv_fname+lv_shift1.
        ENDIF.

        lv_folder = lv_folder && `\` && lv_fname.

        CALL METHOD cl_gui_frontend_services=>gui_download
          EXPORTING
            bin_filesize            = es_attr-length
            filename                = lv_folder
            filetype                = 'BIN'
          CHANGING
            data_tab                = lt_data
          EXCEPTIONS
            file_write_error        = 1
            no_batch                = 2
            gui_refuse_filetransfer = 3
            invalid_type            = 4
            no_authority            = 5
            unknown_error           = 6
            header_not_allowed      = 7
            separator_not_allowed   = 8
            filesize_not_allowed    = 9
            header_too_long         = 10
            dp_error_create         = 11
            dp_error_send           = 12
            dp_error_write          = 13
            unknown_dp_error        = 14
            access_denied           = 15
            dp_out_of_memory        = 16
            disk_full               = 17
            dp_timeout              = 18
            file_not_found          = 19
            dataprovider_exception  = 20
            control_flush_error     = 21
            not_supported_by_gui    = 22
            error_no_gui            = 23
            OTHERS                  = 24.

        CHECK sy-subrc = 0.

        CALL METHOD cl_gui_cfw=>flush
          EXCEPTIONS
            OTHERS = 1.

        CALL METHOD cl_gui_frontend_services=>file_set_attributes
          EXPORTING
            filename = lv_folder
            readonly = abap_true
          IMPORTING
            rc       = DATA(rcode)
          EXCEPTIONS
            OTHERS   = 1.

        CHECK sy-subrc = 0.

        CALL METHOD cl_gui_cfw=>flush
          EXCEPTIONS
            OTHERS = 1.

        CALL METHOD cl_gui_frontend_services=>execute
          EXPORTING
            document               = lv_folder
          EXCEPTIONS
            cntl_error             = 1
            error_no_gui           = 2
            bad_parameter          = 3
            file_not_found         = 4
            path_not_found         = 5
            file_extension_unknown = 6
            error_execute_failed   = 7
            synchronous_failed     = 8
            not_supported_by_gui   = 9
            OTHERS                 = 10.

*        DATA(lv_file_name) = CONV rlgrap-filename( lv_folder ).
*
*        CALL FUNCTION 'GUI_DELETE_FILE'
*          EXPORTING
*            file_name = lv_file_name
*          EXCEPTIONS
*            OTHERS    = 1.
      ELSE.
        CALL METHOD cl_gui_frontend_services=>directory_browse
          CHANGING
            selected_folder      = lv_folder
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            not_supported_by_gui = 3
            OTHERS               = 4.

        CHECK sy-subrc = 0.

        lv_folder = lv_folder && `\` && es_attr-fname.


        CALL METHOD cl_gui_frontend_services=>gui_download
          EXPORTING
            bin_filesize            = es_attr-length
            filename                = lv_folder
            filetype                = 'BIN'
          CHANGING
            data_tab                = lt_data
          EXCEPTIONS
            file_write_error        = 1
            no_batch                = 2
            gui_refuse_filetransfer = 3
            invalid_type            = 4
            no_authority            = 5
            unknown_error           = 6
            header_not_allowed      = 7
            separator_not_allowed   = 8
            filesize_not_allowed    = 9
            header_too_long         = 10
            dp_error_create         = 11
            dp_error_send           = 12
            dp_error_write          = 13
            unknown_dp_error        = 14
            access_denied           = 15
            dp_out_of_memory        = 16
            disk_full               = 17
            dp_timeout              = 18
            file_not_found          = 19
            dataprovider_exception  = 20
            control_flush_error     = 21
            not_supported_by_gui    = 22
            error_no_gui            = 23
            OTHERS                  = 24.
      ENDIF.
    ENDDO.

    rv_subrc = sy-subrc.
  ENDMETHOD.


  METHOD file_save.
    DATA: ls_at_attr TYPE zthr_mss_at_attr
        , ls_at_cont TYPE zthr_mss_at_cont
        .
    ls_at_attr-ftype  = iv_ftype.
    ls_at_attr-object = iv_object.
    ls_at_attr-fname  = iv_name.
    ls_at_attr-datum  = iv_datum.
    ls_at_attr-uzeit  = iv_uzeit.
    ls_at_attr-uname  = iv_uname.
    ls_at_attr-dtype  = iv_dtype.
    ls_at_attr-length = iv_filelength.

    TRY.
        IF iv_unseq IS INITIAL.
          SELECT MAX( seqnr ) FROM zthr_mss_at_attr INTO ls_at_attr-seqnr WHERE object = ls_at_attr-object
                                                                            AND ftype  = ls_at_attr-ftype.
          IF sy-subrc = 0.
            ADD 1 TO ls_at_attr-seqnr.
          ENDIF.
        ENDIF.

        SELECT SINGLE guid FROM zthr_mss_at_attr INTO @DATA(ls_guid_old) WHERE ftype  = @ls_at_attr-ftype
                                                                           AND object = @ls_at_attr-object
                                                                           AND seqnr  = @ls_at_attr-seqnr.
        IF ls_guid_old IS INITIAL.
          ls_at_attr-guid = cl_system_uuid=>create_uuid_x16_static( ).
        ELSE.
          me->file_delete( iv_ftype  = ls_at_attr-ftype
                           iv_object = ls_at_attr-object
                           iv_seqnr  = ls_at_attr-seqnr
                           iv_commit = abap_false ).

          ls_at_attr-guid = ls_guid_old.
        ENDIF.

        ls_at_cont-srtfd = ls_at_attr-guid.

        EXPORT content = iv_content TO DATABASE zthr_mss_at_cont(zz) ID ls_at_cont-srtfd.

        MODIFY zthr_mss_at_attr FROM ls_at_attr.

        COMMIT WORK AND WAIT.
        rv_subrc = 0.
      CATCH cx_uuid_error.

    ENDTRY.
  ENDMETHOD.


  METHOD file_upload.
    DATA: lt_file_table  TYPE filetable
        , lv_rc          TYPE i
        .
    rv_subrc = 8.
    TRY.
        CALL METHOD cl_gui_frontend_services=>file_open_dialog
          CHANGING
            file_table              = lt_file_table
            rc                      = lv_rc
          EXCEPTIONS
            file_open_dialog_failed = 1
            cntl_error              = 2
            error_no_gui            = 3
            not_supported_by_gui    = 4
            OTHERS                  = 5.
        CHECK sy-subrc = 0.

        READ TABLE lt_file_table ASSIGNING FIELD-SYMBOL(<lt_file_table>) INDEX 1.

        CHECK <lt_file_table> IS ASSIGNED AND sy-subrc = 0.

        DATA(lv_filename) = CONV string( <lt_file_table> ).

        DATA: lt_data TYPE solix_tab
            .
        CALL METHOD cl_gui_frontend_services=>gui_upload
          EXPORTING
            filename                = lv_filename
            filetype                = 'BIN'
          IMPORTING
            filelength              = DATA(lv_filelength)
          CHANGING
            data_tab                = lt_data
          EXCEPTIONS
            file_open_error         = 1
            file_read_error         = 2
            no_batch                = 3
            gui_refuse_filetransfer = 4
            invalid_type            = 5
            no_authority            = 6
            unknown_error           = 7
            bad_data_format         = 8
            header_not_allowed      = 9
            separator_not_allowed   = 10
            header_too_long         = 11
            unknown_dp_error        = 12
            access_denied           = 13
            dp_out_of_memory        = 14
            disk_full               = 15
            dp_timeout              = 16
            not_supported_by_gui    = 17
            error_no_gui            = 18
            OTHERS                  = 19.

        CHECK sy-subrc = 0.

        DATA: lv_content TYPE xstring
            .
        CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
          EXPORTING
            input_length = lv_filelength
          IMPORTING
            buffer       = lv_content
          TABLES
            binary_tab   = lt_data
          EXCEPTIONS
            failed       = 1
            OTHERS       = 2.

        REFRESH lt_file_table.

        SPLIT lv_filename AT '\' INTO TABLE lt_file_table.

        LOOP AT lt_file_table ASSIGNING <lt_file_table>.
          DATA(lv_name) = CONV string( <lt_file_table> ).
        ENDLOOP.

        REFRESH lt_file_table.

        SPLIT lv_name AT '.' INTO TABLE lt_file_table.

        LOOP AT lt_file_table ASSIGNING <lt_file_table>.
          DATA(lv_dtype) = CONV string( <lt_file_table> ).
        ENDLOOP.

        CHECK me->file_save( iv_ftype      = iv_ftype
                             iv_dtype      = CONV #( lv_dtype )
                             iv_object     = iv_object
                             iv_name       = CONV #( lv_name )
                             iv_filelength = lv_filelength
                             iv_unseq      = CONV #( iv_unseq )
                             iv_content    = lv_content ) = 0.

*    DATA: ls_at_attr TYPE zthr_mss_at_attr
*        , ls_at_cont TYPE zthr_mss_at_cont
*        .
*    ls_at_attr-ftype  = iv_ftype.
*    ls_at_attr-object = iv_object.
*    ls_at_attr-fname  = lv_name.
*    ls_at_attr-datum  = sy-datum.
*    ls_at_attr-uzeit  = sy-uzeit.
*    ls_at_attr-uname  = sy-uname.
*    ls_at_attr-dtype  = lv_ftype.
*    ls_at_attr-length = lv_filelength.
*
*    IF iv_unseq IS INITIAL.
*      SELECT MAX( seqnr ) FROM zthr_mss_at_attr INTO ls_at_attr-seqnr WHERE object = ls_at_attr-object
*                                                                        AND ftype  = ls_at_attr-ftype.
*      IF sy-subrc = 0.
*        ADD 1 TO ls_at_attr-seqnr.
*      ENDIF.
*    ENDIF.
*
*    SELECT SINGLE guid FROM zthr_mss_at_attr INTO @DATA(ls_guid_old) WHERE ftype  = @ls_at_attr-ftype
*                                                                       AND object = @ls_at_attr-object
*                                                                       AND seqnr  = @ls_at_attr-seqnr.
*    IF ls_guid_old IS INITIAL.
*      ls_at_attr-guid = cl_system_uuid=>create_uuid_x16_static( ).
*    ELSE.
*      me->file_delete( iv_ftype  = ls_at_attr-ftype
*                       iv_object = ls_at_attr-object
*                       iv_seqnr  = ls_at_attr-seqnr
*                       iv_commit = abap_false ).
*
*      ls_at_attr-guid = ls_guid_old.
*    ENDIF.
*
*    ls_at_cont-srtfd = ls_at_attr-guid.
*
*    EXPORT content = lv_content TO DATABASE zthr_mss_at_cont(zz) ID ls_at_cont-srtfd.
*
*    MODIFY zthr_mss_at_attr FROM ls_at_attr.

        COMMIT WORK AND WAIT.
        rv_subrc = 0.
      CATCH cx_uuid_error.
    ENDTRY.
  ENDMETHOD.


  METHOD get_all_users.
    SELECT pa0105~pernr
         , pa0105~usrid
         , pa0002~nachn
         , pa0002~vorna
         , pa0002~midnm
        INTO TABLE @DATA(lt_data)
        FROM pa0105
        LEFT OUTER JOIN pa0002 ON pa0002~pernr = pa0105~pernr AND pa0002~begda <= @iv_begda AND pa0002~endda >= @iv_begda
        WHERE pa0105~begda <= @iv_begda
          AND pa0105~endda >= @iv_begda
          AND pa0105~subty  = '9001'.

    SORT lt_data BY usrid.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lt_data>).
      APPEND INITIAL LINE TO et_users ASSIGNING FIELD-SYMBOL(<et_users>).
      <et_users>-usrid = <lt_data>-usrid.
      <et_users>-ename = <lt_data>-nachn && ` ` && <lt_data>-vorna && ` ` && <lt_data>-midnm.
      <et_users>-pernr = <lt_data>-pernr.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_approver_number.
    DATA: lt_mapping TYPE zttmail_mapping
        , lt_mail_to TYPE bcsy_smtpa
        .
    CASE iv_status.
      WHEN 5 or 4 OR 0. "согласовано/создание
        CASE iv_status_old.
          WHEN 6 OR 0."OR 8
            DO 3 TIMES.
              ev_status = me->get_next_status( iv_status = iv_status_old ).

              CASE ev_status.
                WHEN 6. " Утверждение линейным руководителем
                  ev_approver = me->get_pernr_ruk0( iv_pernr = iv_pernr ).
                WHEN 8. " Утверждение руководителем отдела обучения
                  zcl_hr_get_data=>read_stvarv_par( EXPORTING i_name  = `ZZ_LEARN_APPROVER`
                                                    IMPORTING e_value = ev_approver ).
                WHEN 10." Итоговое согласование
                  ev_approver = me->get_pernr_ruk( iv_pernr = iv_pernr ).
                WHEN OTHERS.
              ENDCASE.

              CHECK NOT ev_approver IS INITIAL.
              DATA(lv_subject) = CONV so_obj_des( 'Индивидуальный план развития работника' ).
              DATA(lv_email) = me->get_pernr_0105( iv_pernr = ev_approver ).
              IF lv_email IS NOT INITIAL.
                APPEND INITIAL LINE TO lt_mail_to ASSIGNING FIELD-SYMBOL(<lt_mail_to>).
                <lt_mail_to> = lv_email.

                APPEND VALUE #( name = `<pernr>`    value = CONV zsmail_mapping-value( me->get_pernr_fio( iv_pernr = iv_pernr ) ) ) TO lt_mapping.
                APPEND VALUE #( name = `<approver>` value = CONV zsmail_mapping-value( me->get_pernr_fio( iv_pernr = ev_approver ) ) ) TO lt_mapping.

                zcl_send_email=>send_mail( EXPORTING itd_to          = lt_mail_to
                                                     iv_textname     = `ZHR_IPR_APPROV_REQ`
                                                     iv_subject      = lv_subject
                                                     itd_mapping     = lt_mapping
                                                     iv_immediately  = abap_false ).
              ENDIF.
              EXIT.
            ENDDO.
          WHEN 10 OR 8 .
            ev_status = iv_status.
            lv_subject = CONV so_obj_des( 'Индивидуальный план развития работника' ).
            lv_email = me->get_pernr_0105( iv_pernr = iv_pernr ).
            CHECK lv_email IS NOT INITIAL.
            APPEND INITIAL LINE TO lt_mail_to ASSIGNING <lt_mail_to>.
            <lt_mail_to> = lv_email.

            APPEND VALUE #( name = `<pernr>`    value = CONV zsmail_mapping-value( me->get_pernr_fio( iv_pernr = iv_pernr ) ) ) TO lt_mapping.
            APPEND VALUE #( name = `<status>` value = `утвержден`   ) TO lt_mapping.

            zcl_send_email=>send_mail( EXPORTING itd_to          = lt_mail_to
                                                 iv_textname     = `ZHR_IPR_APPROVED`
                                                 iv_subject      = lv_subject
                                                 itd_mapping     = lt_mapping
                                                 iv_immediately  = abap_false ).
          WHEN OTHERS.
            ev_status = iv_status.
        ENDCASE.
        ev_status_log = ev_status.
      WHEN 2. "отклонено
        ev_status = iv_status.
        lv_subject = CONV so_obj_des( 'Индивидуальный план развития работника' ).
        lv_email = me->get_pernr_0105( iv_pernr = iv_pernr ).
        CHECK lv_email IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_mail_to ASSIGNING <lt_mail_to>.
        <lt_mail_to> = lv_email.

        APPEND VALUE #( name = `<pernr>`    value = CONV zsmail_mapping-value( me->get_pernr_fio( iv_pernr = iv_pernr ) ) ) TO lt_mapping.
        APPEND VALUE #( name = `<status>` value = `отклонён`   ) TO lt_mapping.

        zcl_send_email=>send_mail( EXPORTING itd_to          = lt_mail_to
                                             iv_textname     = `ZHR_IPR_APPROVED`
                                             iv_subject      = lv_subject
                                             itd_mapping     = lt_mapping
                                             iv_immediately  = abap_false ).
        CASE iv_status_old.
          WHEN 6.
            ev_status_log = 7.
          WHEN 8.
            ev_status_log = 9.
          WHEN 10.
            ev_status_log = 11.
          WHEN OTHERS.
            ev_status_log = iv_status.
        ENDCASE.
      WHEN OTHERS.
        ev_status = iv_status.
    ENDCASE.


  ENDMETHOD.


  METHOD get_attachment.
    DATA:
      lv_pernr   TYPE pernr_d,
      lv_begda   TYPE begda,
      lv_text1   TYPE text20,
      lv_komitet TYPE orgeh.
    TYPES: BEGIN OF ty_guber,
             objid     TYPE hrobjid,
             pernr     TYPE pernr_d,
             fio       TYPE string,
             plans_txt TYPE string,
           END OF ty_guber.
    DATA: lt_objid_guber TYPE TABLE OF ty_guber.
    DATA:
   lv_request_id        TYPE tim_req_id.
    DATA:  t_data_xml TYPE string.
    TYPES:
      BEGIN OF ty_main
            , fio_inits    TYPE text250
            , plans_init   TYPE text250
            , gub_plans    TYPE text250
            , gub_name     TYPE text250
            , dayfrom      TYPE text2
            , monthtxtfrom TYPE text15
            , yearfrom     TYPE text4
            , dayto        TYPE text2
            , monthtxtto   TYPE text15
            , yearto       TYPE text4
            , plans_head   TYPE text250
            , fio_head     TYPE text250
            , vac_days     TYPE text5
            , fio     TYPE text250
            , pernr   TYPE text250
            , abstext TYPE text250
            , begda   TYPE text250
            , endda   TYPE text250
            , comment TYPE text250
            , nexthandlerfio TYPE text250
            , pos      TYPE text250
         , END OF  ty_main.
    DATA: ls_main TYPE ty_main.
    DATA: lv_text_kom   TYPE string,
          lv_orgeh_full TYPE string,
          lv_name_plans TYPE string.

    CHECK is_attabs IS NOT INITIAL.

    ls_main-abstext = zcl_hr_data=>get_awart_txt( i_awart = is_attabs-subty ).
    lv_begda = ls_main-begda =  is_attabs-begda.
    ls_main-endda = is_attabs-endda.
    lv_pernr = ls_main-pernr = is_attabs-pernr.
    ls_main-fio = ls_main-fio_inits = get_pernr_fio( iv_pernr = is_attabs-pernr iv_begda = sy-datum ).

    CALL METHOD zcl_calendar=>get_date_dd_month
      EXPORTING
        i_date = is_attabs-begda
      IMPORTING
        e_date = lv_text1.

    SPLIT lv_text1 AT space INTO ls_main-dayfrom ls_main-monthtxtfrom.
    CONDENSE: ls_main-dayfrom, ls_main-monthtxtfrom .

    ls_main-yearfrom = is_attabs-begda(4).

    CALL METHOD zcl_calendar=>get_date_dd_month
      EXPORTING
        i_date = is_attabs-endda
      IMPORTING
        e_date = lv_text1.

    SPLIT lv_text1 AT space INTO ls_main-dayto ls_main-monthtxtto.
    ls_main-yearto = is_attabs-endda(4).

    DATA: lt_pa0001 TYPE TABLE OF p0001.
    zcl_hr_data=>read_pa_infty( EXPORTING i_pernr = lv_pernr
                                          i_infty = '0001'
                                          i_begda = lv_begda
                                          i_endda = lv_begda
                                IMPORTING e_pnnnn = lt_pa0001 ).
    IF lines( lt_pa0001 ) > 0.
      SORT lt_pa0001 BY begda DESCENDING.
      READ TABLE lt_pa0001 ASSIGNING FIELD-SYMBOL(<lt_pa0001>) INDEX 1.
      IF <lt_pa0001> IS ASSIGNED.
*          >>>Шаталов Б.А. 18.01.2022 Вывод полной структуры ШД
        CLEAR: lv_text_kom, lv_orgeh_full, lv_name_plans.
        CALL METHOD zcl_hr_data=>get_komitet
          EXPORTING
            i_orgeh = <lt_pa0001>-orgeh
            i_date  = <lt_pa0001>-begda
          IMPORTING
            e_lname = lv_text_kom.

        CALL METHOD zcl_hr_data=>get_name_orgeh_full
          EXPORTING
            i_pernr = <lt_pa0001>-pernr
            i_date  = <lt_pa0001>-begda
          IMPORTING
            e_lname = lv_orgeh_full.

        CALL METHOD zcl_hr_data=>get_name_plans
          EXPORTING
            i_plans = <lt_pa0001>-plans
            i_date  = <lt_pa0001>-begda
          IMPORTING
            e_lname = lv_name_plans.

        CONCATENATE lv_name_plans lv_orgeh_full lv_text_kom INTO DATA(lv_fulltxt) SEPARATED BY space.
        ls_main-plans_init = lv_fulltxt.
      ENDIF.
    ENDIF.

*          >>>Шаталов Б.А. Получаем Имя и должность Губернатора и Первого Вице
    SELECT objid FROM hrp1001
     WHERE plvar = '01'
      AND rsign = 'B'
      AND relat = 'ZSP'
      INTO CORRESPONDING FIELDS OF TABLE @lt_objid_guber.
    IF sy-subrc = 0.
      SORT lt_objid_guber BY objid.
      DELETE ADJACENT DUPLICATES FROM lt_objid_guber COMPARING objid.
      LOOP AT lt_objid_guber ASSIGNING FIELD-SYMBOL(<ls_objid_guber>).
        CALL METHOD zcl_hr_data=>get_name_plans
          EXPORTING
            i_plans = CONV plans( <ls_objid_guber>-objid )
            i_date  = lv_begda
          IMPORTING
            e_lname = <ls_objid_guber>-plans_txt.

        CALL METHOD zcl_hr_data=>get_pernr_for_plan
          EXPORTING
            iv_date  = lv_begda
            iv_plans = CONV plans( <ls_objid_guber>-objid )
          RECEIVING
            rv_pernr = <ls_objid_guber>-pernr.

        CALL METHOD zcl_hr_data=>get_fio
          EXPORTING
            i_pernr   = <ls_objid_guber>-pernr
            i_date    = lv_begda
          IMPORTING
            e_fio_end = <ls_objid_guber>-fio.
      ENDLOOP.
    ENDIF.
    DELETE lt_objid_guber WHERE objid IS INITIAL OR
                                pernr IS INITIAL OR
                                fio IS INITIAL OR
                                plans_txt IS INITIAL.
    DELETE lt_objid_guber WHERE NOT plans_txt CS 'убер'.
    LOOP AT lt_objid_guber INTO DATA(ls_vice_data) WHERE ( plans_txt CS 'Первый' AND plans_txt CS 'вице' )
                                                      OR ( plans_txt CS 'Первый' AND plans_txt CS 'Вице' ).
      EXIT.
    ENDLOOP.
    LOOP AT lt_objid_guber INTO DATA(ls_guber_data) WHERE plans_txt CS 'Губер'.
      EXIT.
    ENDLOOP.
*          <<<Шаталов Б.А. 10.09.2021 Вывод полной структуры ШД

    " Руководитель
    CALL METHOD zcl_hr_data=>get_komitet
      EXPORTING
        i_pernr   = lv_pernr
        i_date    = lv_begda
      IMPORTING
        e_komitet = lv_komitet.

    IF lv_komitet IS NOT INITIAL.

      CALL METHOD zcl_hr_data=>get_komitet_lider
        EXPORTING
          i_komitet    = lv_komitet
          i_date       = lv_begda
        IMPORTING
          e_lider_name = ls_main-fio_head
          e_plans_name = ls_main-plans_head.

    ENDIF.

    " Количество дней

    ls_main-vac_days = is_attabs-kaltg.


    DATA: lv_kodshd(3) TYPE c,
          lv_par1      TYPE c,
          lv_par2      TYPE c,
          lv_par3      TYPE c.

    IF <lt_pa0001> IS ASSIGNED.

      CALL METHOD zcl_hr_data=>get_plans_param
        EXPORTING
          i_plans  = <lt_pa0001>-plans
          i_date   = <lt_pa0001>-begda
        IMPORTING
          e_param1 = lv_par1
          e_param2 = lv_par2
          e_param3 = lv_par3.
    ENDIF.

    CONCATENATE lv_par1 lv_par2 lv_par3 INTO lv_kodshd.
*>>>Шаталов Б.А. вывод измененного шаблона с должностями 18.01.2022
    CASE is_attabs-subty.
      WHEN '0109'.
        IF lv_kodshd = '2P1' OR lv_kodshd = '2P2'.
          ls_main-gub_plans = ls_guber_data-plans_txt.
          ls_main-gub_name =  ls_guber_data-fio.
        ELSE.
          ls_main-gub_plans = ls_vice_data-plans_txt.
          ls_main-gub_name =  ls_vice_data-fio.
        ENDIF.
        CALL TRANSFORMATION zhr_ess_hol_study
             SOURCE main = ls_main
             RESULT XML t_data_xml.
      WHEN '0400'.
        IF lv_kodshd = '2P1' OR lv_kodshd = '2P2'.
          ls_main-gub_plans = ls_guber_data-plans_txt.
          ls_main-gub_name =  ls_guber_data-fio.
        ELSE.
          ls_main-gub_plans = ls_vice_data-plans_txt.
          ls_main-gub_name =  ls_vice_data-fio.
        ENDIF.
        CALL TRANSFORMATION zhr_ess_hol_nopay
             SOURCE main = ls_main
             RESULT XML t_data_xml.
      WHEN '0502'.
        IF lv_kodshd = '2P1' OR lv_kodshd = '2P2'.
          ls_main-gub_plans = ls_guber_data-plans_txt.
          ls_main-gub_name =  ls_guber_data-fio.
        ELSE.
          ls_main-gub_plans = ls_vice_data-plans_txt.
          ls_main-gub_name =  ls_vice_data-fio.
        ENDIF.
        CALL TRANSFORMATION zhr_ess_hol_child
             SOURCE main = ls_main
             RESULT XML t_data_xml.
      WHEN '0301'.
        IF lv_kodshd = '2P1' OR lv_kodshd = '2P2'.
          ls_main-gub_plans = ls_guber_data-plans_txt.
          ls_main-gub_name =  ls_guber_data-fio.
        ELSE.
          ls_main-gub_plans = ls_vice_data-plans_txt.
          ls_main-gub_name =  ls_vice_data-fio.
        ENDIF.
        CALL TRANSFORMATION zhr_ess_dis_child
             SOURCE main = ls_main
             RESULT XML t_data_xml.
      WHEN '0202'.
        IF lv_kodshd = '2P1' OR lv_kodshd = '2P2'.
          ls_main-gub_plans = ls_guber_data-plans_txt.
          ls_main-gub_name =  ls_guber_data-fio.
        ELSE.
          ls_main-gub_plans = ls_vice_data-plans_txt.
          ls_main-gub_name =  ls_vice_data-fio.
        ENDIF.
        CALL TRANSFORMATION zhr_ess_pregnant
             SOURCE main = ls_main
             RESULT XML t_data_xml.
      WHEN '0304'.
        IF lv_kodshd = '2P1' OR lv_kodshd = '2P2'.
          ls_main-gub_plans = ls_guber_data-plans_txt.
          ls_main-gub_name =  ls_guber_data-fio.
        ELSE.
          ls_main-gub_plans = ls_vice_data-plans_txt.
          ls_main-gub_name =  ls_vice_data-fio.
        ENDIF.
        CALL TRANSFORMATION zhr_ess_donor_day_off_g
             SOURCE main = ls_main
             RESULT XML t_data_xml.
      WHEN OTHERS.
        IF lv_kodshd = '2P1' OR lv_kodshd = '2P2'.
          ls_main-gub_plans = ls_guber_data-plans_txt.
          ls_main-gub_name =  ls_guber_data-fio.
          CALL TRANSFORMATION zhr_ess_vac_get_np_g
             SOURCE main = ls_main
             RESULT XML t_data_xml.
        ELSE.
          ls_main-gub_plans = ls_vice_data-plans_txt.
          ls_main-gub_name =  ls_vice_data-fio.
          CALL TRANSFORMATION zhr_ess_vac_get_np
             SOURCE main = ls_main
             RESULT XML t_data_xml.
        ENDIF.
    ENDCASE.

    ev_att_hex = cl_document_bcs=>string_to_soli( ip_string = t_data_xml ).

  ENDMETHOD.


  METHOD get_en_user.
    DATA: lv_main_user TYPE zehr_mss_main_user.
    lv_main_user = iv_usrid.
    TRANSLATE lv_main_user  TO UPPER CASE.

    SELECT SINGLE entrance_user INTO rv_euser FROM zthr_entr_user WHERE main_user = lv_main_user
                                                                    AND begda <= iv_begda
                                                                    AND endda >= iv_begda
                                                                    AND del = ''.

  ENDMETHOD.


  METHOD get_inn_orgeh.
*   По пути анализа P-S-O-O определяем орг. единицу 2-го уровня. (ОИВ).
*   Определяем ИНН: T7RU9A-ANSNR при условии, что
*   T7RU9A-SOORT=’01’ и T7RU9A-OTYPE=’O’ и T7RU9A-OBJID=<Id ОИВ> и T7RU9A-ENDDA=’31.12.9999’
    DATA: lt_struc TYPE TABLE OF struc
        .

    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING
        act_otype       = 'P'
        act_objid       = i_pernr
        act_wegid       = 'P-S-O-O'
        act_plvar       = '01'
        act_begda       = i_date
        act_endda       = i_date
        act_tflag       = ' '
        act_vflag       = ' '
        authority_check = ' '
      TABLES
*       RESULT_TAB      =
*       RESULT_OBJEC    =
        result_struc    = lt_struc
      EXCEPTIONS
        no_plvar_found  = 1
        no_entry_found  = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    SORT lt_struc BY level DESCENDING.
    READ TABLE lt_struc INDEX 2 ASSIGNING FIELD-SYMBOL(<fs_struc>).
    IF sy-subrc EQ 0.
      SELECT SINGLE ansnr FROM t7ru9a
        INTO @DATA(lv_inn)
        WHERE soort EQ '01' AND
              otype EQ 'O'  AND
              objid EQ @<fs_struc>-objid AND
              endda eq @hr_high_date.
      IF sy-subrc EQ 0.
        r_inn = lv_inn.
      ENDIF.
    ENDIF.
  ENDMETHOD.


METHOD get_input_filter.

  DATA: lr_filter TYPE RANGE OF string,
        lo_filter TYPE REF TO /iwbep/cl_mgw_req_filter.

  "Получение фильтров
  TRY .
      lo_filter ?= io_tech_request_context->get_filter( ).
    CATCH /iwbep/cx_mgw_med_exception .
  ENDTRY.


  DATA(lt_prop) = lo_filter->get_entity_type( )->get_properties( ).

  LOOP AT it_filter_select_options INTO DATA(ls_filter).

    READ TABLE lt_prop
      ASSIGNING FIELD-SYMBOL(<ls_prop>)
      WITH KEY name = ls_filter-property.
    CHECK sy-subrc = 0.

    ls_filter-property = <ls_prop>-technical_name.

    DATA(lv_property) = ls_filter-property.
    TRANSLATE lv_property TO UPPER CASE.
    ASSIGN COMPONENT ls_filter-property OF STRUCTURE es_input_data TO FIELD-SYMBOL(<lv_input>).
    CHECK sy-subrc = 0.

*    lo_filter->/iwbep/if_mgw_req_filter~convert_select_option( EXPORTING is_select_option = ls_filter
*                                                               IMPORTING et_select_option = lr_filter ).
*    READ TABLE lr_filter ASSIGNING FIELD-SYMBOL(<ls_filter>) INDEX 1.

    READ TABLE ls_filter-select_options
        ASSIGNING FIELD-SYMBOL(<ls_filter>)
        INDEX 1.

    IF sy-subrc = 0.
      <lv_input> = <ls_filter>-low.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


  METHOD get_instance.
    IF not mv_assistent IS BOUND.
      mv_assistent = NEW zcl_mss_data_assistent( ).
    ENDIF.

    rv_assistent = mv_assistent.
  ENDMETHOD.


  METHOD get_kvartal.
    CASE iv_date+4(2).
      WHEN '01' OR '02' OR '03'.
        rv_kvart = '1'.
      WHEN '04' OR '05' OR '06'.
        rv_kvart = '2'.
      WHEN '07' OR '08' OR '09'.
        rv_kvart = '3'.
      WHEN '10' OR '11' OR '12'.
        rv_kvart = '4'.
    ENDCASE.
  ENDMETHOD.


  METHOD get_next_status.
    CASE iv_status.
      WHEN 0.
        rv_status = 6.
      WHEN 6. " Утверждение линейным руководителем
        rv_status = 8.
      WHEN 8. " Утверждение руководителем отдела обучения
        rv_status = 10.
      WHEN 10." Итоговое согласование

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD get_penalty.

    DATA:
      lv_end_pen_day TYPE begda,
      lv_start_day   TYPE begda,
      lv_endda       TYPE endda.  "16.08.2022 Джувага К.

    SELECT *
      FROM pa9002
      INTO TABLE @DATA(lt_9002)
      FOR ALL ENTRIES IN @it_pernr
     WHERE pernr = @it_pernr-objid
       AND soldt <= @iv_endda.
*       AND rtype = '01'.                   " 15.08.2022, Изменил Джувага К
*       AND ( recov = '1' OR recov = '2' ). " 19.07.2022, Изменил Джувага К - проверка поля Взыскание не нужна в запросе

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    DELETE lt_9002 WHERE recov = ''.
    DELETE lt_9002 WHERE soldt IS INITIAL.  " 16.08.2022 Джувага К. - взыскание начинает действовать от soldt
    CHECK lt_9002 IS NOT INITIAL.           " 19.07.2022, Изменил Джувага К - а так то, проверка нужна :), поле должно бьть заполнено

    LOOP AT lt_9002 ASSIGNING FIELD-SYMBOL(<fs_9002>).

      READ TABLE et_pen
          ASSIGNING FIELD-SYMBOL(<fs_pen>)
          WITH KEY pernr = <fs_9002>-pernr.

      "[BEG] 16.08.2022 Джувага К. Расчитаем окончание периода взыскания и проверим что период взыскание пересекается с расматриваемым периодом
      lv_endda = iv_endda.

      " Если конец периода приходит первым днем следуещего месяца, изменим его на последний день месяца расмматриваемого периода
      IF lv_endda+6(2) = '01'.
        lv_endda = lv_endda - 1.
      ENDIF.


      IF <fs_9002>-eardt IS INITIAL.

        lv_start_day = <fs_9002>-soldt.

        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
          EXPORTING
            date      = lv_start_day
            days      = 0
            months    = 0
            signum    = '+'
            years     = 1
          IMPORTING
            calc_date = lv_end_pen_day.

      ELSE.

        lv_end_pen_day = <fs_9002>-eardt.

      ENDIF.

      " Если окончание периода взыскания в прошлом, то игнорим эту запись
      IF lv_end_pen_day < iv_begda.
        CONTINUE.
      ENDIF.
      "[END] 16.08.2022 Джувага К.

*      IF sy-subrc <> 0.
      APPEND INITIAL LINE TO et_pen ASSIGNING <fs_pen>.
*      ENDIF.

      <fs_pen>-pen_recov = <fs_9002>-recov.
      <fs_pen>-pernr     = <fs_9002>-pernr.

      IF <fs_9002>-soldt <= iv_begda.
        <fs_pen>-begda = iv_begda.
      ELSEIF <fs_9002>-soldt >= iv_begda
        AND ( <fs_pen>-begda >= <fs_9002>-soldt
         OR <fs_pen>-begda IS INITIAL ).

        <fs_pen>-begda = <fs_9002>-soldt.

      ENDIF.

      "[BEG] 16.08.2022 Джувага К. - измененил алгоритм определения даты окончания выводимого на экран периода взыскания
      IF lv_end_pen_day <= lv_endda.
        <fs_pen>-endda = lv_end_pen_day.
      ELSE.
        <fs_pen>-endda = lv_endda.
      ENDIF.

*      IF <fs_9002>-eardt IS INITIAL.
*
*        lv_start_day = <fs_9002>-soldt.
*
*        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*          EXPORTING
*            date      = lv_start_day
*            days      = 0
*            months    = 0
*            signum    = '+'
*            years     = 1
*          IMPORTING
*            calc_date = lv_end_pen_day.
*
*        IF lv_end_pen_day >= iv_begda
*          AND lv_end_pen_day <= lv_endda.
**          AND lv_end_pen_day <= iv_endda.  " 16.08.2022 Джувага К.
*
*          <fs_pen>-endda = lv_end_pen_day.
*
*        ELSEIF lv_end_pen_day >= iv_begda
*          AND lv_end_pen_day >= lv_endda.
**          AND lv_end_pen_day >= iv_endda.  " 16.08.2022 Джувага К.
*
*          <fs_pen>-endda = lv_endda.
**          <fs_pen>-endda = iv_endda.       " 16.08.2022 Джувага К.
*
*        ENDIF.
*
*      ELSE.
*
*        IF <fs_9002>-eardt >= lv_endda.
**        IF <fs_9002>-eardt >= iv_endda.    " 16.08.2022 Джувага К.
*
*          <fs_pen>-endda = lv_endda.
**          <fs_pen>-endda = iv_endda.       " 16.08.2022 Джувага К.
*
*        ELSEIF <fs_9002>-eardt <= iv_endda
*          AND ( <fs_pen>-endda <= <fs_9002>-eardt
*          AND <fs_9002>-eardt >= iv_begda ).
*
*          <fs_pen>-endda = <fs_9002>-eardt.
*
*        ENDIF.
*
*      ENDIF.
      "[END] 16.08.2022 Джувага К. изменение
    ENDLOOP.

    DELETE et_pen WHERE endda IS INITIAL.

  ENDMETHOD.


  METHOD get_pernr.
    DATA(lv_usrid) = iv_usrid. TRANSLATE lv_usrid TO UPPER CASE.
    IF iv_usrid IS NOT INITIAL.
      SELECT SINGLE  pa0105~pernr
                   , pa0000~stat2
               INTO @DATA(lv_pernr)
               FROM pa0105
               LEFT OUTER JOIN pa0000 ON pa0000~pernr = pa0105~pernr
               WHERE pa0105~subty  = @iv_subty
                 AND pa0105~begda <= @iv_begda
                 AND pa0105~endda >= @iv_begda
                 AND pa0105~usrid  = @lv_usrid
                 AND pa0000~begda <= @iv_begda
                 AND pa0000~endda >= @iv_begda.
    ELSEIF iv_snils IS NOT INITIAL.
      SELECT SINGLE  pa0290~pernr
                   , pa0000~stat2
                INTO @lv_pernr
                FROM pa0290
                LEFT OUTER JOIN pa0000 ON pa0000~pernr = pa0290~pernr
                WHERE pa0290~subty  = @iv_subty2
                  AND pa0290~begda <= @iv_begda
                  AND pa0290~endda >= @iv_begda
                  AND pa0290~nomer  = @iv_snils
                  AND pa0000~begda <= @iv_begda
                  AND pa0000~endda >= @iv_begda.
    ENDIF.

    rv_pernr = lv_pernr-pernr.
    ev_stat2 = lv_pernr-stat2.
  ENDMETHOD.


  METHOD get_pernr_0105.
    SELECT SINGLE usrid, usrid_long FROM pa0105 INTO @DATA(ls_pa0105) WHERE pernr  = @iv_pernr
                                                                        AND begda <= @iv_begda
                                                                        AND endda >= @iv_begda
                                                                        AND subty =  @iv_subty
                                                                        AND sprps =  @abap_false.

    IF sy-subrc <> 0.
      SELECT SINGLE usrid, usrid_long FROM pa0105 INTO @ls_pa0105 WHERE pernr  = @iv_pernr
                                                                    AND begda <= @iv_begda
                                                                    AND endda >= @iv_begda
                                                                    AND subty =  '0010'
                                                                    AND sprps =  @abap_false.
    ENDIF.
    rv_usrid = ls_pa0105-usrid.

    IF ls_pa0105-usrid_long IS NOT INITIAL.
      rv_usrid = ls_pa0105-usrid_long.
    ENDIF.
  ENDMETHOD.


  METHOD get_pernr_by_plans.
    me->get_struc( EXPORTING iv_objid = iv_plans
                             iv_otype = 'S'
                             iv_begda  = iv_begda
                             iv_endda  = iv_begda
                             iv_wegid  = 'O-O-S-P'
                             iv_acheck = abap_false
                   IMPORTING et_struc  = DATA(lt_struc) ).

    DELETE lt_struc WHERE otype <> 'P'.

    CHECK lt_struc IS NOT INITIAL.

    READ TABLE lt_struc ASSIGNING FIELD-SYMBOL(<lt_struc>) INDEX 1.
    rv_pernr = <lt_struc>-objid.
  ENDMETHOD.


  METHOD get_pernr_fio.
    SELECT SINGLE nachn, vorna, midnm INTO @DATA(ls_name) FROM pa0002 WHERE pernr  = @iv_pernr
                                                                        AND begda <= @iv_begda
                                                                        AND endda >= @iv_begda.

    rv_fio = ls_name-nachn && ` ` && ls_name-vorna && ` ` && ls_name-midnm.

     DEFINE add_str.
      IF &1 is INITIAL.
        &1 = &2.
      else.
        &1 = &1 && ` ` && &2.
      ENDIF.
    END-OF-DEFINITION.

    DATA: lt_str TYPE TABLE OF text100.

    SPLIT rv_fio AT space INTO TABLE lt_str.

    CLEAR: rv_fio.

    LOOP AT lt_str ASSIGNING FIELD-SYMBOL(<fs_str>).
      TRANSLATE <fs_str> TO LOWER CASE.
      TRANSLATE <fs_str>(1) TO UPPER CASE.
      add_str rv_fio <fs_str>.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_pernr_ruk.
    me->get_struc( EXPORTING iv_objid = iv_pernr
                             iv_otype = 'P'
                             iv_begda  = iv_begda
                             iv_endda  = iv_begda
                             iv_wegid  = 'P-S-O-O'
                             iv_acheck = abap_false
                   IMPORTING et_struc  = DATA(lt_struc) ).

    DATA(lv_lines) = lines( lt_struc ).

    lv_lines = lv_lines - iv_level + 1.

    CHECK lv_lines > 0.

    READ TABLE lt_struc ASSIGNING FIELD-SYMBOL(<lt_struc>) INDEX lv_lines.
    CHECK <lt_struc> IS ASSIGNED AND sy-subrc = 0 AND <lt_struc>-otype = 'O'.

    me->get_struc( EXPORTING iv_objid = CONV #( <lt_struc>-objid )
                             iv_otype = <lt_struc>-otype
                             iv_begda  = iv_begda
                             iv_endda  = iv_begda
                             iv_wegid  = 'Z_OIVRUC'
                             iv_acheck = abap_false
                   IMPORTING et_struc  = DATA(zt_struc) ).

    READ TABLE zt_struc ASSIGNING FIELD-SYMBOL(<zt_struc>) WITH KEY otype = 'P'.
    CHECK <zt_struc> IS ASSIGNED AND sy-subrc = 0.

    rv_pernr = <zt_struc>-objid.
  ENDMETHOD.


  METHOD get_pernr_ruk0.
    me->get_struc( EXPORTING iv_objid = iv_pernr
                             iv_otype = 'P'
                             iv_begda  = iv_begda
                             iv_endda  = iv_begda
                             iv_wegid  = 'P-S-O-O'
                             iv_acheck = abap_false
                   IMPORTING et_struc  = DATA(lt_struc) ).

*    DATA(lv_lines) = lines( lt_struc ).
*
*    lv_lines = lv_lines - iv_level + 1.
*
*    CHECK lv_lines > 0.

    LOOP AT  lt_struc ASSIGNING FIELD-SYMBOL(<lt_struc>) WHERE otype = 'O'.

      me->get_struc( EXPORTING iv_objid = CONV #( <lt_struc>-objid )
                               iv_otype = <lt_struc>-otype
                               iv_begda  = iv_begda
                               iv_endda  = iv_begda
                               iv_wegid  = 'Z_OIVRUC'
                               iv_acheck = abap_false
                     IMPORTING et_struc  = DATA(zt_struc) ).

      READ TABLE zt_struc ASSIGNING FIELD-SYMBOL(<zt_struc>) WITH KEY otype = 'P'.
      CHECK <zt_struc> IS ASSIGNED AND sy-subrc = 0.
      CHECK <zt_struc>-objid NE iv_pernr."Шаталов Б.А. Если ТН является руководитлем в своей ошке, то он не будет себе согласующим

      rv_pernr = <zt_struc>-objid.
      RETURN.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_snils.
*   PA0290-NOMER, при условии что PA0290-SUBTY=802 и PA0290-ENDDA>=Сегодня и PA0290-BEGDA<=Сегодня
    DATA: lt_p0290 TYPE TABLE OF p0290
        .

    DATA(lo_data) = NEW zcl_hr_get_data( ).
    lo_data->read_pa_infty( EXPORTING i_pernr = i_pernr
                                      i_begda = i_date
                                      i_endda = i_date
                                      i_infty = '0290'
                                      i_subty = '802'
                            IMPORTING e_pnnnn = lt_p0290 ).
    READ TABLE lt_p0290 INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_p0290>).
    IF sy-subrc EQ 0.
      r_snils = <fs_p0290>-nomer.
    ENDIF.
  ENDMETHOD.


  METHOD get_struc.
    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING
        act_otype       = iv_otype
        act_objid       = iv_objid
        act_wegid       = iv_wegid
        act_plvar       = mv_plvar
        act_begda       = iv_begda
        act_endda       = iv_endda
        act_tflag       = iv_tflag
        act_vflag       = iv_vflag
        authority_check = iv_acheck
      TABLES
        result_objec    = et_objec
        result_struc    = et_struc
      EXCEPTIONS
        no_plvar_found  = 1
        no_entry_found  = 2
        OTHERS          = 3.
  ENDMETHOD.


  METHOD get_user_fio.
    DATA(lv_pernr) = me->get_pernr( iv_usrid = CONV #( iv_usrid ) ).

    rv_name = me->get_pernr_fio( iv_pernr = lv_pernr iv_begda = iv_begda ).

    CHECK rv_name IS INITIAL.
    MESSAGE i002(zhr_mss) WITH iv_usrid INTO rv_name.
  ENDMETHOD.


  METHOD GET_USER_ORGEH.
    DATA(lv_pernr) = me->get_pernr( iv_usrid = CONV #( iv_usrid ) ).

    SELECT SINGLE nachn, vorna, midnm INTO @DATA(ls_name) FROM pa0002 WHERE pernr  = @lv_pernr
                                                                        AND begda <= @iv_begda
                                                                        AND endda >= @iv_begda.

    rv_name = ls_name-nachn && ` ` && ls_name-vorna && ` ` && ls_name-midnm.

    CHECK rv_name is INITIAL.
    MESSAGE i002(zhr_mss) WITH iv_usrid INTO rv_name.
  ENDMETHOD.


  METHOD get_werks_by_pernr.

    DATA:
          lt_p0001 TYPE TABLE OF p0001.

    DATA(lo_data) = NEW zcl_hr_get_data( ).

    lo_data->read_pa_infty(
        EXPORTING
          i_pernr = iv_pernr
          i_begda = sy-datum
          i_endda = sy-datum
          i_infty = '0001'
        IMPORTING
          e_pnnnn = lt_p0001 ).

    READ TABLE lt_p0001
        ASSIGNING FIELD-SYMBOL(<fs_p0001>)
        INDEX 1.

    IF sy-subrc = 0.
      ev_werks = <fs_p0001>-werks.
    ENDIF.

  ENDMETHOD.


  METHOD get_zamestitel_list.

    DATA:
      lv_objid     TYPE hrobjid,
      lt_struc_all TYPE struc_t.


    DATA: lv_plans_txt TYPE string.
    DATA: lt_p0105 TYPE STANDARD TABLE OF p0105.

    DATA(lv_pernr) = get_pernr( iv_usrid = CONV #( iv_main_user )
                                iv_subty = gc_subty_9001 ).

    lv_objid = lv_pernr.

    zcl_hr_data=>get_plans( EXPORTING i_pernr = lv_pernr
                                      i_begda = sy-datum
                                      i_endda = sy-datum
                            IMPORTING e_plans = DATA(lv_plans) ).

    me->get_struc( EXPORTING iv_objid = lv_objid
                             iv_otype = 'P'
                             iv_begda  = sy-datum
                             iv_endda  = sy-datum
                             iv_wegid  = 'ZMSS_Z99'
                             iv_acheck = abap_false
                   IMPORTING et_struc  = DATA(lt_struc_indirect) ).

    me->get_struc( EXPORTING iv_objid = lv_plans
                         iv_otype = 'S'
                         iv_begda  = sy-datum
                         iv_endda  = sy-datum
                         iv_wegid  = 'A012'
                         iv_acheck = abap_false
               IMPORTING et_struc  = DATA(lt_struc_direct) ).

    APPEND LINES OF lt_struc_indirect TO lt_struc_all.
    APPEND LINES OF lt_struc_direct TO lt_struc_all.

    LOOP AT lt_struc_all ASSIGNING FIELD-SYMBOL(<ls_struc>) WHERE otype = 'O'.

      me->get_struc( EXPORTING iv_objid = CONV #( <ls_struc>-objid )
                               iv_otype = 'O'
                               iv_begda  = sy-datum
                               iv_endda  = sy-datum
                               iv_wegid  = 'O-O-S-P '
                               iv_acheck = abap_false
                     IMPORTING et_struc  = DATA(lt_struc1) ).

      DESCRIBE TABLE lt_struc1.
      IF sy-tfill = 0 .
        RETURN.
      ENDIF.

      DATA(lt_struc_s) = lt_struc1.
      DELETE lt_struc_s WHERE otype <> 'S'.
      SORT lt_struc_s BY seqnr.

      LOOP AT lt_struc1 ASSIGNING FIELD-SYMBOL(<ls_struc1>) WHERE otype = 'P'.

        " Имя пользователя
        zcl_hr_data=>read_pa_infty( EXPORTING i_pernr = CONV #( <ls_struc1>-objid )
                                              i_infty = '0105'
                                              i_subty = '9001'
                                              i_begda = sy-datum
                                              i_endda = sy-datum
                                    IMPORTING e_pnnnn = lt_p0105 ).

        READ TABLE lt_p0105 ASSIGNING FIELD-SYMBOL(<ls_p0105>) INDEX 1.

        IF sy-subrc = 0 AND <ls_p0105>-usrid = iv_main_user.
          CONTINUE.
        ENDIF.

        DATA(lv_fio) = zcl_hr_data=>get_pernr_fio( i_pernr = CONV #( <ls_struc1>-objid )
                                                   i_begda = sy-datum ).
        " наименование ШД
        READ TABLE lt_struc_s ASSIGNING FIELD-SYMBOL(<ls_struc_s>) WITH KEY seqnr = <ls_struc1>-pup BINARY SEARCH.
        IF sy-subrc = 0.

          zcl_hr_data=>get_name_plans( EXPORTING i_plans = CONV #( <ls_struc_s>-objid )
                                                 i_date = sy-datum
                                       IMPORTING e_lname = lv_plans_txt ).

        ENDIF.

        APPEND INITIAL LINE TO et_pernrs_tab ASSIGNING FIELD-SYMBOL(<ls_row>).
        IF <ls_row> IS ASSIGNED.
          <ls_row>-main_user = iv_main_user.
          <ls_row>-pernr =   CONV #( <ls_struc1>-objid ).
          <ls_row>-plans_txt = lv_plans_txt.
          IF <ls_p0105> IS ASSIGNED.
            <ls_row>-userid = <ls_p0105>-usrid.
          ENDIF.

          SPLIT lv_fio AT space INTO <ls_row>-nachn <ls_row>-vorna <ls_row>-midnm    .
        ENDIF.
      ENDLOOP.

      SORT et_pernrs_tab BY nachn.
*    ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD hide_org_struc_empty_elem.

    DATA: lv_pup TYPE i.

    LOOP AT ct_org_struc ASSIGNING FIELD-SYMBOL(<fs_org_struc>)
          WHERE object CA 'S'.

      <fs_org_struc>-stat = 'V'.

      READ TABLE it_sort_struc_all ASSIGNING FIELD-SYMBOL(<fs_org_struc_sort>)
            WITH KEY objid = <fs_org_struc>-object-objid.

      lv_pup = <fs_org_struc_sort>-pup.

      UNASSIGN <fs_org_struc_sort>.

      WHILE lv_pup <> 0.

        READ TABLE it_sort_struc_all ASSIGNING <fs_org_struc_sort>
            WITH KEY seqnr = lv_pup BINARY SEARCH.

        IF sy-subrc = 0.

          lv_pup = <fs_org_struc_sort>-pup.

          LOOP AT ct_org_struc ASSIGNING FIELD-SYMBOL(<fs_org_struc_stat>)
                WHERE object CS <fs_org_struc_sort>-objid.
            <fs_org_struc_stat>-stat = 'V'.
          ENDLOOP.

        ENDIF.

      ENDWHILE.
    ENDLOOP.

    DELETE ct_org_struc
        WHERE stat IS INITIAL AND object NA 'P'.

  ENDMETHOD.


METHOD is_argus.
  DATA: lt_actor TYPE TABLE OF swhactor.
  CALL FUNCTION 'RH_GET_ACTORS'
    EXPORTING
      act_object = CONV rhobjects-object( |AC91000003| )
    TABLES
      actor_tab  = lt_actor
    EXCEPTIONS
      OTHERS     = 0.
  rv_is = boolc( line_exists( lt_actor[ objid = iv_pernr ] ) ).
ENDMETHOD.


  METHOD is_redundant.
    DATA: lt_p1014 TYPE TABLE OF p1014.

* -- Должность
    CALL FUNCTION 'RH_READ_INFTY'
      EXPORTING
        authority            = ' '
        with_stru_auth       = ' '
        plvar                = '01'
        otype                = iv_otype
        objid                = iv_objid
        infty                = '1014'
        begda                = iv_begda
        endda                = iv_endda
      TABLES
        innnn                = lt_p1014
      EXCEPTIONS
        all_infty_with_subty = 1
        nothing_found        = 2
        no_objects           = 3
        wrong_condition      = 4
        wrong_parameters     = 5
        OTHERS               = 6.

    IF sy-subrc = 0.

      READ TABLE lt_p1014 INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_p0014>).
      IF sy-subrc EQ 0 AND <fs_p0014>-redun IS NOT INITIAL.
        rv_ok     = abap_true.
      ELSE.
        rv_ok     = abap_false.
      ENDIF.
    ELSE.
      rv_ok     = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD write_ipr_sta.
    DATA: ls_ess_ipr_sta TYPE zthr_ess_ipr_sta
        .
    ls_ess_ipr_sta-req_id = iv_req_id.
    ls_ess_ipr_sta-datum  = iv_datum.
    ls_ess_ipr_sta-uzeit  = iv_uzeit.
    ls_ess_ipr_sta-pernr  = iv_pernr.
    ls_ess_ipr_sta-status = iv_status.

    SELECT  seqnr INTO TABLE @data(lt_seqnr) FROM zthr_ess_ipr_sta WHERE req_id = @iv_req_id
                                                                    AND datum   = @iv_datum
                                                                    AND uzeit   = @iv_uzeit ORDER BY seqnr.
    LOOP AT lt_seqnr ASSIGNING FIELD-SYMBOL(<lt_seqnr>).
      ls_ess_ipr_sta-seqnr = <lt_seqnr>-seqnr.
    ENDLOOP.

    IF sy-subrc = 0.
      ADD 1 TO ls_ess_ipr_sta-seqnr.
    ENDIF.

    MODIFY zthr_ess_ipr_sta FROM ls_ess_ipr_sta.

    CHECK iv_commit IS NOT INITIAL.
    COMMIT WORK AND WAIT.
  ENDMETHOD.


METHOD ws_competition_request_create.

  DATA: lt_object    TYPE STANDARD TABLE OF p9103,
        ls_comment   TYPE zhrt_9103_commt,
        lt_mail_to   TYPE bcsy_smtpa,
        lt_mapping   TYPE zttmail_mapping,
        lv_mail_from TYPE adr6-smtp_addr.

  MOVE-CORRESPONDING is_input_data TO es_output_data.

*  21.03.2020
  IF is_input_data-entrance_user IS NOT INITIAL.
    DATA(lv_pernr) = get_pernr( iv_usrid = CONV #( is_input_data-entrance_user ) ).
  ELSE.
    lv_pernr = get_pernr( iv_usrid = CONV #( is_input_data-main_user ) ).
  ENDIF.
  "Заполение комментария
  IF is_input_data-comment IS NOT INITIAL.

    TRY.
        ls_comment-tabnr = cl_system_uuid=>create_uuid_c32_static( ).
        ls_comment-seqnr  = 1.
        ls_comment-status = gc_kstatus_create.
        ls_comment-author = lv_pernr.
        CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP ls_comment-timestamp TIME ZONE 'UTC'.
        ls_comment-text = is_input_data-comment.
      CATCH cx_uuid_error.
    ENDTRY.

  ENDIF.

  "Заполнение ИТ
  APPEND INITIAL LINE TO lt_object ASSIGNING FIELD-SYMBOL(<ls_object>).
  <ls_object>-mandt = sy-mandt.
  <ls_object>-plvar = zcl_hr_get_data=>a_plvar .
  <ls_object>-otype = 'S'.
  <ls_object>-objid = is_input_data-objid.
  <ls_object>-infty = '9103'.
  <ls_object>-istat = '1'.
  <ls_object>-begda = sy-datum.
  <ls_object>-endda = '99991231'.
  <ls_object>-status = gc_kstatus_create.
  <ls_object>-author = lv_pernr.
  <ls_object>-kadrz  = is_input_data-kadrz.
  <ls_object>-reg_accept  = is_input_data-reg_accept.
  <ls_object>-tabnr  = ls_comment-tabnr.

  CALL FUNCTION 'RH_INSERT_INFTY'
    EXPORTING
      vtask               = 'B'
      authy               = abap_true
    TABLES
      innnn               = lt_object
    EXCEPTIONS
      no_authorization    = 1
      error_during_insert = 2
      repid_form_initial  = 3
      corr_exit           = 4
      begda_greater_endda = 5
      OTHERS              = 6.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      INTO es_output_data-message.
    es_output_data-msgid = sy-msgty.
*    es_output_data-mess_type = sy-msgty.
    RETURN.
  ENDIF.
  CALL FUNCTION 'RH_EXIST_OBJECT'
    EXPORTING
      plvar     = <ls_object>-plvar
      otype     = <ls_object>-otype
      objid     = <ls_object>-objid
*     REALO     = ' '
*  IMPORTING
*     EXTERNAL_OBJECT       =
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
  IF sy-subrc = 1 .
    MESSAGE e021(zhr_mss) WITH <ls_object>-objid INTO es_output_data-message.
    es_output_data-msgid = 'E'.
*    es_output_data-mess_type = sy-msgty.
    RETURN.
  ENDIF.

  "Обновление данных
  IF ls_comment IS NOT INITIAL.
    MODIFY zhrt_9103_commt FROM ls_comment.
  ENDIF.

  CALL FUNCTION 'RH_UPDATE_DATABASE'
    EXPORTING
      vtask     = 'D'
    EXCEPTIONS
      corr_exit = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      INTO es_output_data-message.
    es_output_data-msgid = sy-msgty.
*    es_output_data-mess_type = sy-msgty.
    RETURN.
  ENDIF.

  MESSAGE s000(cl) WITH 'Заявка на конкурс создана' INTO es_output_data-message.
  es_output_data-msgid = 'S'.
*  es_output_data-mess_type = sy-msgty.

  "---------- Отправка письма ----------"

  lv_mail_from = _fill_comp_mail_from( ).

  _fill_comp_mail_to( IMPORTING et_to = lt_mail_to ).

  _fill_comp_mapping( EXPORTING is_input_data = is_input_data
                                is_p9103      = <ls_object>
                                iv_pernr      = lv_pernr
                      IMPORTING et_mapping    = lt_mapping ).

  zcl_send_email=>send_mail( EXPORTING itd_to          = lt_mail_to
                                       iv_sender_email = lv_mail_from
                                       iv_textname     = 'ZHR_COMPET_CREATE'
                                       itd_mapping     = lt_mapping
                                       iv_immediately  = abap_false ).
  "iv_html         = 'X' ).

ENDMETHOD.


METHOD ws_competition_request_withdr.

  DATA: lt_p9103     TYPE STANDARD TABLE OF p9103,
        ls_comment   TYPE zhrt_9103_commt,
        lo_hr_data   TYPE REF TO zcl_hr_get_data,
        lv_seqnr     TYPE zhrt_9103_commt-seqnr,
        lt_mail_to   TYPE bcsy_smtpa,
        lt_mapping   TYPE zttmail_mapping,
        lv_mail_from TYPE adr6-smtp_addr.

  MOVE-CORRESPONDING is_input_data TO es_output_data.

  lo_hr_data = NEW #( ).
  lo_hr_data->read_om_infty( EXPORTING i_otype = 'S'
                                       i_objid = CONV #( is_input_data-objid )
                                       i_infty = '9103'
                                       i_begda = sy-datum
                                       i_endda = sy-datum
                             IMPORTING e_pnnnn = lt_p9103 ).

  READ TABLE lt_p9103 ASSIGNING FIELD-SYMBOL(<ls_p9103>) INDEX 1.
  IF sy-subrc <> 0.
    MESSAGE e000(cl) WITH 'На текущую дату запись в ИТ9103 не найдена' INTO es_output_data-message.
    es_output_data-msgid = 'E'.
    RETURN.
  ENDIF.

  "Блокируем объект
  CALL FUNCTION 'HR_ENQUEUE_OBJECT'
    EXPORTING
      plvar            = <ls_p9103>-plvar
      otype            = <ls_p9103>-otype
      objid            = <ls_p9103>-objid
    EXCEPTIONS
      enqueue_failed   = 1
      objid_is_initial = 2
      illegal_otype    = 3
      internal_error   = 4
      OTHERS           = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      INTO es_output_data-message.
     es_output_data-msgid = 'E'.
*    es_output_data-mess_type = sy-msgty.
    RETURN.
  ENDIF.

  DO 1 TIMES.
*20/02/2020    DATA(lv_pernr) = get_pernr( iv_usrid = CONV #( is_input_data-entrance_user ) ).
    DATA(lv_pernr) = get_pernr( iv_usrid = CONV #( is_input_data-main_user ) ).
    "Заполение комментария
    IF is_input_data-comment IS NOT INITIAL.

      SELECT SINGLE MAX( seqnr )
        INTO lv_seqnr
        FROM zhrt_9103_commt
        WHERE tabnr = <ls_p9103>-tabnr
        GROUP BY tabnr.

      TRY.
          IF <ls_p9103>-tabnr IS INITIAL.
            ls_comment-tabnr = cl_system_uuid=>create_uuid_c32_static( ).
          ELSE.
            ls_comment-tabnr = <ls_p9103>-tabnr.
          ENDIF.
          ls_comment-seqnr  = lv_seqnr + 1.
          ls_comment-status = gc_kstatus_withdraw.
          ls_comment-author = lv_pernr.
          CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP ls_comment-timestamp TIME ZONE 'UTC'.
          ls_comment-text = is_input_data-comment.
        CATCH cx_uuid_error.
      ENDTRY.

    ENDIF.

    "Заполнение ИТ
    <ls_p9103>-status = gc_kstatus_withdraw.
    DELETE lt_p9103 FROM 2.

    CALL FUNCTION 'RH_UPDATE_INFTY'
      EXPORTING
        vtask               = 'B'
        authy               = 'X'
      TABLES
        innnn               = lt_p9103
      EXCEPTIONS
        error_during_update = 1
        no_authorization    = 2
        repid_form_initial  = 3
        corr_exit           = 4
        OTHERS              = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        INTO es_output_data-message.
       es_output_data-msgid = 'E'.
*      es_output_data-mess_type = sy-msgty.
      EXIT.
    ENDIF.


    "Обновление данных
    IF ls_comment IS NOT INITIAL.
      MODIFY zhrt_9103_commt FROM ls_comment.
    ENDIF.
*
    CALL FUNCTION 'RH_UPDATE_DATABASE'
      EXPORTING
        vtask     = 'D'
      EXCEPTIONS
        corr_exit = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        INTO es_output_data-message.
       es_output_data-msgid = 'E'.
*      es_output_data-mess_type = sy-msgty.
      EXIT.
    ENDIF.

  ENDDO.

  CALL FUNCTION 'HR_DEQUEUE_OBJECT'
    EXPORTING
      plvar            = <ls_p9103>-plvar
      otype            = <ls_p9103>-otype
      objid            = <ls_p9103>-objid
    EXCEPTIONS
      illegal_otype    = 1
      objid_is_initial = 2
      internal_error   = 3
      OTHERS           = 4.

  MESSAGE s000(cl) WITH 'Заявка на конкурс отозвана' INTO es_output_data-message.
   es_output_data-msgid = 'S'.
*  es_output_data-mess_type = sy-msgty.

  "---------- Отправка письма ----------"

  lv_mail_from = _fill_comp_mail_from( ).

  _fill_comp_mail_to( EXPORTING it_pernr = value #( ( <ls_p9103>-bkper ) )
                      IMPORTING et_to    = lt_mail_to ).

  _fill_comp_mapping( EXPORTING is_input_data = is_input_data
                                is_p9103      = <ls_p9103>
                                iv_pernr      = lv_pernr
                      IMPORTING et_mapping    = lt_mapping ).

  zcl_send_email=>send_mail( EXPORTING itd_to          = lt_mail_to
                                       iv_sender_email = lv_mail_from
                                       iv_textname     = 'ZHR_COMPET_WITHDRAW'
                                       itd_mapping     = lt_mapping
                                       iv_immediately  = abap_false ).

ENDMETHOD.


  METHOD ws_get_attest_xml.
    DATA: lr_peri   TYPE RANGE OF datum,
          lv_date_c TYPE char10,
          lv_kvart  TYPE zehr_pa_kvart.

    FIELD-SYMBOLS <ls_peri> LIKE LINE OF lr_peri.

    "табельный
    DATA(lv_pernr) = me->get_pernr( iv_usrid = CONV #( iv_euser ) iv_begda = iv_begda ).

    IF lv_pernr IS INITIAL.
      RAISE no_pernr.
    ENDIF.


    me->check_head_is_second_lvl(
            EXPORTING
              iv_pernr = lv_pernr
              iv_begda = iv_begda
              iv_endda = iv_endda
            IMPORTING
              ev_is_second_lvl = DATA(lv_is_second_lvl)
              et_struc = DATA(lt_struc) ).

    IF lv_is_second_lvl IS INITIAL.
      CALL FUNCTION 'RH_STRUC_GET'
        EXPORTING
          act_otype      = 'P'
          act_objid      = CONV hrobjid( lv_pernr )
          act_wegid      = 'P-S-O-O'
          act_plvar      = '01'
          act_begda      = iv_begda
          act_endda      = iv_endda
        TABLES
          result_struc   = lt_struc
        EXCEPTIONS
          no_plvar_found = 1
          no_entry_found = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

    ENDIF.

    DELETE lt_struc WHERE NOT otype = 'O'.

    READ TABLE lt_struc INTO DATA(ls_struc) INDEX 1.
    CHECK sy-subrc = 0.

    APPEND INITIAL LINE TO lr_peri ASSIGNING <ls_peri>.

    <ls_peri>-sign   = 'I'.
    <ls_peri>-option = 'EQ'.
    <ls_peri>-low    = sy-datum.

    lv_kvart = get_kvartal( sy-datum ).


    SUBMIT zhr_pa_d029
     WITH p_objid = ls_struc-objid
     WITH p_kvart = lv_kvart
     "WITH p_var1  = abap_false
     WITH p_var2  = abap_true
     WITH p_xml   = abap_true
     AND RETURN.


    IMPORT lv_xstring TO e_xstring FROM MEMORY ID zcl_z_mss_attest_dpc_ext=>c_memory_id.
    FREE MEMORY ID zcl_z_mss_attest_dpc_ext=>c_memory_id.

  ENDMETHOD.


  METHOD ws_get_compmaindata.
    DATA:  lo_hr_data TYPE REF TO zcl_hr_get_data.
    "табельный
    DATA(lv_pernr) = me->get_pernr( iv_usrid = CONV #( iv_euser )   ).

    IF lv_pernr IS INITIAL.
      RAISE no_pernr.
    ENDIF.
*    " руководитель второй орг ед?
    me->check_head_is_second_lvl(
          EXPORTING
            iv_pernr = lv_pernr
            iv_begda = sy-datum
            iv_endda = sy-datum
          IMPORTING
            ev_is_second_lvl = DATA(lv_is_mss)
            et_struc         = DATA(lt_struc) ).

    IF lv_is_mss = abap_false.
      RAISE no_sec_lev_ruk.
    ENDIF.

    DELETE lt_struc WHERE NOT otype = 'O'.

    READ TABLE lt_struc INTO DATA(ls_struc) INDEX 1.

    me->get_struc( EXPORTING iv_objid  = CONV hrobjid( ls_struc-objid )
                             iv_otype  = ls_struc-otype
*                             iv_begda  = iv_begda
*                             iv_endda  = iv_endda
                             iv_wegid  = 'O-O-S' "-P
                             iv_acheck = abap_false
                   IMPORTING et_struc  = lt_struc ).

    DATA: lt_struc_sort TYPE SORTED TABLE OF struc WITH NON-UNIQUE KEY seqnr
        , lt_object     TYPE hrobject_t
        , lt_pernr      TYPE hrobject_t
        , ls_pa0002     TYPE pa0002
        , lt_p9103      TYPE TABLE OF p9103
        , lt_p1007      TYPE TABLE OF p1007
        , lt_idd07v TYPE TABLE OF  dd07v
        , ls_object     LIKE LINE OF lt_object
        .

    ls_object-plvar = mv_plvar.

    LOOP AT lt_struc ASSIGNING FIELD-SYMBOL(<lt_struc>).
      INSERT <lt_struc> INTO TABLE lt_struc_sort.
      ls_object-otype = <lt_struc>-otype.
      ls_object-objid = <lt_struc>-objid.
      CASE ls_object-otype.
        WHEN 'S'.
          COLLECT ls_object INTO lt_object.
      ENDCASE.
    ENDLOOP.

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'ZD_KSTATUS'   "<-- Your Domain Here
        text           = 'X'
        langu          = sy-langu
      TABLES
        dd07v_tab      = lt_idd07v
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.
    lo_hr_data = NEW #( ).

    LOOP AT lt_object INTO ls_object .
      APPEND INITIAL LINE TO et_compmaindata ASSIGNING FIELD-SYMBOL(<et_compmaindata>).
      <et_compmaindata>-objid = ls_object-objid.

*     Если сегодня вакансия закрыта (HRP1007-STATUS=2, при условии что HRP1007-BEGDA<=Сегодня и HRP1007-ENDDA>=Сегодня),
*     то выводим передает true, иначе пусто
      lo_hr_data->read_om_infty( EXPORTING i_otype = 'S'
                                           i_objid = CONV #( ls_object-objid )
                                           i_infty = '1007'
                                           i_begda = sy-datum
                                           i_endda = sy-datum
                                 IMPORTING e_pnnnn = lt_p1007 ).
      READ TABLE lt_p1007 INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_p1007>).
      IF sy-subrc EQ 0 AND <fs_p1007>-status EQ '2'.
        <et_compmaindata>-subst = abap_true.
      ENDIF.

      lo_hr_data->read_om_infty( EXPORTING i_otype = 'S'
                                           i_objid = CONV #( ls_object-objid )
                                           i_infty = '9103'
                                           i_begda = sy-datum
                                           i_endda = sy-datum
                                 IMPORTING e_pnnnn = lt_p9103 ).

      READ TABLE lt_p9103 ASSIGNING FIELD-SYMBOL(<ls_p9103>) INDEX 1.
      IF sy-subrc <> 0.
        <et_compmaindata>-status = 'Нет конкурсов'.
        <et_compmaindata>-create = 'X'.
        CONTINUE.
      ELSE.
        <et_compmaindata>-view = 'X'.
        READ TABLE lt_idd07v INTO DATA(ls_idd07v) WITH KEY domvalue_l = <ls_p9103>-status.
        IF sy-subrc EQ 0.
          <et_compmaindata>-status = ls_idd07v-ddtext.
        ENDIF.
        IF <ls_p9103>-status EQ '2' OR <ls_p9103>-status EQ '4'.
          <et_compmaindata>-create = 'X'.
        ENDIF.
        IF ( <ls_p9103>-status EQ '1' OR <ls_p9103>-status EQ '3' ) AND ( <ls_p9103>-ibegd > sy-datum OR <ls_p9103>-ibegd EQ '00000000' OR <ls_p9103>-ibegd IS INITIAL ).
          <et_compmaindata>-withdraw = 'X'.
        ENDIF.
*        IF <ls_p9103>-bkper IS NOT INITIAL.
*          SELECT SINGLE * INTO  ls_pa0002  FROM pa0002 WHERE pernr = <ls_p9103>-bkper.
*          IF sy-subrc = 0.
*            CONCATENATE ls_pa0002-nachn ls_pa0002-vorna ls_pa0002-midnm INTO <et_compmaindata>-bkper SEPARATED BY space.
*          ENDIF.
*        ENDIF.
*        <et_compmaindata>-kadrz = <ls_p9103>-kadrz.
*        <et_compmaindata>-begda = <ls_p9103>-begda.
*        <et_compmaindata>-ibegd = <ls_p9103>-ibegd.
*        <et_compmaindata>-iendd = <ls_p9103>-iendd.
*        <et_compmaindata>-pkond = <ls_p9103>-pkond.
*        <et_compmaindata>-kondt = <ls_p9103>-kondt.
*
*        IF <ls_p9103>-knitg IS NOT INITIAL.
*          SELECT SINGLE stext INTO <et_compmaindata>-knitg FROM zthr_knitgt WHERE knitg EQ <ls_p9103>-knitg AND spras EQ 'RU'.
*        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD WS_GET_COMPMAINDESC.
    data(lo_hr_data) = NEW zcl_hr_get_data( ).

    data: lt_p9103 type TABLE OF p9103
        .

    LOOP AT it_objid INTO data(ls_object) .
      APPEND INITIAL LINE TO et_compmaindesc ASSIGNING FIELD-SYMBOL(<et_compmaindesc>).
      <et_compmaindesc>-objid = ls_object-objid.

      lo_hr_data->read_om_infty( EXPORTING i_otype = 'S'
                                           i_objid = CONV #( ls_object-objid )
                                           i_infty = '9103'
                                           i_begda = sy-datum
                                           i_endda = sy-datum
                                 IMPORTING e_pnnnn = lt_p9103 ).

      READ TABLE lt_p9103 ASSIGNING FIELD-SYMBOL(<ls_p9103>) INDEX 1.
      IF sy-subrc <> 0.

      ELSE.
        IF <ls_p9103>-bkper IS NOT INITIAL.
          SELECT SINGLE nachn, vorna, midnm INTO @data(ls_pa0002) FROM pa0002 WHERE pernr = @<ls_p9103>-bkper.
          IF sy-subrc = 0.
            CONCATENATE ls_pa0002-nachn ls_pa0002-vorna ls_pa0002-midnm INTO <et_compmaindesc>-bkper SEPARATED BY space.
          ENDIF.
        ENDIF.
        <et_compmaindesc>-kadrz = <ls_p9103>-kadrz.
        <et_compmaindesc>-begda = <ls_p9103>-begda.
        <et_compmaindesc>-ibegd = <ls_p9103>-ibegd.
        <et_compmaindesc>-iendd = <ls_p9103>-iendd.
        <et_compmaindesc>-pkond = <ls_p9103>-pkond.
        <et_compmaindesc>-kondt = <ls_p9103>-kondt.

        IF <ls_p9103>-knitg IS NOT INITIAL.
          SELECT SINGLE stext INTO <et_compmaindesc>-knitg FROM zthr_knitgt WHERE knitg EQ <ls_p9103>-knitg AND spras EQ 'RU'.
        ENDIF.
      ENDIF.
    ENDLOOP.

    sort et_compmaindesc by objid.
  ENDMETHOD.


  METHOD ws_get_doc.
    DATA: ls_bor TYPE	borident,
          lv_plvar type plvar
        .

    ls_bor-objkey  = mv_plvar &&  iv_objid.
    ls_bor-objtype = 'PDOTYPE_O'.

    DATA(lo_item) = NEW cl_sobl_bor_item( is_bor = ls_bor ).
    DATA(lo_link) = NEW cl_msg_al_linksrv( ).

    lo_link->gp_attachments = abap_true.

    lo_link->if_link_service~get_item_links( EXPORTING io_bitem            = lo_item
                                                       ip_load_restriction = 0
                                             IMPORTING et_links            = DATA(lt_linkbuff)
                                                       ep_num_of_links     = DATA(lv_num_of_links) ).


    LOOP AT lt_linkbuff ASSIGNING FIELD-SYMBOL(<lt_linkbuff>).

      DATA: content_info TYPE TABLE OF scms_acinf
          , content_char TYPE TABLE OF sdokcntasc
          , content_bin  TYPE TABLE OF sdokcntbin
          .

      CALL FUNCTION 'SCMS_R3DB_IMPORT'
        EXPORTING
          mandt        = sy-mandt
          crep_id      = 'SOFFDB'
          doc_id       = <lt_linkbuff>->gp_inst
        TABLES
          content_info = content_info
          content_txt  = content_char
          content_bin  = content_bin
        EXCEPTIONS
          error_import = 1
          error_config = 2
          OTHERS       = 3.
    ENDLOOP.
  ENDMETHOD.


  METHOD ws_get_emp_events.
    DATA:   BEGIN OF s_perio
         , obji1 TYPE hrobjid
         , obji2 TYPE hrobjid
         , a_object TYPE hrobject_t
         , q_object TYPE hrobject_t
         , END OF s_perio .
    DATA: zz_objid TYPE hrobjid.
    DATA: ls_pad31 TYPE pad31 .
    DATA: lv_prev_year TYPE gjahr.
    DATA: ls_ess_docreq TYPE zthr_ess_docreq.
    DATA: lv_lastdayy TYPE dats .

    DATA:
          lv_objid_c TYPE objektid

        , lt_object_c    TYPE          hrobject_t
        , lt_object_s    TYPE          hrobject_t
        , lt_p1001_s     TYPE TABLE OF p1001
        , lt_p1001_p     TYPE TABLE OF p1001
        , lt_pen         TYPE          zhr_tt_penalty

        , ls_object_tmp  TYPE hrobject
        .

    "табельный
    DATA(lv_pernr) = me->get_pernr( iv_usrid = CONV #( iv_euser ) iv_begda = sy-datum ).

    IF iv_xss = me->gc_gss_gov.

      DATA(lo_get_governor) = NEW zcl_hr_get_data( ).

      lo_get_governor->read_stvarv_par(
          EXPORTING
            i_name = me->gc_tvarv_gov_obj_c
          IMPORTING
            e_value = lv_objid_c ).

      IF lv_objid_c IS NOT INITIAL.
        ls_object_tmp-objid = lv_objid_c.
        ls_object_tmp-plvar = '01'.
        ls_object_tmp-otype = 'C'.
      ENDIF.

      APPEND ls_object_tmp TO lt_object_c.

      lo_get_governor->read_om_infty(
          EXPORTING
            i_infty  = '1001'
            i_begda  = iv_begda
            i_endda  = iv_begda
            i_object = lt_object_c
          IMPORTING
            e_pnnnn  = lt_p1001_s ).

      DELETE lt_p1001_s
         WHERE rsign <> 'A'
           AND relat <> '007'.

      SORT lt_p1001_s BY objid.

      LOOP AT lt_p1001_s INTO DATA(ls_p1001_s).
        CLEAR ls_object_tmp.

        ls_object_tmp-plvar = ls_p1001_s-plvar.
        ls_object_tmp-objid = ls_p1001_s-sobid.
        ls_object_tmp-otype = 'S'.

        APPEND ls_object_tmp TO lt_object_s.
      ENDLOOP.

      lo_get_governor->read_om_infty(
         EXPORTING
           i_infty  = '1001'
           i_begda  = iv_begda
           i_endda  = iv_begda
           i_object = lt_object_s
         IMPORTING
           e_pnnnn  = lt_p1001_p ).

      SORT lt_p1001_p BY objid.

      DELETE lt_p1001_p
         WHERE rsign <> 'A'
           AND relat <> '008'
            OR prozt <> '100'.

      READ TABLE lt_p1001_p
          INTO DATA(ls_p1001_p)
          INDEX 1 .

      IF ls_p1001_p-sobid <> lv_pernr.
        lv_pernr = ls_p1001_p-sobid.
      ENDIF.

      FREE lo_get_governor.

    ENDIF.

    IF lv_pernr IS INITIAL.
      RAISE no_pernr.
    ENDIF.

    me->check_head_is_second_lvl(
            EXPORTING
              iv_pernr = lv_pernr
              iv_begda = sy-datum
              iv_endda = sy-datum
            IMPORTING
              ev_is_second_lvl = DATA(lv_is_second_lvl)
              et_struc = DATA(lt_struc) ).

    IF lv_is_second_lvl IS INITIAL
        AND iv_xss <> me->gc_gss_gov
        AND iv_xss IS NOT INITIAL.
      RAISE no_sec_lev_ruk.
    ENDIF.

    DELETE lt_struc
        WHERE otype <> me->gc_obj_type_o.

    READ TABLE lt_struc INTO DATA(ls_struc) INDEX 1.

    me->get_struc( EXPORTING iv_objid  = CONV hrobjid( ls_struc-objid )
                             iv_otype  = ls_struc-otype
                             iv_begda  = iv_begda
                             iv_endda  = iv_endda
                             iv_wegid  = 'O-O-S-P'
                             iv_acheck = abap_false
                   IMPORTING et_struc  = lt_struc ).

    DATA: lt_struc_sort TYPE SORTED TABLE OF struc WITH NON-UNIQUE KEY seqnr
        , lt_object     TYPE hrobject_t
        , lt_pernr      TYPE hrobject_t
        , ls_object     LIKE LINE OF lt_object
        .

    ls_object-plvar = mv_plvar.

    LOOP AT lt_struc ASSIGNING FIELD-SYMBOL(<lt_struc>).
      INSERT <lt_struc> INTO TABLE lt_struc_sort.
      ls_object-otype = <lt_struc>-otype.
      ls_object-objid = <lt_struc>-objid.
      CASE ls_object-otype.
        WHEN 'P'.
          COLLECT ls_object INTO lt_pernr.
        WHEN OTHERS.
          COLLECT ls_object INTO lt_object.
      ENDCASE.
    ENDLOOP.

    DATA: lt_p1002_exp TYPE TABLE OF p1002_exp
        , lt_p1000     TYPE TABLE OF p1000
        , lt_p9109     TYPE TABLE OF p9109
        , lt_p9104     TYPE TABLE OF p9104
        , lt_p1007     TYPE TABLE OF p1007
        .

    IF lt_pernr IS NOT INITIAL.
      SELECT pernr,
             nachn,
             vorna,
             midnm
             INTO TABLE @DATA(lt_pa0002)
             FROM pa0002
             FOR ALL ENTRIES IN @lt_pernr WHERE pernr =  @lt_pernr-objid
                                            AND begda <= @iv_begda
                                            AND endda >= @iv_begda.
      SORT lt_pa0002 BY pernr.
    ENDIF.

    DATA(lo_get_data) = NEW zcl_hr_get_data( ).

    lo_get_data->read_om_infty( EXPORTING i_infty  = '1002'
                                          i_subty  = '0001'
                                          i_begda  = iv_begda
                                          i_endda  = iv_begda
                                          i_object = lt_object
                                IMPORTING e_pnnnn  = lt_p1002_exp ).

    IF iv_xss = 'GSS'.
      me->get_penalty(
            EXPORTING
              iv_begda = iv_begda
              iv_endda = iv_endda
              it_pernr = lt_pernr
            IMPORTING
              et_pen = lt_pen ).
    ENDIF.

*     MAIN_USER type string,
*     ENTRANCE_USER type string,
*     BEGDA type string,
*     ENDDA type string,
*     PERNR type string,
*     ATT_DATE type string,
*     ATT_RES type string,
*     EXAM_DATE type string,
*     EXAM_RES type string,
*     DECL_NEED type string,
*     DECL_DATE Type string,
*     DECL_STAT  Type string,
    DATA: lt_idd07v TYPE TABLE OF  dd07v .
    DATA: ls_idd07v TYPE    dd07v .

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'ZDHR_IPRREQ_STATUS'   "<-- Your Domain Here
        text           = 'X'
        langu          = sy-langu
      TABLES
        dd07v_tab      = lt_idd07v
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.
    zcl_hr_get_data=>read_stvarv_par( EXPORTING i_name  = 'ZZ_HR_ATTESTATION'
                                      IMPORTING e_value = s_perio-obji1 ).

    zcl_hr_get_data=>read_stvarv_par( EXPORTING i_name  = 'ZZ_HR_QUALIFICATION'
                                      IMPORTING e_value = s_perio-obji2 ).



    lo_get_data->get_objid_struc( EXPORTING i_otype = 'QK'
                                         i_objid = s_perio-obji1
                                         i_wegid = 'QUALCATA'
                                         i_begda = iv_begda
                               IMPORTING e_objec  = DATA(z_objec) ).

    DATA: s_object LIKE LINE OF s_perio-a_object
        .

    LOOP AT z_objec ASSIGNING FIELD-SYMBOL(<z_objec>) WHERE otype = 'Q'.
      MOVE-CORRESPONDING <z_objec> TO s_object.
      APPEND s_object TO s_perio-a_object.
    ENDLOOP.

    lo_get_data->get_objid_struc( EXPORTING i_otype = 'QK'
                                         i_objid = s_perio-obji2
                                         i_wegid = 'QUALCATA'
                                         i_begda = iv_begda
                               IMPORTING e_objec  = z_objec ).

    LOOP AT z_objec ASSIGNING <z_objec> WHERE otype = 'Q'.
      MOVE-CORRESPONDING <z_objec> TO s_object.
      APPEND s_object TO s_perio-q_object.
    ENDLOOP.

    SORT: s_perio-a_object BY objid
        , s_perio-q_object BY objid.
    DATA: t_p1001 TYPE TABLE OF p1001.
    DATA: t_p9110 TYPE TABLE OF p9110
        .
    LOOP AT lt_pa0002 ASSIGNING FIELD-SYMBOL(<lt_pa0002>).

      APPEND INITIAL LINE TO et_emp_events ASSIGNING FIELD-SYMBOL(<et_emp_event>).
*     PERNR type string,

      READ TABLE lt_pen
        ASSIGNING FIELD-SYMBOL(<fs_pen>)
        WITH KEY pernr = <lt_pa0002>-pernr.

      IF sy-subrc = 0.
        <et_emp_event>-pen_begda = <fs_pen>-begda.
        <et_emp_event>-pen_endda = <fs_pen>-endda.
        IF <fs_pen>-endda IS INITIAL.
          <et_emp_event>-pen_endda = <et_emp_event>-pen_begda.
        ENDIF.
      ENDIF.

      <et_emp_event>-pernr =     <lt_pa0002>-pernr.
      CLEAR t_p1001. " Сорокин А.Е. 01.10.2020.
      lo_get_data->read_om_infty( EXPORTING i_otype  = 'P'
                                       i_objid  = <lt_pa0002>-pernr
                                       i_infty  = '1001'
                                       i_subty  = 'A032'
                                       i_endda  = iv_endda
                             IMPORTING e_pnnnn  = t_p1001 ).

      SORT t_p1001 BY begda.

      DATA: lt_qual_prof TYPE  hap_t_q_profile .
      DATA: ls_qual_prof LIKE LINE OF lt_qual_prof .
      CLEAR lt_qual_prof. " Сорокин А.Е. 01.10.2020.
      CALL FUNCTION 'BAPI_QUALIFIC_GETLIST'
        EXPORTING
          plvar                = '01'
          otype                = 'P'
          sobid                = CONV sobid( <lt_pa0002>-pernr )
*         FROM_DATE            = '19000101'
*         TO_DATE              = '99990101'
*         NO_HALFVALUE         =
*     IMPORTING
*         RETURN               =
        TABLES
          qualificationprofile = lt_qual_prof.

      "Дата последней аттестации

*     ATT_DATE type string,

*      LOOP AT t_p1001 ASSIGNING FIELD-SYMBOL(<t_p1001>). "WHERE sobid = s_perio-obji1.
*        zz_objid = <t_p1001>-sobid.
*        READ TABLE s_perio-a_object TRANSPORTING NO FIELDS WITH KEY objid = zz_objid BINARY SEARCH.
*        CHECK sy-subrc = 0.
*        DATA(zz_begda) = <t_p1001>-begda.
*        IF <t_p1001>-adata IS NOT INITIAL.
*          CALL METHOD cl_hr_adata_type_cast=>adata_to_padnn
*            EXPORTING
*              adata = <t_p1001>-adata
*            IMPORTING
*              padnn = ls_pad31.
*          IF ls_pad31-chara IS NOT INITIAL.
**     ATT_RES type string,
*            <et_emp_event>-att_res = ls_pad31-chara.
*          ENDIF.
*
*        ENDIF.
*      ENDLOOP.
*      IF zz_begda IS NOT INITIAL.
*        <et_emp_event>-att_date = zz_begda .
*      ENDIF.
***<<< add Shibkova 13.04.2018
      DELETE lt_qual_prof WHERE begda < iv_begda OR begda > iv_endda.
***>>> end

      READ TABLE lt_qual_prof INTO ls_qual_prof WITH KEY qgroup_id = s_perio-obji1.
      IF sy-subrc EQ 0.
        <et_emp_event>-att_res = ls_qual_prof-rating_text.
        <et_emp_event>-att_date = ls_qual_prof-begda.
      ENDIF.

      "Дата последнего квалификационного экзамена (если проводился)
*      CLEAR: zz_begda.
*      LOOP AT t_p1001 ASSIGNING <t_p1001>." WHERE sobid = s_perio-obji2.
*        zz_objid = <t_p1001>-sobid.
*        READ TABLE s_perio-q_object TRANSPORTING NO FIELDS WITH KEY objid = zz_objid BINARY SEARCH.
*        CHECK sy-subrc = 0.
*        zz_begda = <t_p1001>-begda.
*        IF <t_p1001>-adata IS NOT INITIAL.
*          CALL METHOD cl_hr_adata_type_cast=>adata_to_padnn
*            EXPORTING
*              adata = <t_p1001>-adata
*            IMPORTING
*              padnn = ls_pad31.
*          IF ls_pad31-chara IS NOT INITIAL.
**     EXAM_RES type string,
*            <et_emp_event>-exam_res = ls_pad31-chara.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
**     EXAM_DATE type string,
*      IF zz_begda IS NOT INITIAL.
*        <et_emp_event>-exam_date = zz_begda.
*      ENDIF.

      READ TABLE lt_qual_prof INTO ls_qual_prof WITH KEY qgroup_id = s_perio-obji2.
      IF sy-subrc EQ 0.
        <et_emp_event>-exam_res = ls_qual_prof-rating_text.
        <et_emp_event>-exam_date = ls_qual_prof-begda.
      ENDIF.

      DATA: t_p0001 TYPE TABLE OF p0001 .
      lv_lastdayy = sy-datum.
      lv_lastdayy+4(4) = '1231'.
      lv_lastdayy(4) = lv_lastdayy(4) - 1.
      lo_get_data->read_pa_infty( EXPORTING i_pernr = <lt_pa0002>-pernr
                                         i_infty  = '0001'
                                         i_endda  = lv_lastdayy
                               IMPORTING e_pnnnn  = t_p0001 ).
      SORT t_p0001 BY begda DESCENDING.

      READ TABLE t_p0001 INTO DATA(s_p0001) INDEX 1.
      IF sy-subrc EQ 0.
        CLEAR t_p9110. " Сорокин А.Е. 01.10.2020
        lo_get_data->read_om_infty( EXPORTING i_otype  = 'S'
                                       i_objid  = s_p0001-plans
                                       i_infty  = '9110'
                                       i_subty  = '0002'
                                       i_begda  = lv_lastdayy
                                       i_endda  = lv_lastdayy
                             IMPORTING e_pnnnn  = t_p9110 ).
        READ TABLE t_p9110 ASSIGNING FIELD-SYMBOL(<fs_p9110>) INDEX 1.
        IF sy-subrc EQ 0.
          IF <fs_p9110>-corrupt EQ 'X'.
            <et_emp_event>-decl_need = 'X'.
          ENDIF.
        ENDIF.

        IF <et_emp_event>-decl_need EQ 'X'.
          lv_prev_year = lv_lastdayy(4).
          SELECT SINGLE * INTO ls_ess_docreq FROM zthr_ess_docreq WHERE gjahr = lv_prev_year AND pernr  = <lt_pa0002>-pernr. "#EC CI_NOFIELD
          IF sy-subrc EQ 0.
*     DECL_DATE Type string,
*     DECL_STAT  Type string,
*            <et_emp_event>-decl_date = ls_ess_docreq-date_create .
            IF ls_ess_docreq-status IS NOT  INITIAL.
              READ TABLE lt_idd07v INTO ls_idd07v WITH KEY domvalue_l = ls_ess_docreq-status .
              IF sy-subrc EQ 0.
                <et_emp_event>-decl_stat = ls_idd07v-ddtext ."ZEHR_IPRREQ_STATUS
              ENDIF.
            ENDIF.
*          ELSE.
*            DATA(lv_begda) = CONV datum( |{ lv_prev_year }0101| ).
*            DATA(lv_endda) = CONV datum( |{ lv_prev_year }1231| ).
*            SELECT * FROM zthr_argus_pers0 AS per
*              INNER JOIN zthr_argus_base0 AS bas
*              ON per~docid = bas~docid
*              INTO TABLE @DATA(lt_arg)
*              WHERE per~pernr = @<lt_pa0002>-pernr
*              AND bas~begda <= @lv_endda
*              AND bas~endda >= @lv_begda.
*            IF sy-subrc = 0.
*              <et_emp_event>-decl_date = lt_arg[ 1 ]-bas-ordst.
*            ENDIF.
          ENDIF.
*          >>>Шаталов Дата подачи справки о доходах
          DATA lv_category TYPE c.
          DATA lv_decl_dat TYPE datum.
          CASE s_p0001-persk.
            WHEN '30' OR '40' OR '50' OR '60'.
              lv_category = '2'.
            WHEN '01' OR '10'.
              lv_category = '1'.
            WHEN OTHERS.
              lv_category = ''.
          ENDCASE.
          SELECT SINGLE endda FROM zthr_argus_dates WHERE category = @lv_category AND jyear = @iv_begda(4) INTO @DATA(lv_date_month).
          IF sy-subrc <> 0.
            SELECT SINGLE endda FROM zthr_argus_dates WHERE category = @lv_category AND jyear EQ '' INTO @lv_date_month.
          ENDIF.
          IF lv_date_month IS NOT INITIAL.
            lv_decl_dat = |{ iv_begda(4) }{ lv_date_month+3(2) }{ lv_date_month(2) }|.

            IF iv_begda <= lv_decl_dat AND lv_decl_dat <= iv_endda.
              <et_emp_event>-decl_date = lv_decl_dat.
            ENDIF.

          ENDIF.
*          <<<Шаталов
        ENDIF.
      ENDIF.
*     Плановая аттестация
      SELECT SINGLE * FROM pa9011 INTO @DATA(ls_9011)
        WHERE pernr = @<lt_pa0002>-pernr
        AND begda <= @sy-datum
        AND endda >= @sy-datum.
      IF sy-subrc = 0.
        <et_emp_event>-att_date = ls_9011-zzbegin_date.
      ENDIF.
    ENDLOOP.

    DELETE et_emp_events WHERE att_date IS INITIAL AND exam_date IS INITIAL AND decl_date IS INITIAL AND pen_begda IS INITIAL.

  ENDMETHOD.


  METHOD ws_get_emp_reserv_xml.
    DATA: lr_peri        TYPE RANGE OF datum,
          lv_date_c      TYPE char10,
          lr_orgeh       TYPE RANGE OF orgeh,
          ls_orgeh       LIKE LINE OF lr_orgeh,
          lt_struc_plans TYPE TABLE OF p1001,
          lt_gen_orgs    TYPE TABLE OF p1001.

    IF lr_orgeh IS INITIAL.
      ls_orgeh-sign = 'I'.
      ls_orgeh-option = 'EQ'.
      ls_orgeh-low = '50067358'.
      APPEND ls_orgeh TO lr_orgeh.
      ls_orgeh-low = '50004384'.
      APPEND ls_orgeh TO lr_orgeh.
      ls_orgeh-low = '50000260'.
      APPEND ls_orgeh TO lr_orgeh.
      ls_orgeh-low = '50000347'. "Администр Губернатора и Правительства ЛО
      APPEND ls_orgeh TO lr_orgeh.
    ENDIF.

    FIELD-SYMBOLS <ls_peri> LIKE LINE OF lr_peri.

    "табельный
    DATA(lv_pernr) = me->get_pernr( iv_usrid = CONV #( iv_euser ) iv_begda = iv_begda ).

    IF lv_pernr IS INITIAL.
      RAISE no_pernr.
    ENDIF.

    me->check_head_is_second_lvl(
            EXPORTING
              iv_pernr = lv_pernr
              iv_begda = iv_begda
              iv_endda = iv_endda
            IMPORTING
              ev_is_second_lvl = DATA(lv_is_second_lvl)
              et_struc = DATA(lt_struc) ).

    IF lv_is_second_lvl IS INITIAL.
      CALL FUNCTION 'RH_STRUC_GET'
        EXPORTING
          act_otype      = 'P'
          act_objid      = CONV hrobjid( lv_pernr )
          act_wegid      = 'P-S-O-O'
          act_plvar      = '01'
          act_begda      = iv_begda
          act_endda      = iv_endda
        TABLES
          result_struc   = lt_struc
        EXCEPTIONS
          no_plvar_found = 1
          no_entry_found = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.

    DELETE lt_struc WHERE NOT otype = 'O'.

    READ TABLE lt_struc INTO DATA(ls_struc) INDEX 1.
    CHECK sy-subrc = 0.

    IF ls_struc-objid IN lr_orgeh.
      CALL FUNCTION 'RH_READ_INFTY_1001'
        EXPORTING
          plvar            = '01'
          otype            = 'O'
          objid            = CONV hrobjid( ls_struc-objid )
          subty            = 'BZ99'
          begda            = iv_begda
          endda            = iv_endda
        TABLES
          i1001            = lt_struc_plans
        EXCEPTIONS
          nothing_found    = 1
          wrong_condition  = 2
          wrong_parameters = 3
          OTHERS           = 4.
      IF sy-subrc = 0.
        DELETE lt_struc_plans WHERE NOT sclas = 'S'.

        READ TABLE lt_struc_plans INTO DATA(ls_struc_plans) INDEX 1.
        IF sy-subrc = 0.
          CALL FUNCTION 'RH_READ_INFTY_1001'
            EXPORTING
              plvar            = '01'
              otype            = 'S'
              objid            = CONV hrobjid( ls_struc_plans-sobid )
              subty            = 'AZ99'
              begda            = iv_begda
              endda            = iv_endda
            TABLES
              i1001            = lt_gen_orgs
            EXCEPTIONS
              nothing_found    = 1
              wrong_condition  = 2
              wrong_parameters = 3
              OTHERS           = 4.
          IF sy-subrc = 0.
            DELETE lt_struc_plans WHERE NOT sclas = 'O'.
            READ TABLE lt_gen_orgs INTO DATA(ls_gen_orgs) INDEX 1.
            IF sy-subrc = 0.
              ls_struc-objid = ls_gen_orgs-sobid.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    DATA: lv_ld TYPE wsaf_date.
    CALL FUNCTION 'WSAF_GET_LAST_DAY_OF_MONTH'
      EXPORTING
        i_datum          = sy-datum
      IMPORTING
        e_last_day_month = lv_ld.


    APPEND INITIAL LINE TO lr_peri ASSIGNING <ls_peri>.

    <ls_peri>-sign   = 'I'.
    <ls_peri>-option = 'BT'.
    <ls_peri>-low    = |{ sy-datum(6) }01|.
    <ls_peri>-high   = lv_ld.

    SUBMIT zhr_pa_d044
     WITH p_plvar = '01'
     WITH p_otype = 'O'
     WITH p_objid = ls_struc-objid
     WITH so_peri = lr_peri
     WITH p_xml   = abap_true
     AND RETURN.


    IMPORT lv_xstring TO e_xstring FROM MEMORY ID zcl_z_mss_emp_reserv_dpc_ext=>c_memory_id.
    FREE MEMORY ID zcl_z_mss_emp_reserv_dpc_ext=>c_memory_id.

  ENDMETHOD.


  METHOD ws_get_entrance_user.

    SELECT entrance_user INTO TABLE @DATA(lt_entrance) FROM zthr_entr_user WHERE main_user = @iv_muser.
    DATA: ls_ent LIKE LINE OF lt_entrance.
    "Первой строчкой ENTRANCE_USER=MAIN_USER
    ls_ent-entrance_user =  iv_muser.
    insert ls_ent into lt_entrance index 1.


    DATA: ls_entrance LIKE LINE OF et_entrance.

    DATA: objid TYPE hrp1000-objid.
    DATA(lv_filled) = abap_false.

    LOOP AT lt_entrance ASSIGNING FIELD-SYMBOL(<lt_entrance>).
      IF <lt_entrance>-entrance_user = '*'.
        CHECK lv_filled IS INITIAL.
        me->get_all_users( EXPORTING iv_begda = iv_begda
                           IMPORTING et_users = DATA(lt_users) ).

        LOOP AT lt_users ASSIGNING FIELD-SYMBOL(<lt_users>).
          me->get_struc( EXPORTING iv_objid = <lt_users>-pernr
                                   iv_otype = 'P'
                                   iv_begda  = iv_begda
                                   iv_endda  = iv_begda
                                   iv_wegid  = 'Z_RUK_O'
                                   iv_acheck = abap_false
                         IMPORTING et_struc  = DATA(lt_struc) ).

          DELETE lt_struc WHERE NOT otype = 'O'.

          CHECK lines( lt_struc ) = 2.
          ls_entrance-entrance_user = <lt_users>-usrid.
          ls_entrance-fio           = <lt_users>-ename.
          READ TABLE lt_struc ASSIGNING FIELD-SYMBOL(<fs_struc>) INDEX 1.

          IF sy-subrc EQ 0.
            objid = <fs_struc>-objid.
            ls_entrance-orgeh = objid.
            ls_entrance-desc = zcl_hr_data_utils=>get_name_obj( i_objid = objid
                                         i_otype = <fs_struc>-otype
                                         i_begda = iv_begda
                                         i_subty = '0001' ).
            IF ls_entrance-desc  IS INITIAL.
              ls_entrance-desc = zcl_hr_data_utils=>get_name_obj_1000( i_objid = objid
                             i_otype = <fs_struc>-otype
                             i_begda = iv_begda ).

            ENDIF.
          ENDIF.
          COLLECT ls_entrance INTO et_entrance.
        ENDLOOP.
        lv_filled = abap_true.
      ELSE.
        CLEAR ls_entrance.
        ls_entrance-entrance_user = <lt_entrance>-entrance_user.
        ls_entrance-fio           = me->get_user_fio( iv_usrid = CONV #( <lt_entrance>-entrance_user ) ).
        DATA(lv_pernr) = me->get_pernr( iv_usrid = CONV #( <lt_entrance>-entrance_user ) ).
        me->get_struc( EXPORTING iv_objid = lv_pernr
                                   iv_otype = 'P'
                                   iv_begda  = iv_begda
                                   iv_endda  = iv_begda
                                   iv_wegid  = 'Z_RUK_O'
                                   iv_acheck = abap_false
                         IMPORTING et_struc  = DATA(lt_struc2) ).
        DELETE lt_struc2 WHERE NOT otype = 'O'.

        IF lines( lt_struc2 ) = 2.
          READ TABLE lt_struc2 ASSIGNING FIELD-SYMBOL(<fs_struc2>) INDEX 1.
          IF sy-subrc EQ 0.
            objid = <fs_struc2>-objid.
            ls_entrance-orgeh = objid.
            ls_entrance-desc = zcl_hr_data_utils=>get_name_obj( i_objid = objid
                                           i_otype = <fs_struc2>-otype
                                           i_begda = iv_begda
                                           i_subty = '0001' ).
            IF ls_entrance-desc  IS INITIAL.
              ls_entrance-desc = zcl_hr_data_utils=>get_name_obj_1000( i_objid = objid
                             i_otype = <fs_struc2>-otype
                             i_begda = iv_begda ).

            ENDIF.
          ENDIF.
        ENDIF.
        COLLECT ls_entrance INTO et_entrance.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD ws_get_f1gs_xml.
    DATA: lr_peri   TYPE RANGE OF datum,
          lv_date_c TYPE char10.

    FIELD-SYMBOLS <ls_peri> LIKE LINE OF lr_peri.

    "табельный
    DATA(lv_pernr) = me->get_pernr( iv_usrid = CONV #( iv_euser ) iv_begda = iv_begda ).

    IF lv_pernr IS INITIAL.
      RAISE no_pernr.
    ENDIF.


    me->check_head_is_second_lvl(
            EXPORTING
              iv_pernr = lv_pernr
              iv_begda = iv_begda
              iv_endda = iv_endda
            IMPORTING
              ev_is_second_lvl = DATA(lv_is_second_lvl)
              et_struc = DATA(lt_struc) ).

    IF lv_is_second_lvl IS INITIAL.
      CALL FUNCTION 'RH_STRUC_GET'
        EXPORTING
          act_otype      = 'P'
          act_objid      = CONV hrobjid( lv_pernr )
          act_wegid      = 'P-S-O-O'
          act_plvar      = '01'
          act_begda      = iv_begda
          act_endda      = iv_endda
        TABLES
          result_struc   = lt_struc
        EXCEPTIONS
          no_plvar_found = 1
          no_entry_found = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.

    DELETE lt_struc WHERE NOT otype = 'O'.

    READ TABLE lt_struc INTO DATA(ls_struc) INDEX 1.
    CHECK sy-subrc = 0.

    SUBMIT zhr_f1gs_01
     WITH p_objid = ls_struc-objid
     WITH p_xml   = abap_true
     AND RETURN.

    IMPORT lv_xstring TO e_xstring FROM MEMORY ID zcl_z_mss_f1gs_dpc_ext=>c_memory_id.
    FREE MEMORY ID zcl_z_mss_f1gs_dpc_ext=>c_memory_id.
  ENDMETHOD.


  METHOD ws_get_ipr_list.

    "табельный

    DATA(lv_pernr) = me->get_pernr( iv_usrid = CONV #( iv_euser ) iv_begda = sy-datum ).
    IF lv_pernr IS INITIAL.
      RAISE no_pernr.
    ENDIF.
    SELECT SINGLE *  FROM zhr_xss_iprreq INTO @DATA(ls_xss_iprreq).
    IF sy-subrc EQ 0.
      APPEND INITIAL LINE TO et_ipr_list ASSIGNING FIELD-SYMBOL(<et_ipr_list>).
      IF ls_xss_iprreq-edu_type IS NOT INITIAL.
        SELECT SINGLE zztext INTO <et_ipr_list>-edu_type FROM t517t WHERE sprsl EQ 'R' AND slart EQ ls_xss_iprreq-edu_type.
      ENDIF.
      IF ls_xss_iprreq-edu_spec IS NOT INITIAL.
        SELECT SINGLE stext INTO <et_ipr_list>-edu_spec FROM zthr_dlinet WHERE spras EQ 'R' AND dline EQ ls_xss_iprreq-edu_spec.
      ENDIF.
      IF ls_xss_iprreq-edu_frm IS NOT INITIAL.
        SELECT SINGLE cname INTO <et_ipr_list>-edu_frm FROM t7ruokin WHERE  sprsl EQ 'R' AND molga EQ '33' AND facet EQ '33' AND ccode EQ ls_xss_iprreq-edu_frm.
      ENDIF.
      IF ls_xss_iprreq-edu_reas IS NOT INITIAL.
        <et_ipr_list>-edu_goal = ls_xss_iprreq-edu_reas.
*      Select single STEXT into <et_ipr_list>-Edu_frm from ZTHR_DLINET where SPRAS eq 'R' and DLINE eq ls_xss_iprreq-Edu_frm.
      ENDIF.
      IF ls_xss_iprreq-edu_length IS NOT INITIAL AND ls_xss_iprreq-edu_length_type IS NOT INITIAL.
        SELECT SINGLE etext INTO  ls_xss_iprreq-edu_length_type FROM t538t WHERE  sprsl EQ 'R'  AND zeinh EQ ls_xss_iprreq-edu_length_type.

        <et_ipr_list>-edu_length =  ls_xss_iprreq-edu_length .
      ENDIF.
      <et_ipr_list>-edu_year = ls_xss_iprreq-edu_year.
      <et_ipr_list>-edu_result = ls_xss_iprreq-edu_prog.

    ENDIF.
*Edu_type Вид доп. образования  Edu_type, выводим текст из T517T-ZZTEXT (T517T- SPRSL=RU)
*Edu_type  Вид доп. образования  Edu_type, выводим текст из T517T-ZZTEXT (T517T- SPRSL=RU)
*Edu_spec  Направление Edu_spec, выводит текст из ZTHR_DLINE
*Edu_frm  Форма Edu_frm, выводим текст из T7RUOKIN MOLGA = 33, FACET = 33
*Edu_goal  Цель  Edu_goal, выводим текст из ZTHR_хххх
*Edu_length  Продолжительность Edu_length и Edu_length_type
*Текст ед. измерения выводим из T538T SPRSL = RU
*Edu_year  Срок обучения Edu_year
*Edu_result  Ожидаемый результат Edu_result, выводим текст из ZTHR_хххх




  ENDMETHOD.


METHOD ws_get_list_ipr.

  DATA: lt_iprreq TYPE STANDARD TABLE OF zhr_xss_iprreq.

*21.03.2020
  IF is_input_data-entrance_user IS NOT INITIAL.
    DATA(lv_pernr) = get_pernr( iv_usrid = CONV #( is_input_data-entrance_user ) ).
  ELSE.
    lv_pernr = get_pernr( iv_usrid = CONV #( is_input_data-main_user ) ).
  ENDIF.
  IF lv_pernr IS INITIAL.
    RAISE no_pernr.
  ENDIF.


  " руководитель второй орг ед?
  me->get_struc( EXPORTING iv_objid = lv_pernr
                           iv_otype = 'P'
                           iv_begda  = sy-datum
                           iv_endda  = sy-datum
                           iv_wegid  = 'Z_RUK_O'
                           iv_acheck = abap_false
                 IMPORTING et_struc  = DATA(lt_struc) ).

  DELETE lt_struc WHERE NOT otype = 'O'.

*  IF NOT lines( lt_struc ) = 2.
*    RAISE no_sec_lev_ruk.
*  ENDIF.

  DATA: lr_status TYPE RANGE OF zhr_xss_iprreq-status
      .
  APPEND VALUE #( sign = `I` option = `EQ` low = 4 ) TO lr_status.
  APPEND VALUE #( sign = `I` option = `EQ` low = 6 ) TO lr_status.
  APPEND VALUE #( sign = `I` option = `EQ` low = 8 ) TO lr_status.
  APPEND VALUE #( sign = `I` option = `EQ` low = 10 ) TO lr_status.

  SELECT *
    INTO TABLE lt_iprreq
    FROM zhr_xss_iprreq
    WHERE approver_number = lv_pernr
*      AND status          = '4'.
      AND status          IN lr_status.                 "#EC CI_NOFIELD
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  LOOP AT lt_iprreq ASSIGNING FIELD-SYMBOL(<ls_iprreq>).

    APPEND INITIAL LINE TO et_output_data ASSIGNING FIELD-SYMBOL(<ls_output>).
    <ls_output>-pernr  = <ls_iprreq>-pernr_creator.
    <ls_output>-req_id = <ls_iprreq>-req_id.

    zcl_hr_data=>get_fio( EXPORTING i_pernr    = CONV #( <ls_output>-pernr )
                          IMPORTING e_fio_full = <ls_output>-fio ).

    <ls_output>-plans = zcl_hr_data_utils=>get_plans_and_ogreh_name( EXPORTING i_pernr = CONV #( <ls_output>-pernr ) ).
    <ls_output>-msgid    = 'S'.

    MESSAGE s014(zhr_mss)  INTO <ls_output>-message.
  ENDLOOP.

ENDMETHOD.


  METHOD ws_get_main_user_data.
*    ESS  CHAR(1) Роль ESS  Константа ‘X’
*    MSS  CHAR(1) Роль MSS  Есть работник является руководителем орг. единицы 2-го уровня, то Х, иначе пусто
*    FIO  CHAR(120) ФИО работника PA0002-NACHN, PA0002-VORNA, PA0002-MIDNM
*    ТН пользователя определяем от значения MAIN_USER:
*    Собираем список PA0105-USRID=MAIN_USER при условии, что PA0105-SUBTY=9001
*    PERNR  NUM(8)  123 Табельный номер работника
    CONSTANTS: lc_second_lvl TYPE i VALUE '2'.

    "табельный
    DATA:  ls_pa0002 TYPE pa0002,
           lt_p0105  TYPE STANDARD TABLE OF p0105.

    DATA:
      lv_ohead    TYPE hrobjid,
      lv_i        TYPE i,
      lv_head_lvl TYPE i.

    DATA(lv_pernr) = me->get_pernr( iv_usrid = CONV #( iv_euser ) iv_snils = CONV #(  iv_snils ) iv_begda = iv_begda ).

    IF lv_pernr IS INITIAL.
      RAISE no_pernr.
    ENDIF.

    SELECT SINGLE pernr FROM pa0000 INTO @DATA(lv_pernr0) WHERE begda <= @iv_begda
                                                            AND endda >= @iv_begda
                                                            AND pernr =  @lv_pernr
                                                            AND stat2 =  '3'.
    IF sy-subrc NE 0.
      RAISE pernr_inactive.
    ENDIF.

    APPEND INITIAL LINE TO et_mainuserdata ASSIGNING FIELD-SYMBOL(<et_mainuserdata>).


    IF <et_mainuserdata>-main_user IS INITIAL.
      <et_mainuserdata>-main_user = me->get_pernr_0105( iv_pernr = lv_pernr iv_subty = '9001' ).
    ENDIF.

    <et_mainuserdata>-pernr         = lv_pernr.
    <et_mainuserdata>-entrance_user = me->get_en_user( iv_usrid = CONV #( iv_euser ) iv_begda = iv_begda ).
    <et_mainuserdata>-ess           = abap_true.
    <et_mainuserdata>-inn_orgeh     = zcl_mss_data_assistent=>get_inn_orgeh( i_pernr = lv_pernr ).
    <et_mainuserdata>-snils         = zcl_mss_data_assistent=>get_snils( i_pernr = lv_pernr ).
    <et_mainuserdata>-argus         = is_argus( lv_pernr ).
    <et_mainuserdata>-contest       = zcl_hrpa_tvarvc=>read_parameter( i_name = 'ZHR_MSS_SHOW_CONTEST' ).

    me->get_struc(
      EXPORTING
         iv_objid  = lv_pernr
         iv_otype  = me->gc_obj_type_p
         iv_begda  = iv_begda
         iv_endda  = iv_begda
         iv_wegid  = me->gc_wegid_head_all
         iv_acheck = abap_false
       IMPORTING
         et_struc  = DATA(lt_struc) ).

    IF line_exists( lt_struc[ otype = cl_hap_pmp_const=>otype_orgunit ] ).
      <et_mainuserdata>-mss = abap_true.
    ENDIF.

*    me->check_head_is_second_lvl(
*          EXPORTING
*            iv_pernr = lv_pernr
*            iv_begda = iv_begda
*            iv_endda = iv_begda
*          IMPORTING
*            ev_is_second_lvl = <et_mainuserdata>-mss
*            et_struc         = DATA(lt_struc) ).

    "Доступ Пользователя к блоку «Губернатор» - проверка атрибута Z_GSS
    <et_mainuserdata>-gss = check_gss_value( iv_begda = iv_begda
                                             it_struc = lt_struc ).

    IF <et_mainuserdata>-entrance_user IS NOT INITIAL
          AND <et_mainuserdata>-gss IS INITIAL.

      DATA(lv_enpernr) = me->get_pernr( iv_usrid = CONV #( <et_mainuserdata>-entrance_user ) iv_begda = iv_begda ).

      IF lv_enpernr IS INITIAL.
        RAISE no_pernr_sub.
      ENDIF.

      me->check_head_is_second_lvl(
            EXPORTING
              iv_pernr = lv_enpernr
              iv_begda = iv_begda
              iv_endda = iv_begda
            IMPORTING
              ev_is_second_lvl = <et_mainuserdata>-mss ).
    ENDIF.

    SELECT pernr,
           nachn,
           vorna,
           midnm
           INTO TABLE @DATA(lt_pa0002)
           FROM pa0002 WHERE pernr =  @lv_pernr
                                          AND begda <= @iv_begda
                                          AND endda >= @iv_begda.
    IF sy-subrc EQ 0.

      READ TABLE lt_pa0002 INTO DATA(ls_pa) INDEX 1.
      <et_mainuserdata>-fio = ls_pa-nachn && ` ` && ls_pa-vorna && ` ` && ls_pa-midnm.
    ENDIF.

    IF me->check_is_sit_cen_for_gss( lv_pernr ) = abap_true. "Если пользователь работник сит. центра, отключаем все вкладки кроме GSS

      CLEAR:
             <et_mainuserdata>-ess,
             <et_mainuserdata>-mss,
             <et_mainuserdata>-entrance_user.

      <et_mainuserdata>-gss = abap_true.

    ENDIF.

*    DATA(lo_data) = NEW zcl_hr_get_data( ).
*    lo_data->read_pa_infty( EXPORTING i_pernr = lv_pernr
*                                      i_begda = iv_begda
*                                      i_endda = iv_begda
*                                      i_infty = '0105'
*                                      i_subty = '9002'
*                            IMPORTING e_pnnnn = lt_p0105 ).
*    READ TABLE lt_p0105 ASSIGNING FIELD-SYMBOL(<ls_p0105>) INDEX 1.
*    IF sy-subrc = 0.
*      IF <ls_p0105>-usrid = 'X'.
*        <et_mainuserdata>-sign = abap_true.
*      ENDIF.
*    ENDIF.
  ENDMETHOD.


  METHOD ws_get_schedule.
    DATA(lv_pernr) = me->get_pernr( iv_usrid = CONV #( iv_euser ) ).

    me->get_struc( EXPORTING iv_objid = lv_pernr
                             iv_otype = 'P'
                             iv_begda  = iv_begda
                             iv_endda  = iv_begda
                             iv_wegid  = 'Z_RUK_O'
                             iv_acheck = abap_false
                   IMPORTING et_struc  = DATA(lt_struc) ).

    DELETE lt_struc WHERE NOT otype = 'O'.

    CHECK lines( lt_struc ) = 2. " 2-я орг ед

    READ TABLE lt_struc INTO DATA(ls_struc) INDEX 1.

    DATA(lv_object) = mv_plvar && ls_struc-objid.

    SELECT ftype,
           object,
           seqnr
      FROM zthr_mss_at_attr
      INTO TABLE @DATA(lt_attr)
      WHERE ftype  = 'GA'
        AND object = @lv_object
      ORDER BY datum, uzeit.

    LOOP AT lt_attr ASSIGNING FIELD-SYMBOL(<lt_attr>).
    ENDLOOP.

    me->file_download( EXPORTING iv_ftype   = <lt_attr>-ftype
                                 iv_object  = <lt_attr>-object
                                 iv_seqnr   = <lt_attr>-seqnr
                       IMPORTING es_content = DATA(lv_content)
                                 es_attr    = DATA(ls_attr) ).

    CHECK lv_content IS NOT INITIAL.

    APPEND VALUE #( content = lv_content fname = ls_attr-fname ) TO et_schedule.
  ENDMETHOD.


  METHOD ws_get_shtat_chisl_xml.
    DATA: lr_peri   TYPE RANGE OF datum,
          lv_date_c TYPE char10.

    FIELD-SYMBOLS <ls_peri> LIKE LINE OF lr_peri.

    "табельный
    DATA(lv_pernr) = me->get_pernr( iv_usrid = CONV #( iv_euser ) iv_begda = iv_begda ).

    IF lv_pernr IS INITIAL.
      RAISE no_pernr.
    ENDIF.


    me->check_head_is_second_lvl(
            EXPORTING
              iv_pernr = lv_pernr
              iv_begda = iv_begda
              iv_endda = iv_endda
            IMPORTING
              ev_is_second_lvl = DATA(lv_is_second_lvl)
              et_struc = DATA(lt_struc) ).

    IF lv_is_second_lvl IS INITIAL.
      RAISE no_sec_lev_ruk.
    ENDIF.

    DELETE lt_struc WHERE NOT otype = 'O'.

    READ TABLE lt_struc INTO DATA(ls_struc) INDEX 1.
    CHECK sy-subrc = 0.

    APPEND INITIAL LINE TO lr_peri ASSIGNING <ls_peri>.

    <ls_peri>-sign   = 'I'.
    <ls_peri>-option = 'EQ'.
    <ls_peri>-low    = sy-datum.

    SUBMIT zhr_om_d005
     WITH p_orgeh = ls_struc-objid
     WITH p_period = lr_peri
     WITH p_xml   = abap_true
     AND RETURN.


    IMPORT lv_xstring TO e_xstring FROM MEMORY ID zcl_z_mss_shtat_chisl_dpc_ext=>c_memory_id.
    FREE MEMORY ID zcl_z_mss_shtat_chisl_dpc_ext=>c_memory_id.

  ENDMETHOD.


  METHOD ws_get_stafflist_xml.
    DATA: lr_peri   TYPE RANGE OF datum,
          lv_date_c TYPE char10,
          lr_objids TYPE RANGE OF struc-objid,
          lt_objids LIKE lr_objids.

    FIELD-SYMBOLS <ls_peri> LIKE LINE OF lr_peri.

    "табельный
    DATA(lv_pernr) = me->get_pernr( iv_usrid = CONV #( iv_euser ) iv_begda = iv_begda ).

    IF lv_pernr IS INITIAL.
      RAISE no_pernr.
    ENDIF.


    me->check_head_is_second_lvl(
            EXPORTING
              iv_pernr = lv_pernr
              iv_begda = iv_begda
              iv_endda = iv_endda
            IMPORTING
              ev_is_second_lvl = DATA(lv_is_second_lvl)
              et_struc = DATA(lt_struc) ).

    IF lv_is_second_lvl IS INITIAL.
      CALL FUNCTION 'RH_STRUC_GET'
        EXPORTING
          act_otype      = 'P'
          act_objid      = CONV hrobjid( lv_pernr )
          act_wegid      = 'P-S-O-O'
          act_plvar      = '01'
          act_begda      = iv_begda
          act_endda      = iv_endda
        TABLES
          result_struc   = lt_struc
        EXCEPTIONS
          no_plvar_found = 1
          no_entry_found = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.

    DELETE lt_struc WHERE NOT otype = 'O'.
    DELETE lt_struc WHERE vcount = 0.

    LOOP AT lt_struc INTO DATA(ls_struc).
      APPEND INITIAL LINE TO lt_objids ASSIGNING FIELD-SYMBOL(<fs_objid>).
      <fs_objid>-sign = 'I'.
      <fs_objid>-option = 'EQ'.
      <fs_objid>-low = ls_struc-objid.
    ENDLOOP.

    SUBMIT zhr_stafflist2
     WITH p_objid IN lt_objids
     WITH p_xml   = abap_true
     AND RETURN.

    IMPORT lv_xstring TO e_xstring FROM MEMORY ID zcl_z_mss_shtat_raspis_dpc_ext=>c_memory_id.
    FREE MEMORY ID zcl_z_mss_shtat_raspis_dpc_ext=>c_memory_id.
  ENDMETHOD.


  METHOD ws_get_staff_graph.
    DATA: lr_peri   TYPE RANGE OF datum,
          lv_date_c TYPE char10.

    FIELD-SYMBOLS <ls_peri> LIKE LINE OF lr_peri.

    "табельный
    DATA(lv_pernr) = me->get_pernr( iv_usrid = CONV #( iv_euser ) iv_begda = iv_begda ).

    IF lv_pernr IS INITIAL.
      RAISE no_pernr.
    ENDIF.
    " руководитель второй орг ед?
    me->get_struc( EXPORTING iv_objid = lv_pernr
                             iv_otype = 'P'
                             iv_begda  = iv_begda
                             iv_endda  = iv_endda
                             iv_wegid  = 'Z_RUK_O'
                             iv_acheck = abap_false
                   IMPORTING et_struc  = DATA(lt_struc) ).

    DELETE lt_struc WHERE NOT otype = 'O'.

    READ TABLE lt_struc INTO DATA(ls_struc) INDEX 1.
    CHECK sy-subrc = 0.

    APPEND INITIAL LINE TO lr_peri ASSIGNING <ls_peri>.

    <ls_peri>-sign   = 'I'.
    <ls_peri>-option = 'EQ'.
    <ls_peri>-low    = sy-datum.

    SUBMIT zhr_staff_graph
        WITH p_objid = ls_struc-objid
        WITH so_peri IN lr_peri
        WITH p_xml = abap_true
        AND RETURN.

    IMPORT lv_xstring TO e_xstring FROM MEMORY ID zcl_z_mss_staff_graph_dpc_ext=>c_memory_id.
    FREE MEMORY ID zcl_z_mss_staff_graph_dpc_ext=>c_memory_id.

  ENDMETHOD.


  METHOD ws_get_structure.

    CONSTANTS: lc_second_lvl TYPE i VALUE '2'.

    DATA:
          lv_view        TYPE abap_bool
        , lv_struc_check TYPE struc_t
        , lv_objid_c     TYPE objektid



        , lt_object_c    TYPE hrobject_t
        , lt_object_s    TYPE hrobject_t
        , lt_p1001_s     TYPE TABLE OF p1001
        , lt_p1001_p     TYPE TABLE OF p1001

        , ls_object_tmp  TYPE hrobject

    .

    "табельный
    DATA(lv_pernr) = me->get_pernr( iv_usrid = CONV #( iv_euser ) iv_begda = iv_begda ).

    IF iv_xss = me->gc_gss_gov.

      DATA(lo_get_governor) = NEW zcl_hr_get_data( ).

      lo_get_governor->read_stvarv_par(
          EXPORTING
            i_name = me->gc_tvarv_gov_obj_c
          IMPORTING
            e_value = lv_objid_c ).

      IF lv_objid_c IS NOT INITIAL.
        ls_object_tmp-objid = lv_objid_c.
        ls_object_tmp-plvar = '01'.
        ls_object_tmp-otype = 'C'.
      ENDIF.

      APPEND ls_object_tmp TO lt_object_c.

      lo_get_governor->read_om_infty(
          EXPORTING
            i_infty  = '1001'
            i_begda  = iv_begda
            i_endda  = iv_begda
            i_object = lt_object_c
          IMPORTING
            e_pnnnn  = lt_p1001_s ).

      DELETE lt_p1001_s
         WHERE rsign <> 'A'
           AND relat <> '007'.

      SORT lt_p1001_s BY objid.

      LOOP AT lt_p1001_s INTO DATA(ls_p1001_s).
        CLEAR ls_object_tmp.

        ls_object_tmp-plvar = ls_p1001_s-plvar.
        ls_object_tmp-objid = ls_p1001_s-sobid.
        ls_object_tmp-otype = 'S'.

        APPEND ls_object_tmp TO lt_object_s.
      ENDLOOP.

      lo_get_governor->read_om_infty(
         EXPORTING
           i_infty  = '1001'
           i_begda  = iv_begda
           i_endda  = iv_begda
           i_object = lt_object_s
         IMPORTING
           e_pnnnn  = lt_p1001_p ).

      SORT lt_p1001_p BY objid.

      DELETE lt_p1001_p
         WHERE rsign <> 'A'
           AND relat <> '008'
            OR prozt <> '100'.

      READ TABLE lt_p1001_p
          INTO DATA(ls_p1001_p)
          INDEX 1 .

      IF ls_p1001_p-sobid <> lv_pernr.
        lv_pernr = ls_p1001_p-sobid.
      ENDIF.

      FREE lo_get_governor.

    ENDIF.

    IF lv_pernr IS INITIAL.
      RAISE no_pernr.
    ENDIF.

    SELECT SINGLE pernr FROM pa0000 INTO @DATA(lv_pernr0)   WHERE   begda <= @iv_begda
                                              AND endda >= @iv_begda
                                              AND pernr   = @lv_pernr AND stat2 EQ '3'.

    IF sy-subrc NE 0.
      RAISE pernr_inactive.
    ENDIF.

    " руководитель второй орг ед?
*    me->check_head_is_second_lvl(
*            EXPORTING
*              iv_pernr = lv_pernr
*              iv_begda = iv_begda
*              iv_endda = iv_begda
*            IMPORTING
*              ev_is_second_lvl = DATA(lv_is_second_lvl) ).

*    IF lv_is_second_lvl IS INITIAL
*        AND iv_xss <> me->gc_gss_gov.
*      RAISE no_sec_lev_ruk.
*    ENDIF.

    DATA:
      lv_i        TYPE i,
      lv_head_lvl TYPE i.

    me->get_struc(
          EXPORTING
             iv_objid  = lv_pernr
             iv_otype  = 'P'"me->gc_obj_type_p
             iv_begda  = iv_begda
             iv_endda  = iv_begda
             iv_wegid  = 'Z_RUK_O'"me->gc_wegid_head_all
             iv_acheck = abap_false
           IMPORTING
             et_struc  = DATA(lt_struc) ).

    DELETE lt_struc
        WHERE otype <> 'O'."me->gc_obj_type_o.

    LOOP AT lt_struc TRANSPORTING NO FIELDS "Определяем уровень вложения, отсчитывая кол-во орг.ед. "сверху внизу"
      WHERE pdown = '0'.

      lv_head_lvl = lv_i = 1.

      WHILE lv_i <> 0.

        READ TABLE lt_struc                     "Считываем О на уровень ниже и т.д. пока не придем к той, которой руководим
            ASSIGNING FIELD-SYMBOL(<fs_struc>)
            INDEX sy-tabix - 1.

        IF sy-subrc = 0.

          IF <fs_struc>-pdown > 0.    "Проверка на руководство несколькими О, если = 0, то новая иерархия О-шек, проверим их в с след. итерации
            ADD 1 TO lv_head_lvl.
          ELSE.
            CLEAR lv_i.
          ENDIF.

        ELSE.
          CLEAR lv_i.
        ENDIF.
      ENDWHILE.

      IF lv_head_lvl = 2.             "Если есть хотя бы одна О второго уровня
        DATA(lv_is_second_lvl) = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.

    CLEAR lt_struc.


    me->get_struc(
          EXPORTING
            iv_objid = lv_pernr
            iv_otype = 'P'
            iv_begda  = iv_begda
            iv_endda  = iv_begda
            iv_wegid  = 'Z_HEAD_O'
            iv_acheck = abap_false
          IMPORTING
            et_struc  = DATA(lt_struc_head_o) ).

    DELETE lt_struc_head_o
        WHERE NOT otype = 'O'.

    IF iv_xss = me->gc_gss_gov.
      SORT lt_struc_head_o DESCENDING BY level.                        "Для блока Губернатора оперируем всей орг.структурой
      DELETE lt_struc_head_o
          WHERE pprev <> 0.
    ENDIF.

    LOOP AT lt_struc_head_o INTO DATA(ls_struc_head_o).

      me->get_struc( EXPORTING iv_objid  = CONV hrobjid( ls_struc_head_o-objid )
                               iv_otype  = ls_struc_head_o-otype
                               iv_begda  = iv_begda
                               iv_endda  = iv_begda
                               iv_wegid  = 'ZO-O-S-P'
                               iv_acheck = abap_false
                     IMPORTING et_struc  = lt_struc ).

      DATA: lt_struc_sort TYPE SORTED TABLE OF struc WITH NON-UNIQUE KEY seqnr
          , lt_object     TYPE hrobject_t
          , lt_sobject    TYPE hrobject_t
          , lt_pernr      TYPE hrobject_t

          , ls_object     LIKE LINE OF lt_object
          .

      ls_object-plvar = mv_plvar.

      LOOP AT lt_struc ASSIGNING FIELD-SYMBOL(<ls_struc>).
        INSERT <ls_struc> INTO TABLE lt_struc_sort.
        ls_object-otype = <ls_struc>-otype.
        ls_object-objid = <ls_struc>-objid.
        CASE ls_object-otype.
          WHEN 'P'.
            READ TABLE lt_pernr
                TRANSPORTING NO FIELDS
                  WITH KEY otype = ls_object-otype objid = ls_object-objid.

            IF sy-subrc <> 0.
              COLLECT ls_object INTO lt_pernr.
            ENDIF.

          WHEN OTHERS.
            READ TABLE lt_pernr
                TRANSPORTING NO FIELDS
                  WITH KEY otype = ls_object-otype objid = ls_object-objid.

            IF sy-subrc <> 0.
              COLLECT ls_object INTO lt_object.
            ENDIF.

            IF ls_object-otype = 'S'.
              READ TABLE lt_pernr
                TRANSPORTING NO FIELDS
                  WITH KEY otype = ls_object-otype objid = ls_object-objid.

              IF sy-subrc <> 0.
                COLLECT ls_object INTO lt_sobject.
              ENDIF.
            ENDIF.
        ENDCASE.
      ENDLOOP.

      DATA: lt_p1002_exp TYPE TABLE OF p1002_exp
          , lt_p1000     TYPE TABLE OF p1000
          , lt_p9109     TYPE TABLE OF p9109
          , lt_p9104     TYPE TABLE OF p9104
          , lt_p1007     TYPE TABLE OF p1007
          , lt_p1001     TYPE TABLE OF p1001
          .

      IF lt_pernr IS NOT INITIAL.
        SELECT pernr,
               nachn,
               vorna,
               midnm
               INTO TABLE @DATA(lt_pa0002)
               FROM pa0002
               FOR ALL ENTRIES IN @lt_pernr WHERE pernr =  @lt_pernr-objid
                                              AND begda <= @iv_begda
                                              AND endda >= @iv_begda.
        SORT lt_pa0002 BY pernr.
      ENDIF.

      DATA(lo_get_data) = NEW zcl_hr_get_data( ).

      lo_get_data->read_om_infty( EXPORTING i_infty  = '1002'
                                            i_subty  = '0001'
                                            i_begda  = iv_begda
                                            i_endda  = iv_begda
                                            i_object = lt_object
                                  IMPORTING e_pnnnn  = lt_p1002_exp ).

      lo_get_data->read_om_infty( EXPORTING i_infty  = '1000'
                                            i_begda  = iv_begda
                                            i_endda  = iv_begda
                                            i_object = lt_object
                                  IMPORTING e_pnnnn  = lt_p1000 ).

      lo_get_data->read_om_infty( EXPORTING i_infty  = '9109'
                                            i_begda  = iv_begda
                                            i_object = lt_object
                                  IMPORTING e_pnnnn  = lt_p9109 ).

      lo_get_data->read_om_infty( EXPORTING i_infty  = '9104'
                                            i_begda  = iv_begda
                                            i_object = lt_object
                                  IMPORTING e_pnnnn  = lt_p9104 ).

      lo_get_data->read_om_infty( EXPORTING i_infty  = '1007'
                                            i_begda  = iv_begda
                                            i_endda  = iv_begda
                                            i_object = lt_object
                                  IMPORTING e_pnnnn  = lt_p1007 ).
      IF iv_xss = me->gc_gss_gov.
        lo_get_data->read_om_infty( EXPORTING i_infty  = '1001'
                                              i_begda  = iv_begda
                                              i_endda  = iv_begda
                                              i_object = lt_sobject
                                    IMPORTING e_pnnnn  = lt_p1001 ).

        DELETE lt_p1001 WHERE rsign <> 'B' OR relat <> '007'.
*        DELETE lt_p1001 WHERE rsign <> 'B' AND relat <> '007'.
        SORT lt_p1001 BY objid.
      ENDIF.


      SORT: lt_p1002_exp BY otype objid langu tabseqnr
          , lt_p1000     BY otype objid langu
          , lt_p9109     BY otype objid begda DESCENDING
          , lt_p9104     BY otype objid begda DESCENDING
          , lt_p1007     BY otype objid
          .

      IF iv_xss = me->gc_gss_gov
          AND me->gr_c_objec IS INITIAL.
        "чтение набора разрешенных ИД должностей
        zcl_hr_get_data=>read_stvarv_ran( EXPORTING iv_name  = gc_for_view_tc
                                          IMPORTING et_range = gr_c_objec ).
        check_range( CHANGING ct_objec = gr_c_objec ).
      ENDIF.

      LOOP AT lt_struc ASSIGNING FIELD-SYMBOL(<lt_struc>).

        READ TABLE et_struc
          WITH KEY object-objid = <lt_struc>-objid object-otype = <lt_struc>-otype
          TRANSPORTING NO FIELDS.

        IF sy-subrc = 0.
          CONTINUE.
        ENDIF.

        IF iv_xss = me->gc_gss_gov.
          CASE <lt_struc>-otype.
            WHEN 'O'.
              lv_view = abap_true.
            WHEN 'S'.
              lv_view = abap_true.
              lv_view = check_no_view( is_struc = <lt_struc>
                                       it_p1001 = lt_p1001 ).
          ENDCASE.

          IF lv_view = abap_false.
            CONTINUE.
          ENDIF.
        ENDIF.

        IF  <lt_struc>-otype = 'S' AND             " проверка на устаревший

                    is_redundant( iv_objid = CONV #( <lt_struc>-objid )
                                  iv_otype = <lt_struc>-otype
                                  iv_begda = iv_begda
                                  iv_endda = iv_begda ) = abap_true.

          CONTINUE.
        ENDIF.

        APPEND INITIAL LINE TO et_struc ASSIGNING FIELD-SYMBOL(<et_struc>).
        <et_struc>-object-otype = <lt_struc>-otype.
        <et_struc>-object-objid = <lt_struc>-objid.
        "Вышестоящий объект
*        READ TABLE lt_struc_sort ASSIGNING FIELD-SYMBOL(<lt_struc_sort>) WITH KEY seqnr = <lt_struc>-pup BINARY SEARCH.
        READ TABLE lt_struc_sort INTO DATA(ls_struc_sort) WITH KEY seqnr = <lt_struc>-pup BINARY SEARCH.
        IF sy-subrc = 0.
          <et_struc>-parent-otype = ls_struc_sort-otype.
          <et_struc>-parent-objid = ls_struc_sort-objid.
        ENDIF.

        CASE <et_struc>-object-otype.
          WHEN 'P'.
            "Фио
            READ TABLE lt_pa0002 ASSIGNING FIELD-SYMBOL(<lt_pa0002>) WITH KEY pernr = <et_struc>-object-objid BINARY SEARCH.
            IF <lt_pa0002> IS ASSIGNED AND sy-subrc = 0.
              <et_struc>-descr = <lt_pa0002>-nachn && ` ` && <lt_pa0002>-vorna && ` ` &&  <lt_pa0002>-midnm.
            ENDIF.
            CONTINUE.
          WHEN 'S'.
            "Дата положения для О или должностного регламента для S
            READ TABLE lt_p9104 ASSIGNING FIELD-SYMBOL(<lt_p9104>) WITH KEY otype = <et_struc>-object-otype
                                                                            objid = <et_struc>-object-objid BINARY SEARCH.
            IF <lt_p9104> IS ASSIGNED AND sy-subrc = 0.
              <et_struc>-rdate = <lt_p9104>-cndat.
            ENDIF.
            "Дата открытия вакансии
            READ TABLE lt_p1007 ASSIGNING FIELD-SYMBOL(<lt_p1007>) WITH KEY otype = <et_struc>-object-otype
                                                                            objid = <et_struc>-object-objid BINARY SEARCH.
            IF  sy-subrc = 0 AND <lt_p1007>-status = '0'. "<lt_p9104> IS ASSIGNED AND
              <et_struc>-sdate = <lt_p1007>-begda.
            ENDIF.
          WHEN 'O'.
            "Дата положения для О или должностного регламента для S
            READ TABLE lt_p9109 ASSIGNING FIELD-SYMBOL(<lt_p9109>) WITH KEY otype = <et_struc>-object-otype
                                                                            objid = <et_struc>-object-objid BINARY SEARCH.
            IF <lt_p9109> IS ASSIGNED AND sy-subrc = 0.
              <et_struc>-rdate = <lt_p9109>-sdate.
            ENDIF.
          WHEN OTHERS.
            CONTINUE.
        ENDCASE.

        READ TABLE lt_p1002_exp TRANSPORTING NO FIELDS WITH KEY otype = <et_struc>-object-otype
                                                                objid = <et_struc>-object-objid
                                                                langu = sy-langu BINARY SEARCH.
        IF sy-subrc = 0.
          LOOP AT lt_p1002_exp ASSIGNING FIELD-SYMBOL(<lt_p1002_exp>) FROM sy-tabix WHERE otype = <et_struc>-object-otype
                                                                                      AND objid = <et_struc>-object-objid
                                                                                      AND langu = sy-langu.
            <et_struc>-descr = <et_struc>-descr && ` ` && <lt_p1002_exp>-tline.
          ENDLOOP.
        ENDIF.

        IF <et_struc>-descr IS INITIAL.
          READ TABLE lt_p1000 ASSIGNING FIELD-SYMBOL(<lt_p1000>) WITH KEY otype = <et_struc>-object-otype
                                                                          objid = <et_struc>-object-objid
                                                                          langu = sy-langu BINARY SEARCH.
          IF <lt_p1000> IS ASSIGNED AND sy-subrc = 0.
            <et_struc>-descr = <lt_p1000>-stext.
          ENDIF.
        ELSE.
          <et_struc>-descr = <et_struc>-descr+1.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    IF iv_xss = me->gc_gss_gov.
      me->hide_org_struc_empty_elem(
              EXPORTING
                it_sort_struc_all = lt_struc_sort
              CHANGING
                ct_org_struc = et_struc ).
    ENDIF.

  ENDMETHOD.


  METHOD ws_get_t13_xml.
    DATA: lr_peri   TYPE RANGE OF datum,
          lv_date_c TYPE char10.

    FIELD-SYMBOLS <ls_peri> LIKE LINE OF lr_peri.

    "табельный
    DATA(lv_pernr) = me->get_pernr( iv_usrid = CONV #( iv_euser ) iv_begda = iv_begda ).

    IF lv_pernr IS INITIAL.
      RAISE no_pernr.
    ENDIF.


    me->check_head_is_second_lvl(
            EXPORTING
              iv_pernr = lv_pernr
              iv_begda = iv_begda
              iv_endda = iv_endda
            IMPORTING
              ev_is_second_lvl = DATA(lv_is_second_lvl)
              et_struc = DATA(lt_struc) ).

    IF lv_is_second_lvl IS INITIAL.
      CALL FUNCTION 'RH_STRUC_GET'
        EXPORTING
          act_otype      = 'P'
          act_objid      = CONV hrobjid( lv_pernr )
          act_wegid      = 'P-S-O-O'
          act_plvar      = '01'
          act_begda      = iv_begda
          act_endda      = iv_endda
        TABLES
          result_struc   = lt_struc
        EXCEPTIONS
          no_plvar_found = 1
          no_entry_found = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.

    DELETE lt_struc WHERE NOT otype = 'O'.

    READ TABLE lt_struc INTO DATA(ls_struc) INDEX 1.
    CHECK sy-subrc = 0.

    " с отчетом hrultab0 проблема в плане расширений
    " пока параметры передаем через память
    EXPORT gv_objid FROM ls_struc-objid TO MEMORY ID  zcl_z_mss_t13_dpc_ext=>c_memory_xml.

    SUBMIT hrultab0
     WITH p_objid = ls_struc-objid
     WITH p_xml   = abap_true
     WITH pabrj   = iv_begda(4)
     WITH pabrp   = iv_begda+4(2)
     AND RETURN.

    IMPORT lv_xstring TO e_xstring FROM MEMORY ID zcl_z_mss_t13_dpc_ext=>c_memory_id.
    FREE MEMORY ID zcl_z_mss_t13_dpc_ext=>c_memory_id.
  ENDMETHOD.


METHOD _fill_comp_mail_from.

  zcl_hr_get_data=>read_stvarv_par( EXPORTING i_name  = 'ZHR_MAIL_SENDER'
                                    IMPORTING e_value = rv_mail_from ).

ENDMETHOD.


METHOD _fill_comp_mail_to.

  DATA: lt_bkper TYPE STANDARD TABLE OF zthr_bkper-bkper,
        lt_p0105 TYPE STANDARD TABLE OF p0105,
        lv_all   TYPE xfeld.

  IF it_pernr IS NOT INITIAL.
    lt_bkper = it_pernr.
    DELETE lt_bkper WHERE table_line IS INITIAL.
  ENDIF.


  DATA(lo_data) = NEW zcl_hr_get_data( ).

  DO 2 TIMES.
    LOOP AT lt_bkper ASSIGNING FIELD-SYMBOL(<lv_bkper>).

      REFRESH lt_p0105.

      lo_data->read_pa_infty( EXPORTING i_pernr = CONV #( <lv_bkper> )
                                        i_begda = sy-datum
                                        i_endda = sy-datum
                                        i_infty = '0105'
                                        i_subty = '0010'
                              IMPORTING e_pnnnn = lt_p0105 ).

      READ TABLE lt_p0105 ASSIGNING FIELD-SYMBOL(<ls_p0105>) INDEX 1.
      IF sy-subrc = 0.
        CHECK <ls_p0105>-usrid_long IS NOT INITIAL.
        APPEND INITIAL LINE TO et_to ASSIGNING FIELD-SYMBOL(<lv_to>).
        <lv_to> = <ls_p0105>-usrid_long.
      ENDIF.

    ENDLOOP.

    IF et_to IS NOT INITIAL OR lv_all = abap_true.
      EXIT.
    ENDIF.

    IF lt_bkper IS INITIAL.
      SELECT bkper
        INTO TABLE lt_bkper
        FROM zthr_bkper.      "#EC CI_GENBUFF

      lv_all = abap_true.
    ENDIF.
  ENDDO.


ENDMETHOD.


METHOD _fill_comp_mapping.

  DEFINE _fill_map.
    APPEND INITIAL LINE TO et_mapping ASSIGNING <ls_mapping>.
    <ls_mapping>-name  = &1.
    <ls_mapping>-value = &2.
  END-OF-DEFINITION.

  DATA: lv_value TYPE string,
        lv_text  TYPE string.

  FIELD-SYMBOLS: <ls_mapping> TYPE LINE OF zttmail_mapping.

  DATA(lo_hr_data) = NEW zcl_hr_get_data( ).

  lo_hr_data->get_objid_struc( EXPORTING i_otype = 'S'
                                         i_objid = CONV #( is_input_data-objid )
                                         i_begda = sy-datum
                                         i_wegid = 'S-0-0'
                               IMPORTING e_struc = DATA(lt_struc) ).
  "Удаляем ОЕ первого уровня
  IF lt_struc IS NOT INITIAL.
    DELETE lt_struc INDEX lines( lt_struc ).
  ENDIF.


  "ОИВ
  CLEAR lv_value.
  READ TABLE lt_struc ASSIGNING FIELD-SYMBOL(<ls_struc>) INDEX lines( lt_struc ).
  IF sy-subrc = 0.
    _fill_map '<Orgeh2_id>' <ls_struc>-objid.


    lv_value = zcl_hr_data_utils=>get_name_obj( i_objid = CONV #( <ls_struc>-objid )
                                                i_otype = <ls_struc>-otype
                                                i_begda = sy-datum
                                                i_subty = '0001' ).
    IF lv_value IS INITIAL.
      lv_value = zcl_hr_data_utils=>get_name_obj_1000( i_objid = CONV #( <ls_struc>-objid )
                                                       i_otype = <ls_struc>-otype
                                                       i_begda = sy-datum ).
    ENDIF.
  ELSE.
    _fill_map '<Orgeh2_id>' ''.
  ENDIF.
  _fill_map '<Orgeh2_name>' lv_value.

  "Удаляем ОЕ второго уровня
  IF lt_struc IS NOT INITIAL.
    DELETE lt_struc INDEX lines( lt_struc ).
  ENDIF.

  "Штатная должность
  _fill_map '<Plans_id>' is_input_data-objid.
  CLEAR lv_value.
  LOOP AT lt_struc ASSIGNING <ls_struc>.
    CLEAR lv_text.
    lv_text = zcl_hr_data_utils=>get_name_obj( i_objid = CONV #( <ls_struc>-objid )
                                                i_otype = <ls_struc>-otype
                                                i_begda = sy-datum
                                                i_subty = '0001' ).
    IF lv_text IS INITIAL.
      lv_text = zcl_hr_data_utils=>get_name_obj_1000( i_objid = CONV #( <ls_struc>-objid )
                                                      i_otype = <ls_struc>-otype
                                                      i_begda = sy-datum ).
    ENDIF.

    IF lv_value IS INITIAL.
      lv_value = lv_text.
    ELSE.
      lv_value = |{ lv_value }, { lv_text }|.
    ENDIF.
  ENDLOOP.
  _fill_map '<Plans_name>' lv_value.

  "Кадровый резерв
  IF is_p9103-kadrz = 'X'.
    lv_value = 'да'.
  ELSE.
    lv_value = 'нет'.
  ENDIF.
  _fill_map '<Kadrz>' lv_value.

  "Должностной регламент подтвержден
  IF is_p9103-reg_accept = 'X'.
    lv_value = 'да'.
  ELSE.
    lv_value = 'нет'.
  ENDIF.
  _fill_map '<RegAccept>' lv_value.

  "Инициатор заявки
  CLEAR lv_value.
  zcl_hr_data=>get_fio( EXPORTING i_pernr = iv_pernr
                        IMPORTING e_fio_full = lv_value ).
  _fill_map '<Fio>' lv_value.

  "Комментарии
  _fill_map '<Comment>' is_input_data-comment.

ENDMETHOD.
ENDCLASS.
