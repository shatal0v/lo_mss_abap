*----------------------------------------------------------------------*
*       CLASS ZCL_HRPA_TVARVC DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_hrpa_tvarvc DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
*"* public components of class ZCL_HRPA_TVARVC
*"* do not include other source files here!!!

    TYPES:
      tt_range TYPE RANGE OF tvarv_val .
    TYPES:
      ts_range TYPE LINE OF tt_range .

    CLASS-METHODS read_parameter
      IMPORTING
        !i_name TYPE rvari_vnam
      RETURNING
        value(r_value) TYPE tvarv_val
*      RAISING
*        zcx_hrpa_tvarvc
.
    CLASS-METHODS read_range
      IMPORTING
        !i_name TYPE rvari_vnam
      RETURNING
        value(rt_range) TYPE tt_range
*      RAISING
*        zcx_hrpa_tvarvc
.
  PROTECTED SECTION.
*"* protected components of class ZCL_HRPA_TVARVC
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_HRPA_TVARVC
*"* do not include other source files here!!!

    TYPES ts_tvarvc TYPE tvarvc .
    TYPES:
      tt_tvarvc TYPE HASHED TABLE OF ts_tvarvc
                     WITH UNIQUE KEY name type numb .

    CLASS-DATA ao_hrpa_tvarvc TYPE REF TO zcl_hrpa_tvarvc .
    CLASS-DATA at_tvarvc TYPE tt_tvarvc .
    CLASS-DATA as_tvarvc TYPE ts_tvarvc .
    CONSTANTS co_type_param TYPE rsscr_kind VALUE 'P'.      "#EC NOTEXT
    CONSTANTS co_type_range TYPE rsscr_kind VALUE 'S'.      "#EC NOTEXT

    CLASS-METHODS get_tvarvc_from_cache
      IMPORTING
        !i_name TYPE rvari_vnam
        !i_type TYPE rsscr_kind
        !i_numb TYPE tvarv_numb OPTIONAL
      RETURNING
        value(rs_tvarvc) TYPE ts_tvarvc
      RAISING
        zcx_hrpa_tvarvc
        .
    CLASS-METHODS get_tvarvc_from_db
      IMPORTING
        !i_name TYPE rvari_vnam
        !i_type TYPE rsscr_kind
        !i_numb TYPE tvarv_numb
      RETURNING
        value(rs_tvarvc) TYPE ts_tvarvc
      RAISING
        zcx_hrpa_tvarvc
        .
    CLASS-METHODS raise_exception
      IMPORTING
        !i_name TYPE rvari_vnam
        !i_type TYPE rsscr_kind
      RAISING
        zcx_hrpa_tvarvc
        .
    CLASS-METHODS is_exist_in_cache
      IMPORTING
        !i_name TYPE rvari_vnam
      RETURNING
        value(r_result) TYPE boole_d .
ENDCLASS.



CLASS ZCL_HRPA_TVARVC IMPLEMENTATION.


  METHOD get_tvarvc_from_cache.
    READ TABLE at_tvarvc INTO rs_tvarvc
              WITH TABLE KEY name = i_name
                             type = i_type
                             numb = i_numb.

    IF sy-subrc <> 0.
      TRY.
          rs_tvarvc = get_tvarvc_from_db( i_name = i_name
                                         i_type = i_type
                                         i_numb = i_numb ).
        CATCH zcx_hrpa_tvarvc.
      ENDTRY.
    ENDIF.
  ENDMETHOD.                    "GET_TVARVC_FROM_CACHE


  METHOD get_tvarvc_from_db.
    DATA:
          lt_tvarvc TYPE STANDARD TABLE OF tvarvc,
          ls_tvarvc TYPE tvarvc.

* Check before select data from DB
    IF is_exist_in_cache( i_name ) = abap_true.
      raise_exception( i_name = i_name
                       i_type = i_type ).
    ENDIF.

    SELECT *
      FROM tvarvc
      INTO TABLE lt_tvarvc
      WHERE name = i_name
        AND type = i_type.

    IF sy-subrc <> 0.
      raise_exception( i_name = i_name
                       i_type = i_type ).
    ENDIF.

    LOOP AT lt_tvarvc INTO ls_tvarvc.
* Return
      IF ls_tvarvc-numb = i_numb.
        rs_tvarvc = ls_tvarvc.
      ENDIF.

* Move value to buffer
      INSERT ls_tvarvc INTO TABLE at_tvarvc.
      ASSERT sy-subrc = 0.
    ENDLOOP.

    IF rs_tvarvc IS INITIAL.
      raise_exception( i_name = i_name
                       i_type = i_type ).
    ENDIF.
  ENDMETHOD.                    "GET_TVARVC_FROM_DB


  METHOD is_exist_in_cache.
    r_result = abap_false.

    READ TABLE at_tvarvc TRANSPORTING NO FIELDS
              WITH KEY name = i_name.

    IF sy-subrc = 0.
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.                    "IS_EXIST_IN_CACHE


  METHOD raise_exception.
    DATA:
          l_dummy TYPE boole_d
        , ls_sy TYPE syst
        .

    IF i_type = co_type_param.
*    мешает в отладчике, добавляем без message!!!
      ls_sy-msgid = 'ZHR_PA'.
      ls_sy-msgno = '005'.
      ls_sy-msgv1 = i_name.
      IF 1 = 0.
        MESSAGE e005(zhr_pa).
      ENDIF.
*    MESSAGE e005(zhr_pa) WITH i_name INTO l_dummy.
    ELSE.
      ls_sy-msgid = 'ZHR_PA'.
      ls_sy-msgno = '006'.
      ls_sy-msgv1 = i_name.
      IF 1 = 0.
        MESSAGE e006(zhr_pa).
      ENDIF.
*    MESSAGE e006(zhr_pa) WITH i_name INTO l_dummy.
    ENDIF.

    RAISE EXCEPTION TYPE zcx_hrpa_tvarvc.
  ENDMETHOD.                    "RAISE_EXCEPTION


  METHOD read_parameter.
    IF as_tvarvc-name <> i_name.
      as_tvarvc = get_tvarvc_from_cache( i_name = i_name
                                         i_type = co_type_param ).
    ENDIF.

    r_value = as_tvarvc-low.
  ENDMETHOD.                    "READ_PARAMETER


  METHOD read_range.
    DATA:
          l_numb TYPE tvarv_numb.

    FIELD-SYMBOLS:
                   <ls_range> TYPE ts_range.

    DO 9999 TIMES.
      l_numb = sy-index - 1.

      TRY.
          as_tvarvc = get_tvarvc_from_cache( i_name = i_name
                                             i_type = co_type_range
                                             i_numb = l_numb ).

          if as_tvarvc is INITIAL.
            exit.
          ENDIF.

          APPEND INITIAL LINE TO rt_range ASSIGNING <ls_range>.

          CHECK sy-subrc = 0.

          <ls_range>-sign   = as_tvarvc-sign.
          <ls_range>-option = as_tvarvc-opti.
          <ls_range>-low    = as_tvarvc-low.
          <ls_range>-high   = as_tvarvc-high.

        CATCH zcx_hrpa_tvarvc.
         EXIT.
      ENDTRY.
    ENDDO.
  ENDMETHOD.                    "READ_RANGE
ENDCLASS.
