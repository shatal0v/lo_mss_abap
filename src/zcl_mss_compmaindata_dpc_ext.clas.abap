class ZCL_MSS_COMPMAINDATA_DPC_EXT definition
  public
  inheriting from ZCL_MSS_COMPMAINDATA_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
protected section.

  methods COMPMAINDATASET_GET_ENTITYSET
    redefinition .
  methods COMPMAINDESCSET_GET_ENTITYSET
    redefinition .
private section.

  methods _GET_REPORT
    importing
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
    exporting
      !E_XSTRING type XSTRINGVAL
      !E_FILENAME type PSTRING .
  methods _GET_REPORT_TXT
    importing
      !IV_OBJID type HROBJID
    returning
      value(RV_TXT) type PSTRING .
  methods _GET_WWW
    importing
      !IV_OBJID type PSTRING
    returning
      value(RV_X) type XSTRING .
ENDCLASS.



CLASS ZCL_MSS_COMPMAINDATA_DPC_EXT IMPLEMENTATION.


method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM.
    DATA: ls_stream  TYPE ty_s_media_resource,
          ls_lheader TYPE ihttpnvp.
    CASE iv_entity_name.
      WHEN 'Print'.
        ls_stream-mime_type = 'application/vnd.openxmlformats-officedocument.wordprocessing'.
        _get_report(
          EXPORTING
            it_key_tab = it_key_tab
          IMPORTING
            e_xstring = ls_stream-value
            e_filename = DATA(lv_file) ).
        ls_lheader-name = 'Content-Disposition'.
        ls_lheader-value = 'inline; filename="' && lv_file && '";'.
        set_header( is_header = ls_lheader ).
    ENDCASE.
*
    copy_data_to_ref( EXPORTING is_data = ls_stream
                      CHANGING  cr_data = er_stream ).
  endmethod.


  METHOD compmaindataset_get_entityset.

    DATA: BEGIN OF ls_input
              , main_user     TYPE string
              , entrance_user TYPE string
              , END OF ls_input

              , lr_user TYPE RANGE OF string
              .
    DATA: lv_begda TYPE dats.
    DATA: lv_endda TYPE dats.
    TRY.
        DATA(lo_filter) = io_tech_request_context->get_filter( ).

        LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<it_filter>).
          REFRESH lr_user.
          DATA(lv_property) = <it_filter>-property.
          TRANSLATE lv_property TO UPPER CASE.
          ASSIGN COMPONENT <it_filter>-property OF STRUCTURE ls_input TO FIELD-SYMBOL(<fs_input>).
          CHECK <fs_input> IS ASSIGNED AND sy-subrc = 0.

          lo_filter->convert_select_option( EXPORTING is_select_option = <it_filter>
                                            IMPORTING et_select_option = lr_user ).
          READ TABLE lr_user INTO DATA(ls_user) INDEX 1.
          <fs_input> = ls_user-low.
        ENDLOOP.

        DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

*21/03/2020
        IF ls_input-entrance_user IS NOT INITIAL.
          lo_assistent->ws_get_compmaindata( EXPORTING  iv_euser         = ls_input-entrance_user
                                                       IMPORTING   et_compmaindata = DATA(lt_compmaindata)
                                             EXCEPTIONS no_pernr         = 1
                                                        no_sec_lev_ruk   = 2 ).
        ELSE.
          lo_assistent->ws_get_compmaindata( EXPORTING  iv_euser         = ls_input-main_user
                                             IMPORTING   et_compmaindata = lt_compmaindata
                                             EXCEPTIONS no_pernr         = 1
                                                        no_sec_lev_ruk   = 2 ).
        ENDIF.

        DATA: lt_return            TYPE bapiret2_t
            , lo_message_container TYPE REF TO /iwbep/if_message_container
         .
        APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<lt_return>).
        APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<et_entityset0>).
        CASE sy-subrc.
          WHEN 1.
            MESSAGE e002(zhr_mss) WITH ls_input-entrance_user INTO <lt_return>-message.
            <lt_return>-type   = sy-msgty.
            <lt_return>-id     = sy-msgid.
            <lt_return>-number = sy-msgno.
            <et_entityset0>-msgid = 'E'.

            MESSAGE e015(zhr_mss) WITH ls_input-entrance_user INTO <et_entityset0>-message.
*            <et_entityset0>-message = 'Не удалось определить таб номер для пользователя'.
          WHEN 2.
*20/02/2020   DATA(lv_name) = lo_assistent->get_user_fio( iv_usrid = ls_input-entrance_user ).
            DATA(lv_name) = lo_assistent->get_user_fio( iv_usrid = ls_input-main_user ).
            MESSAGE e001(zhr_mss) WITH lv_name INTO <lt_return>-message.
            <lt_return>-type   = sy-msgty.
            <lt_return>-id     = sy-msgid.
            <lt_return>-number = sy-msgno.
            <et_entityset0>-msgid = 'E'.
            MESSAGE e017(zhr_mss) WITH lv_name INTO <et_entityset0>-message.
*            <et_entityset0>-message = 'Для пользователя нет данных для просмотра'.
          WHEN OTHERS.
            REFRESH lt_return.
            REFRESH et_entityset.
        ENDCASE.
        .
        IF NOT lt_return IS INITIAL.
          lo_message_container = mo_context->get_message_container( ).
          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.
        ENDIF.

        LOOP AT lt_compmaindata ASSIGNING FIELD-SYMBOL(<lt_compmaindata>).
          APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<et_entityset>).
          MOVE-CORRESPONDING <lt_compmaindata> TO <et_entityset>.
          <et_entityset>-msgid = 'S'.
          MESSAGE s008(zhr_mss) INTO <et_entityset>-message.
        ENDLOOP.
      CATCH /iwbep/cx_mgw_busi_exception .
      CATCH /iwbep/cx_mgw_tech_exception .
    ENDTRY.


  ENDMETHOD.


  METHOD compmaindescset_get_entityset.
    DATA: lt_objid TYPE hrobject_t
        , ls_objid LIKE LINE OF lt_objid
        , lr_objid TYPE RANGE OF string
        .

    TRY.
        DATA(lo_filter) = io_tech_request_context->get_filter( ).

        LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<it_filter>).
          DATA(lv_property) = <it_filter>-property.
          TRANSLATE lv_property TO UPPER CASE.
          ASSIGN COMPONENT <it_filter>-property OF STRUCTURE ls_objid TO FIELD-SYMBOL(<fs_input>).
          CHECK <fs_input> IS ASSIGNED AND sy-subrc = 0.

          lo_filter->convert_select_option( EXPORTING is_select_option = <it_filter>
                                            IMPORTING et_select_option = lr_objid ).
         loop at lr_objid INTO DATA(ls_objid_range).
          <fs_input> = ls_objid_range-low.
          COLLECT ls_objid INTO lt_objid.
          endloop.
        ENDLOOP.

        DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

        lo_assistent->ws_get_compmaindesc( EXPORTING it_objid        = lt_objid
                                           IMPORTING et_compmaindesc = DATA(lt_compmaindesc) ).

        LOOP AT lt_compmaindesc ASSIGNING FIELD-SYMBOL(<lt_compmaindesc>).
          APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<et_entityset>).
          MOVE-CORRESPONDING <lt_compmaindesc> TO <et_entityset>.
        ENDLOOP.
      CATCH /iwbep/cx_mgw_busi_exception .
      CATCH /iwbep/cx_mgw_tech_exception .
    ENDTRY.
  ENDMETHOD.


METHOD _get_report.
  DATA: lt_p9103 TYPE TABLE OF p9103.
  DATA: BEGIN OF ls_data,
          init_ename TYPE pstring,
          init_plans TYPE pstring,
          plans_txt  TYPE pstring,
        END OF ls_data.
  DATA(lo_hr_data) = NEW zcl_hr_get_data( ).
  lo_hr_data->read_om_infty(
    EXPORTING
      i_otype = cl_hap_pmp_const=>otype_position
      i_objid = VALUE #( it_key_tab[ 1 ]-value OPTIONAL )
      i_infty = '9103'
      i_begda = sy-datum
      i_endda = sy-datum
    IMPORTING
      e_pnnnn = lt_p9103 ).
  IF lt_p9103 IS NOT INITIAL.
    ASSIGN lt_p9103[ 1 ] TO FIELD-SYMBOL(<ls_9103>).
    ls_data-plans_txt = _get_report_txt( <ls_9103>-objid ).
    ls_data-init_ename = lo_hr_data->get_pernr_fio(
        i_pernr = <ls_9103>-author
        i_begda = sy-datum
        i_type  = 1
    ).
    DATA(lv_stext) = VALUE pstring( ).
    zcl_hr_data=>get_name_plans(
      EXPORTING
        i_pernr  = <ls_9103>-author
      IMPORTING
        e_sname  = lv_stext
        e_lname  = ls_data-init_plans
    ).
    IF ls_data-init_plans IS INITIAL.
      ls_data-init_plans = lv_stext.
    ENDIF.
  ENDIF.
  DATA(lv_x) = _get_www( 'ZHR_CONTEST_LETTER' ).
  TRY.
      DATA(lo_docx) = cl_docx_document=>load_document( iv_data = lv_x ).
      DATA(lo_documentpart) = lo_docx->get_maindocumentpart( ).
    CATCH cx_openxml_format cx_openxml_not_found.
  ENDTRY.
  DATA(lv_doctext) = cl_openxml_helper=>xstring_to_string( lo_documentpart->get_data( ) ).
  lv_doctext = replace( val = lv_doctext sub = '[plans_txt]' with = ls_data-plans_txt occ = 0 ).
  lv_doctext = replace( val = lv_doctext sub = '[init_ename]' with = ls_data-init_ename occ = 0 ).
  lv_doctext = replace( val = lv_doctext sub = '[init_plans]' with = ls_data-init_plans occ = 0 ).
  lo_documentpart->feed_data( iv_data = cl_openxml_helper=>string_to_xstring( lv_doctext ) ).
  TRY.
      e_xstring = lo_docx->get_package_data( ).
    CATCH cx_openxml_format.
      RETURN.
  ENDTRY.
  e_filename = cl_http_utility=>escape_url( CONV #( text-rep ) ).
ENDMETHOD.


  METHOD _get_report_txt.
    DATA(lv_stext) = VALUE pstring( ).
    zcl_hr_data=>get_name_plans(
      EXPORTING
        i_plans = iv_objid
      IMPORTING
        e_sname  = lv_stext
        e_lname  = rv_txt
    ).
    IF rv_txt IS INITIAL.
      rv_txt = lv_stext.
    ENDIF.
    DATA(lv_org) = VALUE pstring( ).
    DATA(lv_long) = VALUE pstring( ).
    zcl_hr_data=>get_name_orgeh_full(
      EXPORTING
        i_plans     = iv_objid
      IMPORTING
        e_lname     = lv_long
    ).
    rv_txt = |{ rv_txt }, { lv_long }|.
    DATA(lv_kom2) = VALUE pstring( ).
    zcl_hr_data=>get_komitet(
      EXPORTING
        i_plans    = iv_objid
      IMPORTING
        e_lname    = lv_kom2
    ).
    FIND ALL OCCURRENCES OF lv_kom2 IN rv_txt RESULTS DATA(lv_res).
    IF sy-subrc <> 0.
      rv_txt = |{ rv_txt } { lv_kom2 }|.
    ENDIF.
    IF rv_txt IS NOT INITIAL.
      rv_txt = |{ rv_txt CASE = LOWER }|.
    ENDIF.
  ENDMETHOD.


METHOD _get_www.
  DATA: lt_mime TYPE TABLE OF w3mime.
  DATA(ls_www) = VALUE wwwdatatab( relid = 'MI' objid = iv_objid ).
  CALL FUNCTION 'WWWDATA_IMPORT'
    EXPORTING
      key    = ls_www
    TABLES
      mime   = lt_mime
    EXCEPTIONS
      OTHERS = 3.
  DATA(lv_filesize_c) = VALUE char100( ).
  DATA(lv_filesize) = VALUE int4( ).
  CALL FUNCTION 'WWWPARAMS_READ'
    EXPORTING
      relid  = ls_www-relid
      objid  = ls_www-objid
      name   = 'filesize'
    IMPORTING
      value  = lv_filesize_c
    EXCEPTIONS
      OTHERS = 2.
  lv_filesize = lv_filesize_c.
  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = lv_filesize
    IMPORTING
      buffer       = rv_x
    TABLES
      binary_tab   = lt_mime
    EXCEPTIONS
      OTHERS       = 0.
ENDMETHOD.
ENDCLASS.
