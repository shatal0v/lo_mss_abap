class ZCL_Z_MSS_SIGN_DOC_DPC_EXT definition
  public
  inheriting from ZCL_Z_MSS_SIGN_DOC_DPC
  create public .

public section.

  types:
    BEGIN OF TY_MAIN
          , field_01 TYPE text250
          , field_02 TYPE text250
          , field_03 TYPE text250
          , field_04 TYPE text250
          , field_05 TYPE text250
          , field_06 TYPE text250
          , field_07 TYPE text250
          , field_08 TYPE text250
          , field_09 TYPE text250
          , field_10 TYPE text250
          , field_11 TYPE text250
          , field_12 TYPE text250
          , field_13 TYPE text250
          , field_14 TYPE text250
          , field_15 TYPE text250
          , field_16 TYPE text250
          , field_17 TYPE text250
          , field_18 TYPE text250
          , field_19 TYPE text250
          , field_20 TYPE text250
       , END OF  TY_MAIN .

  class-methods GET_IPR
    importing
      !REQ_ID type ZHR_XSS_IPRREQ_REQID
    exporting
      !RETURN type BAPIRET2
    returning
      value(T_DATA_XML) type XSTRING .
  class-methods GET_LEAVE
    importing
      !REQ_ID type ZHR_XSS_IPRREQ_REQID
      !PERNR type PERNR_D
    exporting
      !ES_MAIN type TY_MAIN
      !RETURN type BAPIRET2
    returning
      value(T_DATA_XML) type XSTRING .
  class-methods GET_LEAVE_XML
    importing
      !REQ_ID type ZHR_XSS_IPRREQ_REQID
      !PERNR type PERNR_D
    exporting
      !ES_MAIN type TY_MAIN
      !RETURN type BAPIRET2
    returning
      value(T_DATA_XML) type XSTRING .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_STREAM
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~UPDATE_STREAM
    redefinition .
protected section.

  methods DOCDATASET_GET_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_MSS_SIGN_DOC_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~create_stream.

    DATA: ls_at_attr TYPE zthr_mss_at_attr
        , ls_at_cont TYPE zthr_mss_at_cont
        , lt_return TYPE bapiret2_t
        .

    DATA(lo_message_container) = mo_context->get_message_container( ).
    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

    LOOP AT it_key_tab ASSIGNING FIELD-SYMBOL(<it_key_tab>).
      CASE <it_key_tab>-name.
        WHEN 'DocTyp'.
          CASE <it_key_tab>-value.
            WHEN 1.
              ls_at_attr-ftype = 'IP'.
            WHEN 2.
              ls_at_attr-ftype = 'LE'.
            WHEN OTHERS.
              APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<lt_return>).
              MESSAGE e019(zhr_mss) INTO <lt_return>-message.
              <lt_return>-type   = sy-msgty.
              <lt_return>-id     = sy-msgid.
              <lt_return>-number = sy-msgno.

              lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

              RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
                EXPORTING
                  message_container = lo_message_container.
          ENDCASE.
        WHEN 'Pernr'.
          ls_at_attr-object = conv pernr_d( <it_key_tab>-value ).
        WHEN `FileName`.
          data(lv_filename) = conv text255( <it_key_tab>-value ).
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    ls_at_attr-fname  = iv_source_name.
    ls_at_attr-datum  = sy-datum.
    ls_at_attr-uzeit  = sy-uzeit.
    ls_at_attr-uname  = sy-uname.

    ls_at_attr-length = xstrlen( is_media_resource-value ).

    SELECT SINGLE doc_type INTO @DATA(lv_doctype) FROM toadd WHERE mimetype EQ @is_media_resource-mime_type.
    IF sy-subrc EQ 0.
      CONCATENATE iv_source_name '.' lv_doctype INTO ls_at_attr-fname.
      ls_at_attr-dtype  = lv_doctype.
    ELSE.
      ls_at_attr-dtype  = is_media_resource-mime_type.
      ls_at_attr-fname  = iv_source_name.
    ENDIF.

    IF lv_filename IS NOT INITIAL.
      ls_at_attr-fname = lv_filename.
    ENDIF.

    IF lo_assistent->file_save( EXPORTING iv_ftype      = ls_at_attr-ftype
                                          iv_object     = ls_at_attr-object
                                          iv_name       = ls_at_attr-fname
                                          iv_filelength = ls_at_attr-length
                                          iv_dtype      = ls_at_attr-dtype
                                          iv_unseq      = abap_true
                                          iv_content    = is_media_resource-value ) = 0 .

*    SELECT MAX( seqnr ) FROM zthr_mss_at_attr INTO ls_at_attr-seqnr WHERE object = ls_at_attr-object
*                                                                      AND ftype  = ls_at_attr-ftype.
*    IF sy-subrc = 0.
*      ADD 1 TO ls_at_attr-seqnr.
*    ENDIF.
*
*    SELECT SINGLE guid FROM zthr_mss_at_attr INTO @DATA(ls_guid_old) WHERE ftype  = @ls_at_attr-ftype
*                                                                       AND object = @ls_at_attr-object
*                                                                       AND seqnr  = @ls_at_attr-seqnr.
*    IF ls_guid_old IS INITIAL.
*      ls_at_attr-guid = cl_system_uuid=>create_uuid_x16_static( ).
*    ELSE.
*      lo_assistent->file_delete( iv_ftype  = ls_at_attr-ftype
*                                 iv_object = ls_at_attr-object
*                                 iv_seqnr  = ls_at_attr-seqnr
*                                 iv_commit = abap_false ).
*
*      ls_at_attr-guid = ls_guid_old.
*    ENDIF.
*
*    ls_at_cont-srtfd = ls_at_attr-guid.
*
*    EXPORT content = is_media_resource-value TO DATABASE zthr_mss_at_cont(zz) ID ls_at_cont-srtfd.
*
*    MODIFY zthr_mss_at_attr FROM ls_at_attr.
*
*    COMMIT WORK AND WAIT.
      APPEND INITIAL LINE TO lt_return ASSIGNING <lt_return>.
      MESSAGE s003(zhr_mss) INTO <lt_return>-message.
      <lt_return>-type   = sy-msgty.
      <lt_return>-id     = sy-msgid.
      <lt_return>-number = sy-msgno.

      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_stream.
    DATA: BEGIN OF ls_input
         , main_user     TYPE string
         , entrance_user TYPE string
         , pernr         TYPE pernr_d
         , doc_typ       TYPE string
         , request_id    TYPE char32
         , xml           TYPE flag
         , END OF ls_input
         , lr_user TYPE RANGE OF string
         , xml_data     TYPE xstring
         .

    DATA: lt_return     TYPE bapiret2_t
        , return        TYPE  bapiret2
        , ls_stream     TYPE ty_s_media_resource
        , ls_lheader    TYPE ihttpnvp
        , lv_file       TYPE string
        .
    LOOP AT it_key_tab ASSIGNING FIELD-SYMBOL(<fs_key>).
      CASE <fs_key>-name.
        WHEN 'Pernr'.
          ls_input-pernr      = <fs_key>-value.
        WHEN 'DocTyp'.
          ls_input-doc_typ    = <fs_key>-value.
        WHEN 'RequestId'.
          ls_input-request_id = <fs_key>-value.
        WHEN 'GetXml'.
          ls_input-xml = <fs_key>-value.
      ENDCASE.
    ENDLOOP.
    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).
    DATA(lo_message_container) = mo_context->get_message_container( ).
    CASE ls_input-doc_typ.
      WHEN '1'.
        ls_stream-value = get_ipr( EXPORTING req_id = ls_input-request_id
                                   IMPORTING return = return ).
        IF return IS NOT INITIAL.
          APPEND return TO lt_return.
          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.
        ENDIF.
        lv_file = 'ИПР.doc'.
      WHEN '2'.
        IF ls_input-xml IS INITIAL.
          ls_stream-value = get_leave( EXPORTING req_id = ls_input-request_id
                                                 pernr  = ls_input-pernr
                                       IMPORTING return = return ).
          lv_file = 'Заявление.doc'.
        ELSE.
          ls_stream-value = get_leave_xml( EXPORTING req_id = ls_input-request_id
                                                     pernr  = ls_input-pernr
                                           IMPORTING return = return ).
          lv_file = 'Zajavlenie.xml'.
        ENDIF.
        IF return IS NOT INITIAL.
          APPEND return TO lt_return.
          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.
        ENDIF.

      WHEN OTHERS.
        APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<lt_return>).
        MESSAGE e019(zhr_mss) INTO <lt_return>-message.
        <lt_return>-type   = sy-msgty.
        <lt_return>-id     = sy-msgid.
        <lt_return>-number = sy-msgno.

        lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = lo_message_container.
    ENDCASE.

    IF ls_input-xml IS INITIAL.
      ls_stream-mime_type = 'application/msword'.
    ELSE.
      ls_stream-mime_type = 'application/xml'.
    ENDIF.
    lv_file = cl_http_utility=>if_http_utility~escape_url( lv_file ).
    ls_lheader-name = 'Content-Disposition'.
    ls_lheader-value = 'inline; filename="' && lv_file && '";'.
    set_header( is_header = ls_lheader ).
    copy_data_to_ref( EXPORTING is_data = ls_stream
                      CHANGING  cr_data = er_stream ).
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~update_stream.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~UPDATE_STREAM
*  EXPORTING
**    iv_entity_name          =
**    iv_entity_set_name      =
**    iv_source_name          =
*    IS_MEDIA_RESOURCE       =
**    it_key_tab              =
**    it_navigation_path      =
**    io_tech_request_context =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
    DATA: ls_at_attr TYPE zthr_mss_at_attr
        , ls_at_cont TYPE zthr_mss_at_cont
        , lt_return TYPE bapiret2_t
        .

    DATA(lo_message_container) = mo_context->get_message_container( ).
    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

    LOOP AT it_key_tab ASSIGNING FIELD-SYMBOL(<it_key_tab>).
      CASE <it_key_tab>-name.
        WHEN 'DocTyp'.
          CASE <it_key_tab>-value.
            WHEN 1.
              ls_at_attr-ftype = 'IP'.
            WHEN 2.
              ls_at_attr-ftype = 'LE'.
            WHEN OTHERS.
              APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<lt_return>).
              MESSAGE e019(zhr_mss) INTO <lt_return>-message.
              <lt_return>-type   = sy-msgty.
              <lt_return>-id     = sy-msgid.
              <lt_return>-number = sy-msgno.

              lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

              RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
                EXPORTING
                  message_container = lo_message_container.
          ENDCASE.
        WHEN 'Pernr'.
          ls_at_attr-object = CONV pernr_d( <it_key_tab>-value ).
        WHEN `FileName`.
          DATA(lv_filename) = CONV text255( <it_key_tab>-value ).
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    SELECT SINGLE doc_type INTO @DATA(lv_doctype) FROM toadd WHERE mimetype EQ @is_media_resource-mime_type.
    IF sy-subrc EQ 0.
      CONCATENATE iv_source_name '.' lv_doctype INTO ls_at_attr-fname.
      ls_at_attr-dtype  = lv_doctype.
    ELSE.
      ls_at_attr-fname  = iv_source_name.
    ENDIF.

    IF lv_filename IS NOT INITIAL.
      ls_at_attr-fname = lv_filename.
    ENDIF.

    ls_at_attr-datum  = sy-datum.
    ls_at_attr-uzeit  = sy-uzeit.
    ls_at_attr-uname  = sy-uname.

    ls_at_attr-length = xstrlen( is_media_resource-value ).

    IF lo_assistent->file_save( EXPORTING iv_ftype      = ls_at_attr-ftype
                                          iv_object     = ls_at_attr-object
                                          iv_name       = ls_at_attr-fname
                                          iv_filelength = ls_at_attr-length
                                          iv_dtype      = ls_at_attr-dtype
                                          iv_unseq      = abap_true
                                          iv_content    = is_media_resource-value ) = 0 .

*    SELECT MAX( seqnr ) FROM zthr_mss_at_attr INTO ls_at_attr-seqnr WHERE object = ls_at_attr-object
*                                                                      AND ftype  = ls_at_attr-ftype.
*    IF sy-subrc = 0.
*      ADD 1 TO ls_at_attr-seqnr.
*    ENDIF.
*
*    SELECT SINGLE guid FROM zthr_mss_at_attr INTO @DATA(ls_guid_old) WHERE ftype  = @ls_at_attr-ftype
*                                                                       AND object = @ls_at_attr-object
*                                                                       AND seqnr  = @ls_at_attr-seqnr.
*    IF ls_guid_old IS INITIAL.
*      ls_at_attr-guid = cl_system_uuid=>create_uuid_x16_static( ).
*    ELSE.
*      lo_assistent->file_delete( iv_ftype  = ls_at_attr-ftype
*                                 iv_object = ls_at_attr-object
*                                 iv_seqnr  = ls_at_attr-seqnr
*                                 iv_commit = abap_false ).
*
*      ls_at_attr-guid = ls_guid_old.
*    ENDIF.
*
*    ls_at_cont-srtfd = ls_at_attr-guid.
*
*    EXPORT content = is_media_resource-value TO DATABASE zthr_mss_at_cont(zz) ID ls_at_cont-srtfd.
*
*    MODIFY zthr_mss_at_attr FROM ls_at_attr.
*
*      COMMIT WORK AND WAIT.
      APPEND INITIAL LINE TO lt_return ASSIGNING <lt_return>.
      MESSAGE s003(zhr_mss) INTO <lt_return>-message.
      <lt_return>-type   = sy-msgty.
      <lt_return>-id     = sy-msgid.
      <lt_return>-number = sy-msgno.

      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
    ENDIF.

*    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*      EXPORTING
*        message_container = lo_message_container.
  ENDMETHOD.


  METHOD docdataset_get_entity.
    DATA: ls_converted_keys LIKE er_entity
        , lt_return         TYPE bapiret2_t
        .
    io_tech_request_context->get_converted_keys( IMPORTING es_key_values = ls_converted_keys ).
    DATA(lo_message_container) = mo_context->get_message_container( ).

    CASE ls_converted_keys-doctyp.
      WHEN '1'.

      WHEN '2'.
        me->get_leave( EXPORTING req_id  = ls_converted_keys-requestid
                                 pernr   = CONV #( ls_converted_keys-pernr )
                       IMPORTING return  = DATA(lv_return)
                                 es_main = DATA(ls_main) ).

        IF lv_return IS NOT INITIAL.
          APPEND lv_return TO lt_return.
          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.
        ENDIF.

        MOVE-CORRESPONDING ls_main TO er_entity.

        DO 20 TIMES.
          DATA(lv_field0) = `field_` && CONV numc2( sy-index ).
          DATA(lv_field1) = `field`  && CONV numc2( sy-index ).
          ASSIGN COMPONENT lv_field0 OF STRUCTURE ls_main   TO FIELD-SYMBOL(<fs_field0>).
          ASSIGN COMPONENT lv_field1 OF STRUCTURE er_entity TO FIELD-SYMBOL(<fs_field1>).
          IF <fs_field0> IS ASSIGNED AND <fs_field1> IS ASSIGNED.
            <fs_field1> = <fs_field0>.
          ENDIF.
          UNASSIGN: <fs_field0>
                  , <fs_field1>
                  .
        ENDDO.
      WHEN OTHERS.
        APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<lt_return>).
        MESSAGE e019(zhr_mss) INTO <lt_return>-message.
        <lt_return>-type   = sy-msgty.
        <lt_return>-id     = sy-msgid.
        <lt_return>-number = sy-msgno.

        lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = lo_message_container.
    ENDCASE.
  ENDMETHOD.


  METHOD get_ipr.
    DATA: "t_data_xml  TYPE swxmlcont
          ls_info     TYPE zhr_xss_iprreq
        , s_main      TYPE ty_main
*        , lv_month TYPE p2050-monat
*        , lv_month_text TYPE t7ru9a-regno
        , get_data    TYPE REF TO zcl_hr_get_data
        , t_p0001     TYPE TABLE OF p0001
        , t_p1001     TYPE TABLE OF p1001
        , lv_plans    TYPE p0001-plans
        , lv_pernr    TYPE pernr_d
        .

    get_data = NEW zcl_hr_get_data( ).

    SELECT SINGLE *
      FROM zhr_xss_iprreq
      INTO ls_info
      WHERE req_id = req_id.

    IF sy-subrc <> 0.
      MESSAGE e022(zhr_mss) INTO return-message WITH 'ZHR_XSS_IPRREQ'.
      return-type   = sy-msgty.
      return-id     = sy-msgid.
      return-number = sy-msgno.
      EXIT.
    ENDIF.
    get_data->get_pernr_orgeh( EXPORTING i_pernr = ls_info-approver_number
                                         i_begda = sy-datum "ls_info-date_create
                                         i_levct = 1
                               IMPORTING e_platx = s_main-field_01 ).
    s_main-field_02 = get_data->get_pernr_fio( i_pernr = ls_info-approver_number
                                               i_begda = sy-datum "ls_info-date_create
                                               i_type  = 1 ).
    CALL METHOD zcl_calendar=>get_date_dd_month_yy
      EXPORTING
        i_date   = ls_info-data_change
        i_short  = 'X'
        i_kavkav = 'X'
      IMPORTING
        e_date   = s_main-field_03.



*      replace first  occurrence of lv_month_text in  s_main-field_03  with ')'.
*     concatenate '(' s_main-field_03 into s_main-field_03.
    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).
    lo_assistent->get_struc( EXPORTING iv_objid = ls_info-approver_number
                                       iv_otype = 'P'
                                       iv_begda  = sy-datum
                                       iv_endda  = sy-datum
                                       iv_wegid  = 'Z_RUK_O'
                                       iv_acheck = abap_false
                             IMPORTING et_struc  = DATA(lt_struc) ).
    DELETE lt_struc WHERE NOT otype = 'O'.

    IF lines( lt_struc ) = 2.
      READ TABLE lt_struc ASSIGNING FIELD-SYMBOL(<fs_struc>) INDEX 1.

      IF sy-subrc EQ 0.
        s_main-field_04 = zcl_hr_data_utils=>get_name_obj( i_objid = CONV #( <fs_struc>-objid )
                                     i_otype = <fs_struc>-otype
                                     i_begda = sy-datum
                                     i_subty = '0001' ).
        IF s_main-field_04  IS INITIAL.
          s_main-field_04 = zcl_hr_data_utils=>get_name_obj_1000( i_objid = CONV #( <fs_struc>-objid )
                                                                  i_otype = <fs_struc>-otype
                                                                  i_begda = sy-datum ).

        ENDIF.
      ENDIF.
    ENDIF.
    s_main-field_05 = get_data->get_pernr_fio( i_pernr = ls_info-pernr_creator
                                               i_begda = sy-datum "ls_info-date_create
                                               ).
    get_data->get_pernr_orgeh( EXPORTING i_pernr = ls_info-pernr_creator
                                         i_begda = sy-datum "ls_info-date_create
                                         i_levct = 1
                               IMPORTING e_platx = s_main-field_06 ).
    s_main-field_07 = ls_info-edu_year.
    SELECT SINGLE cname
      FROM t7ruokin
      INTO  s_main-field_08
      WHERE molga = '33' AND
            sprsl = 'RU' AND
            facet = '30' AND
            ccode = ls_info-edu_type.
    IF sy-subrc <> 0.
      MESSAGE e024(zhr_mss) INTO return-message WITH 'T7RUOKIN'.
      return-type   = sy-msgty.
      return-id     = sy-msgid.
      return-number = sy-msgno.
      EXIT.
    ENDIF.
    IF ls_info-edu_spec EQ '6'.
      SELECT SINGLE edu_spec_another
      FROM zhr_xss_iprreq
      INTO  s_main-field_09
      WHERE req_id EQ req_id.

    ELSE.
      SELECT SINGLE stext
        FROM zthr_dlinet
        INTO  s_main-field_09
        WHERE spras = 'RU' AND
              dline = ls_info-edu_spec.

    ENDIF.
    IF sy-subrc <> 0.
      MESSAGE e024(zhr_mss) INTO return-message WITH 'ZTHR_DLINET'.
      return-type   = sy-msgty.
      return-id     = sy-msgid.
      return-number = sy-msgno.
      EXIT.
    ENDIF.
    SELECT SINGLE cname
      FROM t7ruokin
      INTO  s_main-field_10
      WHERE molga = '33' AND
            sprsl = 'RU' AND
            facet = '33' AND
            ccode = ls_info-edu_frm.
    IF sy-subrc <> 0.
      MESSAGE e024(zhr_mss) INTO return-message WITH 'T7RUOKIN'.
      return-type   = sy-msgty.
      return-id     = sy-msgid.
      return-number = sy-msgno.
      EXIT.
    ENDIF.
*31/03/2020 IBS
*    SELECT SINGLE goal_text
*      FROM zess_0023_t_goal
*      INTO  s_main-field_11
*      WHERE subty = ls_info-edu_type AND
*            goalid = ls_info-edu_goal .
    IF sy-subrc <> 0.
      MESSAGE e024(zhr_mss) INTO return-message WITH 'ZESS_0023_T_GOAL'.
      return-type   = sy-msgty.
      return-id     = sy-msgid.
      return-number = sy-msgno.
      EXIT.
    ENDIF.
    SELECT SINGLE etext
      FROM t538t
      INTO  s_main-field_12
      WHERE sprsl = 'RU' AND
            zeinh = ls_info-edu_length_type.
    IF sy-subrc <> 0.
      MESSAGE e024(zhr_mss) INTO return-message WITH 'T538T'.
      return-type   = sy-msgty.
      return-id     = sy-msgid.
      return-number = sy-msgno.
      EXIT.
    ENDIF.
*31/03/2020 IBS
*    IF ls_info-edu_result EQ '8'.
*      SELECT SINGLE edu_result_another
*     FROM zhr_xss_iprreq
*     INTO  s_main-field_13
*     WHERE req_id EQ req_id.
*
*    ELSE.
*      SELECT SINGLE efct_text
*        FROM zess_0023_t_efct
*        INTO  s_main-field_13
*        WHERE efctid = ls_info-edu_result.
*
*    ENDIF.
    IF sy-subrc <> 0.
      MESSAGE e024(zhr_mss) INTO return-message WITH 'ZESS_0023_T_EFCT'.
      return-type   = sy-msgty.
      return-id     = sy-msgid.
      return-number = sy-msgno.
      EXIT.
    ENDIF.




     data(approver_number) = lo_assistent->get_pernr_ruk0( iv_pernr = ls_info-pernr_creator iv_begda =  sy-datum ).
       get_data->get_pernr_orgeh( EXPORTING i_pernr = approver_number
                                             i_begda = sy-datum "ls_info-date_create
                                             i_levct = 1
                                   IMPORTING e_platx = s_main-field_14 ).
     s_main-field_15     = lo_assistent->get_pernr_fio( iv_pernr = approver_number iv_begda =  sy-datum ).

*    get_data->read_pa_infty( EXPORTING i_pernr = ls_info-pernr_creator
*                                       i_begda = sy-datum "ls_info-date_create
*                                       i_endda = sy-datum "ls_info-date_create
*                                       i_infty = '0001'
*                             IMPORTING e_pnnnn = t_p0001 ).
*    LOOP AT t_p0001 ASSIGNING FIELD-SYMBOL(<t_p0001>).
*      get_data->read_om_infty( EXPORTING i_objid  = <t_p0001>-orgeh
*                                         i_otype  = 'O'
*                                         i_infty  = '1001'
*                                         i_begda = sy-datum "ls_info-date_create
*                                         i_endda = sy-datum "ls_info-date_create
*                               IMPORTING e_pnnnn  = t_p1001 ).
*      LOOP AT t_p1001 ASSIGNING FIELD-SYMBOL(<t_p1001>) WHERE relat = '012'.
*        get_data->get_pernr_orgeh( EXPORTING i_pernr = CONV #( <t_p1001>-sobid )
*                                             i_begda = sy-datum "ls_info-date_create
*                                             i_levct = 1
*                                   IMPORTING e_platx = s_main-field_14 ).
*        s_main-field_15 = get_data->get_pernr_fio( i_pernr = CONV #( <t_p1001>-sobid )
*                                                   i_begda = sy-datum "ls_info-date_create
*                                                   i_type  = 1 ).
*        EXIT.
*      ENDLOOP.
*    ENDLOOP.
    zcl_hr_get_data=>read_stvarv_par( EXPORTING i_name  = 'ZZ_SECTOR_BOSS'
                                      IMPORTING e_value = lv_plans ).

    IF lv_plans IS INITIAL.
      MESSAGE e025(zhr_mss) INTO return-message WITH 'ZZ_SECTOR_BOSS'.
      return-type   = sy-msgty.
      return-id     = sy-msgid.
      return-number = sy-msgno.
      EXIT.
    ENDIF.

    SELECT SINGLE pernr            "#EC CI_NOFIRST
      FROM pa0001
      INTO lv_pernr
      WHERE plans = lv_plans
        AND begda <= sy-datum
        AND endda >= sy-datum.
    IF sy-subrc = 0 .
      s_main-field_16 = get_data->get_pernr_fio( i_pernr = lv_pernr
                                                 i_begda = sy-datum "ls_info-date_create
                                                 i_type  = 1 ).
    ENDIF.

    CALL TRANSFORMATION zt_mss_get_ipr
          SOURCE main = s_main
          RESULT XML t_data_xml.
  ENDMETHOD.


  METHOD get_leave.
    DATA: ls_info           TYPE ptreq_header
        , ls_item           TYPE ptreq_items
        , ls_abs            TYPE ptreq_attabsdata
        , ls_ztmss_sign_doc TYPE ztmss_sign_doc
        , s_main            TYPE ty_main
        , get_data          TYPE REF TO zcl_hr_get_data
        , t_p0001           TYPE TABLE OF p0001
        , t_p1001           TYPE TABLE OF p1001
        , lv_plans          TYPE p0001-plans
        , lv_pernr          TYPE pernr_d
        , l_text            TYPE text100
        .

    get_data = NEW zcl_hr_get_data( ).

    SELECT SINGLE *
      FROM ptreq_header
      INTO ls_info
      WHERE request_id = req_id.
    IF sy-subrc <> 0.
      MESSAGE e022(zhr_mss) INTO return-message WITH 'PTREQ_HEADER'.
      return-type   = sy-msgty.
      return-id     = sy-msgid.
      return-number = sy-msgno.
      EXIT.
    ENDIF.

    SELECT SINGLE *
     FROM ptreq_items
     INTO ls_item
     WHERE item_list_id = ls_info-item_list_id.
    IF sy-subrc <> 0.
      MESSAGE e022(zhr_mss) INTO return-message WITH 'PTREQ_ITEMS'.
      return-type   = sy-msgty.
      return-id     = sy-msgid.
      return-number = sy-msgno.
      EXIT.
    ENDIF.

    SELECT SINGLE *
      FROM ptreq_attabsdata
      INTO ls_abs
      WHERE item_id = ls_item-item_ins.
    IF sy-subrc <> 0.
      MESSAGE e022(zhr_mss) INTO return-message WITH 'PTREQ_ATTABSDATA'.
      return-type   = sy-msgty.
      return-id     = sy-msgid.
      return-number = sy-msgno.
      EXIT.
    ENDIF.
    SELECT SINGLE *
      FROM ztmss_sign_doc
      INTO ls_ztmss_sign_doc
      WHERE awart = ls_abs-subty.
    IF sy-subrc <> 0.
      MESSAGE e023(zhr_mss) INTO return-message.
      return-type   = sy-msgty.
      return-id     = sy-msgid.
      return-number = sy-msgno.
      EXIT.
    ENDIF.

    get_data->read_pa_infty( EXPORTING i_pernr = pernr
                                       i_begda = sy-datum
                                       i_endda = sy-datum
                                       i_infty = '0001'
                             IMPORTING e_pnnnn = t_p0001 ).
    "ФИО работника
    zcl_hr_data=>get_fio( EXPORTING i_pernr    =  pernr   " Табельный номер
                                    i_date     =  sy-datum    " Дата
                          IMPORTING e_fio_full =  s_main-field_04 ).
    LOOP AT t_p0001 ASSIGNING FIELD-SYMBOL(<t_p0001>).
      "Должность работника
      CALL METHOD zcl_hr_data=>get_name_plans
        EXPORTING
          i_plans = <t_p0001>-plans
          i_date  = sy-datum
        IMPORTING
          e_lname = s_main-field_03.

      IF get_data->check_objid_governor( i_otype = 'S'
                                         i_objid = <t_p0001>-plans
                                         i_begda = sy-datum ) IS NOT INITIAL.
*         губернатор
        s_main-field_01 = 'Губернатору Ленинградской области'.
        s_main-field_02 = 'Дрозденко А.Ю.'.
        s_main-field_09 = 'А.Ю. Дрозденко'.
        s_main-field_10 = 'Александр  Юрьевич'.
      ELSE.
*         вице-губернатор
        s_main-field_01 = 'Вице-губернатору Ленинградской области – руководителю аппарата Губернатора и Правительства Ленинградской области'.
        s_main-field_02 = 'Кучерявому М.М.'.
        s_main-field_09 = 'М.М. Кучерявому'.
        s_main-field_10 = 'Михаил Михайлович'.
      ENDIF.
    ENDLOOP.
    s_main-field_05 = ls_ztmss_sign_doc-field_05.
    IF ls_ztmss_sign_doc-need_begda > 1.
      IF ls_ztmss_sign_doc-need_begda = 3.
        s_main-field_06 = 'с'.
      ENDIF.
      zcl_calendar=>get_date_dd_month_yy( EXPORTING
                                            i_date    = ls_abs-begda
                                            i_short   = ' '
                                          IMPORTING
                                            e_date    = l_text ).
      CONCATENATE s_main-field_06 l_text INTO s_main-field_06 SEPARATED BY space.
    ENDIF.
    IF ls_ztmss_sign_doc-need_endda > 1.
      IF ls_ztmss_sign_doc-need_endda = 3.
        CONCATENATE s_main-field_06 'по' INTO s_main-field_06 SEPARATED BY space.
      ENDIF.
      zcl_calendar=>get_date_dd_month_yy( EXPORTING
                                            i_date    = ls_abs-endda
                                            i_short   = ' '
                                          IMPORTING
                                            e_date    = l_text ).
      CONCATENATE s_main-field_06 l_text INTO s_main-field_06 SEPARATED BY space.
    ENDIF.
    IF ls_ztmss_sign_doc-need_days_count > 1.
      IF ls_ztmss_sign_doc-need_days_count = 3.
        CONCATENATE s_main-field_06 'на' INTO s_main-field_06 SEPARATED BY space.
      ENDIF.
      l_text = ls_abs-endda - ls_abs-begda.
      CONCATENATE s_main-field_06 l_text INTO s_main-field_06 SEPARATED BY space.
      CASE ls_ztmss_sign_doc-need_days_count.
        WHEN 3.
          CONCATENATE s_main-field_06 'календарных дней' INTO s_main-field_06 SEPARATED BY space.
        WHEN 4.
          CONCATENATE s_main-field_06 'календарных дня' INTO s_main-field_06 SEPARATED BY space.
      ENDCASE.
    ENDIF.

    IF es_main IS REQUESTED.
      es_main = s_main.
      RETURN.
    ENDIF.

    CASE ls_abs-subty.
*    CASE abap_true.
      WHEN '100'.
        CALL TRANSFORMATION zt_mss_get_leave_vacation
                    SOURCE main = s_main
                    RESULT XML t_data_xml.
      WHEN OTHERS.
        CALL TRANSFORMATION zt_mss_get_leave
                    SOURCE main = s_main
                    RESULT XML t_data_xml.
    ENDCASE.

  ENDMETHOD.


  METHOD get_leave_xml.
    get_leave( EXPORTING req_id  = req_id
                         pernr   = pernr
               IMPORTING return  = return
                         es_main = DATA(ls_main) ).
*    DATA: lv_string TYPE string
*        .
    CALL TRANSFORMATION zt_mss_get_leave_xml
                    SOURCE main = ls_main
                    RESULT XML t_data_xml.
*                    RESULT XML lv_string.
  ENDMETHOD.
ENDCLASS.
