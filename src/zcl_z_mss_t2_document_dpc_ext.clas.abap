class ZCL_Z_MSS_T2_DOCUMENT_DPC_EXT definition
  public
  inheriting from ZCL_Z_MSS_T2_DOCUMENT_DPC
  create public .

public section.

  constants C_MEMORY_ID type TEXT50 value 'ZZ_T2_MEM_ID' ##NO_TEXT.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
protected section.

  methods ATTDOCSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_MSS_T2_DOCUMENT_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_stream.
    DATA: lv_entrance_user  TYPE string
        , lv_main_user      TYPE string
        , ls_stream         TYPE ty_s_media_resource
        , ls_lheader        TYPE ihttpnvp
        , lv_file           TYPE string
        , lt_return         TYPE bapiret2_t
        , lv_xml            TYPE xstring
        , lr_pernr          TYPE RANGE OF p_pernr
        .

    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).
    DATA(lo_message_container) = mo_context->get_message_container( ).

    LOOP AT it_key_tab ASSIGNING FIELD-SYMBOL(<fs_key>).
      CASE <fs_key>-name.
        WHEN 'EntranceUser'.
          lv_entrance_user      = <fs_key>-value.
        WHEN 'MainUser'.
          lv_main_user      = <fs_key>-value.
      ENDCASE.
    ENDLOOP.

    IF lv_entrance_user IS INITIAL.
      lv_entrance_user = lv_main_user.
    ENDIF.

    TRANSLATE lv_entrance_user TO UPPER CASE.
    IF lv_entrance_user IS INITIAL.
      APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<return>).
      MESSAGE e010(zhr_mss) INTO <return>-message.
      <return>-type   = sy-msgty.
      <return>-id     = sy-msgid.
      <return>-number = sy-msgno.
      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

    "Получение необходимых данных
    SELECT SINGLE pernr INTO @DATA(lv_pernr) "#EC CI_NOFIRST
    FROM pa0105 AS p1
    WHERE p1~subty = '9001'
      AND p1~usrid = @lv_entrance_user
      AND p1~begda <= @sy-datum
      AND p1~endda >= @sy-datum
      .

    IF sy-subrc <> 0 OR lv_pernr IS INITIAL.
      APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
      MESSAGE e002(zhr_mss) INTO <return>-message WITH lv_entrance_user.
      <return>-type   = sy-msgty.
      <return>-id     = sy-msgid.
      <return>-number = sy-msgno.
      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

**    APPEND INITIAL LINE TO lr_pernr ASSIGNING FIELD-SYMBOL(<fs_pernr>).
**    <fs_pernr>-low    = lv_pernr.
**    <fs_pernr>-sign   = 'I'.
**    <fs_pernr>-option = 'EQ'.
**
**    SUBMIT Z_HR_T2_GS_WORD
**      WITH pnppernr IN lr_pernr
**      WITH pnpbegda = sy-datum
**      WITH pnpendda = sy-datum
****      WITH pnptimr6 = 'X'
**      WITH p_export = abap_true
**      WITH P_HRMG_S = '-'
**      WITH p_hrmg_n = '-'
**      AND RETURN.
**
**    IMPORT lv_xml TO lv_xml FROM MEMORY ID zcl_z_mss_t2_document_dpc_ext=>c_memory_id.
**    FREE MEMORY ID zcl_z_mss_t2_document_dpc_ext=>c_memory_id.

    zcl_ess_data_assistent=>personal_get_t2_data( EXPORTING iv_pernr    = lv_pernr
                                                  IMPORTING ev_content  = ls_stream-value
                                                            ev_filename = lv_file ).

    IF ls_stream-value IS INITIAL.
      APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
      MESSAGE e026(zhr_mss) INTO <return>-message WITH lv_entrance_user.
      <return>-type   = sy-msgty.
      <return>-id     = sy-msgid.
      <return>-number = sy-msgno.
      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

**    lv_file = 'Т2.doc'.
**      lv_file = 'T2.xml'.
    ls_stream-mime_type = 'application/msword'.
**    ls_stream-value = lv_xml.
**      ls_stream-mime_type = 'application/xml'.
    lv_file = cl_http_utility=>if_http_utility~escape_url( lv_file ).
    ls_lheader-name = 'Content-Disposition'.
    ls_lheader-value = 'inline; filename="' && lv_file && '";'.
    set_header( is_header = ls_lheader ).
    copy_data_to_ref( EXPORTING is_data = ls_stream
                      CHANGING  cr_data = er_stream ).
  ENDMETHOD.


  METHOD attdocset_get_entityset.

  ENDMETHOD.
ENDCLASS.
