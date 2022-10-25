class ZCL_Z_MSS_EMP_RESERV_DPC_EXT definition
  public
  inheriting from ZCL_Z_MSS_EMP_RESERV_DPC
  create public .

public section.

  constants C_MEMORY_ID type TEXT20 value 'ZHR_PA_D044' ##NO_TEXT.

  methods GET_REPORT_XML
    importing
      !IT_KEY type /IWBEP/T_MGW_NAME_VALUE_PAIR
    exporting
      !E_XSTRING type XSTRING
      !E_FILENAME type STRING .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
protected section.

  methods EMP_RESERV_SET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_MSS_EMP_RESERV_DPC_EXT IMPLEMENTATION.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM.

 DATA: ls_stream     TYPE ty_s_media_resource
        , ls_lheader    TYPE ihttpnvp
        .
if 1 = 1 .

endif.
    CASE iv_entity_name.
      WHEN 'Emp_Reserv'.
        ls_stream-mime_type = 'application/vnd.ms-excel'.
        get_report_xml(
          EXPORTING
            it_key    = it_key_tab
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


  METHOD emp_reserv_set_get_entityset.
    DATA: BEGIN OF ls_input
              , main_user     TYPE string
              , entrance_user TYPE string
              , END OF ls_input

              , lr_user TYPE RANGE OF string
              .
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

        IF ls_input-entrance_user IS INITIAL.
          ls_input-entrance_user = ls_input-main_user.
        ENDIF.

        lo_assistent->ws_get_emp_reserv_xml( EXPORTING  iv_euser       = ls_input-entrance_user
*                                                    iv_begda      = lv_begda
*                                                    iv_endda      = lv_endda
                                        IMPORTING   e_xstring       = DATA(lv_string)
                                        EXCEPTIONS no_pernr       = 1
                                                   no_sec_lev_ruk = 2 ).

        DATA: lt_return            TYPE bapiret2_t
            , lo_message_container TYPE REF TO /iwbep/if_message_container
         .
        APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<lt_return>).

        CASE sy-subrc.
          WHEN 1.
            MESSAGE e002(zhr_mss) WITH ls_input-entrance_user INTO <lt_return>-message.
            <lt_return>-type   = sy-msgty.
            <lt_return>-id     = sy-msgid.
            <lt_return>-number = sy-msgno.
          WHEN 2.
            DATA(lv_name) = lo_assistent->get_user_fio( iv_usrid = ls_input-entrance_user ).
            MESSAGE e001(zhr_mss) WITH lv_name INTO <lt_return>-message.
            <lt_return>-type   = sy-msgty.
            <lt_return>-id     = sy-msgid.
            <lt_return>-number = sy-msgno.
          WHEN OTHERS.
            REFRESH lt_return.
        ENDCASE.
        .
        IF NOT lt_return IS INITIAL.
          lo_message_container = mo_context->get_message_container( ).
          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.
        ENDIF.

*      LOOP AT lt_emp_events ASSIGNING FIELD-SYMBOL(<lt_emp_events>).
*          APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<et_entityset>).
*          move-corresponding <lt_emp_events> to <et_entityset>.
*
*      ENDLOOP.
      CATCH /iwbep/cx_mgw_busi_exception .
      CATCH /iwbep/cx_mgw_tech_exception .
    ENDTRY.


  ENDMETHOD.


  METHOD get_report_xml.
    DATA: BEGIN OF ls_input
              , main_user     TYPE string
              , entrance_user TYPE string
              , END OF ls_input

              , lr_user TYPE RANGE OF string
              .
    LOOP AT it_key ASSIGNING FIELD-SYMBOL(<fs_key>).
      CASE <fs_key>-name.
        WHEN 'ENTRANCE_USER'.
          ls_input-entrance_user = <fs_key>-value.
        WHEN 'MAIN_USER'.
          ls_input-main_user = <fs_key>-value.

      ENDCASE.
    ENDLOOP.
    IF ls_input-entrance_user IS INITIAL.
      ls_input-entrance_user = ls_input-main_user.
    ENDIF.
    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).
    lo_assistent->ws_get_emp_reserv_xml( EXPORTING  iv_euser       = ls_input-entrance_user
*                                                    iv_begda      = lv_begda
*                                                    iv_endda      = lv_endda
                                    IMPORTING   e_xstring       = e_xstring
                                    EXCEPTIONS no_pernr       = 1
                                               no_sec_lev_ruk = 2 ).
    e_filename = 'Emp_reserv.xlsx'.

  ENDMETHOD.
ENDCLASS.
