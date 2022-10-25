class ZCL_Z_MSS_MAIN_USER_DA_DPC_EXT definition
  public
  inheriting from ZCL_Z_MSS_MAIN_USER_DA_DPC
  create public .

public section.
protected section.

  methods MAINUSERDATASET_GET_ENTITYSET
    redefinition .
  methods MAINUSERSNILSSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_MSS_MAIN_USER_DA_DPC_EXT IMPLEMENTATION.


  METHOD mainuserdataset_get_entityset.

    DATA: BEGIN OF ls_input
        , main_user     TYPE string
        , entrance_user TYPE string
        , snils         TYPE string
        , END OF ls_input
        , lr_user TYPE RANGE OF string
        .
    TRY.
        DATA(lo_filter) = io_tech_request_context->get_filter( ).

        LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<it_filter>).
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

        lo_assistent->ws_get_main_user_data( EXPORTING  iv_euser        = ls_input-main_user
                                                        iv_snils        = ls_input-snils
                                             IMPORTING  et_mainuserdata = DATA(lt_mainuserdata)
                                             EXCEPTIONS no_pernr        = 1
                                                        no_sec_lev_ruk  = 2
                                                        pernr_inactive  = 3
                                                        no_pernr_sub    = 4
                                                        OTHERS          = 99 ).

        DATA: lt_return            TYPE bapiret2_t
            , lo_message_container TYPE REF TO /iwbep/if_message_container
            .
        APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<lt_return>).
        APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<et_entityset0>).
        CASE sy-subrc.
          WHEN 1.
            MESSAGE e002(zhr_mss) WITH ls_input-main_user INTO <lt_return>-message.
            <lt_return>-type   = sy-msgty.
            <lt_return>-id     = sy-msgid.
            <lt_return>-number = sy-msgno.
            <et_entityset0>-msgid   = 'E'.
            MESSAGE e015(zhr_mss) WITH ls_input-main_user INTO <et_entityset0>-message.
          WHEN 2.
            DATA(lv_name) = lo_assistent->get_user_fio( iv_usrid = ls_input-main_user ).
            MESSAGE e001(zhr_mss) WITH lv_name INTO <lt_return>-message.
            <lt_return>-type   = sy-msgty.
            <lt_return>-id     = sy-msgid.
            <lt_return>-number = sy-msgno.
            <et_entityset0>-msgid   = 'E'.
            MESSAGE e001(zhr_mss) WITH  lv_name INTO <et_entityset0>-message.
          WHEN 3.

            MESSAGE e020(zhr_mss) WITH ls_input-main_user INTO <lt_return>-message.
            <lt_return>-type   = sy-msgty.
            <lt_return>-id     = sy-msgid.
            <lt_return>-number = sy-msgno.
            <et_entityset0>-msgid        = 'E'.
            MESSAGE e020(zhr_mss) WITH ls_input-main_user INTO <et_entityset0>-message.

          WHEN 4.
            MESSAGE e028(zhr_mss) INTO <lt_return>-message.
            <lt_return>-type   = sy-msgty.
            <lt_return>-id     = sy-msgid.
            <lt_return>-number = sy-msgno.
            <et_entityset0>-msgid   = 'E'.
            MESSAGE e028(zhr_mss) INTO <et_entityset0>-message.

          WHEN OTHERS.
            REFRESH lt_return.
            REFRESH et_entityset.
        ENDCASE.
        .
*        IF NOT lt_return IS INITIAL.
*          lo_message_container = mo_context->get_message_container( ).
*          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
*
*          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*            EXPORTING
*              message_container = lo_message_container.
*        ENDIF.

        LOOP AT lt_mainuserdata ASSIGNING FIELD-SYMBOL(<lt_mainuserdata>).
          APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<et_entityset>).
          MOVE-CORRESPONDING <lt_mainuserdata> TO <et_entityset>.
          <et_entityset>-msgid = 'S'.
          TRANSLATE <et_entityset>-main_user TO UPPER CASE.
          MESSAGE s013(zhr_mss)   INTO <et_entityset>-message.
        ENDLOOP.
*      CATCH /iwbep/cx_mgw_busi_exception .
      CATCH /iwbep/cx_mgw_tech_exception .
    ENDTRY.
  ENDMETHOD.


  METHOD mainusersnilsset_get_entityset.
    DATA: BEGIN OF ls_input
        , snils TYPE string
        , END OF ls_input
        , lr_user TYPE RANGE OF string
        .
    TRY.
        DATA(lo_filter) = io_tech_request_context->get_filter( ).

        LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<it_filter>).
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

        lo_assistent->ws_get_main_user_data( EXPORTING  iv_snils        = ls_input-snils
                                             IMPORTING  et_mainuserdata = DATA(lt_mainuserdata)
                                             EXCEPTIONS no_pernr        = 1
                                                        no_sec_lev_ruk  = 2
                                                        pernr_inactive  = 3
                                                        OTHERS          = 99 ).

        DATA: lt_return            TYPE bapiret2_t
            , lo_message_container TYPE REF TO /iwbep/if_message_container
            .

        LOOP AT lt_mainuserdata ASSIGNING FIELD-SYMBOL(<lt_mainuserdata>).
          APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<et_entityset>).
          MOVE-CORRESPONDING <lt_mainuserdata> TO <et_entityset>.
          TRANSLATE <et_entityset>-main_user TO UPPER CASE.
        ENDLOOP.

        IF NOT ( <et_entityset> IS ASSIGNED AND <et_entityset>-main_user IS NOT INITIAL ).
          APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<lt_return>).
          MESSAGE e027(zhr_mss) INTO <lt_return>-message.
          <lt_return>-type   = sy-msgty.
          <lt_return>-id     = sy-msgid.
          <lt_return>-number = sy-msgno.
        ENDIF.

        IF NOT lt_return IS INITIAL.
          lo_message_container = mo_context->get_message_container( ).
          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.
        ENDIF.

*      CATCH /iwbep/cx_mgw_busi_exception .
      CATCH /iwbep/cx_mgw_tech_exception .
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
