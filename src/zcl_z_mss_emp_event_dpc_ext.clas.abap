class ZCL_Z_MSS_EMP_EVENT_DPC_EXT definition
  public
  inheriting from ZCL_Z_MSS_EMP_EVENT_DPC
  create public .

public section.
protected section.

  methods EMP_EVENT_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_MSS_EMP_EVENT_DPC_EXT IMPLEMENTATION.


  METHOD emp_event_get_entityset.

    DATA: BEGIN OF ls_input
              , main_user     TYPE string
              , entrance_user TYPE string
              , xss           TYPE string
              , begda TYPE string
              , endda TYPE string
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

        IF ls_input-begda IS NOT INITIAL.
          lv_begda = ls_input-begda.
        ELSE.
          lv_begda = sy-datum.

        ENDIF.
        IF ls_input-endda IS NOT INITIAL.
          lv_endda = ls_input-endda.
        ELSE.
          lv_endda = sy-datum.
        ENDIF.
        DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

        IF ls_input-entrance_user IS NOT INITIAL.
          lo_assistent->ws_get_emp_events( EXPORTING  iv_euser       = ls_input-entrance_user
                                                       iv_begda      = lv_begda
                                                       iv_endda      = lv_endda
                                        IMPORTING   et_emp_events       = DATA(lt_emp_events)
                                        EXCEPTIONS no_pernr       = 1
                                                   no_sec_lev_ruk = 2 ).
        ELSE.
          lo_assistent->ws_get_emp_events( EXPORTING  iv_euser      = ls_input-main_user
                                                      iv_begda      = lv_begda
                                                      iv_endda      = lv_endda
                                                      iv_xss        = ls_input-xss
                                          IMPORTING   et_emp_events       = lt_emp_events
                                          EXCEPTIONS no_pernr       = 1
                                                     no_sec_lev_ruk = 2 ).
        ENDIF.

**        >>>Шаталов Б.А. не выводить декларацию о доходах из другого месяца
        LOOP AT lt_emp_events ASSIGNING FIELD-SYMBOL(<ls_emp_events>) WHERE decl_date NOT BETWEEN lv_begda AND lv_endda.
          CLEAR <ls_emp_events>-decl_date.
        ENDLOOP.
        LOOP AT lt_emp_events ASSIGNING <ls_emp_events> WHERE att_date NOT BETWEEN lv_begda AND lv_endda.
          CLEAR <ls_emp_events>-att_date.
        ENDLOOP.
**        <<<Шаталов Б.А. не выводить декларацию о доходах из другого месяца

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


          WHEN 2.
            IF ls_input-entrance_user IS NOT INITIAL.
              DATA(lv_name) = lo_assistent->get_user_fio( iv_usrid = ls_input-entrance_user ).
            ELSE.
              lv_name = lo_assistent->get_user_fio( iv_usrid = ls_input-main_user ).
            ENDIF.
            MESSAGE e001(zhr_mss) WITH lv_name INTO <lt_return>-message.
            <lt_return>-type   = sy-msgty.
            <lt_return>-id     = sy-msgid.
            <lt_return>-number = sy-msgno.
            <et_entityset0>-msgid = 'E'.
            MESSAGE e017(zhr_mss) WITH  lv_name INTO <et_entityset0>-message.

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

        LOOP AT lt_emp_events ASSIGNING FIELD-SYMBOL(<lt_emp_events>).
          APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<et_entityset>).
          MOVE-CORRESPONDING <lt_emp_events> TO <et_entityset>.
          <et_entityset>-msgid = 'S'.
          MESSAGE s009(zhr_mss)  INTO <et_entityset>-message.
        ENDLOOP.
      CATCH /iwbep/cx_mgw_busi_exception .
      CATCH /iwbep/cx_mgw_tech_exception .
    ENDTRY.



  ENDMETHOD.
ENDCLASS.
