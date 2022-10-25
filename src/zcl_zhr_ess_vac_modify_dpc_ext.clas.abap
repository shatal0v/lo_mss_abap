CLASS zcl_zhr_ess_vac_modify_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_zhr_ess_vac_modify_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-DATA mv_dummy TYPE string .
  PROTECTED SECTION.

    METHODS vacapplicationse_create_entity
         REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ZHR_ESS_VAC_MODIFY_DPC_EXT IMPLEMENTATION.


  METHOD vacapplicationse_create_entity.
    DATA:
        ls_vac                TYPE zmss_vacation_application,

        lv_entrance_user      TYPE zlogin_act_dir,
        lv_request_id         TYPE tim_req_id,
        lv_pernr              TYPE pernr_d,
        lv_name               TYPE string,
        lv_begda_new          TYPE datum,
        lv_endda_new          TYPE datum,
        lv_abstype            TYPE char4,
        lv_owner_number       TYPE pernr_d,
        lv_begda              TYPE datum,
        lv_endda              TYPE datum,
        lv_comment            TYPE string,

        lt_return             TYPE TABLE OF bapiret2,
        ls_return             LIKE LINE OF lt_return,
        lt_mes                TYPE ptarq_uia_messages_tab,
        ls_mes                LIKE LINE OF lt_mes,
        lt_com                TYPE ptarq_uia_command_tab,
        lv_has_errors         TYPE ptreq_has_error_flag,
        ls_req_old            TYPE ptarq_uia_request,
        ls_req_new            TYPE ptarq_uia_request,
        ls_req_def            TYPE ptarq_uia_request,
        ls_req_new2           TYPE ptarq_uia_request,
        lv_show_change        TYPE boolean,
        ls_req                TYPE ptreq_header,
        lv_need               TYPE flag,
        lv_days               TYPE abwtg,
        ls_pa2001             TYPE pa2001,

        lo_pt_arq_application TYPE REF TO cl_pt_arq_application,
        lo_pt_arq_reqs_list   TYPE REF TO cl_pt_arq_reqs_list,
        lt_status_range       TYPE rseloption,
        ls_status_range       LIKE LINE OF lt_status_range,
        lt_pernr_tab          TYPE ptim_pernr_tab,

        lo_message_container  TYPE REF TO /iwbep/if_message_container
      , ls_origin             TYPE zcl_zhr_ess_vac_modify_mpc=>ts_vacapplication
      .


    lo_message_container = mo_context->get_message_container( ).

*   обработка входящих параметров
    io_data_provider->read_entry_data( IMPORTING es_data = er_entity ).
    ls_origin = er_entity.
    MOVE-CORRESPONDING er_entity TO ls_vac.

    lv_comment = ls_vac-comment.
    lv_begda_new = ls_vac-begda_new.
    lv_endda_new = ls_vac-endda_new.
    IF ls_vac-entranceuser IS NOT INITIAL.
      lv_entrance_user = ls_vac-entranceuser.
    ELSE.
      lv_entrance_user = ls_vac-mainuser.
    ENDIF.
    lv_request_id = ls_vac-request_id.
    lv_begda = ls_vac-begda.
    lv_endda = ls_vac-endda.
    lv_owner_number = ls_vac-owner_number.
    lv_abstype = CONV numc4( ls_vac-abstype ).
    CLEAR er_entity.

*   пользователь пытается изменить заявку и при  старые и новые даты вообще не изменены  -
*   выдавать ошибку с текстом – вы не изменили период отпуска.
    IF lv_begda EQ lv_begda_new AND
        lv_endda EQ lv_endda_new.
      MESSAGE e008(zhr_ess) INTO mv_dummy.
      ls_return-type = sy-msgty.
      ls_return-id   = sy-msgid.
      ls_return-number = sy-msgno.
      ls_return-message = mv_dummy.
      APPEND ls_return TO lt_return.

      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.


*   табельный того, кто решил изменить заявку берем из 'LOGINACTDIR'
*   однако, это может быть и технический пользователь, тогда табельный берем из 'OWNERNUMBER'
    TRANSLATE lv_entrance_user TO UPPER CASE.

    CALL METHOD zcl_vacation_appl=>is_need_0105_check
      EXPORTING
        iv_login = lv_entrance_user
      RECEIVING
        rv_need  = lv_need.
*
*    SELECT SINGLE pernr
*      FROM pa0105
*      INTO lv_pernr
*      WHERE subty = '0001'
*        AND usrid = lv_entrance_user.
    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

    lv_pernr = lo_assistent->get_pernr( iv_usrid = CONV #( lv_entrance_user ) ).



    IF sy-subrc NE 0.
      IF lv_need EQ 'X'.
        CLEAR ls_return.
        ls_return-type = 'E'.
        ls_return-id   = 'ZHR_PA'.
        ls_return-number = '015'.
        ls_return-message_v1 = lv_entrance_user.
        APPEND ls_return TO lt_return.

        lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = lo_message_container.
      ELSE.
        lv_pernr = ls_vac-owner_number.
      ENDIF.
    ENDIF.

*   читаем инфу по заявке на отпуск
    IF lv_request_id IS NOT INITIAL.
      CALL METHOD zcl_vacation_appl=>get_req_header_data
        EXPORTING
          iv_request_id = lv_request_id
        IMPORTING
          es_request    = ls_req.
    ELSE.
      ls_req-status = 'POSTED'.
    ENDIF.

    CLEAR:
      lt_mes, lt_com.

    CASE ls_req-status.
      WHEN 'POSTED'.

        CLEAR:
          ls_req_old, ls_req_new, ls_req_new2.

        IF lv_request_id IS NOT INITIAL. " читаем существующую заявку
          CLEAR:
            lt_mes, lt_com.

          CALL FUNCTION 'PT_ARQ_REQUEST_PREPARE'
            EXPORTING
              im_request_id = lv_request_id
              im_command    = cl_pt_req_const=>c_cmd_select
              im_pernr      = lv_pernr
              im_modus      = cl_pt_req_const=>c_role_employee
            IMPORTING
              ex_request    = ls_req_old
              ex_has_errors = lv_has_errors
            TABLES
              ex_messages   = lt_mes
              ex_commands   = lt_com.

          CLEAR lt_return.
          IF lv_has_errors IS INITIAL.

          ELSE.
            CLEAR er_entity-check.
            LOOP AT lt_mes INTO ls_mes WHERE type = 'E'.
              CLEAR ls_return.
              ls_return-type = ls_mes-type.
              ls_return-id   = ls_mes-id.
              ls_return-number = ls_mes-number.
              ls_return-message = ls_mes-message.
              APPEND ls_return TO lt_return.
            ENDLOOP.
          ENDIF.
          IF lt_return IS NOT INITIAL.
            lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
              EXPORTING
                message_container = lo_message_container.
          ENDIF.
        ENDIF.

        CREATE OBJECT lo_pt_arq_application.

        CALL METHOD lo_pt_arq_application->initialize
          EXPORTING
            im_modus           = cl_pt_req_const=>c_role_employee
            im_initiator_objid = lv_pernr
            im_owner_objid     = lv_pernr.

        CREATE OBJECT lo_pt_arq_reqs_list
          EXPORTING
            im_selection_begin = '19000101'.

        CLEAR:
          lt_pernr_tab, lt_status_range.
        APPEND lv_pernr TO lt_pernr_tab.
        ls_status_range-sign = 'I'.
        ls_status_range-option = 'CP'.
        ls_status_range-low = '*'.
        APPEND ls_status_range TO lt_status_range.

        CALL METHOD lo_pt_arq_reqs_list->sel_reqs_attsabs_for_owner
          EXPORTING
            im_pernr_tab       = lt_pernr_tab
            im_status_range    = lt_status_range
            im_selection_begin = '19000101'.

        PERFORM create_modify_request IN PROGRAM saplpt_arq_request_uia
          USING lv_request_id CHANGING ls_req_new.

*       выше не определяется next_processor, возьмем его по дефолту
        zcl_vacation_operation=>request_prepare(
          EXPORTING
            pernr      = lv_pernr    " Табельный номер
          IMPORTING
            req        = ls_req_def ).

        ls_req_new-next_processor = ls_req_def-next_processor.

        IF lv_request_id IS NOT INITIAL.
          IF ls_req_new-ins_item-infotype EQ '2002'.
            ls_req_new-request_or_attabs = cl_pt_arq_const=>c_datakind_attendance.
          ELSE.
            ls_req_new-request_or_attabs = cl_pt_arq_const=>c_datakind_absence.
          ENDIF.

          ls_req_new-ins_item-begda = lv_begda_new.
          ls_req_new-ins_item-endda = lv_endda_new.

          CALL FUNCTION 'DAYS_BETWEEN_TWO_DATES'
            EXPORTING
              i_datum_bis             = lv_endda_new
              i_datum_von             = lv_begda_new
            IMPORTING
              e_tage                  = lv_days
            EXCEPTIONS
              days_method_not_defined = 1
              OTHERS                  = 2.

          IF sy-subrc EQ 0.
            ls_req_new-ins_item-abwtg = lv_days + 1.
            ls_req_new-ins_item-attabs_hours = ls_req_new-ins_item-abwtg * 8.
          ELSE.
            CLEAR ls_req_new-ins_item-abwtg.
            CLEAR ls_req_new-ins_item-attabs_hours.
          ENDIF.
        ELSE.

          SELECT SINGLE *
            INTO ls_pa2001
            FROM pa2001
            WHERE
              pernr EQ lv_pernr   AND
              subty EQ lv_abstype AND
              sprps EQ ''         AND
              endda EQ lv_endda   AND
              begda EQ lv_begda.

          IF sy-subrc NE 0.
            CLEAR ls_return.
            ls_return-type = 'E'.
            ls_return-id   = 'ZHR_PA'.
            ls_return-number = '020'.
            ls_return-message_v1 = lv_pernr.
            ls_return-message_v2 = lv_begda.
            ls_return-message_v3 = lv_endda.
            APPEND ls_return TO lt_return.

            IF lt_return IS NOT INITIAL.
              lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
              RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
                EXPORTING
                  message_container = lo_message_container.
            ENDIF.
          ENDIF.

          ls_req_new-request_or_attabs = 'R'.

          ls_req_new-del_item-infotype = '2001'.
          ls_req_new-del_item-begda = lv_begda.
          ls_req_new-del_item-endda = lv_endda.
          ls_req_new-del_item-abwtg = ls_pa2001-abwtg.
          ls_req_new-del_item-attabs_hours = ls_pa2001-stdaz.
          ls_req_new-del_item-subty = lv_abstype.

          CALL FUNCTION 'DAYS_BETWEEN_TWO_DATES'
            EXPORTING
              i_datum_bis             = lv_endda_new
              i_datum_von             = lv_begda_new
            IMPORTING
              e_tage                  = lv_days
            EXCEPTIONS
              days_method_not_defined = 1
              OTHERS                  = 2.

          IF sy-subrc EQ 0.
            ls_req_new-ins_item-abwtg = lv_days + 1.
            ls_req_new-ins_item-attabs_hours = ls_req_new-ins_item-abwtg * 8.
          ELSE.
            CLEAR ls_req_new-ins_item-abwtg.
            CLEAR ls_req_new-ins_item-attabs_hours.
          ENDIF.

          ls_req_new-ins_item-infotype = '2001'.
          ls_req_new-ins_item-begda = lv_begda_new.
          ls_req_new-ins_item-endda = lv_endda_new.
          ls_req_new-ins_item-subty = lv_abstype.

        ENDIF.

        CLEAR:
          lt_mes, lt_com.

        CALL FUNCTION 'PT_ARQ_REQUEST_CHECK'
          EXPORTING
            im_request     = ls_req_new
            im_command     = cl_pt_req_const=>c_cmd_check_create
            im_pernr       = lv_pernr
            im_modus       = 'R'
          IMPORTING
            ex_has_errors  = lv_has_errors
            ex_request     = ls_req_new2
            ex_show_change = lv_show_change
          TABLES
            ex_messages    = lt_mes
            ex_commands    = lt_com.

        DELETE lt_mes WHERE id EQ 'HRTIM_ABS_REQ' AND number EQ '077'.
        DELETE lt_mes WHERE id EQ 'HRTIM_ABS_REQ' AND number EQ '136'.
        LOOP AT lt_mes INTO ls_mes WHERE type = 'E'.
          EXIT.
        ENDLOOP.
        IF sy-subrc NE 0.
          CLEAR lv_has_errors.
        ENDIF.

        CLEAR lt_return.
        IF lv_has_errors IS INITIAL.

        ELSE.
          CLEAR er_entity-check.
          LOOP AT lt_mes INTO ls_mes WHERE type = 'E'.
            CLEAR ls_return.
            ls_return-type = ls_mes-type.
            ls_return-id   = ls_mes-id.
            ls_return-number = ls_mes-number.
            ls_return-message = ls_mes-message.
            APPEND ls_return TO lt_return.
          ENDLOOP.
        ENDIF.
        IF lt_return IS NOT INITIAL.
          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.
        ENDIF.

        CLEAR:
          lt_mes, lt_com.

        CALL FUNCTION 'PT_ARQ_REQUEST_EXECUTE'
          EXPORTING
            im_request_id = ls_req_new2-request_id
            im_command    = cl_pt_req_const=>c_cmd_execute_send
            im_pernr      = lv_pernr
            im_modus      = cl_pt_req_const=>c_role_employee
          IMPORTING
            ex_request    = ls_req_new2
            ex_has_errors = lv_has_errors
          TABLES
            ex_messages   = lt_mes
            ex_commands   = lt_com.

        CLEAR lt_return.
        IF lv_has_errors IS INITIAL.
          er_entity-check = 'X'.
          er_entity-request_id = ls_req_new2-request_id.
        ELSE.
          CLEAR er_entity-check.
          LOOP AT lt_mes INTO ls_mes WHERE type = 'E'.
            CLEAR ls_return.
            ls_return-type = ls_mes-type.
            ls_return-id   = ls_mes-id.
            ls_return-number = ls_mes-number.
            ls_return-message = ls_mes-message.
            APPEND ls_return TO lt_return.
          ENDLOOP.
        ENDIF.
        IF lt_return IS NOT INITIAL.
          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.
        ENDIF.

      WHEN OTHERS.

        er_entity-request_id = lv_request_id.

        CALL FUNCTION 'PT_ARQ_REQUEST_PREPARE'
          EXPORTING
            im_request_id = lv_request_id
            im_command    = cl_pt_req_const=>c_cmd_prepare_modify
            im_pernr      = lv_pernr
            im_modus      = cl_pt_req_const=>c_role_employee
          IMPORTING
            ex_request    = ls_req_old
            ex_has_errors = lv_has_errors
          TABLES
            ex_messages   = lt_mes
            ex_commands   = lt_com.
*{KLOKOVNYU 15.05.2019 Затираем ошибку т.к. пользователи против нормалного ведения данных
        DATA lt_pa0105 TYPE TABLE OF p0105.

        CALL FUNCTION 'HR_READ_INFOTYPE'
          EXPORTING
*           TCLAS           = 'A'
            pernr           = lv_pernr
            infty           = '0105'
            begda           = sy-datum
            endda           = sy-datum
          TABLES
            infty_tab       = lt_pa0105
          EXCEPTIONS
            infty_not_found = 1
            OTHERS          = 2.

        READ TABLE lt_pa0105 TRANSPORTING NO FIELDS WITH KEY subty = '9001'.
        IF sy-subrc = 0.

          READ TABLE lt_mes TRANSPORTING NO FIELDS WITH KEY id = 'HRTIM_ABS_REQ'
                                                            type = 'E'
                                                            number = '117'.
          IF sy-subrc = 0.
            CLEAR: lv_has_errors, lt_mes.
          ENDIF.
        ENDIF.

*}KLOKOVNYU 15.05.2019 Затираем ошибку т.к. пользователи против нормалного ведения данных
        CLEAR lt_return.
        IF lv_has_errors IS INITIAL.

        ELSE.
          CLEAR er_entity-check.
          LOOP AT lt_mes INTO ls_mes WHERE type = 'E'.
            CLEAR ls_return.
            ls_return-type = ls_mes-type.
            ls_return-id   = ls_mes-id.
            ls_return-number = ls_mes-number.
            ls_return-message = ls_mes-message.
            APPEND ls_return TO lt_return.
          ENDLOOP.
        ENDIF.
        IF lt_return IS NOT INITIAL.
          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.
        ENDIF.

        ls_req_old-ins_item-begda = lv_begda_new.
        ls_req_old-ins_item-endda = lv_endda_new.
        ls_req_old-curr_notice = lv_comment.

        CLEAR:
          lt_mes, lt_com.

        CALL FUNCTION 'PT_ARQ_REQUEST_CHECK'
          EXPORTING
            im_request     = ls_req_old
            im_command     = cl_pt_req_const=>c_cmd_check_modify
            im_pernr       = lv_pernr
            im_modus       = 'R'
          IMPORTING
            ex_has_errors  = lv_has_errors
            ex_request     = ls_req_new
            ex_show_change = lv_show_change
          TABLES
            ex_messages    = lt_mes
            ex_commands    = lt_com.

        DELETE lt_mes WHERE id EQ 'HRTIM_ABS_REQ' AND number EQ '077'.
        DELETE lt_mes WHERE id EQ 'HRTIM_ABS_REQ' AND number EQ '136'.

        CLEAR lt_return.
        IF lv_has_errors IS INITIAL.

        ELSE.
          CLEAR er_entity-check.
          LOOP AT lt_mes INTO ls_mes WHERE type = 'E'.
            CLEAR ls_return.
            ls_return-type = ls_mes-type.
            ls_return-id   = ls_mes-id.
            ls_return-number = ls_mes-number.
            ls_return-message = ls_mes-message.
            APPEND ls_return TO lt_return.
          ENDLOOP.
        ENDIF.
        IF lt_return IS NOT INITIAL.
          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.
        ENDIF.

        CLEAR:
          lt_mes, lt_com.

        CALL FUNCTION 'PT_ARQ_REQUEST_EXECUTE'
          EXPORTING
            im_request_id = lv_request_id
            im_command    = cl_pt_req_const=>c_cmd_execute_send
            im_pernr      = lv_pernr
            im_modus      = cl_pt_req_const=>c_role_employee
          IMPORTING
            ex_request    = ls_req_new
            ex_has_errors = lv_has_errors
          TABLES
            ex_messages   = lt_mes
            ex_commands   = lt_com.

        CLEAR lt_return.
        IF lv_has_errors IS INITIAL.
          er_entity-check = 'X'.
        ELSE.
          CLEAR er_entity-check.
          LOOP AT lt_mes INTO ls_mes WHERE type = 'E'.
            CLEAR ls_return.
            ls_return-type = ls_mes-type.
            ls_return-id   = ls_mes-id.
            ls_return-number = ls_mes-number.
            ls_return-message = ls_mes-message.
            APPEND ls_return TO lt_return.
          ENDLOOP.
        ENDIF.
        IF lt_return IS NOT INITIAL.
          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.
        ENDIF.

    ENDCASE.

    ls_origin-check = er_entity-check.
    ls_origin-request_id = er_entity-request_id.
    er_entity = ls_origin.
  ENDMETHOD.
ENDCLASS.
