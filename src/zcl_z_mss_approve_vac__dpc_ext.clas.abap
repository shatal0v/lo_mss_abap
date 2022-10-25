class ZCL_Z_MSS_APPROVE_VAC__DPC_EXT definition
  public
  inheriting from ZCL_Z_MSS_APPROVE_VAC__DPC
  create public .

public section.

  class-methods ZZZ_CREATE_2001
    importing
      !T_REQ type PTREQ_REQ_GUID_32_TAB
    returning
      value(RETURN) type PTARQ_UIA_MESSAGES_TAB .
protected section.

  methods VACAPPLICATION01_CREATE_ENTITY
    redefinition .
  methods VACAPPLICATIONSE_GET_ENTITYSET
    redefinition .
  methods VACAPPLICATION01_GET_ENTITY
    redefinition .
private section.

  class-data ENTRANCEUSER type STRING value 'EntranceUser' ##NO_TEXT.
  class-data MAINUSER type STRING value 'MainUser' ##NO_TEXT.

  class-methods ADD_RECEIVER
    importing
      !IO_ASSIST type ref to ZCL_MSS_DATA_ASSISTENT
    changing
      !CT_RECEIVER type BCSY_SMTPA .
ENDCLASS.



CLASS ZCL_Z_MSS_APPROVE_VAC__DPC_EXT IMPLEMENTATION.


  METHOD add_receiver.
    DATA: lv_name TYPE tvarvc-name VALUE 'ZHR_MSS_OBJ_S_HR_FOR_LR'
        , lt_0105 TYPE p0105_tbl.


    zcl_hr_get_data=>read_stvarv_tab( EXPORTING i_name   = lv_name
                                      IMPORTING et_value = DATA(t_value) ).

    LOOP AT t_value ASSIGNING FIELD-SYMBOL(<t_value>).
      DATA(lv_pernr) = io_assist->get_pernr_by_plans( iv_plans = CONV #( <t_value> ) iv_begda = sy-datum ).

      zcl_hr_data=>read_pa_infty( EXPORTING i_pernr = lv_pernr
                                      i_infty = '0105'
                                      i_subty = '0010'
                                      i_begda = sy-datum
                                      i_endda = sy-datum
                                   IMPORTING e_pnnnn = lt_0105 ).
      IF lines( lt_0105 ) > 0.
        READ TABLE lt_0105 ASSIGNING FIELD-SYMBOL(<ls_0105>) INDEX 1.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO ct_receiver ASSIGNING FIELD-SYMBOL(<ls_row>).
          IF <ls_row> is ASSIGNED.
            <ls_row> = <ls_0105>-usrid_long.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD vacapplication01_create_entity.
    DATA:
             lv_entranceuser     TYPE zlogin_act_dir,
             lv_request_id        TYPE tim_req_id,
             lv_pernr             TYPE pernr_d,
             lv_name              TYPE string,
             lv_action            TYPE char1,

             lt_return            TYPE TABLE OF bapiret2,
             ls_return            LIKE LINE OF lt_return,
             lt_mes               TYPE  ptarq_uia_messages_tab,
             ls_mes               LIKE LINE OF lt_mes,
             lt_com               TYPE  ptarq_uia_command_tab,
             lv_has_errors        TYPE ptreq_has_error_flag,
             lv_command           TYPE ptreq_command,
             lv_need              TYPE flag

           , lo_message_container TYPE REF TO /iwbep/if_message_container
           , ls_request           TYPE ptarq_uia_request

           , lt_req               TYPE ptreq_req_guid_32_tab
           , lt_file              TYPE zttbcs_file .
    .

    FIELD-SYMBOLS:
                   <fs_key> LIKE LINE OF it_key_tab.

    io_data_provider->read_entry_data(
      IMPORTING
        es_data = er_entity ).

    lo_message_container = mo_context->get_message_container( ).


    lv_request_id = er_entity-request_id.

    lv_action = er_entity-status(1).
    TRANSLATE lv_action TO UPPER CASE.
    IF er_entity-entranceuser IS NOT INITIAL.
      lv_entranceuser = er_entity-entranceuser.
    ELSE  .
      lv_entranceuser = er_entity-mainuser.
    ENDIF.
    TRANSLATE lv_entranceuser TO UPPER CASE.
*    if lv_entranceuser is initial.
*      lv_entranceuser = er_entity-login_act_dir.
*    endif.
*    SELECT SINGLE pernr
*      FROM pa0105
*      INTO lv_pernr
*      WHERE subty = '0001'
*        AND usrid = lv_entranceuser.
    DATA(lo_assist) = zcl_mss_data_assistent=>get_instance( ).
    lv_pernr = lo_assist->get_pernr( iv_usrid = CONV #( lv_entranceuser )   ).

    IF sy-subrc NE 0.
      CLEAR ls_return.
      ls_return-type = 'E'.
      ls_return-id   = 'ZHR_PA'.
      ls_return-number = '015'.
      ls_return-message_v1 = lv_entranceuser.
      APPEND ls_return TO lt_return.

      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

*   одобрение/отклонение заявки на отпуск
    CLEAR:
      lt_mes, lt_com.

    CASE lv_action.
      WHEN 'A'.
        lv_command = cl_pt_req_const=>c_cmd_execute_approve.

      WHEN 'R'.
        lv_command = cl_pt_req_const=>c_cmd_execute_reject.

      WHEN OTHERS.
        CLEAR ls_return.
        ls_return-type = 'E'.
        ls_return-id   = 'ZHR_PA'.
        ls_return-number = '017'.
        ls_return-message_v1 = lv_action.
        APPEND ls_return TO lt_return.

        lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = lo_message_container.
    ENDCASE.

    zcl_vacation_appl=>get_requests_with_divided(
      EXPORTING
        i_req_id = lv_request_id    " Ид. документа
      IMPORTING
        et_req   = lt_req ).

*   сначала проверяем все записи, потом только сохраняем в БД
*    IF 1 = 2. " for tests only
    DO 2 TIMES.
      IF lv_has_errors EQ abap_true.
        EXIT.
      ENDIF.

      IF sy-index EQ 2.
        DATA(lv_save) = abap_true.
      ENDIF.

      LOOP AT lt_req ASSIGNING FIELD-SYMBOL(<fs_req>).
        CALL FUNCTION 'PT_ARQ_REQUEST_PREPARE'
          EXPORTING
            im_request_id = <fs_req> "lv_request_id
            im_command    = cl_pt_req_const=>c_cmd_select
            im_pernr      = lv_pernr
            im_modus      = cl_pt_req_const=>c_role_employee
          IMPORTING
            ex_request    = ls_request
            ex_has_errors = lv_has_errors
          TABLES
            ex_messages   = lt_mes
            ex_commands   = lt_com.
        IF lv_has_errors EQ abap_true.
          EXIT.
        ENDIF.

        ls_request-curr_notice = er_entity-comment.
        IF lv_save EQ abap_false.
          zcl_vacation_operation=>request_check(
            EXPORTING
              request          = ls_request
              pernr            = lv_pernr
            IMPORTING
              checked_request  = ls_request
              messages         = lt_mes
              e_has_errors     = lv_has_errors ).
        ELSE.
          CALL FUNCTION 'PT_ARQ_REQUEST_EXECUTE'
            EXPORTING
              im_request_id = <fs_req> "lv_request_id
              im_command    = lv_command
              im_pernr      = lv_pernr
              im_modus      = cl_pt_req_const=>c_role_approver
            IMPORTING
              ex_has_errors = lv_has_errors
            TABLES
              ex_messages   = lt_mes
              ex_commands   = lt_com.
        ENDIF.

        IF lv_has_errors EQ abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDDO.
*    ENDIF.

    CLEAR lt_return.
    IF lv_has_errors IS INITIAL.
      er_entity-check = 'X'.
***      <<< костыль!!!!!!!!
***      <<< сохраняем 2001 ИТ, если все ок
      IF lv_action = 'A'.
        lt_return = me->zzz_create_2001( lt_req ).
      ENDIF.
      IF lt_return IS NOT INITIAL.
        lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = lo_message_container.
      ENDIF.
***      >>> end
    ELSE.
      LOOP AT lt_mes INTO ls_mes WHERE type = 'E'.
        CLEAR ls_return.
        ls_return-type = ls_mes-type.
        ls_return-id   = ls_mes-id.
        ls_return-number = ls_mes-number.
        ls_return-message = ls_mes-message.
        APPEND ls_return TO lt_return.
      ENDLOOP.
    ENDIF.

    DATA: lt_mapping  TYPE zttmail_mapping
        , lt_mail_to  TYPE bcsy_smtpa
            .
    IF lv_has_errors IS INITIAL.
      CASE lv_action.
        WHEN 'A' OR 'R'. "утверждение!

          IF lv_action = 'A'.
            DATA(lv_subject) = CONV string('Отпуск согласован').
            DATA(lv_form)    = CONV string('ZHR_REQUES_VACATION_APPROVED').
          ELSE.
            lv_subject = 'Oтпуск отклонен'.
            lv_form    = `ZHR_REQUES_VACATION_REJECTED`.
          ENDIF.

          DATA(lv_pernr1) = ls_request-ins_item-pernr.

          DATA(lv_email) = lo_assist->get_pernr_0105( iv_pernr =  lv_pernr1 ).

          CHECK lv_email IS NOT INITIAL.
          APPEND INITIAL LINE TO lt_mail_to ASSIGNING FIELD-SYMBOL(<lt_mail_to>).
          <lt_mail_to> = lv_email.

          zcl_send_email=>send_mail( EXPORTING itd_to          = lt_mail_to
*                                           iv_sender_email = `tech@lo.com`
                                               iv_textname     = CONV #( lv_form )
                                               iv_subject      = CONV #( lv_subject )
                                               itd_mapping     = lt_mapping
                                               iv_immediately  = abap_false ).

          UNASSIGN <lt_mail_to>.
          CLEAR lt_mail_to.

          IF lv_action = 'A'.

           DATA(lv_fio) = lo_assist->GET_pernr_FIO( EXPORTING iv_pernr = lv_pernr1 ).

            lv_subject = |Отпуск сотрудника согласован|.
            lv_form    = 'ZHR_LQ_APPROVED_HR'.

            add_receiver(
              EXPORTING
                io_assist = lo_assist
              CHANGING
                 ct_receiver = lt_mail_to ).

            DESCRIBE TABLE lt_mail_to LINES DATA(lv_lines).

            IF lv_lines < 1.
              RETURN.
            ENDIF.

            APPEND VALUE #( name = `<fio_init>` value = lv_fio ) TO lt_mapping.

            "атрибуты заявки
            zcl_vacation_appl=>get_attabs(
              EXPORTING
                i_req_id = lv_request_id
              IMPORTING
                es_data  = DATA(ls_attabs) ).

            lo_assist->get_attachment(
              EXPORTING
                is_attabs   = ls_attabs
              IMPORTING
                ev_att_hex  = DATA(lt_att_hex) ).

            APPEND INITIAL LINE TO lt_file ASSIGNING FIELD-SYMBOL(<ls_file>).
            IF <ls_file> IS ASSIGNED.
              <ls_file>-attachment_subject = 'Заявка на отпуск'.
              <ls_file>-extension = 'DOC'.
              <ls_file>-add_content_text = lt_att_hex.
            ENDIF.

            zcl_send_email=>send_mail(
                EXPORTING
                  itd_to          = lt_mail_to
                  iv_textname     = CONV #( lv_form )
                  iv_subject      = CONV #( lv_subject )
                  itd_mapping     = lt_mapping
                  iv_immediately  = abap_false
                  itd_file        = lt_file ).

          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

    IF lt_return IS NOT INITIAL.
      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.
  ENDMETHOD.


  METHOD vacapplication01_get_entity.
    DATA:
      lv_entranceuser      TYPE zlogin_act_dir,
      lv_request_id        TYPE tim_req_id,
      lv_pernr             TYPE pernr_d,
      lv_name              TYPE string,
      lv_action            TYPE char1,

      lt_return            TYPE TABLE OF bapiret2,
      ls_return            LIKE LINE OF lt_return,
      lt_mes               TYPE  ptarq_uia_messages_tab,
      ls_mes               LIKE LINE OF lt_mes,
      lt_com               TYPE  ptarq_uia_command_tab,
      lv_has_errors        TYPE ptreq_has_error_flag,
      lv_command           TYPE ptreq_command,
      lv_need              TYPE flag,

      lo_message_container TYPE REF TO /iwbep/if_message_container.

    FIELD-SYMBOLS:
                   <fs_key> LIKE LINE OF it_key_tab.


    lo_message_container = mo_context->get_message_container( ).

*   обработка входящих параметров
    LOOP AT it_key_tab ASSIGNING <fs_key>.

      lv_name = <fs_key>-name.
      TRANSLATE lv_name TO UPPER CASE.

      CASE lv_name.
        WHEN 'STATUS'.
          lv_action = <fs_key>-value(1).
          TRANSLATE lv_action TO UPPER CASE.

        WHEN 'MAINUSER'.
          lv_entranceuser = <fs_key>-value.
          TRANSLATE lv_entranceuser TO UPPER CASE.
          er_entity-entranceuser = lv_entranceuser.

          DATA(lo_assist) = zcl_mss_data_assistent=>get_instance( ).
          lv_pernr = lo_assist->get_pernr( iv_usrid = CONV #( lv_entranceuser )   ). " здесь уже mainuser

          IF lv_pernr IS INITIAL.
            CLEAR ls_return.
            ls_return-type = 'E'.
            ls_return-id   = 'ZHR_MSS'.
            ls_return-number = '001'.
            ls_return-message_v1 = lv_entranceuser.
            APPEND ls_return TO lt_return.

            lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
              EXPORTING
                message_container = lo_message_container.
          ENDIF.
        WHEN 'ENTRANCEUSER'.
          lv_entranceuser = <fs_key>-value.
          TRANSLATE lv_entranceuser TO UPPER CASE.
          er_entity-entranceuser = lv_entranceuser.

          lo_assist = zcl_mss_data_assistent=>get_instance( ).
          lv_pernr = lo_assist->get_pernr( iv_usrid = CONV #( lv_entranceuser )   ). " здесь уже mainuser

          IF lv_pernr IS INITIAL.
            CLEAR ls_return.
            ls_return-type = 'E'.
            ls_return-id   = 'ZHR_MSS'.
            ls_return-number = '001'.
            ls_return-message_v1 = lv_entranceuser.
            APPEND ls_return TO lt_return.

            lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
              EXPORTING
                message_container = lo_message_container.
          ENDIF.

        WHEN 'REQUESTID'.
          lv_request_id = <fs_key>-value.
          er_entity-request_id = lv_request_id.

        WHEN OTHERS.
          CLEAR ls_return.
          ls_return-type = 'E'.
          ls_return-id   = 'ZHR_PA'.
          ls_return-number = '014'.
          APPEND ls_return TO lt_return.

          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.
      ENDCASE.
    ENDLOOP.

*   одобрение/отклонение заявки на отпуск
    CLEAR:
      lt_mes, lt_com.

    CASE lv_action.
      WHEN 'A'.
        lv_command = cl_pt_req_const=>c_cmd_execute_approve.

      WHEN 'R'.
        lv_command = cl_pt_req_const=>c_cmd_execute_reject.

      WHEN OTHERS.
        CLEAR ls_return.
        ls_return-type = 'E'.
        ls_return-id   = 'ZHR_PA'.
        ls_return-number = '017'.
        ls_return-message_v1 = lv_action.
        APPEND ls_return TO lt_return.

        lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = lo_message_container.
    ENDCASE.

    CALL FUNCTION 'PT_ARQ_REQUEST_EXECUTE'
      EXPORTING
        im_request_id = lv_request_id
        im_command    = lv_command
        im_pernr      = lv_pernr
        im_modus      = cl_pt_req_const=>c_role_approver
      IMPORTING
        ex_has_errors = lv_has_errors
      TABLES
        ex_messages   = lt_mes
        ex_commands   = lt_com.

    CLEAR lt_return.
    IF lv_has_errors IS INITIAL.
      er_entity-check = 'X'.
    ELSE.
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

  ENDMETHOD.


  METHOD vacapplicationse_get_entityset.
    TYPES: BEGIN OF ty_ktart,
             subty TYPE subty,
             ktart TYPE p2006-ktart,
           END OF ty_ktart.
    DATA:
          lv_entranceuser     TYPE zlogin_act_dir,
          lv_request_id        TYPE tim_req_id,
          lv_pernr             TYPE pernr_d,
          lv_name              TYPE string,
          lv_property          TYPE string,
          lv_found             TYPE flag,

          lt_return            TYPE TABLE OF bapiret2,
          ls_return            LIKE LINE OF lt_return,
          lt_mes               TYPE  ptarq_uia_messages_tab,
          ls_mes               LIKE LINE OF lt_mes,
          lt_com               TYPE  ptarq_uia_command_tab,
          lt_request           TYPE ptarq_uia_reqlist_tab,
          ls_request           LIKE LINE OF lt_request,
          lv_has_errors        TYPE ptreq_has_error_flag,
          ls_filter            LIKE LINE OF it_filter_select_options,
          ls_selopt            TYPE /iwbep/s_cod_select_option,
          ls_data              LIKE LINE OF et_entityset,
          lv_need              TYPE flag,

          lo_message_container TYPE REF TO /iwbep/if_message_container
        , lv_year_endda         TYPE dats
        , lv_ktart              TYPE p2006-ktart
        , lt_ktart              TYPE HASHED TABLE OF ty_ktart WITH UNIQUE KEY subty
        , ls_ktart              TYPE ty_ktart
        .
    FIELD-SYMBOLS: <fs_ktart> LIKE LINE OF lt_ktart
                 .

    lo_message_container = mo_context->get_message_container( ).

*   читаем входящие параметры

    READ TABLE it_filter_select_options ASSIGNING FIELD-SYMBOL(<filter>) WITH KEY property = entranceuser.
    IF sy-subrc <> 0.
      READ TABLE it_filter_select_options ASSIGNING <filter> WITH KEY property = mainuser.
    ENDIF.

    IF sy-subrc = 0.

      READ TABLE <filter>-select_options ASSIGNING FIELD-SYMBOL(<option>) INDEX 1.
      IF sy-subrc = 0 AND <option>-low IS NOT INITIAL.
        DATA(lv_entrance_user) = <option>-low.
        lv_found = abap_true.
      ELSE.

        READ TABLE it_filter_select_options ASSIGNING <filter> WITH KEY property = mainuser.
        IF sy-subrc = 0.

          READ TABLE <filter>-select_options ASSIGNING <option> INDEX 1.
          IF sy-subrc = 0 AND <option>-low IS NOT INITIAL.
            lv_entrance_user = <option>-low.
            lv_found = abap_true.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.


    IF lv_found EQ ''. " если параметры заданы некорректно
      CLEAR ls_return.
      ls_return-type = 'E'.
      ls_return-id   = 'ZHR_PA'.
      ls_return-number = '014'.
      APPEND ls_return TO lt_return.

      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

*   табельный номер
    TRANSLATE lv_entranceuser TO UPPER CASE.
    DATA(lo_assist) = zcl_mss_data_assistent=>get_instance( ).
    lv_pernr = lo_assist->get_pernr( iv_usrid = CONV #( lv_entrance_user )   ).

    IF lv_pernr IS INITIAL.
      CLEAR ls_return.
      ls_return-type = 'E'.
      ls_return-id   = 'ZHR_MSS'.
      ls_return-number = '001'.
      ls_return-message_v1 = lv_entrance_user.
      APPEND ls_return TO lt_return.

      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

*   список запросов
    CALL FUNCTION 'PT_ARQ_REQLIST_GET'
      EXPORTING
        im_pernr            = lv_pernr
*       IM_TEAM             =
        im_date             = '19000101'
        im_command          = 'UPDATE_WORKLIST'
*       IM_TIME_FORMAT      = 2
        im_modus            = 'A'
*       IM_DEBUG            =
        im_deactivate_check = abap_true
      IMPORTING
        ex_request_list     = lt_request
*       EX_AS_OF_DATE       =
      TABLES
        ex_messages         = lt_mes
        ex_commands         = lt_com.

    DELETE lt_mes WHERE type = 'E' AND number = '075' AND id = 'HRTIM_ABS_REQ'.

    CLEAR lt_return.
    LOOP AT lt_mes INTO ls_mes WHERE type = 'E'.
      CLEAR ls_return.
      ls_return-type = ls_mes-type.
      ls_return-id   = ls_mes-id.
      ls_return-number = ls_mes-number.
      ls_return-message = ls_mes-message.
      APPEND ls_return TO lt_return.
    ENDLOOP.

    IF lt_return IS NOT INITIAL.
      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.


    LOOP AT lt_request INTO ls_request.
      CLEAR ls_data.
*     параметр changing_vis: заполняем значение X, только если заявка находится в статусе POSTED, либо если  это отсутствие
*     параметр deletion_vis: заполняем значение X, только если заявка находится в статусе POSTED, либо если это отсутствие
      IF ls_request-status EQ 'POSTED' OR
          ls_request-request_or_attabs NE 'R'.
        ls_data-changing_vis = abap_true.
        ls_data-deletion_vis = abap_true.
      ENDIF.
      ls_data-request_id = ls_request-request_id.
      ls_data-begda = ls_request-begda.
      ls_data-endda = ls_request-endda.
      ls_data-abstype = ls_request-subty.
      ls_data-status = ls_request-status.
      ls_data-create_date = ls_request-first_subm_date.
      ls_data-owner_fio = ls_request-owner-name.
      ls_data-owner_number = ls_request-owner-pernr.
      ls_data-abs_days = ls_request-abwtg.
      ls_data-comment = ls_request-curr_notice.
      zcl_vacation_appl=>fix_uppercase_fio(
        CHANGING
          cv_fio = ls_data-owner_fio ).

      UNASSIGN <fs_ktart>.
      READ TABLE lt_ktart ASSIGNING <fs_ktart>
        WITH TABLE KEY subty = ls_request-subty.
      IF sy-subrc NE 0.
        zcl_vacation_appl=>get_timetypes(
          EXPORTING
            i_subty  = ls_request-subty    " Подтип
          IMPORTING
            et_types = DATA(lt_types) ).
        LOOP AT lt_types ASSIGNING FIELD-SYMBOL(<fs_types>).
          ls_ktart-subty = ls_request-subty.
          ls_ktart-ktart = <fs_types>-type.
          INSERT ls_ktart INTO TABLE lt_ktart ASSIGNING <fs_ktart>.
          EXIT.
        ENDLOOP.
      ENDIF.

      IF <fs_ktart> IS ASSIGNED.
        lv_year_endda = ls_request-begda(4) && '1231'.
        zcl_vacation_appl=>get_limit_days(
          EXPORTING
            i_pernr = ls_request-owner-pernr
            i_date  = lv_year_endda
            i_ktart = <fs_ktart>-ktart
          IMPORTING
            e_limit = ls_data-limit_days ).
      ENDIF.

      APPEND ls_data TO et_entityset.
    ENDLOOP.
  ENDMETHOD.


  METHOD zzz_create_2001.
    DATA: lr_req    TYPE RANGE OF tim_req_id
        , ls_return TYPE bapireturn1
        , ls_p2001  TYPE p2001
        , ls_key    TYPE bapipakey.
    LOOP AT t_req ASSIGNING FIELD-SYMBOL(<t_req>) .
      APPEND INITIAL LINE TO lr_req ASSIGNING FIELD-SYMBOL(<lr_req>).
      <lr_req>-option = 'EQ'.
      <lr_req>-sign = 'I'.
      <lr_req>-low = <t_req>.
    ENDLOOP.

    SELECT item_list_id, version_no
      FROM ptreq_header
      INTO TABLE @DATA(lt_header)
      WHERE request_id IN @lr_req.

    SORT lt_header BY item_list_id version_no DESCENDING.
    LOOP AT lt_header ASSIGNING FIELD-SYMBOL(<lt_header>).
      DELETE lt_header WHERE item_list_id = <lt_header>-item_list_id
                        AND version_no <> <lt_header>-version_no.
    ENDLOOP.

    SELECT item_ins
      FROM ptreq_items AS a1
      INTO TABLE @DATA(lt_items)
      FOR ALL ENTRIES IN @lt_header
      WHERE a1~item_list_id EQ @lt_header-item_list_id.

    CLEAR lr_req[].
    LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<fs_items>).
      APPEND INITIAL LINE TO lr_req ASSIGNING <lr_req>.
      <lr_req>-option = 'EQ'.
      <lr_req>-sign = 'I'.
      <lr_req>-low = <fs_items>-item_ins.
    ENDLOOP.


    SELECT item_id, pernr, subty, endda, begda FROM ptreq_attabsdata
        INTO TABLE @DATA(lt_attr)
        WHERE item_id IN @lr_req.

    CHECK sy-subrc = 0.
    READ TABLE lt_attr ASSIGNING FIELD-SYMBOL(<lt_attr>) INDEX 1.
    CHECK sy-subrc = 0 AND <lt_attr> IS ASSIGNED.

    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = <lt_attr>-pernr
      IMPORTING
        return = ls_return.

    IF ls_return IS NOT INITIAL.
      APPEND INITIAL LINE TO return ASSIGNING FIELD-SYMBOL(<return>).
      MOVE-CORRESPONDING ls_return TO <return>.
      RETURN.
    ENDIF.

    LOOP AT lt_attr ASSIGNING <lt_attr>.
      MOVE-CORRESPONDING <lt_attr> TO ls_p2001.
      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
        EXPORTING
          infty         = '2001'
          number        = <lt_attr>-pernr
          subtype       = <lt_attr>-subty
          validityend   = <lt_attr>-endda
          validitybegin = <lt_attr>-begda
          lockindicator = abap_true
          record        = ls_p2001
          operation     = 'INSS'
        IMPORTING
          return        = ls_return
          key           = ls_key.

      IF ls_return IS NOT INITIAL AND ( ls_return-type = 'A' OR ls_return-type = 'E' ).
        APPEND INITIAL LINE TO return ASSIGNING <return>.
        MOVE-CORRESPONDING ls_return TO <return>.
        RETURN.
      ENDIF.
    ENDLOOP.


    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = <lt_attr>-pernr
      IMPORTING
        return = ls_return.

  ENDMETHOD.
ENDCLASS.
