class ZCL_Z_MSS_REQUEST_SAVE_DPC_EXT definition
  public
  inheriting from ZCL_Z_MSS_REQUEST_SAVE_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~UPDATE_STREAM
    redefinition .
protected section.

  methods REQUESTSET_CREATE_ENTITY
    redefinition .
  methods REQUESTSGETSET_GET_ENTITYSET
    redefinition .
  methods REQUESTSET_DELETE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_MSS_REQUEST_SAVE_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~update_stream.
    DATA: ls_zthr_ess_eksreqf TYPE zthr_ess_eksreqf.
    TRY.
        READ TABLE it_key_tab ASSIGNING FIELD-SYMBOL(<reid>) WITH KEY name = 'ReqId'.
        IF sy-subrc = 0.
          READ TABLE it_key_tab ASSIGNING FIELD-SYMBOL(<fina>) WITH KEY name = 'fileName'.
          IF sy-subrc = 0.
            SELECT SINGLE req_id MAX( req_pos ) FROM zthr_ess_eksreqf INTO ls_zthr_ess_eksreqf
            WHERE req_id = <reid>-value GROUP BY req_id.
            ls_zthr_ess_eksreqf-req_id = <reid>-value.
            ADD 1 TO ls_zthr_ess_eksreqf-req_pos.
            ls_zthr_ess_eksreqf-filename = <fina>-value.
            ls_zthr_ess_eksreqf-filedata = is_media_resource-value.

            CALL FUNCTION 'ZHR_ESS_EKSREQF_INS' IN UPDATE TASK
              EXPORTING
                is_data = ls_zthr_ess_eksreqf.
            COMMIT WORK AND WAIT.
          ENDIF.
        ENDIF.
      CATCH /iwbep/cx_mgw_busi_exception .
      CATCH /iwbep/cx_mgw_tech_exception .
    ENDTRY.
  ENDMETHOD.


  METHOD requestset_create_entity.
    DATA: ls_rq    TYPE zcl_z_mss_request_save_mpc=>ts_request,
          ls_db    TYPE zthr_ess_eksreq,
          lv_usrid TYPE p0105-usrid.

    DATA(lo_assist) = zcl_mss_data_assistent=>get_instance( ).

    io_data_provider->read_entry_data( IMPORTING es_data = ls_rq ).

    "Заполнение данных
    MOVE-CORRESPONDING ls_rq TO er_entity.

    "Заполнение данными по алгоритму
    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_32 = ls_db-req_id.
    er_entity-reqid      = ls_db-req_id.
    ls_db-date_create    = sy-datum.
    ls_db-time_create    = sy-timlo.
*21/03/2020
    IF ls_rq-entranceuser IS NOT INITIAL.
      lv_usrid             = ls_rq-entranceuser.
    ELSE.
      lv_usrid             = ls_rq-mainuser.
    ENDIF.
    ls_db-perner_creator = lo_assist->get_pernr( iv_usrid = lv_usrid ).

    "Заполнение данными из запроса
    ls_db-approver_pernr = ls_rq-approverpernr.
    ls_db-req_type       = CONV numc2( ls_rq-reqtype ).
    ls_db-status         = '0'.
    ls_db-coment         = ls_rq-coment.
    ls_db-second_name    = ls_rq-secondname.
    ls_db-uname          = ls_rq-mainuser.

    "Внесем в БД
    CALL FUNCTION 'ZHR_ESS_EKSREQ_INS' IN UPDATE TASK
      EXPORTING
        is_data = ls_db.

    COMMIT WORK AND WAIT.

    DATA: lt_mapping  TYPE zttmail_mapping
        , lt_mail_to TYPE bcsy_smtpa
        .
    DATA(lv_email) = lo_assist->get_pernr_0105( iv_pernr = ls_db-approver_pernr ).

    IF lv_email IS NOT INITIAL.
      APPEND INITIAL LINE TO lt_mail_to ASSIGNING FIELD-SYMBOL(<lt_mail_to>).
      <lt_mail_to> = lv_email.
*21/03/2020
      IF ls_rq-entranceuser IS NOT INITIAL.
        DATA(lv_pernr)  = lo_assist->get_pernr( iv_usrid = CONV #( ls_rq-entranceuser ) ).
      ELSE.
       lv_pernr  = lo_assist->get_pernr( iv_usrid = CONV #( ls_rq-mainuser ) ).
      ENDIF.

      APPEND VALUE #( name = `<pernr>` value = CONV zsmail_mapping-value( lv_pernr && ` ` && lo_assist->get_pernr_fio( iv_pernr = lv_pernr ) ) ) TO lt_mapping.

      SELECT SINGLE text FROM zthr_ess_ekstype INTO @DATA(lv_text) WHERE code = @ls_rq-reqtype.
      APPEND VALUE #( name = `<reqname>` value = lv_text ) TO lt_mapping.

      zcl_send_email=>send_mail( EXPORTING itd_to          = lt_mail_to
*                                           iv_sender_email = `tech@lo.com`
                                           iv_textname     = 'ZHR_REQUEST_SAVE'
                                           iv_subject      = 'Новое обращения в ЕКС '
                                           itd_mapping     = lt_mapping
                                           iv_immediately  = abap_false ).
*      COMMIT WORK AND WAIT.
    ENDIF.
  ENDMETHOD.


  METHOD requestset_delete_entity.
    "Получение данных запроса
    DATA: lv_reqid TYPE zehr_ess_eksreq_reqid.
    READ TABLE it_key_tab ASSIGNING FIELD-SYMBOL(<key>) WITH KEY name = 'ReqId'.
    IF sy-subrc = 0.
      lv_reqid = <key>-value.
    ENDIF.

    "Удаление файлов
    CALL FUNCTION 'ZHR_ESS_EKSREQF_DEL' IN UPDATE TASK
      EXPORTING
        iv_req_id = lv_reqid.

    "Удаление заявки
    CALL FUNCTION 'ZHR_ESS_EKSREQ_DEL' IN UPDATE TASK
      EXPORTING
        iv_req_id = lv_reqid.

    COMMIT WORK AND WAIT.
  ENDMETHOD.


  METHOD requestsgetset_get_entityset.
    DATA: ls_rq      TYPE zcl_z_mss_request_save_mpc=>ts_requestsget,
          lv_begda   TYPE datum,
          lv_endda   TYPE datum,
          lv_usrid   TYPE p0105-usrid,
          lv_pernr   TYPE p_pernr,
          lv_reqtype TYPE dd07v-domvalue_l,
          ls_entity  LIKE LINE OF et_entityset.

    "Получение входных параметров
    LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<filter>).
      READ TABLE <filter>-select_options ASSIGNING FIELD-SYMBOL(<option>) INDEX 1.
      IF sy-subrc = 0.
        DATA(lv_property) = <filter>-property.
        TRANSLATE lv_property TO UPPER CASE.
        ASSIGN COMPONENT lv_property OF STRUCTURE ls_rq TO FIELD-SYMBOL(<fs_rq>).
        CHECK <fs_rq> IS ASSIGNED AND sy-subrc = 0.
        <fs_rq> = <option>-low.
      ENDIF.
    ENDLOOP.

    "Подготовка данных
    DATA(lo_assist) = zcl_mss_data_assistent=>get_instance( ).
*21/03/2020
    IF ls_rq-entranceuser IS NOT INITIAL.
      lv_usrid = ls_rq-entranceuser.
    ELSE.
      lv_usrid = ls_rq-mainuser.
    ENDIF.
    lv_pernr = lo_assist->get_pernr( iv_usrid = lv_usrid ).
    IF ls_rq-begda IS INITIAL.
      lv_begda = sy-datum.
      lv_begda+6(2) = '01'.
      SUBTRACT 1 FROM lv_begda.
      lv_begda+6(2) = '01'.
      SUBTRACT 1 FROM lv_begda.
      lv_begda+6(2) = '01'.
    ELSE.
      lv_begda = ls_rq-begda .
    ENDIF.

    IF ls_rq-endda IS INITIAL.
      lv_endda = sy-datum.
    ELSE.
      lv_endda = ls_rq-endda.
    ENDIF.

    CHECK lv_pernr IS NOT INITIAL AND lv_begda IS NOT INITIAL AND lv_endda IS NOT INITIAL.

    "Выборка данных
    SELECT * FROM zthr_ess_eksreq INTO TABLE @DATA(lt_data)
    WHERE date_create    >= @lv_begda
      AND date_create    <= @lv_endda
      AND perner_creator =  @lv_pernr.

    "Формирование выходных данных
    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<data>).
      CLEAR: ls_entity.

      ls_entity-entranceuser  = ls_rq-entranceuser.
      ls_entity-mainuser      = ls_rq-mainuser.
      ls_entity-begda         = ls_rq-begda.
      ls_entity-endda         = ls_rq-endda.
      ls_entity-datecreate    = <data>-date_create.
      ls_entity-coment        = <data>-coment.
      ls_entity-hrcomment      = <data>-hrcomment.
      "ФИО
*      ls_entity-approverpernr = lv_pernr.
      ls_entity-approverpernr = <data>-approver_pernr.

*      SELECT SINGLE * INTO @DATA(ls_pa0002) FROM pa0002 WHERE pernr = @lv_pernr.
*      IF sy-subrc = 0.
*        CONCATENATE ls_pa0002-nachn ls_pa0002-vorna ls_pa0002-midnm INTO ls_entity-approverfio SEPARATED BY space.
*      ENDIF.

      ls_entity-approverfio = lo_assist->get_pernr_fio( iv_pernr = CONV #( ls_entity-approverpernr ) ).

      "Описание заявки
      ls_entity-reqid = <data>-req_id.
      SELECT SINGLE text FROM zthr_ess_ekstype INTO ls_entity-reqname WHERE code = <data>-req_type.

      "Статус
      lv_reqtype = <data>-status.
      CALL FUNCTION 'DOMAIN_VALUE_GET'
        EXPORTING
          i_domname  = 'ZDHR_ESS_EKSREQ_STATUS'
          i_domvalue = lv_reqtype
        IMPORTING
          e_ddtext   = ls_entity-status
        EXCEPTIONS
          not_exist  = 1
          OTHERS     = 2.

      "Внесем найденное
      APPEND ls_entity TO et_entityset.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
