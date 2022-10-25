class ZCL_Z_MSS_VAC_CALENDAR_DPC_EXT definition
  public
  inheriting from ZCL_Z_MSS_VAC_CALENDAR_DPC
  create public .

public section.

  constants C_HOLIDAY type STRING value 'C' ##NO_TEXT.
  constants C_FREE type STRING value 'H' ##NO_TEXT.

  class-methods READ_WORK_SCHEDULE
    importing
      !IV_BEGDA type DATUM
      !IV_ENDDA type DATUM
      !IV_PERNR type PERNR_D
    exporting
      !ET_DAYGEN type PWSDAYGEN_TAB
      !ET_DAYINT type PWSDAYINT_TAB
      !ET_P0001 type PTT_P0001
      !ET_PSP type PTPSP_TAB .
  class-methods GET_HOLIDAYS
    importing
      !I_PERNR type PERSNO
      !I_BEGDA type BEGDA
      !I_ENDDA type ENDDA
    exporting
      !ET_ENTITYSET type ZCL_Z_MSS_VAC_CALENDAR_MPC=>TT_HOLIDAY .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITYSET
    redefinition .
protected section.

  methods HOLIDAYSET_GET_ENTITYSET
    redefinition .
private section.

  class-data MV_DUMMY type STRING .
  data MO_MESS type ref to /IWBEP/IF_MESSAGE_CONTAINER .
  data GC_TVARV_GSS_ORGVIEW type RVARI_VNAM value 'ZXSS_ORGVIEW_FOR_GSS' ##NO_TEXT.

  methods GET_ALL_EMPL_BY_OADP
    importing
      !IV_PERNR type PERNR_D
      !IV_BEGDA type BEGDA
      !IV_ENDDA type ENDDA
      !IV_ORGVIEW type HRWPC_ORGVIEW
    exporting
      !ET_EMPLS_DATA type PTARQ_UIA_ROWLABEL_TAB
    exceptions
      OBJSEL_NOT_FOUND
      OBJECTSELECTION_INVALID .
  methods RAISE_EXCEPTION
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_GSS_ORGVIEW
    returning
      value(RV_ORGVIEW) type HRWPC_ORGVIEW .
ENDCLASS.



CLASS ZCL_Z_MSS_VAC_CALENDAR_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_expanded_entityset.

    DATA: ls_deep_entity TYPE zcl_z_mss_vac_calendar_mpc_ext=>ts_deep_entity
        , lt_deep_entity TYPE TABLE OF zcl_z_mss_vac_calendar_mpc_ext=>ts_deep_entity
        .

    DATA: lv_orgview       TYPE hrwpc_orgview
        , lv_obj_orgview   TYPE objektid
        , lv_entrance_user TYPE zlogin_act_dir
        , lv_main_user     TYPE zlogin_act_dir
        , lv_begda TYPE begda
        , lv_endda TYPE endda
        , lv_property TYPE string
        , lt_return TYPE bapiret2_t
        .
    TYPES:
      BEGIN OF t_row_id,
        row_id TYPE ptarq_uia_weekcale_struc-row_id,
      END OF t_row_id.

    DATA:
      lv_pernr            TYPE pernr_d,
      lv_pernr2           TYPE pernr_d,
      lv_name             TYPE string,
      ls_return           LIKE LINE OF lt_return,
      lv_cache_validity   TYPE int4,

      lt_mes              TYPE ptarq_uia_messages_tab,
      ls_mes              LIKE LINE OF lt_mes,
      lt_col              TYPE ptarq_uia_custcolumns_tab,
      ls_col              LIKE LINE OF lt_col,
      lt_com              TYPE ptarq_uia_command_tab,
      ls_com              LIKE LINE OF lt_com,
      lt_legend           TYPE ptreq_uia_legend_tab,
      ls_legend           LIKE LINE OF lt_legend,
      lt_weekday          TYPE ptarq_uia_weekday_tab,
      ls_weekday          LIKE LINE OF lt_weekday,
      lt_team             TYPE ptreq_uia_team_tab,
      ls_team             LIKE LINE OF lt_team,

      ls_team_id          TYPE ptreq_uia_teamid_struc,
      ls_team_id2         TYPE ptreq_uia_teamid_struc,
      lv_cache_date       TYPE sy-datum,
      lv_cache_time       TYPE sy-uzeit,
      lt_calendar         TYPE ptarq_uia_weekcale_tab,
      ls_calendar         LIKE LINE OF lt_calendar,
      lt_rowlabel         TYPE ptarq_uia_rowlabel_tab,
      ls_rowlabel         LIKE LINE OF lt_rowlabel,
      ls_position         TYPE ptreq_uia_pagepos,

      ls_req              TYPE ptreq_uia_request_id_struc,
      lt_row_id           TYPE TABLE OF t_row_id,
      ls_row_id           LIKE LINE OF lt_row_id,

      lt_ptreq_items      TYPE TABLE OF ptreq_items,
      ls_ptreq_items      LIKE LINE OF lt_ptreq_items,
      lt_ptreq_header     TYPE TABLE OF ptreq_header,
      ls_ptreq_header     LIKE LINE OF lt_ptreq_header,
      lt_ptreq_attabsdata TYPE TABLE OF ptreq_attabsdata,
      ls_ptreq_attabsdata LIKE LINE OF lt_ptreq_attabsdata,

      ls_abs              LIKE LINE OF ls_deep_entity-navabs,

      lv_need             TYPE flag,
      lv_str              TYPE string
    , lv_notice_text      TYPE string
    , lv_xss              TYPE zhr_e_xss
    , lv_user             TYPE sy-uname
    .


    CASE iv_entity_set_name.
      WHEN 'EntityMainSet'.

        DATA lo_message_container TYPE REF TO /iwbep/if_message_container.
        lo_message_container = mo_context->get_message_container( ).
        LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<filter>).
          LOOP AT <filter>-select_options ASSIGNING FIELD-SYMBOL(<range>).
            lv_property = <filter>-property.
            TRANSLATE lv_property TO UPPER CASE.
            CASE lv_property.
              WHEN 'ENDDA'.
                lv_endda = <range>-low.
              WHEN 'BEGDA'.
                lv_begda = <range>-low.
              WHEN 'ENTRANCE_USER'.
                lv_entrance_user = <range>-low.
              WHEN 'MAIN_USER'.
                lv_main_user = <range>-low.
              WHEN 'XSS'.
                lv_xss = <range>-low.
            ENDCASE.
          ENDLOOP.
        ENDLOOP.

        IF lv_entrance_user IS INITIAL.
          lv_entrance_user = lv_main_user.
        ENDIF.

        IF lv_endda IS INITIAL OR
           lv_begda IS INITIAL OR
           lv_begda > lv_endda.
          APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<return>).
          <return>-type = 'E'.
          <return>-id   = 'ZTEST'.
          <return>-number = '009'.
          <return>-message = 'Неверный период'.

          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.

        ENDIF.

        IF lv_entrance_user IS INITIAL.
          APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
          <return>-type = 'E'.
          <return>-id   = 'ZTEST'.
          <return>-number = '007'.
          <return>-message = 'Ошибка при передачи логина'.

          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.
        ENDIF.

        DATA: lv_sprps TYPE p2001-sprps
            .

        TRANSLATE lv_entrance_user TO UPPER CASE.
        DATA(lo_assist) = zcl_mss_data_assistent=>get_instance( ).

        lv_pernr = lo_assist->get_pernr(  iv_usrid = CONV #( lv_entrance_user )   ).

        IF lv_xss = 'GSS'.

          lo_assist->check_pernr_is_top_head(
                EXPORTING
                  iv_pernr = lv_pernr
                  iv_begda = lv_begda
                  iv_endda = lv_endda
                IMPORTING
                  ev_pernr_gov = lv_pernr ).

        ENDIF.

        lv_user = CONV #( lo_assist->get_pernr_0105( iv_pernr = lv_pernr iv_subty = '0001' ) ).

        IF lv_pernr IS INITIAL.
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
        ENDIF.

        ls_deep_entity-owner_number = lv_pernr.

        CALL METHOD cl_pt_arq_customizing=>get_tcale_attribs
          EXPORTING
            im_pernr            = lv_pernr
            im_date             = lv_begda
          IMPORTING
            ex_cache_validity   = lv_cache_validity
          EXCEPTIONS
            it0001_not_found    = 1
            it0008_not_found    = 2
            missing_customizing = 3
            OTHERS              = 4.
        IF sy-subrc NE 0.
          lv_cache_validity = 1.
        ENDIF.

        "Skorobagatov A.N. IBS 30.12.2019 12:43:57
        me->get_all_empl_by_oadp(
              EXPORTING
                iv_pernr = lv_pernr
                iv_begda = lv_begda
                iv_endda = lv_endda
                iv_orgview = get_gss_orgview( )
              IMPORTING
                et_empls_data = lt_rowlabel ).

        SORT lt_rowlabel ASCENDING.
        DELETE ADJACENT DUPLICATES FROM lt_rowlabel COMPARING row_id.
        "Skorobagatov A.N. IBS 30.12.2019 17:32:51

        DATA: lt_pernr TYPE hrobject_t
            , ls_pernr LIKE LINE OF lt_pernr
            .

* соберем список табельных
        CLEAR lt_row_id.
        LOOP AT lt_rowlabel INTO ls_rowlabel.
          ls_row_id-row_id = ls_rowlabel-row_id.
          COLLECT ls_row_id INTO lt_row_id.
          ls_pernr-objid = ls_rowlabel-row_id.
          COLLECT ls_pernr INTO lt_pernr.
        ENDLOOP.

        DATA: BEGIN OF ls_pa2001
            , pernr TYPE pa2001-pernr
            , subty TYPE pa2001-subty
            , begda TYPE pa2001-begda
            , endda TYPE pa2001-endda
            , sprps TYPE pa2001-sprps
            , zdone TYPE flag
            , END OF ls_pa2001

            , lt_pa2001 LIKE TABLE OF ls_pa2001 WITH NON-UNIQUE KEY pernr begda endda
            .

        IF lt_pernr IS NOT INITIAL.
          SELECT pernr,
                 subty,
                 begda,
                 endda,
                 sprps
                 INTO TABLE @lt_pa2001
                 FROM pa2001
                 FOR ALL ENTRIES IN @lt_pernr
                 WHERE pernr = @lt_pernr-objid
                   AND sprps = @abap_false
                   AND begda < @lv_endda
                   AND endda >= @lv_begda.
        ENDIF.

* заполним таблицу
        LOOP AT lt_row_id INTO ls_row_id.

          CLEAR:
            ls_deep_entity, lt_ptreq_header.

          lv_pernr2 = ls_row_id-row_id.

          IF zcl_vacation_appl=>check_valid_pernr(
              i_pernr = lv_pernr2 ) EQ abap_false.
            CONTINUE.
          ENDIF.

          lv_name = zcl_vacation_appl=>get_pernr_name( iv_pernr = lv_pernr2 ).

          ls_deep_entity-owner_fio = lv_name.
          ls_deep_entity-owner_number = lv_pernr2.

          LOOP AT lt_calendar INTO ls_calendar WHERE row_id EQ ls_row_id-row_id.
            LOOP AT ls_calendar-request INTO ls_req.

              SELECT *
                INTO TABLE lt_ptreq_header
                FROM ptreq_header
                WHERE
                  request_id  EQ ls_req-request_id.

              IF sy-subrc EQ 0.

                SORT lt_ptreq_header BY version_no DESCENDING.
                READ TABLE lt_ptreq_header INTO ls_ptreq_header INDEX 1.

                CLEAR: lv_notice_text.
                IF sy-subrc EQ 0.
                  SELECT SINGLE notice_text FROM ptreq_notice
                    INTO lv_notice_text
                    WHERE request_id EQ ls_ptreq_header-request_id AND
                          version_no EQ ls_ptreq_header-version_no.
                ENDIF.

                SELECT *
                  INTO TABLE lt_ptreq_items
                  FROM ptreq_items
                  WHERE
                    item_list_id  EQ ls_ptreq_header-item_list_id.

                CHECK sy-subrc EQ 0.

                LOOP AT lt_ptreq_items INTO ls_ptreq_items.

                  SELECT SINGLE *
                    INTO ls_ptreq_attabsdata
                    FROM ptreq_attabsdata
                    WHERE
                      item_id EQ ls_ptreq_items-item_ins.

                  CHECK sy-subrc EQ 0.
                  "CHECK ls_ptreq_attabsdata-operation EQ 'INS'.
                  CLEAR ls_abs.
                  ls_abs-request_id = ls_ptreq_header-request_id.
                  ls_abs-status     = ls_ptreq_header-status.
                  ls_abs-comment    = lv_notice_text.
                  ls_abs-owner_number = lv_pernr2.
                  ls_abs-begda = ls_calendar-begda.
                  ls_abs-endda = ls_calendar-endda.
                  ls_abs-abstype = ls_ptreq_attabsdata-subty.
                  COLLECT ls_abs INTO ls_deep_entity-navabs.

                ENDLOOP.

              ELSE.

                lv_str = ls_req-tooltip.
                CONDENSE lv_str NO-GAPS.
                IF strlen( lv_str ) GE 21.
                  CLEAR ls_abs.
                  ls_abs-status = 'POSTED'.
                  ls_abs-begda = lv_str+6(4) && lv_str+3(2) && lv_str(2).
                  IF lv_str+10(1) EQ '-'.
                    ls_abs-endda = lv_str+17(4) && lv_str+14(2) && lv_str+11(2).
                  ELSE.
                    ls_abs-endda = ls_abs-begda.
                  ENDIF.

*                  SELECT SINGLE subty sprps
*                    INTO (ls_abs-abstype, lv_sprps)
*                    FROM pa2001
*                    WHERE
*                      pernr EQ lv_pernr2    AND
*                      begda EQ ls_abs-begda AND
*                      endda EQ ls_abs-endda.
*                  CHECK sy-subrc EQ 0 AND lv_sprps EQ abap_false.
                  READ TABLE lt_pa2001 ASSIGNING FIELD-SYMBOL(<lt_pa2001>) WITH TABLE KEY pernr = lv_pernr2
                                                                                          begda = ls_abs-begda
                                                                                          endda = ls_abs-endda.
                  CHECK <lt_pa2001> IS ASSIGNED AND sy-subrc = 0.
                  ls_abs-abstype = <lt_pa2001>-subty.

                  <lt_pa2001>-zdone = abap_true.

                  ls_abs-owner_number = lv_pernr2.

                  COLLECT ls_abs INTO ls_deep_entity-navabs.
                ENDIF.
              ENDIF.

            ENDLOOP.
          ENDLOOP.

          READ TABLE lt_pa2001 TRANSPORTING NO FIELDS WITH KEY pernr = lv_pernr2 BINARY SEARCH.

          LOOP AT lt_pa2001 ASSIGNING <lt_pa2001> FROM sy-tabix WHERE NOT zdone = abap_true.
            IF NOT <lt_pa2001>-pernr = lv_pernr2.
              EXIT.
            ENDIF.

            CLEAR ls_abs.
            ls_abs-status       = 'POSTED'.

            IF <lt_pa2001>-begda <= lv_begda.
              ls_abs-begda = lv_begda.
            ELSEIF <lt_pa2001>-begda >= lv_begda.
              ls_abs-begda = <lt_pa2001>-begda.
            ENDIF.
*            ls_abs-begda        = <lt_pa2001>-begda.
            IF <lt_pa2001>-endda <= lv_endda.
              ls_abs-endda = <lt_pa2001>-endda.
            ELSEIF <lt_pa2001>-endda >= lv_begda.
              ls_abs-endda = lv_endda.
            ENDIF.

*            ls_abs-endda        = <lt_pa2001>-endda.
            ls_abs-owner_number = <lt_pa2001>-pernr.
            ls_abs-abstype      = <lt_pa2001>-subty.
            COLLECT ls_abs INTO ls_deep_entity-navabs.
          ENDLOOP.

          APPEND ls_deep_entity TO lt_deep_entity.

        ENDLOOP.

***CALL FUNCTION 'PT_ARQ_TEAMCALE_GET'
***  EXPORTING
****   IM_COMMAND                 =
***    im_pernr                   =
****   IM_USER                    = SY-UNAME
****   IM_LANGUAGE                = SY-LANGU
***    im_team                    =
***    im_begda                   = lv_begda
***    im_endda                   = lv_endda
***    im_cache_validity          =
****   IM_GOTO_PAGE_NUMBER        = 1
****   IM_TIME_FORMAT             = 2
***    im_modus                   =
****   IM_DEBUG                   =
****   IM_DEACTIVATE_CHECK        =
****   IM_DEACTIVATE_PAGING       =
****   IM_ABAP_FLAG               =
**** IMPORTING
****   EX_TEAM                    =
****   EX_CACHE_DATE              =
****   EX_CACHE_TIME              =
****   EX_CALENDAR_TAB            =
****   EX_ROWLABEL                =
****   EX_POSITION                =
***  TABLES
***    ex_team_tab                =
***    ex_weekday_tab             =
***    ex_legend_tab              =
***    ex_commands                =
***    ex_custcolumns_tab         =
***    ex_messages                =
***          .


*{   DELETE         HRDK957127                                        3
*\****<<< ВРЕМЕННАЯ ЗАГЛУШКА
*\        ls_deep_entity-owner_fio = 'Иванов Иван Иванович'.
*\        ls_deep_entity-owner_number = '00000588'.
*\**        ls_deep_entity-endda = lv_endda.
*\**        ls_deep_entity-begda = lv_begda.
*\        APPEND INITIAL LINE TO ls_deep_entity-navabs ASSIGNING FIELD-SYMBOL(<abs>).
*\        <abs>-owner_number = '00000588'.
*\        <abs>-abstype = '0101'.
*\        <abs>-begda = '20170713'.
*\        <abs>-endda ='20170717'.
*\        APPEND INITIAL LINE TO ls_deep_entity-navabs ASSIGNING <abs>.
*\        <abs>-owner_number = '00000588'.
*\        <abs>-abstype = '0101'.
*\        <abs>-begda = '20170720'.
*\        <abs>-endda ='20170725'.
*\
*\        APPEND ls_deep_entity TO lt_deep_entity.
*\
*\        ls_deep_entity-owner_fio = 'Иванов Иван Иванович2'.
*\        ls_deep_entity-owner_number = '00000589'.
*\**        ls_deep_entity-endda = lv_endda.
*\**        ls_deep_entity-begda = lv_begda.
*\        APPEND INITIAL LINE TO ls_deep_entity-navabs ASSIGNING <abs>.
*\        <abs>-owner_number = '00000589'.
*\        <abs>-abstype = '0101'.
*\        <abs>-begda = '20170713'.
*\        <abs>-endda ='20170717'.
*\
*\        APPEND ls_deep_entity TO lt_deep_entity.
*\****>>>КОНЕЦ ЗАГЛУШКИ
*}   DELETE

        copy_data_to_ref(
                EXPORTING
                  is_data = lt_deep_entity
                CHANGING
                  cr_data = er_entityset ).

    ENDCASE.

  ENDMETHOD.


  METHOD get_all_empl_by_oadp.

    DATA: lv_is_ok              TYPE boole_d,
          lv_plvar              TYPE plvar,
          lv_user               TYPE sy-uname,
          lv_rootselrule        TYPE hrwpc_objselrule,
          lv_navselrule         TYPE hrwpc_objselrule,
          lv_objselrule         TYPE hrwpc_objselrule,
          lv_pernr              TYPE struc-objid,
          lv_ch_orgview         TYPE hrwpc_orgview,

          lt_keyobjects         TYPE hrwpc_t_keyobjec,
          lt_keyobjectstructure TYPE hrwpc_t_keystruc,
          lt_root_objects       TYPE hrrootob_t,
          lt_root_objec         TYPE objec_t,
          lt_root_struc         TYPE struc_t,
          lt_objects            TYPE TABLE OF hrwpc_s_keyobjec,
          lt_objectstructure    TYPE TABLE OF hrwpc_s_keystruc,
          lt_ex_tean_tab        TYPE          ptreq_rtteam_tab,
          lt_views              TYPE hrwpc_t_oadp_orgview,

          ls_team_view          TYPE ptreq_teamid_struc,
          ls_viewid_or_selid    TYPE ptreq_view_or_selid,
          ls_objselrule_root    TYPE twpc_objselrule,
          ls_objselrule_obj     TYPE twpc_objselrule,
          ls_views              TYPE hrwpc_s_oadp_orgview.

    CALL FUNCTION 'HRWPC_RFC_OADP_GET_ORGVIEWGRP'
      EXPORTING
        orgviewgrp           = iv_orgview
        langu                = sy-langu
      TABLES
        t_orgviews           = lt_views
      EXCEPTIONS
        orgviewgrp_not_found = 1
        OTHERS               = 2.

    LOOP AT lt_views INTO ls_views.

      CLEAR:
      lt_root_struc,
      lt_objectstructure,
      lt_objects,
      lt_root_objec,
      lt_root_objects,
      lv_rootselrule,
      lv_navselrule,
      lv_objselrule,
      ls_objselrule_root,
      lv_is_ok,
      ls_objselrule_obj,
      lt_keyobjects,
      lt_keyobjectstructure.

      CALL FUNCTION 'HRWPC_RFC_OADP_GET_OBJSEL'
        EXPORTING
          objsel           = ls_views-orgview
          langu            = sy-langu
        IMPORTING
*         OBJSELTEXT       =
          rootselrule      = lv_rootselrule
*         ROOTSELRULETEXT  =
          navselrule       = lv_navselrule
*         NAVSELRULETEXT   =
          objselrule       = lv_objselrule
*         OBJSELRULETEXT   =
*         OBJSEARCHCLASS   =
*         OBJSEARCHPARAMGRP           =
*         OBJSEARCHPARAMGRPTEXT       =
        EXCEPTIONS
          objsel_not_found = 1
          OTHERS           = 2.

      IF sy-subrc <> 0.
        RAISE objsel_not_found.
      ENDIF.

      cl_hrwpc_twpc_objselrule=>read(
        EXPORTING
          objselrule      = lv_rootselrule
        IMPORTING
          twpc_objselrule = ls_objselrule_root
          is_ok           = lv_is_ok ).

      IF lv_is_ok IS INITIAL.
        RAISE objectselection_invalid.
      ENDIF.

      cl_hrwpc_twpc_objselrule=>read(
        EXPORTING
          objselrule      = lv_objselrule
        IMPORTING
          twpc_objselrule = ls_objselrule_obj
          is_ok           = lv_is_ok ).

      IF lv_is_ok IS INITIAL.
        RAISE objectselection_invalid.
      ENDIF.

      CALL FUNCTION 'RH_GET_ACTIVE_WF_PLVAR'
        EXPORTING
          ask_plvar_dialog = ' '
*         SET_DEFAULT_PLVAR =
        IMPORTING
          act_plvar        = lv_plvar
        EXCEPTIONS
          no_active_plvar  = 1
          OTHERS           = 2.

      IF sy-subrc <> 0.
        RAISE objectselection_invalid.
      ENDIF.

      lv_pernr = iv_pernr.

      CALL FUNCTION 'HRWPC_GET_PATHROOTS'
        EXPORTING
*         user                    =
          otype                   = 'P'
          objid                   = lv_pernr
          plvar                   = lv_plvar
          begda                   = iv_begda
          endda                   = iv_endda
          pathid                  = ls_objselrule_root-evaluationpath
          depth                   = ls_objselrule_root-evaluationdepth
          fm_pathroots            = ls_objselrule_root-objselfunction
        TABLES
*         paramvalues             =
*         ROOT_OBJECTS            =
          keyobjec                = lt_keyobjects[]
          keystruc                = lt_keyobjectstructure[]
        EXCEPTIONS
          no_roots_found          = 1
          invalid_function_module = 2
          OTHERS                  = 3.

      CHECK sy-subrc = 0.

      PERFORM modify_rootobjects IN PROGRAM saplhrwpc_oadp_ui
        USING ls_objselrule_root-objselrule
              lv_plvar
              iv_begda
              iv_endda
              lv_user
              ls_objselrule_root-xdelduplicates
              ' '
     CHANGING lt_keyobjects
              lt_keyobjectstructure.

      PERFORM fill_rootobjects IN PROGRAM saplhrwpc_oadp_ui
        USING lt_keyobjects
     CHANGING lt_root_objects.

      PERFORM fill_rootobjec IN PROGRAM saplhrwpc_oadp_ui
        USING lt_keyobjects
     CHANGING lt_root_objec.

      PERFORM fill_rootstruc IN PROGRAM saplhrwpc_oadp_ui
        USING lt_keyobjectstructure
     CHANGING lt_root_struc.


      CHECK NOT lt_root_objects[] IS INITIAL.

      CALL FUNCTION 'HRWPC_GET_OBJECTS_FROM_ROOTS'
        EXPORTING
          pathid           = ls_objselrule_obj-evaluationpath
          objects_fm       = ls_objselrule_obj-objselfunction
*         LEVEL            = 1
          depth            = ls_objselrule_obj-evaluationdepth
          plvar            = lv_plvar
          begda            = iv_begda
          endda            = iv_endda
        TABLES
          root_object      = lt_root_objects
          root_objec       = lt_root_objec
          root_struc       = lt_root_struc
*         paramvalues      =
          keyobjec         = lt_objects
          keystruc         = lt_objectstructure
        EXCEPTIONS
          no_objects_found = 1
          OTHERS           = 2.
      CHECK sy-subrc = 0.

      PERFORM modify_rootobjects IN PROGRAM saplhrwpc_oadp_ui
          USING ls_objselrule_obj-objselrule
                lv_plvar
                iv_begda
                iv_endda
                lv_user
                ls_objselrule_obj-xdelduplicates
                ' '
       CHANGING lt_objects
                lt_objectstructure.

      LOOP AT lt_objects ASSIGNING FIELD-SYMBOL(<fs_objects>).

        APPEND INITIAL LINE TO et_empls_data
             ASSIGNING FIELD-SYMBOL(<fs_empls_data>).

        <fs_empls_data>-row_id = <fs_objects>-objid.

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_gss_orgview.

    SELECT low
      FROM tvarvc
      INTO TABLE @DATA(lt_tvarvc)
     WHERE name = @gc_tvarv_gss_orgview
       AND type = 'P'.

    READ TABLE lt_tvarvc
        ASSIGNING FIELD-SYMBOL(<fs_tvarvc>)
        INDEX 1.

    CHECK sy-subrc = 0
      AND <fs_tvarvc> IS ASSIGNED.

    rv_orgview = <fs_tvarvc>-low.

  ENDMETHOD.


  METHOD GET_HOLIDAYS.

    DATA: lt_p0001  TYPE ptt_p0001
        , lt_psp    TYPE ptpsp_tab
        , lt_dayint TYPE TABLE OF  pwsdayint
        , lt_daygen TYPE TABLE OF  pwsdaygen

        , lo_badi_additional_info TYPE REF TO hress_b_team_calendar_enh
        , lt_ftkla                TYPE TABLE OF ftkla
        .

    read_work_schedule(
      EXPORTING
        iv_begda  = i_begda                      " datum
        iv_endda  = i_endda                      " datum
        iv_pernr  = i_pernr                               " pernr_d
      IMPORTING
        et_p0001  = lt_p0001                              " ptt_p0001
        et_psp    = lt_psp                                 " ptpsp_tab
        et_dayint = lt_dayint
        et_daygen = lt_daygen ).

    GET BADI lo_badi_additional_info.
    CALL BADI lo_badi_additional_info->set_disp_public_holidays
      CHANGING
        im_ftkla = lt_ftkla.
    LOOP AT lt_psp ASSIGNING FIELD-SYMBOL(<fs_psp>) WHERE ftkla EQ 0.
      READ TABLE lt_daygen ASSIGNING FIELD-SYMBOL(<fs_daygen>)
        WITH KEY datum = <fs_psp>-datum.
      IF sy-subrc EQ 0 AND <fs_daygen>-sollz IS INITIAL.    "LAK2057053
*       выходной
        APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_out>).
        <fs_out>-type = c_free.
        <fs_out>-text = cl_wd_utilities=>get_otr_text_by_alias( alias = 'PAOC_ESS_WDA_OTR/NON_WORKDAY' ).
        <fs_out>-begda = <fs_psp>-datum.
        <fs_out>-endda = <fs_psp>-datum.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_psp ASSIGNING <fs_psp> WHERE ftkla NE 0.
      READ TABLE lt_ftkla WITH KEY table_line = <fs_psp>-ftkla TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        APPEND INITIAL LINE TO et_entityset ASSIGNING <fs_out>.
        <fs_out>-type = c_holiday.  " праздник
        <fs_out>-text = cl_hress_ptarq_tim_services=>get_public_holiday_description(
                                   iv_pernr = i_pernr
                                   iv_date = <fs_psp>-datum ).
        IF <fs_out>-text IS INITIAL.
          <fs_out>-type = c_free. " выходной
          <fs_out>-text = cl_wd_utilities=>get_otr_text_by_alias( alias = 'PAOC_ESS_WDA_OTR/NON_WORKDAY' ).
        ENDIF.
        <fs_out>-begda = <fs_psp>-datum.
        <fs_out>-endda = <fs_psp>-datum.
      ELSE.
        READ TABLE lt_daygen ASSIGNING <fs_daygen>
          WITH KEY datum = <fs_psp>-datum.
        IF sy-subrc EQ 0 AND <fs_daygen>-sollz IS INITIAL.
          APPEND INITIAL LINE TO et_entityset ASSIGNING <fs_out>.
          <fs_out>-type = c_free.
          <fs_out>-text = cl_wd_utilities=>get_otr_text_by_alias( alias = 'PAOC_ESS_WDA_OTR/NON_WORKDAY' ).
          <fs_out>-begda = <fs_psp>-datum.
          <fs_out>-endda = <fs_psp>-datum.
        ENDIF.
      ENDIF.
    ENDLOOP.

    SORT et_entityset BY begda.
  ENDMETHOD.


  METHOD holidayset_get_entityset.
*   логика взята из HRESS_C_LEA_TEAM_CALENDAR ракурс V_TEAM_CALENDAR метод BIND_CALENDAR_DATA_NEW

    DATA: lv_login  TYPE zlogin_act_dir
        , lv_begda  TYPE begda
        , lv_endda  TYPE endda
        , lv_pernr  TYPE persno
        .

    LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<filter>).
      READ TABLE <filter>-select_options INDEX 1 ASSIGNING FIELD-SYMBOL(<range>).
      CHECK sy-subrc EQ 0.
      CASE <filter>-property.
        WHEN 'Entrance_User'.
          IF <range>-low IS NOT INITIAL.
            lv_login = <range>-low.
          ENDIF.
        WHEN 'MainUser'.
          IF <range>-low IS NOT INITIAL.
            lv_login = <range>-low.
          ENDIF.
        WHEN 'begda'.
          lv_begda = <range>-low.
        WHEN 'endda'.
          lv_endda = <range>-low.
      ENDCASE.
    ENDLOOP.

    IF lv_begda IS INITIAL.
      MESSAGE e003(zhr_ess) WITH 'begda' INTO mv_dummy.
*      add_msg( ).
      raise_exception( ).
    ENDIF.

    IF lv_endda IS INITIAL.
      MESSAGE e003(zhr_ess) WITH 'endda' INTO mv_dummy.
*      add_msg( ).
      raise_exception( ).
    ENDIF.

    TRANSLATE lv_login TO UPPER CASE.
    DATA(lo_assist) = zcl_mss_data_assistent=>get_instance( ).
*    lv_pernr = zcl_vacation_appl_renault=>get_pernr_by_ad( i_ad = lv_login ).
    lv_pernr = lo_assist->get_pernr( iv_usrid = CONV #( lv_login )   ).
    IF lv_pernr IS INITIAL.
      MESSAGE e001(zhr_ess) INTO mv_dummy.
*      add_msg( ).
      raise_exception( ).
    ENDIF.

    get_holidays(
      EXPORTING
        i_pernr      = lv_pernr    " Табельный номер
        i_begda      = lv_begda    " Начало срока действия
        i_endda      = lv_endda    " Конец срока действия
      IMPORTING
        et_entityset = et_entityset ).
  ENDMETHOD.


  METHOD RAISE_EXCEPTION.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = mo_mess.
  ENDMETHOD.


  METHOD READ_WORK_SCHEDULE.
    DATA: lt_p0000  TYPE TABLE OF p0000,
          lt_p0001  TYPE TABLE OF p0001,
          lt_p0002  TYPE TABLE OF p0002,
          lt_p0007  TYPE TABLE OF p0007,
          lt_p2001  TYPE TABLE OF p2001,
          lt_p2002  TYPE TABLE OF p2002,
          lt_p2003  TYPE TABLE OF p2003,
          lt_psp    TYPE TABLE OF ptpsp,
          lt_daygen TYPE TABLE OF pwsdaygen,
          lt_dayint TYPE TABLE OF pwsdayint,
          lt_pernr  TYPE          tim_tmw_pernr_d_tab,
          lt_perws  TYPE          ptm_psp,
          ls_dayint TYPE          pwsdayint.

    DATA rdclust TYPE rdclst.
*  DATA ls_msgtext          TYPE string.
*  DATA constraints         TYPE ptarq_tconstr.                   "INS Note 1721171
*
*  CALL METHOD cl_pt_arq_customizing=>get_time_constraints        "INS Note 1721171
*    EXPORTING                                                    "INS Note 1721171
*      im_pernr       = wd_this->gv_pernr                         "INS Note 1721171
*      im_date        = sy-datum                                  "INS Note 1721171
*    IMPORTING                                                    "INS Note 1721171
*      ex_constraints = constraints                               "INS Note 1721171
*    EXCEPTIONS                                                   "INS Note 1721171
*      OTHERS         = 1.                                        "INS Note 1721171

    CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
*       TCLAS           = 'A'
        pernr           = iv_pernr
        infty           = '0000'
*       BEGDA           = '18000101'
*       ENDDA           = '99991231'
*       BYPASS_BUFFER   = ' '
*       LEGACY_MODE     = ' '
* IMPORTING
*       SUBRC           =
      TABLES
        infty_tab       = lt_p0000
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE e050(rp) WITH '0000' iv_pernr space space
         INTO mv_dummy.
      RETURN.
    ENDIF.

    CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = iv_pernr
        infty           = '0001'
      TABLES
        infty_tab       = lt_p0001
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE e050(rp) WITH '0001' iv_pernr space space
         INTO mv_dummy.
      RETURN.
    ENDIF.

    CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = iv_pernr
        infty           = '0002'
      TABLES
        infty_tab       = lt_p0002
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE e050(rp) WITH '0002' iv_pernr space space
         INTO mv_dummy.
      RETURN.
    ENDIF.

    CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = iv_pernr
        infty           = '0007'
      TABLES
        infty_tab       = lt_p0007
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE e050(rp) WITH '0007' iv_pernr space space
         INTO mv_dummy.
      RETURN.
    ENDIF.

    CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = iv_pernr
        infty           = '2003'
      TABLES
        infty_tab       = lt_p2003
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    rdclust = ' '.

    CALL FUNCTION 'HR_PERSONAL_WORK_SCHEDULE'
      EXPORTING
        pernr             = iv_pernr
        begda             = iv_begda
        endda             = iv_endda
*       KUG               =
*       REFRESH           = 'X'
*       WORKING_HOURS     = 'X'
        switch_activ      = 0
        i0001_i0007_error = '0'
        read_cluster      = rdclust
*    IMPORTING
*       WARNING_OCCURED   =
      TABLES
        i0000             = lt_p0000
        i0001             = lt_p0001
        i0002             = lt_p0002
        i0007             = lt_p0007
*       I0049             =
        i2001             = lt_p2001
        i2002             = lt_p2002
        i2003             = lt_p2003
        perws             = lt_psp
      EXCEPTIONS
        error_occured     = 1
        abort_occured     = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
*    call function 'HR_DISPLAY_ERROR_LIST'
*         EXPORTING
*              no_popup         = ' '
*              no_print         = 'X'
*              no_img           = ' '
*         EXCEPTIONS
*              invalid_linesize = 1
*              others           = 2.
**********************Begin of LAK1709799**********************
    ELSE.
*   Get the Work Schedule times.
      CALL FUNCTION 'HR_WORK_SCHEDULE_TIMES'
        EXPORTING
          pernr                   = iv_pernr
          begda                   = iv_begda
          endda                   = iv_endda
          kug                     = abap_false
          break_overtime          = abap_false
          refresh_infotype_buffer = abap_false
        TABLES
*         i0001                   = p0001
*         i0007                   = p0007
*         I0049                   = p0049
*         i2003                   = p2003
          i0001                   = lt_p0001
          i0007                   = lt_p0007
*         I0049                   = INFOTYPE_0000_TAB
          i2003                   = lt_p2003
          perws                   = lt_psp
          daygen                  = lt_daygen
          dayint                  = lt_dayint
        EXCEPTIONS
          error_occured           = 1
          perws_error             = 2
          OTHERS                  = 3.
**********************End of LAK1709799*******************
    ENDIF.

    et_psp = lt_psp.
    et_dayint = lt_dayint.                                  "LAK1790799
    et_p0001 = lt_p0001.                                    "LAK1709799
    et_daygen = lt_daygen.                                  "LAK2057073
  ENDMETHOD.
ENDCLASS.
