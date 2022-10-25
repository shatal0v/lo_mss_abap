class ZCL_Z_MSS_CATALOGS_GET_DPC_EXT definition
  public
  inheriting from ZCL_Z_MSS_CATALOGS_GET_DPC
  create public .

public section.
protected section.

  methods CATALOGSSET_GET_ENTITYSET
    redefinition .
  methods EKSTYPESET_GET_ENTITYSET
    redefinition .
private section.

  class-data ENTRANCEUSER type STRING value 'EntranceUser' ##NO_TEXT.
  class-data MAINUSER type STRING value 'MainUser' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_Z_MSS_CATALOGS_GET_DPC_EXT IMPLEMENTATION.


  METHOD catalogsset_get_entityset.
    DATA: actor_container TYPE TABLE OF swcont,
          actor_tab       TYPE TABLE OF swhactor.

    TRY.
        "Получение входных параметров
*21/03/2020
        IF mainuser IS NOT INITIAL.
          READ TABLE it_filter_select_options ASSIGNING FIELD-SYMBOL(<filter>) WITH KEY property = mainuser.
        ENDIF.
        IF sy-subrc = 0.
          READ TABLE <filter>-select_options ASSIGNING FIELD-SYMBOL(<option>) INDEX 1.
          IF sy-subrc = 0.
            DATA(lv_entrance_user) = <option>-low.
          ENDIF.
        ENDIF.

        CHECK lv_entrance_user IS NOT INITIAL.

        TRANSLATE lv_entrance_user TO UPPER CASE.

        DATA(lv_begda) = sy-datum.

        "Получение необходимых данных
        SELECT SINGLE p2~werks INTO @DATA(lv_werks)
        FROM pa0105 AS p1
        JOIN pa0001 AS p2 ON p2~pernr = p1~pernr
        WHERE p1~subty = '9001'
          AND p1~usrid = @lv_entrance_user
          AND p1~begda <= @lv_begda
          AND p1~endda >= @lv_begda
          AND p2~begda <= @lv_begda
          AND p2~endda >= @lv_begda
          .
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO actor_container ASSIGNING FIELD-SYMBOL(<cont>).
          <cont>-element = 'PERSA'.
          <cont>-value = lv_werks.

          CALL FUNCTION 'RH_GET_ACTORS'
            EXPORTING
              act_object                = 'AC91000001'
              act_plvar                 = '01'
            TABLES
              actor_container           = actor_container[]
              actor_tab                 = actor_tab[]
            EXCEPTIONS
              no_active_plvar           = 1
              no_actor_found            = 2
              exception_of_role_raised  = 3
              no_valid_agent_determined = 4
              no_container              = 5
              OTHERS                    = 6.

          DELETE actor_tab[] WHERE otype <> 'S'.
          LOOP AT actor_tab ASSIGNING FIELD-SYMBOL(<objid>).
            DATA: lt_objid TYPE TABLE OF swhactor.
            CALL FUNCTION 'RH_STRUC_GET'
              EXPORTING
                act_otype      = 'S'
                act_objid      = <objid>-objid
                act_wegid      = 'A008'
                act_plvar      = '01'
              TABLES
                result_tab     = lt_objid
              EXCEPTIONS
                no_plvar_found = 1
                no_entry_found = 2
                OTHERS         = 3.
            READ TABLE lt_objid ASSIGNING FIELD-SYMBOL(<objid2>) INDEX 1.
            IF sy-subrc = 0.
              SELECT SINGLE * FROM pa0002 INTO @DATA(ls_pa0002) WHERE pernr = @<objid2>-objid.
              IF sy-subrc = 0.
                APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<set>).
                <set>-entrance_user = lv_entrance_user.
                <set>-approver_pernr = ls_pa0002-pernr.
                CONCATENATE ls_pa0002-nachn ls_pa0002-vorna ls_pa0002-midnm INTO <set>-approver_fio SEPARATED BY space.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      CATCH /iwbep/cx_mgw_busi_exception .
      CATCH /iwbep/cx_mgw_tech_exception .
    ENDTRY.
  ENDMETHOD.


  METHOD ekstypeset_get_entityset.
    DATA: actor_container TYPE TABLE OF swcont,
          actor_tab       TYPE TABLE OF swhactor.

    DATA: BEGIN OF ls_input
        , mainuser    TYPE string
        , entranceuser TYPE string
        , END OF ls_input
        , lr_user TYPE RANGE OF string.

    TRY.
        "Получение входных параметров
*21/03/2020

        READ TABLE it_filter_select_options ASSIGNING FIELD-SYMBOL(<filter>) WITH KEY property = entranceuser.
        IF sy-subrc <> 0.
          READ TABLE it_filter_select_options ASSIGNING <filter> WITH KEY property = mainuser.
        ENDIF.
        IF sy-subrc = 0.
          READ TABLE <filter>-select_options ASSIGNING FIELD-SYMBOL(<option>) INDEX 1.
          IF sy-subrc = 0.
            DATA(lv_entrance_user) = <option>-low.
          ENDIF.
        ENDIF.

        CHECK lv_entrance_user IS NOT INITIAL.

        SELECT * FROM zthr_ess_ekstype INTO TABLE @DATA(lt_ekstype).
        LOOP AT lt_ekstype ASSIGNING FIELD-SYMBOL(<ekstype>).
          APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<set>).
          <set>-entrance_user = lv_entrance_user.
          <set>-req_type = <ekstype>-code.
          <set>-req_name = <ekstype>-text.
        ENDLOOP.
      CATCH /iwbep/cx_mgw_busi_exception .
      CATCH /iwbep/cx_mgw_tech_exception .
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
