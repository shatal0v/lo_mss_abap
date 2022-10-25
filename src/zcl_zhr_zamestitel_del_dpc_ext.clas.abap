class ZCL_ZHR_ZAMESTITEL_DEL_DPC_EXT definition
  public
  inheriting from ZCL_ZHR_ZAMESTITEL_DEL_DPC
  create public .

public section.
protected section.

  methods ENTR_USERSET_GET_ENTITY
    redefinition .
  methods ENTR_USERSET_GET_ENTITYSET
    redefinition .
  methods ENTR_USERSET_UPDATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZHR_ZAMESTITEL_DEL_DPC_EXT IMPLEMENTATION.


  METHOD entr_userset_get_entity.
    TRY.
        DATA: ls_key_tab       TYPE /iwbep/s_mgw_name_value_pair,
              lv_main_user     TYPE zehr_mss_main_user,
              lv_entrance_user TYPE zehr_mss_entrance_user,
              lv_begda         TYPE zehr_mss_begda,
              lv_endda         TYPE zehr_mss_endda,
              ls_entr_user     TYPE zthr_entr_user.

* Get the key property values
        READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'MAIN_USER' .
        IF sy-subrc = 0.
          lv_main_user = ls_key_tab-value.
        ENDIF.

* Get the key property values
        READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'ENTRANCE_USER' .
        IF sy-subrc = 0.
          lv_entrance_user = ls_key_tab-value.
        ENDIF.

        READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'BEGDA' .
        IF sy-subrc = 0.
          lv_begda = ls_key_tab-value.
        ENDIF.

        READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'ENDDA' .
        IF sy-subrc = 0.
          lv_endda = ls_key_tab-value.
        ENDIF.



        SELECT SINGLE * INTO CORRESPONDING FIELDS OF ls_entr_user
          FROM zthr_entr_user
        WHERE main_user = lv_entrance_user
          AND entrance_user = lv_main_user
          AND begda         = lv_begda
          AND endda         = lv_endda.
        IF sy-subrc = 0.
          er_entity-main_user     = ls_entr_user-main_user.
          er_entity-entrance_user = ls_entr_user-entrance_user.
          er_entity-begda         = ls_entr_user-begda.
          er_entity-endda         = ls_entr_user-endda.
        ELSE.
          RAISE EXCEPTION TYPE  /iwbep/cx_mgw_busi_exception.
        ENDIF.

      CATCH /iwbep/cx_mgw_busi_exception .
      CATCH /iwbep/cx_mgw_tech_exception .
    ENDTRY.
  ENDMETHOD.


  METHOD entr_userset_get_entityset.
    TRY.
        DATA: ls_key_tab       TYPE /iwbep/s_mgw_name_value_pair,
              lv_main_user     TYPE zehr_mss_main_user,
              lv_entrance_user TYPE zehr_mss_entrance_user,
              lv_begda         TYPE zehr_mss_begda,
              lv_endda         TYPE zehr_mss_endda,
              lt_entr_user     TYPE STANDARD TABLE OF zthr_entr_user.


* Get the key property values
        READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'MAIN_USER' .
        IF sy-subrc = 0.
          lv_main_user = ls_key_tab-value.
        ENDIF.

* Get the key property values
        READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'ENTRANCE_USER' .
        IF sy-subrc = 0.
          lv_entrance_user = ls_key_tab-value.
        ENDIF.

        READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'BEGDA' .
        IF sy-subrc = 0.
          lv_begda = ls_key_tab-value.
        ENDIF.

        READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'ENDDA' .
        IF sy-subrc = 0.
          lv_begda = ls_key_tab-value.
        ENDIF.

        SELECT  * INTO CORRESPONDING FIELDS OF TABLE lt_entr_user
          FROM  zthr_entr_user
          WHERE main_user     = lv_main_user
            AND entrance_user = lv_entrance_user
            AND begda         = lv_begda
            AND endda         = lv_endda.

        IF sy-subrc = 0.

          LOOP AT lt_entr_user ASSIGNING FIELD-SYMBOL(<ls_entr_user>).
            APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<ls_new>).
            MOVE-CORRESPONDING <ls_entr_user> TO <ls_new>.

*          <ls_new>-main_user     = ls_entr_user-main_user.
*          <ls_new>-entrance_user = ls_entr_user-entrance_user.
*          <ls_new>-num           = ls_entr_user-num.
          ENDLOOP.
        ENDIF.

      CATCH /iwbep/cx_mgw_busi_exception .
      CATCH /iwbep/cx_mgw_tech_exception .
    ENDTRY.
  ENDMETHOD.


  method ENTR_USERSET_UPDATE_ENTITY.
    DATA: ls_user_row TYPE zcl_zhr_zamestitel_del_mpc=>ts_entr_user.

    TRY.

        io_data_provider->read_entry_data( IMPORTING es_data = ls_user_row ).


        CALL FUNCTION 'Z_HR_ENTR_USER_DEL' IN UPDATE TASK
          EXPORTING
            is_user_row = ls_user_row
          EXCEPTIONS
            no_block    = 1
            OTHERS      = 2.

        IF sy-subrc = 0.
          commit work.
        ENDIF.

      CATCH /iwbep/cx_mgw_busi_exception .
      CATCH /iwbep/cx_mgw_tech_exception .
    ENDTRY.
  endmethod.
ENDCLASS.
