class ZCL_ZHR_ZAMESTITEL_SAV_DPC_EXT definition
  public
  inheriting from ZCL_ZHR_ZAMESTITEL_SAV_DPC
  create public .

public section.
protected section.

  methods ENTR_USER_CREATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZHR_ZAMESTITEL_SAV_DPC_EXT IMPLEMENTATION.


  METHOD entr_user_create_entity.

    DATA: ls_user_row TYPE zcl_zhr_zamestitel_del_mpc=>ts_entr_user.

    TRY.

        io_data_provider->read_entry_data( IMPORTING es_data = ls_user_row ).

        DATA(lv_main_user) = ls_user_row-entrance_user.

        ls_user_row-entrance_user = ls_user_row-main_user.
        ls_user_row-main_user = lv_main_user.

        CALL FUNCTION 'Z_HR_ENTR_USER_UPD' IN UPDATE TASK
          EXPORTING
            is_user_row = ls_user_row
          EXCEPTIONS
            no_block    = 1
            OTHERS      = 2.

        IF sy-subrc = 0.
          COMMIT WORK.

          MOVE-CORRESPONDING ls_user_row TO er_entity.

        ENDIF.

      CATCH /iwbep/cx_mgw_busi_exception .
      CATCH /iwbep/cx_mgw_tech_exception .
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
