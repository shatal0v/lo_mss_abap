class ZCL_ZHR_ZAMESTITEL_REA_DPC_EXT definition
  public
  inheriting from ZCL_ZHR_ZAMESTITEL_REA_DPC
  create public .

public section.
protected section.

  methods ENTR_USER_GET_ENTITYSET
    redefinition .
private section.

  constants C_INFTY_0002 type INFTY value '0002' ##NO_TEXT.

  methods ADD_FIO
    changing
      !CS_ENTITYSET type ZCL_ZHR_ZAMESTITEL_REA_MPC=>TS_ENTR_USER .
ENDCLASS.



CLASS ZCL_ZHR_ZAMESTITEL_REA_DPC_EXT IMPLEMENTATION.


  METHOD add_fio.
    DATA: lt_p0002 TYPE STANDARD TABLE OF p0002.

    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

    DATA(lv_pernr) = lo_assistent->get_pernr( iv_usrid = CONV #( cs_entityset-entrance_user ) ).

    zcl_hr_data=>read_pa_infty( EXPORTING i_pernr = lv_pernr
                                          i_infty = c_infty_0002
                                          i_begda = sy-datum
                                          i_endda = sy-datum
                                IMPORTING e_pnnnn = lt_p0002 ).

    READ TABLE lt_p0002 ASSIGNING FIELD-SYMBOL(<ls_p0002>) INDEX 1.
    IF sy-subrc = 0.
         cs_entityset-nachn = <ls_p0002>-nachn.
         cs_entityset-vorna = <ls_p0002>-vorna.
         cs_entityset-midnm = <ls_p0002>-midnm.
    ENDIF.
  ENDMETHOD.


  METHOD entr_user_get_entityset.

    DATA: BEGIN OF ls_input
        , main_user     TYPE string
        , entrance_user TYPE string
        , xss           TYPE string
        , END OF ls_input.

    DATA: lt_user TYPE TABLE OF zthr_entr_user
        , lr_user TYPE RANGE OF string
        , lv_main_user TYPE zehr_mss_main_user
        .

    FIELD-SYMBOLS:
      <ls_user>      LIKE LINE OF lt_user,
      <ls_entityset> LIKE LINE OF et_entityset.

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

          REFRESH lr_user.
        ENDLOOP.

        IF ls_input-entrance_user IS NOT INITIAL.
          lv_main_user = ls_input-entrance_user.
        ELSE.
          lv_main_user = ls_input-main_user.
        ENDIF.

        IF lv_main_user IS NOT INITIAL.
          SELECT * INTO TABLE lt_user FROM zthr_entr_user
            WHERE entrance_user = lv_main_user
              AND endda >= sy-datum
              AND del = ''.

          IF sy-subrc = 0.

            LOOP AT  lt_user ASSIGNING <ls_user> WHERE begda IS NOT INITIAL.

              APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.

              lv_main_user = <ls_user>-entrance_user.
              <ls_user>-entrance_user = <ls_user>-main_user.
              <ls_user>-main_user = lv_main_user.

              MOVE-CORRESPONDING <ls_user> TO <ls_entityset>.
              add_fio( CHANGING cs_entityset = <ls_entityset> ).

            ENDLOOP.
          ELSE.
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception.
          ENDIF.
        ELSE.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception.
        ENDIF.
      CATCH /iwbep/cx_mgw_busi_exception .
      CATCH /iwbep/cx_mgw_tech_exception .
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
