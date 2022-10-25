class ZCL_ZHR_READ_STRUCTURE_DPC_EXT definition
  public
  inheriting from ZCL_ZHR_READ_STRUCTURE_DPC
  create public .

public section.
protected section.

  methods SUBPERNRS_GET_ENTITYSET
    redefinition .
private section.

  constants MC_0105_INFTY type INFTY value '0105' ##NO_TEXT.
  constants MC_9001_SUBTY type SUBTY value '9001' ##NO_TEXT.
  constants MC_MAINUSER type CHAR10 value 'MAIN_USER' ##NO_TEXT.
  class-data MV_MAIN_USER type P0105-USRID .
ENDCLASS.



CLASS ZCL_ZHR_READ_STRUCTURE_DPC_EXT IMPLEMENTATION.


  METHOD subpernrs_get_entityset.
    DATA: BEGIN OF ls_input
        , main_user     TYPE string
        , entrance_user TYPE string
        , xss           TYPE string
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

          REFRESH lr_user.
        ENDLOOP.

        DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

        IF ls_input-entrance_user IS NOT INITIAL.

          lo_assistent->get_zamestitel_list( EXPORTING iv_main_user  = CONV #( ls_input-entrance_user )
                                             IMPORTING et_pernrs_tab = et_entityset ).
        ELSE.
          lo_assistent->get_zamestitel_list( EXPORTING iv_main_user  = CONV #( ls_input-main_user )
                                           IMPORTING et_pernrs_tab = et_entityset ).
        ENDIF.


      CATCH /iwbep/cx_mgw_tech_exception .
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
