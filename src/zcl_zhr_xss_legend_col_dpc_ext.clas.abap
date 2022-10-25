class ZCL_ZHR_XSS_LEGEND_COL_DPC_EXT definition
  public
  inheriting from ZCL_ZHR_XSS_LEGEND_COL_DPC
  create public .

public section.
protected section.

  methods ENTITYMAINSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZHR_XSS_LEGEND_COL_DPC_EXT IMPLEMENTATION.


  METHOD entitymainset_get_entityset.
    DATA: lt_aw_color TYPE STANDARD TABLE OF zhrt_gp_aw_color.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_aw_color FROM zhrt_gp_aw_color.
    IF sy-subrc = 0.
      LOOP AT lt_aw_color ASSIGNING FIELD-SYMBOL(<ls_data>).
        APPEND INITIAL LINE TO  et_entityset ASSIGNING FIELD-SYMBOL(<ls_new>).
        MOVE-CORRESPONDING <ls_data> TO <ls_new>.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
