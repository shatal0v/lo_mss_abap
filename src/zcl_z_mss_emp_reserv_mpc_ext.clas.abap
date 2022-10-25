class ZCL_Z_MSS_EMP_RESERV_MPC_EXT definition
  public
  inheriting from ZCL_Z_MSS_EMP_RESERV_MPC
  create public .

public section.

  methods DEFINE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_Z_MSS_EMP_RESERV_MPC_EXT IMPLEMENTATION.


  METHOD define.

    super->define( ).

   data: lo_entity type ref to /iwbep/if_mgw_odata_entity_typ,

    lo_property type ref to /iwbep/if_mgw_odata_property.

    lo_entity = model->get_entity_type( iv_entity_name = 'Emp_Reserv' ).

    IF lo_entity IS BOUND.

      lo_property = lo_entity->get_property( iv_property_name = 'MAIN_USER' ).

      lo_property = lo_entity->get_property( iv_property_name = 'ENTRANCE_USER' ).

      lo_property = lo_entity->get_property( iv_property_name = 'CONTENT' ).

      lo_property = lo_entity->get_property( iv_property_name = 'MsgId' ).

      lo_property = lo_entity->get_property( iv_property_name = 'Message' ).

      lo_property->set_as_content_type( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
