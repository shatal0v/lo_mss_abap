*&---------------------------------------------------------------------*
*&  Include           ZHR_PA_D028_TOP
*&---------------------------------------------------------------------*
REPORT ZHR_PA_D040.

TABLES: pernr .
TYPES: BEGIN OF ts_data,
  pernr   TYPE pernr_d,
  komitet TYPE orgeh,
  begda   TYPE begda,
  endda   TYPE endda,
  chara	  TYPE chara,
  delete  TYPE flag.      " flag of delete

  INCLUDE TYPE PS9001.

TYPES: END OF ts_data.
 DATA:  mt_data TYPE STANDARD TABLE OF ts_data.
 DATA:  ms_data TYPE ts_data.

 CLASS lcl DEFINITION DEFERRED.

      .
