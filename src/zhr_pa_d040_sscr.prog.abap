*&---------------------------------------------------------------------*
*&  Include           ZHR_PA_D040_SSCR
*&---------------------------------------------------------------------*

DATA: gv_dats  TYPE dats.
DATA: gv_orgeh TYPE orgeh.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
SELECT-OPTIONS: p_orgeh FOR gv_orgeh NO INTERVALS NO-EXTENSION OBLIGATORY.
parameters: p_date type dats   OBLIGATORY.
parameters: p_xml  type flag  no-display.
SELECTION-SCREEN END OF BLOCK b1.
