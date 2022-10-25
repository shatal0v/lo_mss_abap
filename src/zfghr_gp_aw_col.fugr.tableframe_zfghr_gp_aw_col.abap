*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFGHR_GP_AW_COL
*   generation date: 18.12.2019 at 12:55:38
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFGHR_GP_AW_COL    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
