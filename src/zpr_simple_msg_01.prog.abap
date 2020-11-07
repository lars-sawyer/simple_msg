report zpr_simple_msg_01.

class lcl_msg definition inheriting from zcl_simple_msg.
endclass.


start-of-selection.

  break-point.

  data(ls_t100) = value t100( msgnr = '001' arbgb = '60'  ).
  data(ls_bapi) = value bapiret2( ).


  data(lt_bapi) = value bapirettab( ( id = 'ABC' number = '234' ) ).
  data(lt_bapi2) = value bapirettab( ( id = 'ABC' number = '234' ) ).
*  clear ls_t100.


*do 10000 times.

  lcl_msg=>map(
    exporting
      input  = ls_t100
    importing
      result = ls_bapi
  ).

  lcl_msg=>map(
  exporting
    input  = ls_t100
  importing
    result = lt_bapi
).
*enddo.

*  do 10000 times.
  lcl_msg=>map(
  exporting
    input  = lt_bapi
  importing
    result = lt_bapi2
).

*  enddo.


  data(lv_text) = lcl_msg=>get( ls_bapi )-txt.

  if lcl_msg=>get( value bapiret2( type = 'E' ) )-type = 'E'.
    data(lv_dummy) = 'test'.
  endif.

  if lcl_msg=>get( value bapiret2( type = 'S' ) )-type = 'E'.
*    data(lv_Dummy) = 'test'.
  endif.


*enddo.

*  data(lt_bapi2) = lcl_msg=>log_factory( )->log( ls_bapi )->log( ls_t100 )->log_get( )-t_msg.

*  if lcl_msg=>get( ls_bapi )-is_error = abap_true.

*  endif.

*  if lcl_msg=>log_factory( )->log( ls_bapi )->log( ls_t100 )->log_get( )-is_error = abap_true.

*  endif.




*  lcl_msg=>factory( )->msg->clear(  )->



  break-point.

*  select *
*    from spfli
*    where contains ( ( cityfrom, cityto), 'Tokio', fuzzy(0.8) )
*  into table @data(data)
*  .
*
*
*  data(ls_bapi) = value bapiret2(  ).
*  data(ls_balm) = value balm(  ).
*
*  lcl_msg=>mapper(
*    exporting
*      i_any = ls_bapi
*    importing
*      e_any = ls_balm
*  ).
