CLASS zcl_20_select_multiquery_c345 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .
    INTERFACES if_amdp_marker_hdb .

    TYPES: BEGIN OF ty_structure,
             carrier_id    TYPE /dmo/carrier_id,
             plane_type_id TYPE /dmo/plane_type_id,
             name          TYPE /dmo/carrier_name,
           END OF ty_structure,
           ty_table TYPE TABLE OF ty_structure.

    CLASS-METHODS full_join AMDP OPTIONS CDS SESSION CLIENT DEPENDENT
      EXPORTING VALUE(et_results) TYPE ty_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_20_select_multiquery_c345 IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
*AS – Alternative Name
    SELECT FROM /dmo/carrier
           FIELDS carrier_id AS carrier_code_user,
                  name AS carrier_name_user
           INTO TABLE @DATA(lt_carriers).
    IF sy-subrc EQ 0.
      LOOP AT lt_carriers INTO DATA(ls_carriers).
        out->write( |{ ls_carriers-carrier_code_user } = { ls_carriers-carrier_name_user }| ).
      ENDLOOP.
    ENDIF.
    out->write( cl_abap_char_utilities=>newline ).
    out->write( cl_abap_char_utilities=>newline ).

*Subquery
    SELECT FROM /dmo/flight
           FIELDS plane_type_id
           WHERE price GT ( SELECT FROM /dmo/flight FIELDS MIN( price ) )
           INTO TABLE @DATA(lt_flights).
    IF sy-subrc EQ 0.
      out->write( lt_flights ).
    ENDIF.
    out->write( cl_abap_char_utilities=>newline ).

*Subquery ALL
    SELECT FROM /dmo/flight
           FIELDS connection_id
           WHERE seats_occupied GT ALL ( SELECT FROM /dmo/flight
                                            FIELDS seats_occupied
                                              WHERE currency_code EQ 'EUR' )
           INTO TABLE @DATA(lt_airports).

    IF sy-subrc EQ 0.
      out->write( |{ sy-dbcnt } records were consulted| ).
    ENDIF.
    out->write( cl_abap_char_utilities=>newline ).
    out->write( cl_abap_char_utilities=>newline ).

*Subquery ANY/SOME
    SELECT FROM /dmo/flight
           FIELDS connection_id
           WHERE seats_max GT ANY ( SELECT FROM /dmo/flight
                                    FIELDS seats_max
                                    WHERE carrier_id EQ 'AA' )
           INTO TABLE @lt_flights.

    IF sy-subrc EQ 0.
      out->write( name = 'ANY' data = lt_flights ).
    ENDIF.
    out->write( cl_abap_char_utilities=>newline ).

*Subquery EXISTS
    SELECT FROM /dmo/carrier AS a
           FIELDS carrier_id
           WHERE EXISTS ( SELECT FROM /dmo/flight AS f
                            FIELDS carrier_id
                          WHERE f~carrier_id EQ a~carrier_id )
    INTO TABLE @DATA(lt_carrier).

    IF sy-subrc EQ 0.
      out->write( lt_carrier ).
    ENDIF.
    out->write( cl_abap_char_utilities=>newline ).

*Subquery IN
    SELECT FROM /dmo/flight
           FIELDS connection_id
           WHERE carrier_id IN ('AA', 'AZ')
           INTO TABLE @DATA(lt_connection).

    IF sy-subrc EQ 0.
      out->write( lt_connection ).
    ENDIF.
    out->write( cl_abap_char_utilities=>newline ).

*INNER JOIN
    SELECT FROM /dmo/flight AS f
           INNER JOIN /dmo/carrier AS a
           ON f~carrier_id EQ a~carrier_id
           FIELDS f~connection_id,
                  f~flight_date,
                  a~name AS airport_name
           INTO TABLE @DATA(lt_flight_details).
    IF sy-subrc EQ 0.
      out->write( lt_flight_details ).
    ENDIF.
    out->write( cl_abap_char_utilities=>newline ).

*LEFT [OUTER] JOIN
    SELECT FROM /dmo/flight AS f
           LEFT JOIN /dmo/carrier AS a
           ON f~carrier_id EQ a~carrier_id
           FIELDS f~connection_id,
                  f~flight_date,
                  a~name AS airport_name
           INTO TABLE @lt_flight_details.
    IF sy-subrc EQ 0.
      out->write( lt_flight_details ).
    ENDIF.
    out->write( cl_abap_char_utilities=>newline ).

*RIGHT [OUTER] JOIN
    SELECT FROM /dmo/flight AS f
           RIGHT JOIN /dmo/carrier AS a
           ON f~carrier_id EQ a~carrier_id
           FIELDS f~connection_id,
                  f~flight_date,
                  a~name AS airport_name
           INTO TABLE @lt_flight_details.
    IF sy-subrc EQ 0.
      out->write( lt_flight_details ).
    ENDIF.
    out->write( cl_abap_char_utilities=>newline ).

*LEFT | RIGHT EXCLUDING INNER JOIN
    SELECT FROM /dmo/flight
           FIELDS connection_id,
                  flight_date
           WHERE carrier_id NOT IN ( SELECT FROM /dmo/carrier
                                     FIELDS carrier_id )
           INTO TABLE @lt_flight_details.
    IF sy-subrc EQ 0.
      out->write( lt_flight_details ).
    ENDIF.
    out->write( cl_abap_char_utilities=>newline ).

*CROSS JOIN
    SELECT FROM /dmo/flight AS f
           CROSS JOIN /dmo/carrier AS a
           FIELDS f~connection_id,
                  f~flight_date,
                  a~name AS airport_name
           INTO TABLE @lt_flight_details.
    IF sy-subrc EQ 0.
      out->write( |{ sy-dbcnt } records were consulted| ).
    ENDIF.
    out->write( cl_abap_char_utilities=>newline ).

*FULL [OUTER] JOIN
    DATA lt_full_results TYPE ty_table.
    zcl_20_select_multiquery_c345=>full_join( IMPORTING et_results = lt_full_results ).
    IF sy-subrc EQ 0.
      out->write( lt_full_results ).
    ENDIF.
  ENDMETHOD.

  METHOD full_join BY DATABASE PROCEDURE FOR HDB
                   LANGUAGE SQLSCRIPT
                   OPTIONS READ-ONLY
                   USING /dmo/flight
                         /dmo/carrier.
*    et_results = select coalesce( db1.carrier_id, db1.plane_type_id ) as a, db2.name
    et_results = select  db1.carrier_id, db1.plane_type_id, db2.name
                    FROM "/DMO/FLIGHT" as db1
                    FULL OUTER JOIN "/DMO/CARRIER" as db2 on db1.carrier_id = db2.carrier_id
                    WHERE db1.carrier_id is not null
                    ORDER BY db1.carrier_id;
  ENDMETHOD.
ENDCLASS.
