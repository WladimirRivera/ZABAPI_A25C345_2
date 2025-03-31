CLASS zcl_18_select_expressions_c345 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_18_select_expressions_c345 IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
*   MIN / MAX
    SELECT FROM zemployee_c345
           FIELDS MIN( salary_amount ) AS minsalary_amount,
                  MAX( salary_amount ) AS maxsalary_amount
           INTO @DATA(ls_salary_amounts).
    IF sy-subrc EQ 0.
      out->write( | The Minimum salary_amount is: { ls_salary_amounts-minsalary_amount } | ).
      out->write( | The Maximun salary_amount is: { ls_salary_amounts-maxsalary_amount } | ).
    ENDIF.
    out->write( cl_abap_char_utilities=>newline ).

*   AVG / SUM
    SELECT FROM zemployee_c345
           FIELDS AVG( salary_amount ) AS avgsalary_amount,
                  SUM( salary_amount ) AS sumsalary_amount
           INTO @DATA(ls_totals).
    IF sy-subrc EQ 0.
      out->write( | The Minimum salary_amount is: { ls_totals-avgsalary_amount } | ).
      out->write( | The Maximun salary_amount is: { ls_totals-sumsalary_amount } | ).
    ENDIF.
    out->write( cl_abap_char_utilities=>newline ).

*   DISTINCT
    SELECT FROM zemployee_c345
           FIELDS MIN( DISTINCT salary_amount ) AS minsalary_amount,
                  MAX( DISTINCT salary_amount ) AS maxsalary_amount,
                  COUNT( DISTINCT salary_amount ) AS ctsalary_amount
           INTO @DATA(ls_dis_salary_amounts).
    IF sy-subrc EQ 0.
      out->write( | The Distinct Minimum salary_amount is: { ls_dis_salary_amounts-minsalary_amount } | ).
      out->write( | The Distinct Maximun salary_amount is: { ls_dis_salary_amounts-maxsalary_amount } | ).
      out->write( | The Distinct Count salary_amount is:   { ls_dis_salary_amounts-ctsalary_amount } | ).
    ENDIF.
    out->write( cl_abap_char_utilities=>newline ).

*   COUNT
    SELECT FROM zemployee_c345
           FIELDS COUNT( first_name ) AS productCount
           INTO ( @DATA(lv_cont) ).
    IF sy-subrc EQ 0.
      out->write( | The Product Count is: { lv_cont } | ).
    ENDIF.
    out->write( cl_abap_char_utilities=>newline ).

*   GROUP BY / HAVING
    SELECT FROM zemployee_c345
           FIELDS role,
                  AVG( salary_amount ) AS avgsalary_amount
           WHERE salary_amount GE 100
           GROUP BY role HAVING role IN ( 2 )
           INTO TABLE @DATA(lt_group).
    IF sy-subrc EQ 0.
      LOOP AT lt_group INTO DATA(ls_group).
        out->write( | Category: { ls_group-role } | ).
        out->write( | Average salary_amount is: { ls_group-avgsalary_amount } | ).
      ENDLOOP.
    ENDIF.
    out->write( cl_abap_char_utilities=>newline ).

*   ORDER BY / OFFSET
*    SELECT FROM zemployee_c345
*           FIELDS first_name
*           ORDER BY salary_amount DESCENDING
*           INTO TABLE @DATA(lt_products)
*           OFFSET 2.
*    IF sy-subrc EQ 0.
*      LOOP AT lt_products INTO DATA(ls_products).
*        out->write( | Product: { ls_products-first_name } | ).
*      ENDLOOP.
*    ENDIF.
********* Order of Execution *******
* 1. FROM
* 2. WHERE
* 3. GROUP BY
* 4. HAVING
* 5. ORDER BY
* 6. OFFSET
* 7. UP TO (LIMIT)

    DATA: lv_page_number      TYPE i VALUE 2,
          lv_records_per_page TYPE i VALUE 5.

    DATA gv_offset TYPE int8.

* Page 1 = Block 0
* Page 2 = Block 1

    gv_offset = ( lv_page_number - 1 ) * lv_records_per_page.

    SELECT FROM zemployee_c345
           FIELDS first_name
           ORDER BY salary_amount DESCENDING
           INTO TABLE @DATA(lt_results)
           OFFSET @gv_offset
           UP TO @lv_records_per_page ROWS.

    IF sy-subrc EQ 0.
      out->write( name = |Page:{ lv_page_number }| data = lt_results ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
