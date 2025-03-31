CLASS zcl_17_select_filters_c345 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_17_select_filters_c345 IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
*Binary relational operators
    SELECT FROM zemployee_c345
           FIELDS *
           WHERE salary_amount GE 100
           INTO TABLE @DATA(lt_products).

    LOOP AT lt_products INTO DATA(ls_product).
      out->write( | { ls_product-first_name } = { ls_product-salary_amount }| ).
    ENDLOOP.
    out->write( cl_abap_char_utilities=>newline ).

*BETWEEN
    SELECT FROM zemployee_c345
           FIELDS *
           WHERE salary_amount BETWEEN 100 AND 1000
           INTO TABLE @lt_products.

    LOOP AT lt_products INTO ls_product.
      out->write( | { ls_product-first_name } = { ls_product-salary_amount }| ).
    ENDLOOP.

    out->write( cl_abap_char_utilities=>newline ).

*Wildcard characters with LIKE
    SELECT FROM zemployee_c345
       FIELDS *
       WHERE first_name LIKE '%S%'
       INTO TABLE @lt_products.

    LOOP AT lt_products INTO ls_product.
      out->write( | { ls_product-first_name } | ).
    ENDLOOP.
    out->write( cl_abap_char_utilities=>newline ).

**Escape characters
*    INSERT zemployee_c345 FROM @( VALUE #( employeeid = 11
*                                          first_name = 'Lenovos_1'
*                                          role = 2
*                                          adress-num = 1
*                                          salary_amount = '200.05' ) ).

    DATA lv_condition(1) TYPE c VALUE '$'.
    SELECT FROM zemployee_c345
       FIELDS *
       WHERE first_name LIKE '%$_%' ESCAPE @lv_condition
       INTO TABLE @lt_products.

    LOOP AT lt_products INTO ls_product.
      out->write( | { ls_product-first_name } | ).
    ENDLOOP.
    out->write( cl_abap_char_utilities=>newline ).

*IN
    SELECT FROM zemployee_c345
           FIELDS *
           WHERE role IN ( '2', '10' )
           INTO TABLE @lt_products.

    LOOP AT lt_products INTO ls_product.
      out->write( | { ls_product-first_name } : { ls_product-role }| ).
    ENDLOOP.
    out->write( cl_abap_char_utilities=>newline ).

*IN with range table
    DATA lr_category TYPE RANGE OF i.

    lr_category = VALUE #( ( sign = 'I'
                           option = 'EQ'
                           low = 2 )
                           ( sign = 'I'
                           option = 'EQ'
                           low = 10 ) ).

    SELECT FROM zemployee_c345
           FIELDS *
           WHERE role IN @lr_category
           INTO TABLE @lt_products.

    LOOP AT lt_products INTO ls_product.
      out->write( | { ls_product-first_name } : { ls_product-role }| ).
    ENDLOOP.
    out->write( cl_abap_char_utilities=>newline ).

*NULL
    SELECT FROM zemployee_c345
           FIELDS *
           WHERE first_name IS NOT NULL
           INTO TABLE @lt_products.

    LOOP AT lt_products INTO ls_product.
      out->write( | { ls_product-first_name } | ).
    ENDLOOP.
    out->write( cl_abap_char_utilities=>newline ).

*AND/OR/NOT
    SELECT FROM zemployee_c345
           FIELDS *
           WHERE ( role eq 2 or
                   role eq 10 ) AND
                  salary_amount ge 100
           INTO TABLE @lt_products.

    LOOP AT lt_products INTO ls_product.
      out->write( | { ls_product-first_name } : { ls_product-role } = { ls_product-salary_amount } | ).
    ENDLOOP.
    out->write( cl_abap_char_utilities=>newline ).
  ENDMETHOD.
ENDCLASS.
