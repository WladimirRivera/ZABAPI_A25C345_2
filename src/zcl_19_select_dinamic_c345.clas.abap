CLASS zcl_19_select_dinamic_c345 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_19_select_dinamic_c345 IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
*   Dynamic specification of font, columns and filters
    TYPES: BEGIN OF ty_product,
             first_name    TYPE zemployee_c345-first_name,
             salary_amount TYPE zemployee_c345-salary_amount,
           END OF ty_product.

    DATA: lv_datasource_name  TYPE string VALUE 'zemployee_c345',
          lv_selected_columns TYPE string VALUE 'first_name, salary_amount',
          lv_where_conditions TYPE string VALUE 'salary_amount ge 100',
          lt_products         TYPE STANDARD TABLE OF ty_product.

    TRY.
        SELECT FROM (lv_datasource_name)
                     FIELDS (lv_selected_columns)
                     WHERE (lv_where_conditions)
                     INTO CORRESPONDING FIELDS OF TABLE @lt_products.
        IF sy-subrc EQ 0.
          out->write( lt_products ).
        ENDIF.
      CATCH cx_sy_dynamic_osql_syntax cx_sy_dynamic_osql_semantics
          cx_sy_dynamic_osql_error INTO DATA(lx_dynamic_osql).
    ENDTRY.

    out->write( cl_abap_char_utilities=>newline ).

*    Dynamic programming for ABAP SQL
    DATA  lo_generic_data     TYPE REF TO data.
    FIELD-SYMBOLS <lt_itab> TYPE STANDARD TABLE.
    TRY.
        " Describe the structure of the table
        DATA(lo_comp_table) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( lv_datasource_name ) )->get_components( ).
        DATA(lo_struct_type) = cl_abap_structdescr=>create( lo_comp_table ).
        DATA(lo_table_type) = cl_abap_tabledescr=>create( lo_struct_type ).

        " Create dynamic table and assign to field-symbol
        CREATE DATA lo_generic_data TYPE HANDLE lo_table_type.
        ASSIGN lo_generic_data->* TO <lt_itab>.
        lv_selected_columns = '*'.
        SELECT FROM (lv_datasource_name)
                     FIELDS (lv_selected_columns)
                     WHERE (lv_where_conditions)
                     INTO TABLE @<lt_itab>.
        IF sy-subrc EQ 0.
          out->write( <lt_itab> ).
        ENDIF.
      CATCH cx_sy_dynamic_osql_syntax cx_sy_dynamic_osql_semantics
          cx_sy_dynamic_osql_error INTO lx_dynamic_osql.
        out->write( lx_dynamic_osql->get_text( ) ).
        RETURN.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
