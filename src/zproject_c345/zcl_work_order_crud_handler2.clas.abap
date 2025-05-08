CLASS zcl_work_order_crud_handler2 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS create_work_order IMPORTING iv_customer_id       TYPE zde_customer_id_437
                                        iv_technician_id     TYPE zde_technician_id_437
                                        iv_priority          TYPE zde_priority_437
                                        iv_description       TYPE zdt_work_ord4372-description
                              RETURNING VALUE(rv_sql_result) TYPE string.

    METHODS read_work_order IMPORTING iv_work_order_id     TYPE zde_work_order_id_437
                            RETURNING VALUE(rs_work_order) TYPE zdt_work_ord4372.

    METHODS read_work_orders RETURNING VALUE(rt_work_order) TYPE zdt_work_ord4372.

    METHODS update_work_order IMPORTING iv_work_order_id     TYPE zde_work_order_id_437
                                        iv_status            TYPE zde_status_437
                              RETURNING VALUE(rv_sql_result) TYPE string.

    METHODS delete_work_order IMPORTING iv_work_order_id     TYPE zde_work_order_id_437
                              RETURNING VALUE(rv_sql_result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: BEGIN OF mc_valid_status,
                 Pending   TYPE zde_status_437 VALUE 'PE',
                 Completed TYPE zde_status_437 VALUE 'CO',
               END OF mc_valid_status,

               BEGIN OF mc_valid_priority,
                 High TYPE zde_priority_437 VALUE 'A',
                 Low  TYPE zde_priority_437 VALUE 'B',
               END OF mc_valid_priority.
ENDCLASS.

CLASS zcl_work_order_crud_handler2 IMPLEMENTATION.
  METHOD create_work_order.
    DATA(lr_work_order_validator) = NEW zcl_work_order_validator2( ).

* Validate CustomerID, TechnicianID and priority shared
    lr_work_order_validator->validate_create_order(
      EXPORTING
        iv_customer_id   = iv_customer_id
        iv_technician_id = iv_technician_id
        iv_priority      = iv_priority
    IMPORTING
        ev_error =  rv_sql_result
   RECEIVING
     rv_valid         = DATA(lv_valid)
    ).

* Insert new Work Order
    IF lv_valid EQ abap_true.
* Get last work order id
      SELECT SINGLE FROM zdt_work_ord4372
          FIELDS MAX( work_order_id )
          WHERE work_order_id IS NOT INITIAL
          INTO @DATA(lv_work_order_id).

      INSERT zdt_work_ord4372 FROM @( VALUE #( work_order_id = lv_work_order_id + 1
                                               customer_id = iv_customer_id
                                               technician_id = iv_customer_id
                                               creation_date = cl_abap_context_info=>get_system_date( )
                                               status = mc_valid_status-pending
                                               priority = iv_priority
                                               description = iv_description ) ).
      IF sy-subrc EQ 0.
        rv_sql_result = |Work order: { lv_work_order_id + 1 } was created|.
      ELSE.
        rv_sql_result = 'Create work order id'.
      ENDIF.

    ENDIF.
  ENDMETHOD.

  METHOD read_work_order.
*   Select Single work order record
    SELECT SINGLE FROM zdt_work_ord4372
           FIELDS *
           WHERE work_order_id EQ @iv_work_order_id
           INTO @rs_work_order.
  ENDMETHOD.

  METHOD read_work_orders.
*   Select Work orders
*    SELECT FROM zdt_work_ord4372
    SELECT SINGLE FROM zdt_work_ord4372
           FIELDS *
           WHERE work_order_id IS NOT INITIAL
*           INTO TABLE  @rt_work_order.
           INTO @rt_work_order.
  ENDMETHOD.

  METHOD update_work_order.
    DATA(lr_work_order_validator) = NEW zcl_work_order_validator2( ).

* Validate CustomerID, TechnicianID and priority shared
    lr_work_order_validator->validate_update_order(
      EXPORTING
        iv_work_order_id = iv_work_order_id
        iv_status        = iv_status
      IMPORTING
        ev_error         = rv_sql_result
      RECEIVING
        rv_valid         =  DATA(lv_valid)
    ).

* Updating a Work Order
    IF lv_valid EQ abap_true.
      UPDATE zdt_work_ord4372 SET status = @iv_status
                              WHERE work_order_id = @iv_work_order_id.
      IF sy-subrc EQ 0.
        rv_sql_result = |Work order: { iv_work_order_id } was updated|.

* Get last History ID
        SELECT SINGLE FROM zdt_work_o_h4372
            FIELDS MAX( history_id )
            WHERE history_id IS NOT INITIAL
            INTO @DATA(lv_history_id).

* Add Work Order History
        INSERT zdt_work_o_h4372 FROM @( VALUE #( history_id = lv_history_id + 1
                                                 work_order_id = iv_work_order_id
                                                 modification_date = cl_abap_context_info=>get_system_date( )
                                                 change_description = 'Modify status' ) ).
      ELSE.
        rv_sql_result = 'Update Work Order Error'.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD delete_work_order.
    DATA(lr_work_order_validator) = NEW zcl_work_order_validator2( ).
* Get status from work order
    SELECT SINGLE FROM zdt_work_ord4372
           FIELDS status
           WHERE work_order_id EQ @iv_work_order_id
           INTO @DATA(ls_status).

* Validate CustomerID, TechnicianID and priority shared
    lr_work_order_validator->validate_delete_order(
      EXPORTING
        iv_work_order_id = iv_work_order_id
        iv_status        = ls_status
      IMPORTING
        ev_error         = rv_sql_result
      RECEIVING
        rv_valid         =  DATA(lv_valid)
    ).

* Delete a Work order
    IF lv_valid EQ abap_true.
      DELETE FROM zdt_work_ord4372 WHERE work_order_id = @iv_work_order_id.
      IF sy-subrc EQ 0.
        rv_sql_result = |{ sy-dbcnt } records were deleted|.
        rv_sql_result = |Work order: { iv_work_order_id } was deleted|.

*   Delete Work Order History
        DELETE FROM zdt_work_o_h4372 WHERE work_order_id = @iv_work_order_id.
      ELSE.
        rv_sql_result = 'Delete Work Order Error'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
