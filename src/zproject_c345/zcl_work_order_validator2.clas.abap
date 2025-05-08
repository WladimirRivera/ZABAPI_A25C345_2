CLASS zcl_work_order_validator2 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS validate_create_order IMPORTING iv_customer_id   TYPE zde_customer_id_437
                                            iv_technician_id TYPE zde_technician_id_437
                                            iv_priority      TYPE zde_priority_437
                                  EXPORTING ev_error         TYPE string
                                  RETURNING VALUE(rv_valid)  TYPE abap_bool.

    METHODS validate_update_order IMPORTING iv_work_order_id TYPE zde_work_order_id_437
                                            iv_status        TYPE zde_status_437
                                  EXPORTING ev_error         TYPE string
                                  RETURNING VALUE(rv_valid)  TYPE abap_bool.

    METHODS validate_delete_order IMPORTING iv_work_order_id TYPE zde_work_order_id_437
                                            iv_status        TYPE zde_status_437
                                  EXPORTING ev_error         TYPE string
                                  RETURNING VALUE(rv_valid)  TYPE abap_bool.

    METHODS validate_status_and_priority IMPORTING iv_status       TYPE zde_status_437
                                                   iv_priority     TYPE zde_priority_437
                                         RETURNING VALUE(rv_valid) TYPE abap_bool.


    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS check_customer_exists IMPORTING iv_customer_id   TYPE zde_customer_id_437
                                  RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_technician_exists IMPORTING iv_technician_id TYPE zde_technician_id_437
                                    RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_order_exists IMPORTING iv_work_order_id TYPE zde_work_order_id_437
                               RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_order_history IMPORTING iv_work_order_id TYPE zde_work_order_id_437
                                RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_status             IMPORTING iv_status       TYPE zde_status_437
                                     RETURNING VALUE(rv_valid) TYPE abap_bool.

    METHODS check_priority IMPORTING iv_priority     TYPE zde_priority_437
                           RETURNING VALUE(rv_valid) TYPE abap_bool.

    CONSTANTS: BEGIN OF mc_valid_status,
                 Pending   TYPE zde_status_437 VALUE 'PE',
                 Completed TYPE zde_status_437 VALUE 'CO',
               END OF mc_valid_status,

               BEGIN OF mc_valid_priority,
                 High TYPE zde_priority_437 VALUE 'A',
                 Low  TYPE zde_priority_437 VALUE 'B',
               END OF mc_valid_priority.

    DATA: mt_valid_status   TYPE RANGE OF zde_status_437,
          mt_valid_priority TYPE RANGE OF zde_priority_437.
ENDCLASS.

CLASS zcl_work_order_validator2 IMPLEMENTATION.
  METHOD check_customer_exists.
    SELECT SINGLE FROM zdt_customer4372
    FIELDS customer_id
    WHERE customer_id EQ @iv_customer_id
    INTO @DATA(lv_customer_id).

    IF sy-subrc EQ 0.
      rv_exists = abap_true.
    ELSE.
      rv_exists = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD check_order_exists.
    SELECT SINGLE FROM zdt_work_ord4372
    FIELDS work_order_id
    WHERE work_order_id EQ @iv_work_order_id
    INTO @DATA(lv_work_order_id).

    IF sy-subrc EQ 0.
      rv_exists = abap_true.
    ELSE.
      rv_exists = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD check_order_history.
    SELECT SINGLE FROM zdt_work_o_h4372
    FIELDS work_order_id
    WHERE work_order_id EQ @iv_work_order_id
    INTO @DATA(lv_work_order_id).

    IF sy-subrc EQ 0.
      rv_exists = abap_true.
    ELSE.
      rv_exists = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD check_priority.
* Validate the priority value
    IF iv_priority NOT IN mt_valid_priority.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.


  METHOD check_status.
* Validate the status value
    IF iv_status NOT IN mt_valid_status.
      rv_valid = abap_false.
      RETURN.
    ENDIF.
    rv_valid = abap_true.
  ENDMETHOD.


  METHOD validate_status_and_priority.
* Validate the status value
    IF iv_status NOT IN mt_valid_status.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

* Validate the priority value
    IF iv_priority NOT IN mt_valid_priority.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.


  METHOD check_technician_exists.
    SELECT SINGLE FROM zdt_technici4372
    FIELDS technician_id
    WHERE technician_id EQ @iv_technician_id
    INTO @DATA(lv_technician_id).

    IF sy-subrc EQ 0.
      rv_exists = abap_true.
    ELSE.
      rv_exists = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
* Initialize range values of valid status and priority
    mt_valid_status = VALUE #( ( sign = 'I'
                                 option = 'EQ'
                                 low = mc_valid_status-completed )
                               ( sign = 'I'
                                 option = 'EQ'
                                 low = mc_valid_status-pending ) ).

    mt_valid_priority = VALUE #( ( sign = 'I'
                                        option = 'EQ'
                                        low = mc_valid_priority-high )
                                      ( sign = 'I'
                                        option = 'EQ'
                                        low = mc_valid_priority-low ) ).
  ENDMETHOD.


  METHOD validate_create_order.
* Check if customer exists
    DATA(lv_customer_exists) = check_customer_exists( iv_customer_id ).
    IF lv_customer_exists IS INITIAL.
      rv_valid = abap_false.
      ev_error = |Customer ID: { iv_customer_id } does not exist|.
      RETURN.
    ENDIF.

* Check if technician exists
    DATA(lv_technician_exists) = check_technician_exists( iv_technician_id ).
    IF lv_technician_exists IS INITIAL.
      rv_valid = abap_false.
      ev_error = |Technician ID: { iv_technician_id } does not exist|.
      RETURN.
    ENDIF.

* Check if priority is valid
    DATA(lv_priority) = check_priority( iv_priority ).
    IF lv_priority IS INITIAL.
      rv_valid = abap_false.
      ev_error = |Priority: { iv_priority } is not valid|.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.


  METHOD validate_delete_order.
* Check if the order exists
    DATA(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists IS INITIAL.
      rv_valid = abap_false.
      ev_error = |Work Order ID: { iv_work_order_id } does not exist|.
      RETURN.
    ENDIF.

    " Check if the order status is "PE" (Pending)
    IF iv_status NE 'PE'.
      rv_valid = abap_false.
      ev_error = |Status: { iv_status } must be Pending(PE) to delete|.
      RETURN.
    ENDIF.

* Check if the order has a history (i.e., if it has been modified before)
    DATA(lv_has_history) = check_order_history( iv_work_order_id ).
    IF lv_has_history IS NOT INITIAL.
      rv_valid = abap_false.
      ev_error = |Work Order ID: { iv_work_order_id } has history|.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.


  METHOD validate_update_order.
* Check if the work order exists
    DATA(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists IS INITIAL.
      rv_valid = abap_false.
      ev_error = |Work Order ID: { iv_work_order_id } does not exist|.
      RETURN.
    ENDIF.

* Check if the order status is editable (e.g., Pending)
    DATA(lv_status) = check_status( iv_status ).
    IF lv_status IS INITIAL.
      rv_valid = abap_false.
      ev_error = |Status: { iv_status } is not valid|.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.
ENDCLASS.
