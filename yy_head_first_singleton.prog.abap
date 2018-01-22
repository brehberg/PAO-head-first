*&---------------------------------------------------------------------*
*&  Sample for One of a Kind Objects - the Singleton Pattern
*&    based on Head First Design Patterns: Chapter 5
*&---------------------------------------------------------------------*
REPORT yy_head_first_singleton.

*&---------------------------------------------------------------------*
*&  Our class is declared as create private; only Chocolate Boiler
*&  can instaniate this class!
*&---------------------------------------------------------------------*
CLASS lcl_chocolate_boiler DEFINITION CREATE PRIVATE FINAL.
  PUBLIC SECTION.
    " The instance() method give us a way to instantiate the class
    " and also to return an instance of it.
    CLASS-METHODS instance
      RETURNING VALUE(ro_instance) TYPE REF TO lcl_chocolate_boiler.
    METHODS:
      " Of course, Chocolate Boilder is a normal class; it has other
      " useful methods and instance variables.
      constructor,
      fill, drain, boil,
      is_empty  RETURNING VALUE(rv_flag) TYPE abap_bool,
      is_boiled RETURNING VALUE(rv_flag) TYPE abap_bool.
  PRIVATE SECTION.
    " We have a static variable to hold our one instance of the class.
    CLASS-DATA go_unique_instance TYPE REF TO lcl_chocolate_boiler.
    DATA:
      mv_empty  TYPE abap_bool,
      mv_boiled TYPE abap_bool.
ENDCLASS.

CLASS lcl_chocolate_boiler IMPLEMENTATION.
  METHOD instance.
    " go_unique_instance holds our ONE instance; remember, it is a
    " static variable. If it is not bound, then we haven't created
    " the instance yet ...
    IF go_unique_instance IS NOT BOUND.
      cl_demo_output=>write( |Creating instance of Chocolate Boiler| ).
      " ... and, if it doesn't exist, we instantiate Chocolate Boiler
      "  through its private constructor and assign it to variable
      " go_unique_instance. Note that if we never need the instance,
      " it never gets created; this is lazy instantiation.
      go_unique_instance = NEW #( ).
    ENDIF.
    cl_demo_output=>write( |Returning instance of Chocolate Boiler| ).
    " If go_unique_instance was bound, then it was previously created
    " so we just fall through to the return statement. By the time we
    " hit this code, we have an instance and we can return it.
    ro_instance = go_unique_instance.
  ENDMETHOD.
  METHOD constructor.
    mv_empty = abap_true.
    mv_boiled = abap_false.
  ENDMETHOD.
  METHOD fill.
    " To fill the boiler it must be empty, and, once it's full, we set
    " the empty and boiled flags.
    CHECK is_empty( ).
    " fill the boiler with a milk/chocolate mixture...
    mv_empty = abap_false.
    mv_boiled = abap_false.
  ENDMETHOD.
  METHOD drain.
    " To drain the boiler, it must be full (non-empty) and also boiled.
    " Once it is drained we set the empty flag back to true.
    CHECK NOT is_empty( ) AND is_boiled( ).
    " drain the boiled milk and chocolate...
    mv_empty = abap_true.
  ENDMETHOD.
  METHOD boil.
    " To boil the mixture, the boiler has to be full and not already
    " boiled. Once it's boiled we set the boiled flag to true.
    CHECK NOT is_empty( ) AND NOT is_boiled( ).
    " bring the contents to a boil...
    mv_boiled = abap_true.
  ENDMETHOD.
  METHOD is_empty.
    rv_flag = mv_empty.
  ENDMETHOD.
  METHOD is_boiled.
    rv_flag = mv_boiled.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Controller class for Choc-O-Holic industrial strength boiler.
*&---------------------------------------------------------------------*
CLASS lcl_chocolate_controller DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS lcl_chocolate_controller IMPLEMENTATION.
  METHOD main.

    cl_demo_output=>write_text( |Fill the Choc-O-Holic boiler...| ).
    DATA(lo_boiler) = lcl_chocolate_boiler=>instance( ).
    lo_boiler->fill( ).
    cl_demo_output=>line( ).

    cl_demo_output=>write_text( |Boil the Choc-O-Holic boiler...| ).
    " will return the existing instance
    DATA(lo_boiler2) = lcl_chocolate_boiler=>instance( ).
    lo_boiler2->boil( ).
    cl_demo_output=>line( ).

    cl_demo_output=>write_text( |Drain the Choc-O-Holic boiler...| ).
    " we can also access the instance inline with method chaining
    lcl_chocolate_boiler=>instance( )->drain( ).
    cl_demo_output=>line( ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
    |One of a Kind Objects - the Singleton Pattern| ).
  cl_demo_output=>line( ).
  lcl_chocolate_controller=>main( ).
  cl_demo_output=>display( ).
