*&---------------------------------------------------------------------*
*&  Sample for One of a Kind Objects - the Singleton Pattern
*&    based on Head First Design Patterns: Chapter 5
*&---------------------------------------------------------------------*
REPORT yy_head_first_single_classic.

*&---------------------------------------------------------------------*
*&  A class implementing the Singleton Pattern is more than a Singleton;
*&  it is a general purpose class with its own set of data and methods.
*&---------------------------------------------------------------------*
CLASS lcl_singleton DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    " The instance() method below is a class method, which means it's
    " static, so you can conveniently access this method from anywhere
    " in your code using lcl_singleton=>instance( ). That's just as
    " easy as accessing a global variable, but we get benefits like
    " lazy instantiation from the Singleton.
    CLASS-METHODS:
      instance RETURNING VALUE(ro_instance) TYPE REF TO lcl_singleton.
    METHODS:
      " other useful methods here
      description RETURNING VALUE(rv_text) TYPE string.
  PRIVATE SECTION.
    " The go_unique_instance class varaible holds our one and only
    " instance of Singleton.
    CLASS-DATA go_unique_instance TYPE REF TO lcl_singleton.
ENDCLASS.

CLASS lcl_singleton IMPLEMENTATION.
  METHOD instance.
    IF go_unique_instance IS NOT BOUND.
      go_unique_instance = NEW #( ).
    ENDIF.
    ro_instance = go_unique_instance.
  ENDMETHOD.
  METHOD description.
    rv_text = |I'm a classic Singleton!|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Some client code to test drive our Singleton.
*&---------------------------------------------------------------------*
CLASS lcl_singleton_test_drive DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS lcl_singleton_test_drive IMPLEMENTATION.
  METHOD main.

    DATA(lo_singleton) = lcl_singleton=>instance( ).
    cl_demo_output=>write( lo_singleton->description( ) ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
    |One of a Kind Objects - the Classic Singleton Pattern| ).
  cl_demo_output=>line( ).
  lcl_singleton_test_drive=>main( ).
  cl_demo_output=>display( ).
