*&---------------------------------------------------------------------*
*&  Sample for One of a Kind Objects - the Singleton Pattern
*&    based on Head First Design Patterns: Chapter 5
*&---------------------------------------------------------------------*
REPORT yy_head_first_single_static.

*&---------------------------------------------------------------------*
*&  Change to an eagerly created instance rather than using a lazily
*&  created one. Using this approach, we rely on the ABAP runtime to
*&  create the unique instance of Singleton when the class is loaded.
*&---------------------------------------------------------------------*
CLASS lcl_singleton DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor,
      instance RETURNING VALUE(ro_instance) TYPE REF TO lcl_singleton.
    METHODS:
      " other useful methods here
      description RETURNING VALUE(rv_text) TYPE string.
  PRIVATE SECTION.
    CLASS-DATA go_unique_instance TYPE REF TO lcl_singleton.
ENDCLASS.

CLASS lcl_singleton IMPLEMENTATION.
  METHOD class_constructor.
    " Go ahead and create an instance of Singleton in the method
    " class_constructor. This code is guaranteed to execute!
    go_unique_instance = NEW #( ).
  ENDMETHOD.
  METHOD instance.
    " We've already go an instance, so just return it.
    ro_instance = go_unique_instance.
  ENDMETHOD.
  METHOD description.
    rv_text = |I'm a statically initialized Singleton!|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Some client code to test drive our Singleton.
*&---------------------------------------------------------------------*
CLASS lcl_singleton_client DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS lcl_singleton_client IMPLEMENTATION.
  METHOD main.

    DATA(lo_singleton) = lcl_singleton=>instance( ).
    cl_demo_output=>write( lo_singleton->description( ) ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
    |One of a Kind Objects - the Static Singleton Pattern| ).
  cl_demo_output=>line( ).
  lcl_singleton_client=>main( ).
  cl_demo_output=>display( ).
