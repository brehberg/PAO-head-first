*&---------------------------------------------------------------------*
*&  Sample for Being Adaptive - the Adapter Pattern
*&    based on Head First Design Patterns: Chapter 7
*&---------------------------------------------------------------------*
REPORT yy_head_first_adapter.

*&---------------------------------------------------------------------*
*&  This time around, our ducks implement a Duck interface that allows
*&  Ducks to quack and fly.
*&---------------------------------------------------------------------*
INTERFACE lif_duck.
  METHODS: quack, fly.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Here's a concrete implementation of Duck, the Mallard Duck.
*&---------------------------------------------------------------------*
CLASS lcl_mallard_duck DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_duck.
ENDCLASS.

CLASS lcl_mallard_duck IMPLEMENTATION.
  METHOD lif_duck~quack.
    " Simple implementation: the duck just prints out what it is doing.
    cl_demo_output=>write( |Quack| ).
  ENDMETHOD.
  METHOD lif_duck~fly.
    cl_demo_output=>write( |I'm flying| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  The newest fowl on the block: Turkeys don't quack, they gobble.
*&---------------------------------------------------------------------*
INTERFACE lif_turkey.
  METHODS: gobble, fly.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Here's a concrete implementation of Turkey, the Wild Turkey. Like
*&  the Duck implementation, it just prints out its actions.
*&---------------------------------------------------------------------*
CLASS lcl_wild_turkey DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_turkey.
ENDCLASS.

CLASS lcl_wild_turkey IMPLEMENTATION.
  METHOD lif_turkey~gobble.
    cl_demo_output=>write( |Gobble gobble| ).
  ENDMETHOD.
  METHOD lif_turkey~fly.
    " Turkeys can fly, although they can only fly short distances.
    cl_demo_output=>write( |I'm flying a short distance| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Here's our adapter to make Turkeys look like Ducks.
*&---------------------------------------------------------------------*
*&  The Adapter implements the Target interface, Duck, and holds an
*&  instance of the Adaptee interface, Turkey.
*&---------------------------------------------------------------------*
CLASS lcl_turkey_adapter DEFINITION FINAL.
  PUBLIC SECTION.
    " First, we need to implement the interface of the type we're
    " adapting to. This is the interface our client expects to see.
    INTERFACES lif_duck.
    " Next, we need to get a reference to the object that we are
    " adapting; here we do that through the constructor.
    METHODS constructor IMPORTING io_turkey TYPE REF TO lif_turkey.
  PRIVATE SECTION.
    DATA mo_turkey TYPE REF TO lif_turkey.
ENDCLASS.

CLASS lcl_turkey_adapter IMPLEMENTATION.
  METHOD constructor.
    mo_turkey = io_turkey.
  ENDMETHOD.
  METHOD lif_duck~quack.
    " Now we need to implement all the methods in the interface; the
    " quack() translation between classes is easy; just call gobble().
    mo_turkey->gobble( ).
  ENDMETHOD.
  METHOD lif_duck~fly.
    " Even though both interfaces have a fly() method, the Turkeys fly
    " in short spurts - they can't do long-distance flying like ducks.
    " To map between a Duck's fly() method and a Turkey's, we need to
    " call the Turkey's fly() method five times to make up for it.
    DO 5 TIMES.
      mo_turkey->fly( ).
    ENDDO.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Now we are adapting Ducks to look like Turkeys, so we implement
*&  the Turkey interface.
*&---------------------------------------------------------------------*
CLASS lcl_duck_adapter DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_turkey.
    METHODS constructor IMPORTING io_duck TYPE REF TO lif_duck.
  PRIVATE SECTION.
    DATA:
      mo_duck   TYPE REF TO lif_duck,
      mo_random TYPE REF TO cl_abap_random_int.
ENDCLASS.

CLASS lcl_duck_adapter IMPLEMENTATION.
  METHOD constructor.
    " We stash a reference to the Duck we are adapting.
    mo_duck = io_duck.
    " We also create a random object; take a look at the fly() method
    " to see how it is used.
    mo_random = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( ) min = 1 max = 5 ).
  ENDMETHOD.
  METHOD lif_turkey~gobble.
    " A gobble just becomes a quack.
    mo_duck->quack( ).
  ENDMETHOD.
  METHOD lif_turkey~fly.
    " Since Ducks fly a lot longer than Turkeys, we decided to only
    " fly the Duck on average one out of five times.
    CHECK mo_random->get_next( ) = 1.
    mo_duck->fly( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Some client code to test drive our Turkey Adapter.
*&---------------------------------------------------------------------*
CLASS lcl_duck_test_drive DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS main.
  PRIVATE SECTION.
    " Here's our test_duck() method; it gets a duck reference and
    " calls its quack() and fly() methods.
    CLASS-METHODS test_duck IMPORTING io_duck TYPE REF TO lif_duck.
ENDCLASS.

CLASS lcl_duck_test_drive IMPLEMENTATION.
  METHOD main.

    DATA:
      lo_duck    TYPE REF TO lif_duck,
      lo_turkey  TYPE REF TO lif_turkey,
      lo_adapter TYPE REF TO lif_duck.

    lo_duck = NEW lcl_mallard_duck( ).    " Let's create a duck ...
    lo_turkey = NEW lcl_wild_turkey( ).   " ... and a turkey.

    " And then wrap the turkey in a Turkey Adapater, which makes
    " it look like a Duck.
    lo_adapter = NEW lcl_turkey_adapter( lo_turkey ).

    " Then let's test the Turkey: make it gobble, make it fly.
    cl_demo_output=>write_text( |The Turkey says...| ).
    lo_turkey->gobble( ).
    lo_turkey->fly( ).
    cl_demo_output=>line( ).

    " Now let's test the duck by calling the test_duck() method,
    " which expects a Duck object
    cl_demo_output=>write_text( |The Duck says...| ).
    test_duck( lo_duck ).
    cl_demo_output=>line( ).

    " Now the big test: we try to pass off the turkey as a duck...
    cl_demo_output=>write_text( |The Turkey Adapter says...| ).
    test_duck( lo_adapter ).
    cl_demo_output=>line( ).

  ENDMETHOD.
  METHOD test_duck.
    " Note that the Client and Adaptee are decoupled in this method
    " implementation - neither knows about the other.
    io_duck->quack( ).
    io_duck->fly( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Another client to test drive our Duck Adapter.
*&---------------------------------------------------------------------*
CLASS lcl_turkey_test_drive DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS lcl_turkey_test_drive IMPLEMENTATION.
  METHOD main.

    DATA lo_adapter TYPE REF TO lif_turkey.

    " Let's create a duck and then wrap the duck in a Duck Adapter.
    lo_adapter = NEW lcl_duck_adapter( NEW lcl_mallard_duck( ) ).

    DO 10 TIMES.
      " Then let's test the Duck Adapter: make it gobble, make it fly.
      cl_demo_output=>write_text( |The Duck Adapter says...| ).
      lo_adapter->gobble( ).
      lo_adapter->fly( ).
      cl_demo_output=>line( ).
    ENDDO.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
    |Being Adaptive - the Adapter Pattern| ).
  cl_demo_output=>line( ).
  lcl_duck_test_drive=>main( ).
  lcl_turkey_test_drive=>main( ).
  cl_demo_output=>display( ).
