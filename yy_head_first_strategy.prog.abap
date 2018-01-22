*&---------------------------------------------------------------------*
*&  Sample code for Welcome to Design Patterns - the Strategy Pattern
*&    based on Head First Design Patterns: Chapter 1
*&---------------------------------------------------------------------*
REPORT yy_head_first_strategy.

*&---------------------------------------------------------------------*
*&  Encapsulated fly behavior stategies
*&---------------------------------------------------------------------*
*&  Fly Behavior is an interface that all flying classes implement.
*&  All new flying classes just need to implement the do_fly() method.
*&---------------------------------------------------------------------*
INTERFACE lif_fly_behavior.
  METHODS do_fly.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Here's the implementation of flying for all ducks that have wings.
*&---------------------------------------------------------------------*
CLASS lcl_fly_with_wings DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_fly_behavior.
ENDCLASS.

CLASS lcl_fly_with_wings IMPLEMENTATION.
  METHOD lif_fly_behavior~do_fly.
    cl_demo_output=>write( |I'm flying!| ).   " implements duck flying
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  And here's the implementation for all ducks that can't fly.
*&---------------------------------------------------------------------*
CLASS lcl_fly_no_way DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_fly_behavior.
ENDCLASS.

CLASS lcl_fly_no_way IMPLEMENTATION.
  METHOD lif_fly_behavior~do_fly.
    cl_demo_output=>write( |I can't fly| ).   " do nothing - can't fly!
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  And here we're creating a rocket-powered flying behavior.
*&---------------------------------------------------------------------*
CLASS lcl_fly_rocket_powered DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_fly_behavior.
ENDCLASS.

CLASS lcl_fly_rocket_powered IMPLEMENTATION.
  METHOD lif_fly_behavior~do_fly.
    cl_demo_output=>write( |I'm flying with a rocket!!| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Encapsulated quack behavior stategies
*&---------------------------------------------------------------------*
*&  Same thing here for the quack behavior; we have an interface that
*&  just includes a do_quack() method that needs to be implemented.
*&---------------------------------------------------------------------*
INTERFACE lif_quack_behavior.
  METHODS do_quack.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Quacks that really do_quack.
*&---------------------------------------------------------------------*
CLASS lcl_quack DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_quack_behavior.
ENDCLASS.

CLASS lcl_quack IMPLEMENTATION.
  METHOD lif_quack_behavior~do_quack.
    cl_demo_output=>write( |Quack| ).         " implements duck quacking
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Quacks that squeak.
*&---------------------------------------------------------------------*
CLASS lcl_squeak DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_quack_behavior.
ENDCLASS.

CLASS lcl_squeak IMPLEMENTATION.
  METHOD lif_quack_behavior~do_quack.
    cl_demo_output=>write( |Squeak| ).        " rubber duckie squeak
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Quacks that make no sound at all.
*&---------------------------------------------------------------------*
CLASS lcl_mute_quack DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_quack_behavior.
ENDCLASS.

CLASS lcl_mute_quack IMPLEMENTATION.
  METHOD lif_quack_behavior~do_quack.
    cl_demo_output=>write( |<< Silence >>| ). " do nothing - can't quack
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Quacks that don't sound right.
*&---------------------------------------------------------------------*
CLASS lcl_fake_quack DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_quack_behavior.
ENDCLASS.

CLASS lcl_fake_quack IMPLEMENTATION.
  METHOD lif_quack_behavior~do_quack.
    cl_demo_output=>write( |Qwak| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Duck client makes use of an encapsulated family of algorithms for
*&  both flying and quacking.
*&---------------------------------------------------------------------*
*&  Think of each set of behaviors as a family of algorithms.
*&  These behaviors, or "algorithms", are interchangeable.
*&---------------------------------------------------------------------*
CLASS lcl_duck DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      fly, quack, swim,
      display ABSTRACT,
      set_fly_behavior
        IMPORTING io_fb TYPE REF TO lif_fly_behavior,
      set_quack_behavior
        IMPORTING io_qb TYPE REF TO lif_quack_behavior.
  PROTECTED SECTION.
    " Declare two reference variables for the behavior interface
    " types. All duck subclasses inherit these.
    DATA:
      mo_fly_behavior   TYPE REF TO lif_fly_behavior,
      mo_quack_behavior TYPE REF TO lif_quack_behavior.
ENDCLASS.

CLASS lcl_duck IMPLEMENTATION.
  METHOD fly.
    mo_fly_behavior->do_fly( ).   " delegate to the fly behavior class
  ENDMETHOD.
  METHOD quack.
    mo_quack_behavior->do_quack( ).  " and to the quack behavior class
  ENDMETHOD.
  METHOD swim.
    cl_demo_output=>write( |All ducks float, even decoys!| ).
  ENDMETHOD.
  METHOD set_fly_behavior.
    mo_fly_behavior = io_fb.
  ENDMETHOD.
  METHOD set_quack_behavior.
    mo_quack_behavior = io_qb.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Mallard Duck concrete class
*&---------------------------------------------------------------------*
CLASS lcl_mallard_duck DEFINITION FINAL INHERITING FROM lcl_duck.
  PUBLIC SECTION.
    METHODS: constructor, display REDEFINITION.
    " Remember, Mallard Duck inherits Quack Behavior and Fly Behavior
    " instance variables from class Duck.
ENDCLASS.

CLASS lcl_mallard_duck IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    " A Mallard Duck uses the Quack class to handle its quack, so
    " when quack() is called, the responsibility for the quack is
    " delegated to the Quack object and we get a real quack.
    mo_quack_behavior = NEW lcl_quack( ).
    " And it uses Fly With Wings as its Fly Behavior type.
    mo_fly_behavior = NEW lcl_fly_with_wings( ).
  ENDMETHOD.
  METHOD display.
    cl_demo_output=>write_text( |I'm a real Mallard duck| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Red Headed Duck concrete class
*&---------------------------------------------------------------------*
CLASS lcl_redhead_duck DEFINITION FINAL INHERITING FROM lcl_duck.
  PUBLIC SECTION.
    METHODS: constructor, display REDEFINITION.
ENDCLASS.

CLASS lcl_redhead_duck IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mo_fly_behavior = NEW lcl_fly_with_wings( ).
    mo_quack_behavior = NEW lcl_quack( ).
  ENDMETHOD.
  METHOD display.
    cl_demo_output=>write_text( |I'm a real Red Headed duck| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Rubber Duckie concrete class
*&---------------------------------------------------------------------*
CLASS lcl_rubber_duck DEFINITION FINAL INHERITING FROM lcl_duck.
  PUBLIC SECTION.
    METHODS: constructor, display REDEFINITION.
ENDCLASS.

CLASS lcl_rubber_duck IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mo_fly_behavior = NEW lcl_fly_no_way( ).
    mo_quack_behavior = NEW lcl_squeak( ).
  ENDMETHOD.
  METHOD display.
    cl_demo_output=>write_text( |I'm a rubber duckie| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Duck Decoy concrete class
*&---------------------------------------------------------------------*
CLASS lcl_decoy_duck DEFINITION FINAL INHERITING FROM lcl_duck.
  PUBLIC SECTION.
    METHODS: constructor, display REDEFINITION.
ENDCLASS.

CLASS lcl_decoy_duck  IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mo_fly_behavior = NEW lcl_fly_no_way( ).
    mo_quack_behavior = NEW lcl_mute_quack( ).
  ENDMETHOD.
  METHOD display.
    cl_demo_output=>write_text( |I'm a duck Decoy| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Model Duck concrete class
*&---------------------------------------------------------------------*
CLASS lcl_model_duck DEFINITION FINAL INHERITING FROM lcl_duck.
  PUBLIC SECTION.
    METHODS: constructor, display REDEFINITION.
ENDCLASS.

CLASS lcl_model_duck IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    " Our model duck begins life grounded... without a way to fly.
    mo_fly_behavior = NEW lcl_fly_no_way( ).
    mo_quack_behavior = NEW lcl_quack( ).
  ENDMETHOD.
  METHOD display.
    cl_demo_output=>write_text( |I'm a model duck| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Highly realistic duck pond simulation
*&---------------------------------------------------------------------*
CLASS lcl_mini_duck_simulator DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS lcl_mini_duck_simulator IMPLEMENTATION.
  METHOD main.

    DATA:
      lo_mallard TYPE REF TO lcl_duck,
      lo_duckie  TYPE REF TO lcl_duck,
      lo_decoy   TYPE REF TO lcl_duck.

    lo_mallard = NEW lcl_mallard_duck( ).
    lo_duckie = NEW lcl_rubber_duck( ).
    lo_decoy = NEW lcl_decoy_duck( ).

    lo_mallard->display( ).
    " This calls the Mallard Duck's inherited quack() method,
    " which then delegates to the object's Quack Behavior (i.e.,
    " calls do_quack() on the duck's Quack Behavior reference).
    lo_mallard->quack( ).

    " Then we do the same thing with the Rubber Duckie and Duck Decoy
    lo_duckie->display( ).
    lo_duckie->quack( ).

    lo_decoy->display( ).
    lo_decoy->quack( ).
    lo_decoy->swim( ).

    cl_demo_output=>line( ).

    DATA:
      lo_model TYPE REF TO lcl_duck,
      lo_duck  TYPE REF TO lcl_duck.

    lo_model = NEW lcl_model_duck( ).
    lo_duck = NEW lcl_redhead_duck( ).

    lo_model->display( ).
    " The first call delegates to the Fly Behavior object set in the
    " Model Duck constructor, which is a Fly No Way instance.
    lo_model->fly( ).
    " Then invoke the model's inherited behavior setter method, and...
    lo_model->set_fly_behavior( NEW lcl_fly_rocket_powered( ) ).
    " if it worked, the model duck dynamically changed its fly behavior!
    lo_model->fly( ).

    " Let's do something similar with the Redhead Duck and Fake Quack
    lo_duck->display( ).
    lo_duck->quack( ).
    lo_duck->set_quack_behavior( NEW lcl_fake_quack( ) ).
    lo_duck->quack( ).

    cl_demo_output=>line( ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
    |Welcome to Design Patterns - the Strategy Pattern| ).
  cl_demo_output=>line( ).
  lcl_mini_duck_simulator=>main( ).
  cl_demo_output=>display( ).
