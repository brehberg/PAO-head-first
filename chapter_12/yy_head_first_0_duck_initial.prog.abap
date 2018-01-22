*&---------------------------------------------------------------------*
*&  Sample for Patterns of Patterns - Compound Pattern
*&    based on Head First Design Patterns: Chapter 12
*&---------------------------------------------------------------------*
REPORT yy_head_first_0_duck_initial.

*&---------------------------------------------------------------------*
*&  §1) First, we'll create a Quackable interface.
*&---------------------------------------------------------------------*
INTERFACE lif_quackable.
  METHODS quack.  " Quackables only need to do one thing well: Quack!
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  §2) Now, some Ducks that implment Quackable.
*&---------------------------------------------------------------------*
*&  Start with your standard Mallard duck.
*&---------------------------------------------------------------------*
CLASS lcl_mallard_duck DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_quackable.
ENDCLASS.

CLASS lcl_mallard_duck IMPLEMENTATION.
  METHOD lif_quackable~quack.
    cl_demo_output=>write( |Quack| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  We've got to have some variation of species if we want
*&  this to be an interesting simulator. Add a Redhead duck.
*&---------------------------------------------------------------------*
CLASS lcl_redhead_duck DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_quackable.
ENDCLASS.

CLASS lcl_redhead_duck IMPLEMENTATION.
  METHOD lif_quackable~quack.
    cl_demo_output=>write( |Quack| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  This wouldn't be much fun if we didn't add other kinds of Ducks too.
*&---------------------------------------------------------------------*
*&  A Duck Call that quacks but doesn't sound quite like the real thing.
*&---------------------------------------------------------------------*
CLASS lcl_duck_call DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_quackable.
ENDCLASS.

CLASS lcl_duck_call IMPLEMENTATION.
  METHOD lif_quackable~quack.
    cl_demo_output=>write( |Kwak| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  A Rubber Duck that makes a squeak when it quacks.
*&---------------------------------------------------------------------*
CLASS lcl_rubber_duck DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_quackable.
ENDCLASS.

CLASS lcl_rubber_duck IMPLEMENTATION.
  METHOD lif_quackable~quack.
    cl_demo_output=>write( |Squeak| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  A Decoy Duck that doesn't make any sound.
*&---------------------------------------------------------------------*
CLASS lcl_decoy_duck DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_quackable.
ENDCLASS.

CLASS lcl_decoy_duck IMPLEMENTATION.
  METHOD lif_quackable~quack.
    cl_demo_output=>write( |<< Silence >>| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  §3) Okay, we've got our ducks; now all we need is a simulator.
*&  Not too exciting yet, but we haven't added patterns!
*&---------------------------------------------------------------------*
*&  They all implement the same Quackable interface, but their
*&  unique implementations allow them to quack in their own way.
*&---------------------------------------------------------------------*
CLASS lcl_duck_simulator DEFINITION FINAL.
  PUBLIC SECTION.
    " Here's our main method to get everything going.
    CLASS-METHODS main.
  PRIVATE SECTION.
    " Here we "overload" the simulate method to simulate just one duck.
    METHODS simulate
      IMPORTING io_duck TYPE REF TO lif_quackable OPTIONAL.
ENDCLASS.

CLASS lcl_duck_simulator IMPLEMENTATION.
  METHOD main.
    " We create a simulator and then call its simulate() method.
    NEW lcl_duck_simulator( )->simulate( ).
  ENDMETHOD.
  METHOD simulate.

    IF io_duck IS NOT SUPPLIED.

      " We need some ducks, so here we create one of each Quackable...
      DATA(lo_mallard_duck) = NEW lcl_mallard_duck( ).
      DATA(lo_redhead_duck) = NEW lcl_redhead_duck( ).
      DATA(lo_duck_call)    = NEW lcl_duck_call( ).
      DATA(lo_rubber_duck)  = NEW lcl_rubber_duck( ).
      DATA(lo_decoy_duck)   = NEW lcl_decoy_duck( ).

      " ...then we simulate each one.
      cl_demo_output=>write_text( |\nDuck Simulator:| ).
      simulate( lo_mallard_duck ).
      simulate( lo_redhead_duck ).
      simulate( lo_duck_call ).
      simulate( lo_rubber_duck ).
      simulate( lo_decoy_duck ).

    ELSE.
      " Here we let polymorphism do its magic: no matter what kind of
      " Quackable gets passed in, the simulate method asks it to quack.
      io_duck->quack( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
    |Patterns of Patterns - Compound Pattern| ).
  cl_demo_output=>line( ).
  lcl_duck_simulator=>main( ).
  cl_demo_output=>display( ).
