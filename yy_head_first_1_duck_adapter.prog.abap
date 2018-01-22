*&---------------------------------------------------------------------*
*&  Sample for Patterns of Patterns - Compound Pattern
*&    based on Head First Design Patterns: Chapter 12
*&---------------------------------------------------------------------*
REPORT yy_head_first_1_duck_adapter.

*&---------------------------------------------------------------------*
*&  First, we'll create a Quackable interface.
*&---------------------------------------------------------------------*
INTERFACE lif_quackable.
  METHODS quack.  " Quackables only need to do one thing well: Quack!
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Now, some Ducks that implment Quackable.
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
*&  §4) When ducks are around, geese can't be far.
*&---------------------------------------------------------------------*
CLASS lcl_goose DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS honk.  " A Goose is a honker, not a quacker.
ENDCLASS.

CLASS lcl_goose IMPLEMENTATION.
  METHOD honk.
    cl_demo_output=>write( |Honk| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  §5) We need a goose adapter so geese can play in the simulator, too.
*&---------------------------------------------------------------------*
CLASS lcl_goose_adapter DEFINITION FINAL.
  PUBLIC SECTION.
    " Remember, an Adapter implements the target interface,
    " which in this case is Quackable.
    INTERFACES lif_quackable.
    METHODS constructor IMPORTING io_goose TYPE REF TO lcl_goose.
  PRIVATE SECTION.
    DATA mo_goose TYPE REF TO lcl_goose.
ENDCLASS.

CLASS lcl_goose_adapter IMPLEMENTATION.
  METHOD constructor.
    " The constructor takes the goose we are going to adapt.
    mo_goose = io_goose.
  ENDMETHOD.
  METHOD lif_quackable~quack.
    " When quack is called, it is delegated to the goose's honk method.
    mo_goose->honk( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  §6) Now the Goose can quack with the rest of the Ducks.
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

      " We make a Goose that acts like a Quackable duck by wrapping
      " the Goose in the Goose Adapter
      DATA(lo_goose_duck) = NEW lcl_goose_adapter( NEW lcl_goose( ) ).

      " ...then we simulate each one.
      cl_demo_output=>write_text(
        |\nDuck Simulator with Goose Adapter:| ).
      simulate( lo_mallard_duck ).
      simulate( lo_redhead_duck ).
      simulate( lo_duck_call ).
      simulate( lo_rubber_duck ).
      simulate( lo_decoy_duck ).

      " §7) Once the Goose is wrapped, we can treat it like other ducks
      simulate( lo_goose_duck ).

    ELSE.
      " Here we let polymorphism do its magic: no matter what kind of
      " Quackable gets passed in, the simulate method asks it to quack.
      io_duck->quack( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
    |Patterns of Patterns - Compound Pattern (adapter)| ).
  cl_demo_output=>line( ).
  lcl_duck_simulator=>main( ).
  cl_demo_output=>display( ).
