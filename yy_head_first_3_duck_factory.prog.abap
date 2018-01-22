*&---------------------------------------------------------------------*
*&  Sample for Patterns of Patterns - Compound Pattern
*&    based on Head First Design Patterns: Chapter 12
*&---------------------------------------------------------------------*
REPORT yy_head_first_3_duck_factory.

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
*&  When ducks are around, geese can't be far.
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
*&  We need a goose adapter so geese can play in the simulator, too.
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
*&  We're going to make those Quackologists happy and give them
*&  some quack counts. Quack Counter is a decorator.
*&---------------------------------------------------------------------*
CLASS lcl_quack_counter DEFINITION FINAL.
  PUBLIC SECTION.
    " As with Adapter, we need to implement the target interface.
    INTERFACES lif_quackable.
    METHODS constructor IMPORTING io_duck TYPE REF TO lif_quackable.
    " We're adding one other method to the decorator. This static
    " method just returns the number of quacks that have occured
    " in all Quackable instances.
    CLASS-METHODS quacks RETURNING VALUE(rv_number_of_quacks) TYPE i.
  PRIVATE SECTION.
    " We've got an instance variable to hold on to the
    " quacker we're decorating.
    DATA mo_duck TYPE REF TO lif_quackable.
    " And we're counting ALL quacks, so we'll use a static
    " variable to keep track.
    CLASS-DATA gv_number_of_quacks TYPE i VALUE 0.
ENDCLASS.

CLASS lcl_quack_counter IMPLEMENTATION.
  METHOD constructor.
    " We get the reference to the Quackable we're decorating
    " in the constructor.
    mo_duck = io_duck.
  ENDMETHOD.
  METHOD lif_quackable~quack.
    " When quack is called, we delegate the call to the Quackable
    " we're decorating...
    mo_duck->quack( ).
    " ...then we increase the number of quacks.
    ADD 1 TO gv_number_of_quacks.
  ENDMETHOD.
  METHOD quacks.
    rv_number_of_quacks = gv_number_of_quacks.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  §10) We need a factory to produce ducks!
*&---------------------------------------------------------------------*
*&  We're defining an abstract factory that subclasses will implement
*&  to create different families.
*&---------------------------------------------------------------------*
CLASS lcl_abstract_duck_factory DEFINITION ABSTRACT.
  PUBLIC SECTION.
    " Each method creates one kind of duck.
    METHODS:
      create_mallard_duck ABSTRACT
        RETURNING VALUE(ro_duck) TYPE REF TO lif_quackable,
      create_redhead_duck ABSTRACT
        RETURNING VALUE(ro_duck) TYPE REF TO lif_quackable,
      create_duck_call ABSTRACT
        RETURNING VALUE(ro_duck) TYPE REF TO lif_quackable,
      create_rubber_duck ABSTRACT
        RETURNING VALUE(ro_duck) TYPE REF TO lif_quackable,
      create_decoy_duck ABSTRACT
        RETURNING VALUE(ro_duck) TYPE REF TO lif_quackable.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Basic Duck Factory inherits from the abstract factory.
*&---------------------------------------------------------------------*
CLASS lcl_basic_duck_factory DEFINITION
  INHERITING FROM lcl_abstract_duck_factory.
  PUBLIC SECTION.
    " Each method creates a product: a particular kind of Quackable.
    " The actual type of product is unknown to the simulator - it just
    " knows it's getting a Quackable oject.
    METHODS:
      create_mallard_duck REDEFINITION,
      create_redhead_duck REDEFINITION,
      create_duck_call    REDEFINITION,
      create_rubber_duck  REDEFINITION,
      create_decoy_duck   REDEFINITION.
ENDCLASS.

CLASS lcl_basic_duck_factory IMPLEMENTATION.
  METHOD create_mallard_duck.
    ro_duck = NEW lcl_mallard_duck( ).
  ENDMETHOD.
  METHOD create_redhead_duck.
    ro_duck = NEW lcl_redhead_duck( ).
  ENDMETHOD.
  METHOD create_duck_call.
    ro_duck = NEW lcl_duck_call( ).
  ENDMETHOD.
  METHOD create_rubber_duck.
    ro_duck = NEW lcl_rubber_duck( ).
  ENDMETHOD.
  METHOD create_decoy_duck.
    ro_duck = NEW lcl_decoy_duck( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Counting Duck Factory also inherits from the abstract factory.
*&---------------------------------------------------------------------*
CLASS lcl_counting_duck_factory DEFINITION FINAL
  INHERITING FROM lcl_basic_duck_factory.
  PUBLIC SECTION.
    " Each method wraps the Quackable with the quack counting decorator.
    " The simulator will never know the difference; it just gets back
    " a Quackable object. But now our rangers can be sure that all
    " quacks are being counted.
    METHODS:
      create_mallard_duck REDEFINITION,
      create_redhead_duck REDEFINITION,
      create_duck_call    REDEFINITION,
      create_rubber_duck  REDEFINITION,
      create_decoy_duck   REDEFINITION.
ENDCLASS.

CLASS lcl_counting_duck_factory IMPLEMENTATION.
  METHOD create_mallard_duck.
    ro_duck = NEW lcl_quack_counter( super->create_mallard_duck( ) ).
  ENDMETHOD.
  METHOD create_redhead_duck.
    ro_duck = NEW lcl_quack_counter( super->create_redhead_duck( ) ).
  ENDMETHOD.
  METHOD create_duck_call.
    ro_duck = NEW lcl_quack_counter( super->create_duck_call( ) ).
  ENDMETHOD.
  METHOD create_rubber_duck.
    ro_duck = NEW lcl_quack_counter( super->create_rubber_duck( ) ).
  ENDMETHOD.
  METHOD create_decoy_duck.
    ro_duck = NEW lcl_quack_counter( super->create_decoy_duck( ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  §11) Let's set up the simulator to use the factory. The output is
*&  the same as last time, but this time we're ensuring that the ducks
*&  are all decorated because we are using the Couting Duck Factory.
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
      IMPORTING
        io_duck    TYPE REF TO lif_quackable OPTIONAL
        io_factory TYPE REF TO lcl_abstract_duck_factory OPTIONAL
      PREFERRED PARAMETER io_duck.
ENDCLASS.

CLASS lcl_duck_simulator IMPLEMENTATION.
  METHOD main.
    " First we create the duck factory that we're going to pass
    " into the simulate() method.
    DATA(lo_duck_factory) = NEW lcl_counting_duck_factory( ).
    " We create a simulator and then call its simulate() method.
    NEW lcl_duck_simulator( )->simulate( io_factory = lo_duck_factory ).
  ENDMETHOD.
  METHOD simulate.

    IF io_duck IS NOT SUPPLIED.

      " We need some ducks, so here we create one of each Quackable...
      " The simulate() method takes an Abstract Duck Factory and uses
      " it to create ducks rather than instantiating them directly.
      DATA(lo_mallard_duck) = io_factory->create_mallard_duck( ).
      DATA(lo_redhead_duck) = io_factory->create_redhead_duck( ).
      DATA(lo_duck_call)    = io_factory->create_duck_call( ).
      DATA(lo_rubber_duck)  = io_factory->create_rubber_duck( ).
      DATA(lo_decoy_duck)   = io_factory->create_decoy_duck( ).

      " We make a Goose that acts like a Quackable duck by wrapping
      " the Goose in the Goose Adapter. The park ranger told us he
      " didn't want to count geese honks, so we don't decorate it.
      DATA(lo_goose_duck) = NEW lcl_goose_adapter( NEW lcl_goose( ) ).

      " ...then we simulate each one. Nothing changes! Same ol' code.
      cl_demo_output=>write_text(
        |\nDuck Simulator with Abstract Duck Factory:| ).
      simulate( lo_mallard_duck ).
      simulate( lo_redhead_duck ).
      simulate( lo_duck_call ).
      simulate( lo_rubber_duck ).
      simulate( lo_decoy_duck ).
      simulate( lo_goose_duck ).

      " Here's where we gather the quack behavior for the Quackologists.
      cl_demo_output=>write_text(
        |\nThe ducks quacked { lcl_quack_counter=>quacks( ) } times| ).

    ELSE.
      " Here we let polymorphism do its magic: no matter what kind of
      " Quackable gets passed in, the simulate method asks it to quack.
      " Nothing changes here; the decorated objects are still Quackable
      io_duck->quack( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
    |Patterns of Patterns - Compound Pattern (abstract factory)| ).
  cl_demo_output=>line( ).
  lcl_duck_simulator=>main( ).
  cl_demo_output=>display( ).
