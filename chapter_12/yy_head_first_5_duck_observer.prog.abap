*&---------------------------------------------------------------------*
*&  Sample for Patterns of Patterns - Compound Pattern
*&    based on Head First Design Patterns: Chapter 12
*&---------------------------------------------------------------------*
REPORT yy_head_first_5_duck_observer.

*&---------------------------------------------------------------------*
*&  §14) First we need an Observable interface.
*&---------------------------------------------------------------------*
*&  Observable is the interface that our Quackables should
*&  implement if they want to be observed.
*&---------------------------------------------------------------------*
INTERFACE lif_observable.
  METHODS description RETURNING VALUE(rv_text) TYPE string.
  " It has an ABAP event for notifying the observers.
  EVENTS notify_observers.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  We also need the Observer side of the pattern.
*&---------------------------------------------------------------------*
INTERFACE lif_quack_observer.
  " The Quack Observer interface just has one event handler method,
  " which is passed the Observable object that is quacking.
  METHODS update
    FOR EVENT notify_observers OF lif_observable
    IMPORTING sender.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  §15) Now, we need to make sure all the concrete classes that
*&  implement Quackable can also handle being an Observable.
*&---------------------------------------------------------------------*
*&  So, we include Observable within the Quackable interface.
*&---------------------------------------------------------------------*
INTERFACE lif_quackable.
  INTERFACES lif_observable.
  METHODS: quack,
    " We have defined a new method for registering observers. Any object
    " implementing the Quack Observer interface can listen for quacks.
    register_observer IMPORTING io_spy TYPE REF TO lif_quack_observer.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Quack Observable implements all the functionality a Quackable
*&  needs to also be an Observable. We just need to plug it into a
*&  class and have that class delegate to the Quack Observable.
*&---------------------------------------------------------------------*
CLASS lcl_quack_observable DEFINITION FINAL.
  PUBLIC SECTION.
    " This class must implement the Observable because these are
    " the same method calls that are going to be delegated to it.
    INTERFACES lif_observable.
    METHODS:
      constructor       IMPORTING io_duck TYPE REF TO lif_observable,
      register_observer IMPORTING io_spy TYPE REF TO lif_quack_observer,
      notify_observers.
  PRIVATE SECTION.
    DATA mo_duck TYPE REF TO lif_observable.
ENDCLASS.

CLASS lcl_quack_observable IMPLEMENTATION.
  METHOD constructor.
    " In the constructor we get passed the Quackable that is using
    " the helper object to manage its observable behavior. Check out
    " the name() method below; you'll see that when a notify occurs,
    " Quack Observable delegates to this object so that the observer
    " know which object is quacking.
    mo_duck = io_duck.
  ENDMETHOD.
  METHOD register_observer.
    " Here's the code for registering an observer.
    SET HANDLER io_spy->update FOR me.
  ENDMETHOD.
  METHOD notify_observers.
    " And the code for doing the notifications.
    RAISE EVENT lif_observable~notify_observers.
  ENDMETHOD.
  METHOD lif_observable~description.
    rv_text = mo_duck->description( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  §16) Integrate the observable helper with the Quackable classes.
*&---------------------------------------------------------------------*
*&  Let's see how to use this, start with your standard Mallard duck...
*&---------------------------------------------------------------------*
CLASS lcl_mallard_duck DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_quackable.
    METHODS constructor.
  PRIVATE SECTION.
    " Each Quackable has an observable instance variable.
    DATA mo_observable TYPE REF TO lcl_quack_observable.
ENDCLASS.

CLASS lcl_mallard_duck IMPLEMENTATION.
  METHOD constructor.
    " In the constructor, we create a Quack Observable and
    " pass it a reference to the Mallard Duck object.
    mo_observable = NEW #( me ).
  ENDMETHOD.
  METHOD lif_quackable~quack.
    cl_demo_output=>write( |Quack| ).
    " When we quack, we need to let the observers know about it.
    mo_observable->notify_observers( ).
  ENDMETHOD.
  METHOD lif_quackable~register_observer.
    " Notice that we just delegate to the helper.
    mo_observable->register_observer( io_spy ).
  ENDMETHOD.
  METHOD lif_observable~description.
    rv_text = |Mallard Duck|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  We've got to have some variation of species if we want
*&  this to be an interesting simulator. Add a Redhead duck.
*&---------------------------------------------------------------------*
CLASS lcl_redhead_duck DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_quackable.
    METHODS constructor.
  PRIVATE SECTION.
    DATA mo_observable TYPE REF TO lcl_quack_observable.
ENDCLASS.

CLASS lcl_redhead_duck IMPLEMENTATION.
  METHOD constructor.
    mo_observable = NEW #( me ).
  ENDMETHOD.
  METHOD lif_quackable~quack.
    cl_demo_output=>write( |Quack| ).
    mo_observable->notify_observers( ).
  ENDMETHOD.
  METHOD lif_quackable~register_observer.
    mo_observable->register_observer( io_spy ).
  ENDMETHOD.
  METHOD lif_observable~description.
    rv_text = |Redhead Duck|.
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
    METHODS constructor.
  PRIVATE SECTION.
    DATA mo_observable TYPE REF TO lcl_quack_observable.
ENDCLASS.

CLASS lcl_duck_call IMPLEMENTATION.
  METHOD constructor.
    mo_observable = NEW #( me ).
  ENDMETHOD.
  METHOD lif_quackable~quack.
    cl_demo_output=>write( |Kwak| ).
    mo_observable->notify_observers( ).
  ENDMETHOD.
  METHOD lif_quackable~register_observer.
    mo_observable->register_observer( io_spy ).
  ENDMETHOD.
  METHOD lif_observable~description.
    rv_text = |Duck Call|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  A Rubber Duck that makes a squeak when it quacks.
*&---------------------------------------------------------------------*
CLASS lcl_rubber_duck DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_quackable.
    METHODS constructor.
  PRIVATE SECTION.
    DATA mo_observable TYPE REF TO lcl_quack_observable.
ENDCLASS.

CLASS lcl_rubber_duck IMPLEMENTATION.
  METHOD constructor.
    mo_observable = NEW #( me ).
  ENDMETHOD.
  METHOD lif_quackable~quack.
    cl_demo_output=>write( |Squeak| ).
    mo_observable->notify_observers( ).
  ENDMETHOD.
  METHOD lif_quackable~register_observer.
    mo_observable->register_observer( io_spy ).
  ENDMETHOD.
  METHOD lif_observable~description.
    rv_text = |Rubber Duck|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  A Decoy Duck that doesn't make any sound.
*&---------------------------------------------------------------------*
CLASS lcl_decoy_duck DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_quackable.
    METHODS constructor.
  PRIVATE SECTION.
    DATA mo_observable TYPE REF TO lcl_quack_observable.
ENDCLASS.

CLASS lcl_decoy_duck IMPLEMENTATION.
  METHOD constructor.
    mo_observable = NEW #( me ).
  ENDMETHOD.
  METHOD lif_quackable~quack.
    cl_demo_output=>write( |<< Silence >>| ).
    mo_observable->notify_observers( ).
  ENDMETHOD.
  METHOD lif_quackable~register_observer.
    mo_observable->register_observer( io_spy ).
  ENDMETHOD.
  METHOD lif_observable~description.
    rv_text = |Decoy Duck|.
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
    " Remember, an Adapter implements the target interface, which
    " in this case is Quackable, so now its an Observable too.
    INTERFACES lif_quackable.
    METHODS constructor IMPORTING io_goose TYPE REF TO lcl_goose.
  PRIVATE SECTION.
    DATA:
      mo_goose      TYPE REF TO lcl_goose,
      mo_observable TYPE REF TO lcl_quack_observable.
ENDCLASS.

CLASS lcl_goose_adapter IMPLEMENTATION.
  METHOD constructor.
    " The constructor takes the goose we are going to adapt.
    mo_goose = io_goose.
    mo_observable = NEW #( me ).
  ENDMETHOD.
  METHOD lif_quackable~quack.
    " When quack is called, it is delegated to the goose's honk method.
    mo_goose->honk( ).
    mo_observable->notify_observers( ).
  ENDMETHOD.
  METHOD lif_quackable~register_observer.
    mo_observable->register_observer( io_spy ).
  ENDMETHOD.
  METHOD lif_observable~description.
    rv_text = |Goose pretending to be a Duck|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  We're going to make those Quackologists happy and give them
*&  some quack counts. Quack Counter is a decorator.
*&---------------------------------------------------------------------*
CLASS lcl_quack_counter DEFINITION FINAL.
  PUBLIC SECTION.
    " Quack Counter is a Quackable, so now it's an Observable too.
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
    " Here's the duck that the Quack Counter is decorating. It's this
    " duck that really needs to handle the observable methods.
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
  METHOD lif_quackable~register_observer.
    " Here are the two methods added for Observable. Notice that
    " we just delegate calls to the duck that we're decorating.
    mo_duck->register_observer( io_spy ).
  ENDMETHOD.
  METHOD lif_observable~description.
    rv_text = mo_duck->lif_observable~description( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  We need a factory to produce ducks!
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
*&  Let's create a flock of ducks (actually a flock of Quackables)
*&---------------------------------------------------------------------*
CLASS lcl_flock DEFINITION FINAL.
  PUBLIC SECTION.
    " Remember, the composite needs to implement the same interface as
    " the leaf elements. Our leaf elements are Quackables. Therefore,
    " Flock is a Quackable, so now its an Observable too.
    INTERFACES lif_quackable.
    " The add() method adds a Quackable to the Flock.
    METHODS add IMPORTING io_quacker TYPE REF TO lif_quackable.
  PRIVATE SECTION.
    " We're using an internal table inside each Flock to hold
    " the Quackables that belong to the Flock.
    DATA mt_quackers TYPE STANDARD TABLE
      OF REF TO lif_quackable WITH EMPTY KEY.
ENDCLASS.

CLASS lcl_flock IMPLEMENTATION.
  METHOD add.
    INSERT io_quacker INTO TABLE mt_quackers.
  ENDMETHOD.
  METHOD lif_quackable~quack.
    " Now for the quack method - after all, the Flock is a Quackable
    " too. The quack() method in Flock needs to work over the entire
    " Flock. Here we iterate through the internal table and call the
    " quack() method on each element.
    LOOP AT mt_quackers INTO DATA(lo_quacker).
      lo_quacker->quack( ).
    ENDLOOP.
    " Each Quackable does its own notification, so Flock doesn't have
    " to worry about it. This happens when Flock delegates quack() to
    " each Quackable object in the Flock.
  ENDMETHOD.
  METHOD lif_quackable~register_observer.
    " When you register as an Observer with the Flock, you actually
    " get registered with everything that's IN the flock, which is
    " every Quackable, whether it's a duck or another Flock.
    LOOP AT mt_quackers INTO DATA(lo_quacker).
      lo_quacker->register_observer( io_spy ).
    ENDLOOP.
  ENDMETHOD.
  METHOD lif_observable~description.
    rv_text = |Flock of Ducks|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  §17) We're almost there! We need to implement the Quack Observer
*&  interface or else we won't be able to register with a Quackable.
*&---------------------------------------------------------------------*
CLASS lcl_quackologist DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_quack_observer.
    METHODS constructor IMPORTING iv_name TYPE string.
  PRIVATE SECTION.
    DATA mv_name TYPE string.
ENDCLASS.

CLASS lcl_quackologist IMPLEMENTATION.
  METHOD lif_quack_observer~update.
    " The Quackologist is simple, it just has one method, which prints
    " out the description of the Observable object that just quacked.
    DATA(lo_duck) = CAST lif_observable( sender ).
    cl_demo_output=>write( |Quackologist { mv_name } observed that a | &
                           |{ lo_duck->description( ) } just quacked| ).
  ENDMETHOD.
  METHOD constructor.
    mv_name = iv_name.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  §18) Let's update the simulator, give it a try and see how it works!
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
    " First we create the factory that we're going to pass into
    " the simulate() method.
    DATA(lo_duck_factory) = NEW lcl_counting_duck_factory( ).
    " We create a simulator and then call its simulate() method.
    NEW lcl_duck_simulator( )->simulate( io_factory = lo_duck_factory ).
  ENDMETHOD.
  METHOD simulate.

    IF io_duck IS NOT SUPPLIED.

      " We need some ducks, so here we create one of each Quackable...
      " The simulate() method takes an Abstract Duck Factory and uses
      " it to create ducks rather than instantiating them directly.
      DATA(lo_redhead_duck) = io_factory->create_redhead_duck( ).
      DATA(lo_duck_call)    = io_factory->create_duck_call( ).
      DATA(lo_rubber_duck)  = io_factory->create_rubber_duck( ).
      DATA(lo_decoy_duck)   = io_factory->create_decoy_duck( ).

      " We make a Goose that acts like a Quackable duck by wrapping
      " the Goose in the Goose Adapter. The park ranger told us he
      " didn't want to count geese honks, so we don't decorate it.
      DATA(lo_goose_duck) = NEW lcl_goose_adapter( NEW lcl_goose( ) ).

      "First we create a Flock, and load it up with Quackables.
      cl_demo_output=>write_text(
        |\nDuck Simulator with Quack Observer:| ).
      DATA(lo_flock_of_ducks) = NEW lcl_flock( ).
      lo_flock_of_ducks->add( lo_redhead_duck ).
      lo_flock_of_ducks->add( lo_duck_call ).
      lo_flock_of_ducks->add( lo_rubber_duck ).
      lo_flock_of_ducks->add( lo_decoy_duck ).
      lo_flock_of_ducks->add( lo_goose_duck ).

      " Then we create a new Flock of mallards.
      DATA(lo_flock_of_mallards) = NEW lcl_flock( ).
      DO 4 TIMES.
        " Here we're creating a little family of mallards...
        DATA(lo_mallard_duck) = io_factory->create_mallard_duck( ).
        " ...and adding them to the Flock of mallards.
        lo_flock_of_mallards->add( lo_mallard_duck ).
      ENDDO.
      " Then we add the Flock of mallards to the main flock.
      lo_flock_of_ducks->add( lo_flock_of_mallards ).

      " All we do here is create a Quackologist named Bob and
      " set him as an observer of the flock.
      DATA(lo_dr_bob) = NEW lcl_quackologist( |Bob| ).
      lo_flock_of_ducks->lif_quackable~register_observer( lo_dr_bob ).

      " This time we'll just simulate the enitre flock.
      cl_demo_output=>write_text( |\n-- Whole Flock Simulation| ).
      simulate( lo_flock_of_ducks ).

      " Finally, let's give the Quackologist the data.
      cl_demo_output=>write_text(
        |\nThe ducks quacked { lcl_quack_counter=>quacks( ) } times| ).

    ELSE.
      " Here we let polymorphism do its magic: no matter what kind of
      " Quackable gets passed in, the simulate method asks it to quack.
      " Nothing needs to change here; a Flock is a Quackable!
      io_duck->quack( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
    |Patterns of Patterns - Compound Pattern (observer)| ).
  cl_demo_output=>line( ).
  lcl_duck_simulator=>main( ).
  cl_demo_output=>display( ).
