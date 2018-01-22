*&---------------------------------------------------------------------*
*&  Sample code for The State of Things - the State Pattern
*&    based on Head First Design Patterns: Chapter 10
*&---------------------------------------------------------------------*
REPORT yy_head_first_state_winner.

*&---------------------------------------------------------------------*
*&  Here's the interface for all states. These methods map directly
*&  to actions that could happen to the Gumball Machine (these are
*&  the same methods as in the previous code).
*&---------------------------------------------------------------------*
INTERFACE lif_state.
  METHODS:
    insert_quarter, eject_quarter, turn_crank, dispense, refill,
    description RETURNING VALUE(rv_text) TYPE string.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  A high tech gumball machine controller definition in ABAP
*&---------------------------------------------------------------------*
CLASS lcl_gumball_machine DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_gumball_count TYPE i,
      refill      IMPORTING iv_number_of_gumballs TYPE i,
      description RETURNING VALUE(rv_text) TYPE string,
      " We are implementing the actions as methods...
      insert_quarter, eject_quarter, turn_crank, release_ball,
      " More methods here including getters for each State...
      no_quarter_state  RETURNING VALUE(ro_state) TYPE REF TO lif_state,
      has_quarter_state RETURNING VALUE(ro_state) TYPE REF TO lif_state,
      sold_out_state    RETURNING VALUE(ro_state) TYPE REF TO lif_state,
      sold_state        RETURNING VALUE(ro_state) TYPE REF TO lif_state,
      " Don't forget you have to add a getter method for Winner State.
      winner_state      RETURNING VALUE(ro_state) TYPE REF TO lif_state,
      gumball_count     RETURNING VALUE(rv_count) TYPE i,
      set_state         IMPORTING io_new_state TYPE REF TO lif_state.
  PRIVATE SECTION.
    DATA:
      " These are the four states; they match the states in Mighty
      " Gumball's state diagram. We update the code to use the new
      " classes rather than the static integers. The code is quite
      " similar, except that in one class we have constants and in
      " the other we have objects...
      mo_sold_out    TYPE REF TO lif_state,
      mo_no_quarter  TYPE REF TO lif_state,
      mo_has_quarter TYPE REF TO lif_state,
      mo_sold        TYPE REF TO lif_state,
      " All you need to add here is the new Winner State and then
      " initialize it in the constructor.
      mo_winner      TYPE REF TO lif_state,
      " Here's the instance variable that is going to keep track of
      " the current state we're in. We'll set it in the constructor.
      mo_state TYPE REF TO lif_state,
      " We have a second instance variable that keeps track of the
      " number of gumballs in the machine - initially it is empty.
      mv_gumball_count TYPE i VALUE 0.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete State object: No Quarter
*&---------------------------------------------------------------------*
CLASS lcl_no_quarter_state DEFINITION FINAL.
  PUBLIC SECTION.
    " First we need to implement the State interface.
    INTERFACES lif_state.
    METHODS constructor
      IMPORTING io_machine TYPE REF TO lcl_gumball_machine.
  PRIVATE SECTION.
    DATA mo_machine TYPE REF TO lcl_gumball_machine.
ENDCLASS.

CLASS lcl_no_quarter_state IMPLEMENTATION.
  METHOD constructor.
    " We get passed a reference to the Gumball Machine through the
    " constructor. We just stash this in an instance variable.
    mo_machine = io_machine.
  ENDMETHOD.
  METHOD lif_state~insert_quarter.
    " If someone inserts a quarter, we print a message saying
    " the quarter was accepted and then change the machine's
    " state to the Has Quarter State.
    cl_demo_output=>write( |You inserted a quarter| ).
    " You'll see how these work in just a sec...
    mo_machine->set_state( mo_machine->has_quarter_state( ) ).
  ENDMETHOD.
  METHOD lif_state~eject_quarter.
    " You can't get money back if you never gave it to us!
    cl_demo_output=>write( |You haven't inserted a quarter| ).
  ENDMETHOD.
  METHOD lif_state~turn_crank.
    " And you can't get a gumball if you don't pay us.
    cl_demo_output=>write( |You turned, but there's no quarter| ).
  ENDMETHOD.
  METHOD lif_state~dispense.
    " We can't be dispensing gumballs without payment.
    cl_demo_output=>write( |You need to pay first| ).
  ENDMETHOD.
  METHOD lif_state~refill.
    " In every state except the Sold Out, this method does nothing.
    RETURN.
  ENDMETHOD.
  METHOD lif_state~description.
    rv_text = |waiting for quarter|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete State object: Has Quarter
*&---------------------------------------------------------------------*
CLASS lcl_has_quarter_state DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_state.
    METHODS constructor
      IMPORTING io_machine TYPE REF TO lcl_gumball_machine.
  PRIVATE SECTION.
    DATA:
      mo_machine       TYPE REF TO lcl_gumball_machine,
      mo_random_winner TYPE REF TO cl_abap_random_int.
ENDCLASS.

CLASS lcl_has_quarter_state IMPLEMENTATION.
  METHOD constructor.
    " When the state is instantiated we pass it a reference to the
    " Gumball Machine. This will be used to transition the machine
    " to a different state.
    mo_machine = io_machine.
    " First we add a random number generator to generate
    " the 10% chance of winning...
    mo_random_winner = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( ) min = 1 max = 10 ).
  ENDMETHOD.
  METHOD lif_state~insert_quarter.
    " An inappropriate action for this state.
    cl_demo_output=>write( |You can't insert another quarter| ).
  ENDMETHOD.
  METHOD lif_state~eject_quarter.
    " Return the customer's quarter and transition back
    " to the No Quarter State.
    cl_demo_output=>write( |Quarter returned| ).
    mo_machine->set_state( mo_machine->no_quarter_state( ) ).
  ENDMETHOD.
  METHOD lif_state~turn_crank.
    " When the crank is turned we transition the machine to the
    " Sold state by calling its set_state() method and passing it
    " the Sold State object. The Sold State object is retrieved by
    " the sold_state() getter method (there is one of these getter
    " methods for each state object).
    cl_demo_output=>write( |You turned...| ).
    " ... then we determine if this customer won.
    DATA(lv_winner) = mo_random_winner->get_next( ).
    " If they won, and there's enough gumballs left for them to
    " get two, we go to the Winner State; otherwise, we go to
    " the Sold State (just like we always did).
    IF lv_winner = 1 AND mo_machine->gumball_count( ) > 1.
      mo_machine->set_state( mo_machine->winner_state( ) ).
    ELSE.
      mo_machine->set_state( mo_machine->sold_state( ) ).
    ENDIF.
  ENDMETHOD.
  METHOD lif_state~dispense.
    " Another inappropriate action for this state.
    cl_demo_output=>write( |No gumball dispensed| ).
  ENDMETHOD.
  METHOD lif_state~refill.
    RETURN.
  ENDMETHOD.
  METHOD lif_state~description.
    rv_text = |waiting for turn of crank|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete State object: Sold
*&---------------------------------------------------------------------*
CLASS lcl_sold_state DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_state.
    METHODS constructor
      IMPORTING io_machine TYPE REF TO lcl_gumball_machine.
  PRIVATE SECTION.
    DATA mo_machine TYPE REF TO lcl_gumball_machine.
ENDCLASS.

CLASS lcl_sold_state IMPLEMENTATION.
  METHOD constructor.
    mo_machine = io_machine.
  ENDMETHOD.
  " Here are all the inappropriate actions for this state.
  METHOD lif_state~insert_quarter.
    cl_demo_output=>write( |Please wait, we're already | &
                           |giving you a gumball| ).
  ENDMETHOD.
  METHOD lif_state~eject_quarter.
    cl_demo_output=>write( |Sorry, you already turned the crank| ).
  ENDMETHOD.
  METHOD lif_state~turn_crank.
    cl_demo_output=>write( |Turning twice doesn't get | &
                           |you another gumball!| ).
  ENDMETHOD.
  " And here's where the real work begins...
  METHOD lif_state~dispense.
    " We're in the Sold State, which means the customer paid. So, we
    " first need to ask the machine to release a gumball.
    mo_machine->release_ball( ).
    " Then we ask the machine what the gumball count is, and either
    " transition to the No Quarter State or the Sold Out State.
    IF mo_machine->gumball_count( ) >  0.
      mo_machine->set_state( mo_machine->no_quarter_state( ) ).
    ELSE.
      cl_demo_output=>write( |Oops, out of gumballs!| ).
      mo_machine->set_state( mo_machine->sold_out_state( ) ).
    ENDIF.
  ENDMETHOD.
  METHOD lif_state~refill.
    RETURN.
  ENDMETHOD.
  METHOD lif_state~description.
    rv_text = |dispensing a gumball|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete State object: Sold Out
*&---------------------------------------------------------------------*
CLASS lcl_sold_out_state DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_state.
    METHODS constructor
      IMPORTING io_machine TYPE REF TO lcl_gumball_machine.
  PRIVATE SECTION.
    DATA mo_machine TYPE REF TO lcl_gumball_machine.
ENDCLASS.

CLASS lcl_sold_out_state IMPLEMENTATION.
  METHOD constructor.
    mo_machine = io_machine.
  ENDMETHOD.
  " In the Sold Out State, we really can't do anything until
  " someone refills the Gumball Machine.
  METHOD lif_state~insert_quarter.
    cl_demo_output=>write( |You can't insert a quarter, | &
                           |the machine is sold out| ).
  ENDMETHOD.
  METHOD lif_state~eject_quarter.
    cl_demo_output=>write( |You can't eject, you haven't | &
                           |inserted a quarter yet| ).
  ENDMETHOD.
  METHOD lif_state~turn_crank.
    cl_demo_output=>write( |You turned, but there are no gumballs| ).
  ENDMETHOD.
  METHOD lif_state~dispense.
    cl_demo_output=>write( |No gumball dispensed| ).
  ENDMETHOD.
  METHOD lif_state~refill.
    " In Sold Out State, refill() transitions to No Quarter State.
    mo_machine->set_state( mo_machine->no_quarter_state( ) ).
  ENDMETHOD.
  METHOD lif_state~description.
    rv_text = |sold out|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete State object: Winner
*&---------------------------------------------------------------------*
CLASS lcl_winner_state DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_state.
    METHODS constructor
      IMPORTING io_machine TYPE REF TO lcl_gumball_machine.
  PRIVATE SECTION.
    DATA mo_machine TYPE REF TO lcl_gumball_machine.
ENDCLASS.

CLASS lcl_winner_state IMPLEMENTATION.
  " Constructor, insert_quarter, eject_quarter, and turn_crank
  " methods are implemented just like Sold State.
  METHOD constructor.
    mo_machine = io_machine.
  ENDMETHOD.
  METHOD lif_state~insert_quarter.
    cl_demo_output=>write( |Please wait, we're already | &
                           |giving you a Gumball| ).
  ENDMETHOD.
  METHOD lif_state~eject_quarter.
    cl_demo_output=>write( |Please wait, we're already | &
                           |giving you a Gumball| ).
  ENDMETHOD.
  METHOD lif_state~turn_crank.
    cl_demo_output=>write( |Turning again doesn't get you | &
                           |another gumball!| ).
  ENDMETHOD.
  METHOD lif_state~dispense.
    " Here we release two gumballs and then either go to the
    " No Quarter State or the Sold Out State.
    mo_machine->release_ball( ).
    IF mo_machine->gumball_count( ) = 0.
      mo_machine->set_state( mo_machine->sold_out_state( ) ).
      RETURN.
    ENDIF.
    " If we have a second gumball we release it.
    mo_machine->release_ball( ).
    " If we were able to release two gumballs, we let
    " they user know they were a winner.
    cl_demo_output=>write( |YOU'RE A WINNER! You got two | &
                           |gumballs for your quarter| ).
    IF mo_machine->gumball_count( ) >  0.
      mo_machine->set_state( mo_machine->no_quarter_state( ) ).
    ELSE.
      cl_demo_output=>write( |Oops, out of gumballs!| ).
      mo_machine->set_state( mo_machine->sold_out_state( ) ).
    ENDIF.
  ENDMETHOD.
  METHOD lif_state~refill.
    RETURN.
  ENDMETHOD.
  METHOD lif_state~description.
    rv_text = |despensing two gumballs for your quarter, | &
              |because YOU'RE A WINNER!|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  A high tech gumball machine controller implementation in ABAP
*&---------------------------------------------------------------------*
CLASS lcl_gumball_machine IMPLEMENTATION.
  METHOD constructor.
    " All the State objects are created and assigned in the constructor.
    mo_sold_out = NEW lcl_sold_out_state( me ).
    mo_no_quarter = NEW lcl_no_quarter_state( me ).
    mo_has_quarter = NEW lcl_has_quarter_state( me ).
    mo_sold = NEW lcl_sold_state( me ).
    mo_winner = NEW lcl_winner_state( me ).
    " The constructor takes an initial inventory of gumballs.
    mv_gumball_count = iv_gumball_count.
    " This now holds a State object, not an integer. If there are
    " more than 0 gumballs we set the state to the No Quarter State;
    " otherwise, we start in the Sold Out State.
    mo_state = COND #( WHEN mv_gumball_count > 0
                 THEN mo_no_quarter ELSE mo_sold_out ).
  ENDMETHOD.
  " Now we start implementing the actions as methods...
  METHOD insert_quarter.
    mo_state->insert_quarter( ).
  ENDMETHOD.
  METHOD eject_quarter.
    " These are VERY EASY to implement now. We just delegate to
    " the current state object.
    mo_state->eject_quarter( ).
  ENDMETHOD.
  METHOD turn_crank.
    mo_state->turn_crank( ).
    " Note that we don't need an action method for dispense() in
    " Gumball Machine because it's just an internal action; a user
    " can't ask the machine to dispense directly. But we do call
    " dispense() on the State object from the turn_crank() method.
    mo_state->dispense( ).
  ENDMETHOD.
  METHOD release_ball.
    " The machine supports a release_ball() helper method that releases
    " the ball and decrements mv_gumball_count instance variable.
    cl_demo_output=>write( |A gumball comes rolling out the slot| ).
    IF gumball_count( ) <> 0.
      SUBTRACT 1 FROM mv_gumball_count.
    ENDIF.
  ENDMETHOD.
  METHOD set_state.
    " This method allows other objects (like our State objects) to
    " transition the machine to a different state.
    mo_state = io_new_state.
  ENDMETHOD.
  " This class includes methods like no_quarter_state() for getting
  " each state object, and gumball_count() for getting the count.
  METHOD gumball_count.
    rv_count = mv_gumball_count.
  ENDMETHOD.
  METHOD no_quarter_state.
    ro_state = mo_no_quarter.
  ENDMETHOD.
  METHOD has_quarter_state.
    ro_state = mo_has_quarter.
  ENDMETHOD.
  METHOD sold_state.
    ro_state = mo_sold.
  ENDMETHOD.
  METHOD sold_out_state.
    ro_state = mo_sold_out.
  ENDMETHOD.
  METHOD winner_state.
    ro_state = mo_winner.
  ENDMETHOD.
  " And a few other methods like description() and refill()
  METHOD refill.
    CHECK iv_number_of_gumballs > 0.
    mv_gumball_count = mv_gumball_count + iv_number_of_gumballs.
    cl_demo_output=>write( |The gumball machine was just refilled; | &
                           |it's new count is: { gumball_count( ) }| ).
    mo_state->refill( ).
  ENDMETHOD.
  METHOD description.
    rv_text = |Mighty Gumball, Inc.\n| &
              |ABAP-enabled Standing Gumball Model #2017\n| &
              |Inventory: { gumball_count( ) } gumball{ COND #(
                WHEN gumball_count( ) <> 1 THEN |s| ) }\n| &
              |Machine is { mo_state->description( ) }\n|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  In-house gumball machine test harness
*&---------------------------------------------------------------------*
CLASS lcl_gumball_machine_test_drive DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS lcl_gumball_machine_test_drive IMPLEMENTATION.
  " This code really hasn't changed; we just shortened it a bit.
  METHOD main.

    " Once, again, start with a gumball machine with 5 gumballs.
    DATA(lo_gumball_machine) = NEW lcl_gumball_machine( 5 ).

    cl_demo_output=>write_text( lo_gumball_machine->description( ) ).
    cl_demo_output=>line( ).

    " We want to get a winning state, so we just keep pumping in
    " those quarters and turning the crank. We print out the state
    " of the gumball machine every so often...
    DO 4 TIMES.
      lo_gumball_machine->insert_quarter( ).
      lo_gumball_machine->turn_crank( ).
      lo_gumball_machine->insert_quarter( ).
      lo_gumball_machine->turn_crank( ).

      cl_demo_output=>write_text( lo_gumball_machine->description( ) ).
      cl_demo_output=>line( ).

      lo_gumball_machine->insert_quarter( ).
      lo_gumball_machine->turn_crank( ).
      lo_gumball_machine->insert_quarter( ).
      lo_gumball_machine->turn_crank( ).

      cl_demo_output=>write_text( lo_gumball_machine->description( ) ).
      cl_demo_output=>line( ).

      lo_gumball_machine->refill( 6 ).
      lo_gumball_machine->insert_quarter( ).
      lo_gumball_machine->turn_crank( ).

      cl_demo_output=>write_text( lo_gumball_machine->description( ) ).
      cl_demo_output=>line( ).
    ENDDO.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
    |The State of Things - the State Pattern (added Winner state)| ).
  cl_demo_output=>line( ).
  lcl_gumball_machine_test_drive=>main( ).
  cl_demo_output=>display( ).
