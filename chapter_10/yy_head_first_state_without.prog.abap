*&---------------------------------------------------------------------*
*&  Sample code for The State of Things - the State Pattern
*&    based on Head First Design Patterns: Chapter 10
*&---------------------------------------------------------------------*
REPORT yy_head_first_state_without.

*&---------------------------------------------------------------------*
*&  A high tech gumball machine controller implemented in ABAP
*&---------------------------------------------------------------------*
CLASS lcl_gumball_machine DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_gumball_count TYPE i,
      refill      IMPORTING iv_number_of_gumballs TYPE i,
      description RETURNING VALUE(rv_text) TYPE string,
      " These actions are the gumball machine's interface -
      " the things you can do with it.
      insert_quarter, eject_quarter, turn_crank.
  PRIVATE SECTION.
    " Dispense is an internal action the machine invokes on itself.
    METHODS dispense.
    CONSTANTS:
      " These are the four states; they match the states in Mighty
      " Gumball's state diagram. Here each state is represented as
      " a unique integer...
      c_sold_out    TYPE i VALUE 0,
      c_no_quarter  TYPE i VALUE 1,
      c_has_quarter TYPE i VALUE 2,
      c_sold        TYPE i VALUE 3.
    DATA:
      " ...and here's an instance variable that holds the current state.
      " We'll go ahead and set it to "Sold Out" since the machine will
      " be unfilled when it's first taken out of its box and turned on.
      mv_state TYPE i VALUE c_sold_out,
      " We have a second instance variable that keeps track of the
      " number of gumballs in the machine - initially it is empty.
      mv_gumball_count TYPE i VALUE 0.
ENDCLASS.

CLASS lcl_gumball_machine IMPLEMENTATION.
  METHOD constructor.
    " The constructor takes an initial inventory of gumballs.
    " If the inventory isn't zero, the machine enters state
    " NO_QUARTER, meaning it is waiting for someone to insert
    " a quarter, otherwise it stays in the SOLD_OUT state.
    mv_gumball_count = iv_gumball_count.
    mv_state = COND #( WHEN mv_gumball_count > 0
                 THEN c_no_quarter ELSE c_sold_out ).
  ENDMETHOD.
  " Now we start implementing the actions as methods...
  METHOD insert_quarter.
    " When a quarter is inserted, if...
    IF mv_state = c_has_quarter.
      " ...a quarter is already inserted we tell the customer...
      cl_demo_output=>write( |You can't insert another quarter| ).
    ELSEIF mv_state = c_no_quarter.
      " ...otherwise we accept the quarter and transition
      " to the HAS_QUARTER state.
      cl_demo_output=>write( |You inserted a quarter| ).
      mv_state = c_has_quarter.
    ELSEIF mv_state = c_sold_out.
      " And if the machine is sold out, we reject the quarter.
      cl_demo_output=>write( |You can't insert a quarter, | &
                             |the machine is sold out| ).
    ELSEIF mv_state = c_sold.
      " If the customer just bought a gumball he needs to wait until
      " the transaction is complete before inserting another quarter.
      cl_demo_output=>write( |Please wait, we're already | &
                             |giving you a gumball| ).
    ENDIF.
  ENDMETHOD.
  METHOD eject_quarter.
    " If the customer tries to remove a quarter...
    IF mv_state = c_has_quarter.
      " If there is a quarter, we return it and go back
      " to the NO_QUARTER state.
      cl_demo_output=>write( |Quarter returned| ).
      mv_state = c_no_quarter.
    ELSEIF mv_state = c_no_quarter.
      " Otherwise, if there isn't one we can't give it back.
      cl_demo_output=>write( |You haven't inserted a quarter| ).
    ELSEIF mv_state = c_sold.
      " If the customer just turned the crank, we can't give
      " a refund; he already has the gumball!
      cl_demo_output=>write( |Sorry, you already turned the crank| ).
    ELSEIF mv_state = c_sold_out.
      " You can't eject if the machine is sold out,
      " it doesn't accept quarters!
      cl_demo_output=>write( |You can't eject, you haven't | &
                             |inserted a quarter yet| ).
    ENDIF.
  ENDMETHOD.
  METHOD turn_crank.
    " The customer tries to turn the crank...
    IF mv_state = c_sold.
      " Someone's trying to cheat the machine.
      cl_demo_output=>write( |Turning twice doesn't get | &
                             |you another gumball!| ).
    ELSEIF mv_state = c_no_quarter.
      " We need a quarter first.
      cl_demo_output=>write( |You turned, but there's no quarter| ).
    ELSEIF mv_state = c_sold_out.
      " We can't deliver gumballs; there are none.
      cl_demo_output=>write( |You turned, but there are no gumballs| ).
    ELSEIF mv_state = c_has_quarter.
      " Success! They get a gumball. Change the state to SOLD and call
      " the machine's dispense() method.
      cl_demo_output=>write( |You turned...| ).
      mv_state = c_sold.
      dispense( ).
    ENDIF.
  ENDMETHOD.
  METHOD dispense.
    " Called to dispense a gumball.
    IF mv_state = c_sold.
      " We're in the SOLD state; give 'em a gumball!
      cl_demo_output=>write( |A gumball comes rolling out the slot| ).
      SUBTRACT 1 FROM mv_gumball_count.
      " Here's where we handle the "out of gumballs" condition:
      " If this was the last one, we set the machine's state to
      " SOLD_OUT; otherwise, we're back to not having a quarter.
      IF mv_gumball_count = 0.
        cl_demo_output=>write( |Oops, out of gumballs!| ).
        mv_state = c_sold_out.
      ELSE.
        mv_state = c_no_quarter.
      ENDIF.
    ELSEIF mv_state = c_no_quarter.
      " None of these should ever happen, but if they do,
      " we give 'em an error, not a gumball.
      cl_demo_output=>write( |You need to pay first| ).
    ELSEIF mv_state = c_sold_out.
      cl_demo_output=>write( |No gumball dispensed| ).
    ELSEIF mv_state = c_has_quarter.
      cl_demo_output=>write( |No gumball dispensed| ).
    ENDIF.
  ENDMETHOD.
  " And a few other methods like description() and refill()
  METHOD refill.
    CHECK iv_number_of_gumballs > 0.
    mv_gumball_count = mv_gumball_count + iv_number_of_gumballs.
    cl_demo_output=>write( |The gumball machine was just refilled; | &
                           |it's new count is: { mv_gumball_count }| ).
    mv_state = c_no_quarter.
  ENDMETHOD.
  METHOD description.
    rv_text = |Mighty Gumball, Inc.\n| &
              |ABAP-enabled Standing Gumball Model #2017\n| &
              |Inventory: { mv_gumball_count } gumball{ COND #(
                WHEN mv_gumball_count <> 1 THEN |s| ) }\n| &
              |Machine is { SWITCH #( mv_state
                WHEN c_sold_out    THEN |sold out|
                WHEN c_no_quarter  THEN |waiting for quarter|
                WHEN c_has_quarter THEN |waiting for turn of crank|
                WHEN c_sold        THEN |delivering a gumball| ) }\n|.
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
  METHOD main.

    " Load it up with five gumballs total.
    DATA(lo_gumball_machine) = NEW lcl_gumball_machine( 5 ).

    " Print out the state of the machine.
    cl_demo_output=>write_text( lo_gumball_machine->description( ) ).
    cl_demo_output=>line( ).

    lo_gumball_machine->insert_quarter( ). " Throw a quarter in...
    lo_gumball_machine->turn_crank( ).     " Turn the crank;
    " we should get our gumball.

    " Print out the state of the machine, again.
    cl_demo_output=>write_text( lo_gumball_machine->description( ) ).
    cl_demo_output=>line( ).

    lo_gumball_machine->insert_quarter( ). " Throw a quarter in...
    lo_gumball_machine->eject_quarter( ).  " Ask for it back.
    lo_gumball_machine->turn_crank( ).     " Turn the crank;
    " we shouldn't get a gumball.

    " Print out the state of the machine, again.
    cl_demo_output=>write_text( lo_gumball_machine->description( ) ).
    cl_demo_output=>line( ).

    lo_gumball_machine->insert_quarter( ). " Throw a quarter in...
    lo_gumball_machine->turn_crank( ).     " Turn the crank;
    " we should get our gumball.
    lo_gumball_machine->insert_quarter( ). " Throw a quarter in...
    lo_gumball_machine->turn_crank( ).     " Turn the crank;
    " we should get our gumball.
    lo_gumball_machine->eject_quarter( ).  " Ask for a quarter back
    " that we didn't put in.

    " Print out the state of the machine, again.
    cl_demo_output=>write_text( lo_gumball_machine->description( ) ).
    cl_demo_output=>line( ).

    lo_gumball_machine->insert_quarter( ). " Throw TWO quarters in...
    lo_gumball_machine->insert_quarter( ).
    lo_gumball_machine->turn_crank( ).     " Turn the crank;
    " we should get our gumball.
    lo_gumball_machine->insert_quarter( ). " Now for the stress testing.
    lo_gumball_machine->turn_crank( ).
    lo_gumball_machine->insert_quarter( ).
    lo_gumball_machine->turn_crank( ).

    " Print out the state of the machine, again.
    cl_demo_output=>write_text( lo_gumball_machine->description( ) ).
    cl_demo_output=>line( ).

    lo_gumball_machine->refill( 5 ).
    lo_gumball_machine->insert_quarter( ).
    lo_gumball_machine->turn_crank( ).

    " Print that machine state one more time.
    cl_demo_output=>write_text( lo_gumball_machine->description( ) ).
    cl_demo_output=>line( ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
    |The State of Things - the State Pattern (without using objects)| ).
  cl_demo_output=>line( ).
  lcl_gumball_machine_test_drive=>main( ).
  cl_demo_output=>display( ).
