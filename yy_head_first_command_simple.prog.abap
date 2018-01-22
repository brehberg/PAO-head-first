*&---------------------------------------------------------------------*
*&  Sample for Encapsulating Invocation - the Command Pattern
*&    based on Head First Design Patterns: Chapter 6
*&---------------------------------------------------------------------*
REPORT yy_head_first_command_simple.

*&---------------------------------------------------------------------*
*&  Here's the Command interface. All we need is one method execute()
*&---------------------------------------------------------------------*
INTERFACE lif_command.
  METHODS execute.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Here's a Light object we would like to control.
*&---------------------------------------------------------------------*
CLASS lcl_light DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS: on, off.
ENDCLASS.

CLASS lcl_light IMPLEMENTATION.
  METHOD on.
    cl_demo_output=>write( |Light is On| ).
  ENDMETHOD.
  METHOD off.
    cl_demo_output=>write( |Light is Off| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Here's a command to turn a light on.
*&---------------------------------------------------------------------*
CLASS lcl_light_on_command DEFINITION FINAL.
  PUBLIC SECTION.
    " This is a command, so we need to implement the Command interface.
    INTERFACES lif_command.
    METHODS constructor IMPORTING io_light TYPE REF TO lcl_light.
  PRIVATE SECTION.
    DATA mo_light TYPE REF TO lcl_light.
ENDCLASS.

CLASS lcl_light_on_command IMPLEMENTATION.
  METHOD constructor.
    " The constructor is passed the specific light that this command is
    " going to control - say the living room light - and stashes it in
    " the mo_light instance variable. When execute gets called, this is
    " the light object that is going to be the Receiver of the request.
    mo_light = io_light.
  ENDMETHOD.
  METHOD lif_command~execute.
    " The execute method calls the on() method on the receiving object,
    " which is the light we are controlling.
    mo_light->on( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  And here's a command to turn a light off.
*&---------------------------------------------------------------------*
CLASS lcl_light_off_command DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_command.
    METHODS constructor IMPORTING io_light TYPE REF TO lcl_light.
  PRIVATE SECTION.
    DATA mo_light TYPE REF TO lcl_light.
ENDCLASS.

CLASS lcl_light_off_command IMPLEMENTATION.
  METHOD constructor.
    mo_light = io_light.
  ENDMETHOD.
  METHOD lif_command~execute.
    " The Light Off Command works exactly the same way as the Light On
    " Command, except that we are binding the receiver to a different
    " action: the off() method.
    mo_light->off( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Here's a Garage Door object we would like to control.
*&---------------------------------------------------------------------*
CLASS lcl_garage_door DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS: up, down, light_on, light_off.
ENDCLASS.

CLASS lcl_garage_door IMPLEMENTATION.
  METHOD up.
    cl_demo_output=>write( |Garage Door is Open| ).
  ENDMETHOD.
  METHOD down.
    cl_demo_output=>write( |Garage Door is Closed| ).
  ENDMETHOD.
  METHOD light_on.
    cl_demo_output=>write( |Garage light is on| ).
  ENDMETHOD.
  METHOD light_off.
    cl_demo_output=>write( |Garage light is off| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Here's a command to open a garage door.
*&---------------------------------------------------------------------*
CLASS lcl_garage_door_up_command DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_command.
    METHODS constructor IMPORTING io_door TYPE REF TO lcl_garage_door.
  PRIVATE SECTION.
    DATA mo_garage_door TYPE REF TO lcl_garage_door.
ENDCLASS.

CLASS lcl_garage_door_up_command IMPLEMENTATION.
  METHOD constructor.
    mo_garage_door = io_door.
  ENDMETHOD.
  METHOD lif_command~execute.
    mo_garage_door->up( ).
    mo_garage_door->light_on( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  And here's a command to close it.
*&---------------------------------------------------------------------*
CLASS lcl_garage_door_down_command DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_command.
    METHODS constructor IMPORTING io_door TYPE REF TO lcl_garage_door.
  PRIVATE SECTION.
    DATA mo_garage_door TYPE REF TO lcl_garage_door.
ENDCLASS.

CLASS lcl_garage_door_down_command IMPLEMENTATION.
  METHOD constructor.
    mo_garage_door = io_door.
  ENDMETHOD.
  METHOD lif_command~execute.
    mo_garage_door->down( ).
    mo_garage_door->light_off( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  This is the invoker.
*&---------------------------------------------------------------------*
CLASS lcl_simple_remote_control DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      set_command IMPORTING io_command TYPE REF TO lif_command,
      button_was_pressed.
  PRIVATE SECTION.
    " We have a slot to hold our command, which will control one device.
    DATA mo_slot TYPE REF TO lif_command.
ENDCLASS.

CLASS lcl_simple_remote_control IMPLEMENTATION.
  METHOD set_command.
    " We have a method for setting the command the slot is going to
    " control. This could be called multiple times if the client of
    " this code wanted to change the behavior of the remote button.
    mo_slot = io_command.
  ENDMETHOD.
  METHOD button_was_pressed.
    " This method is called when the button is pressed. All we do is
    " take the current command bound to the slot and call execute()
    CHECK mo_slot IS BOUND.
    mo_slot->execute( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  This is our Client in Command Pattern-speak.
*&---------------------------------------------------------------------*
CLASS lcl_remote_control_test DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS lcl_remote_control_test IMPLEMENTATION.
  METHOD main.

    " The remote is our Invoker; it will be passed a command
    " object that can be used to make requests.
    DATA(lo_remote) = NEW lcl_simple_remote_control( ).

    " Now we create a Light object and Garage Door object. These
    " will be the Receivers of the requests.
    DATA(lo_light) = NEW lcl_light( ).
    DATA(lo_door) = NEW lcl_garage_door( ).

    " Here, we create all the commands and pass the corresponding
    " Receivers to them.
    DATA(lo_light_on) = NEW lcl_light_on_command( lo_light ).
    DATA(lo_light_off) = NEW lcl_light_off_command( lo_light ).
    DATA(lo_garage_up) = NEW lcl_garage_door_up_command( lo_door ).
    DATA(lo_garage_down) = NEW lcl_garage_door_down_command( lo_door ).

    " Now, pass the command to the Invoker.
    lo_remote->set_command( lo_light_on ).
    " And then we simulate the button being pressed.
    cl_demo_output=>write_text( |Push the remote button...| ).
    lo_remote->button_was_pressed( ).
    cl_demo_output=>line( ).

    lo_remote->set_command( lo_light_off ).
    cl_demo_output=>write_text( |Push the remote button...| ).
    lo_remote->button_was_pressed( ).
    cl_demo_output=>line( ).

    lo_remote->set_command( lo_garage_up ).
    cl_demo_output=>write_text( |Push the remote button...| ).
    lo_remote->button_was_pressed( ).
    cl_demo_output=>line( ).

    lo_remote->set_command( lo_garage_down ).
    cl_demo_output=>write_text( |Push the remote button...| ).
    lo_remote->button_was_pressed( ).
    cl_demo_output=>line( ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
    |Encapsulating Invocation - the Command Pattern (simple)| ).
  cl_demo_output=>line( ).
  lcl_remote_control_test=>main( ).
  cl_demo_output=>display( ).
