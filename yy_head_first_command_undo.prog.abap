*&---------------------------------------------------------------------*
*&  Sample for Encapsulating Invocation - the Command Pattern
*&    based on Head First Design Patterns: Chapter 6
*&---------------------------------------------------------------------*
REPORT yy_head_first_command_undo.

*&---------------------------------------------------------------------*
*&  Here's the Command interface with method called execute()
*&---------------------------------------------------------------------*
INTERFACE lif_command.
  METHODS: execute, undo.      " And here's the new undo() method.
  TYPES tt_commands TYPE STANDARD TABLE
    OF REF TO lif_command WITH EMPTY KEY.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Here's a Light object we would like to control.
*&---------------------------------------------------------------------*
CLASS lcl_light DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_location TYPE string,
      on, off.
  PRIVATE SECTION.
    DATA mv_location TYPE string.
ENDCLASS.

CLASS lcl_light IMPLEMENTATION.
  METHOD constructor.
    mv_location = iv_location.
  ENDMETHOD.
  METHOD on.
    cl_demo_output=>write( |{ mv_location } light is on| ).
  ENDMETHOD.
  METHOD off.
    cl_demo_output=>write( |{ mv_location } light is off| ).
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
  METHOD lif_command~undo.
    " execute() turns the light on, so undo() simply turns it back off.
    mo_light->off( ).
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
    mo_light->off( ).
  ENDMETHOD.
  METHOD lif_command~undo.
    mo_light->on( ).       " And here, undo() turns the light back on.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Here's a vendor Ceiling Fan object to control.
*&---------------------------------------------------------------------*
CLASS lcl_ceiling_fan DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_location TYPE string,
      " These methods set the speed of the ceiling fan.
      high, medium, low, off,
      " We can get the current speed of the ceiling fan using speed().
      speed RETURNING VALUE(rv_speed) TYPE i.
    CONSTANTS:
      c_high   TYPE i VALUE 3,
      c_medium TYPE i VALUE 2,
      c_low    TYPE i VALUE 1,
      c_off    TYPE i VALUE 0.
  PRIVATE SECTION.
    DATA:
      mv_location TYPE string,
      " Notice that the Ceiling Fan class holds local state
      " representing the speed of the ceiling fan.
      mv_speed TYPE i.
ENDCLASS.

CLASS lcl_ceiling_fan IMPLEMENTATION.
  METHOD constructor.
    mv_location = iv_location.
    mv_speed = c_off.
  ENDMETHOD.
  METHOD high.
    mv_speed = c_high.  " turns the ceiling fan on to high
    cl_demo_output=>write( |{ mv_location } ceiling fan is on high| ).
  ENDMETHOD.
  METHOD medium.
    mv_speed = c_medium.  " turns the ceiling fan on to medium
    cl_demo_output=>write( |{ mv_location } ceiling fan is on medium| ).
  ENDMETHOD.
  METHOD low.
    mv_speed = c_low.  " turns the ceiling fan on to low
    cl_demo_output=>write( |{ mv_location } ceiling fan is on low| ).
  ENDMETHOD.
  METHOD off.
    mv_speed = c_off.  " turns the ceiling fan off
    cl_demo_output=>write( |{ mv_location } ceiling fan is off| ).
  ENDMETHOD.
  METHOD speed.
    rv_speed = mv_speed.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Typically, we need to manage a bit of state to implement undo.
*&---------------------------------------------------------------------*
*&  Here is an abstract base class for all Ceiling Fan command object.
*&  It needs to track the last speed setting of the fan and, if undo()
*&  method is called, restore the fan to its previous setting.
*&---------------------------------------------------------------------*
CLASS lcl_ceiling_fan_command DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES lif_command ABSTRACT METHODS execute.
    METHODS constructor IMPORTING io_fan TYPE REF TO lcl_ceiling_fan.
  PROTECTED SECTION.
    DATA:
      mo_ceiling_fan TYPE REF TO lcl_ceiling_fan,
      " We've added local state to keep track of the previous fan speed.
      mv_previous_speed TYPE i.
ENDCLASS.

CLASS lcl_ceiling_fan_command IMPLEMENTATION.
  METHOD constructor.
    mo_ceiling_fan = io_fan.
  ENDMETHOD.
  METHOD lif_command~undo.
    " To undo, we set the speed of the fan back to its previous speed.
    CASE mv_previous_speed.
      WHEN lcl_ceiling_fan=>c_high.
        mo_ceiling_fan->high( ).
      WHEN lcl_ceiling_fan=>c_medium.
        mo_ceiling_fan->medium( ).
      WHEN lcl_ceiling_fan=>c_low.
        mo_ceiling_fan->low( ).
      WHEN OTHERS.
        mo_ceiling_fan->off( ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Here's a command to turn a ceiling fan on high.
*&---------------------------------------------------------------------*
CLASS lcl_ceiling_fan_high_command DEFINITION FINAL
  INHERITING FROM lcl_ceiling_fan_command.
  PUBLIC SECTION.
    METHODS lif_command~execute REDEFINITION.
ENDCLASS.

CLASS lcl_ceiling_fan_high_command IMPLEMENTATION.
  METHOD lif_command~execute.
    " In execute, before we change the speed of the fan, we need to
    " first record its previous state, just in case we need to undo.
    mv_previous_speed = mo_ceiling_fan->speed( ).
    mo_ceiling_fan->high( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Here's a command to turn a ceiling fan on medium.
*&---------------------------------------------------------------------*
CLASS lcl_ceiling_fan_medium_command DEFINITION FINAL
  INHERITING FROM lcl_ceiling_fan_command.
  PUBLIC SECTION.
    METHODS lif_command~execute REDEFINITION.
ENDCLASS.

CLASS lcl_ceiling_fan_medium_command IMPLEMENTATION.
  METHOD lif_command~execute.
    mv_previous_speed = mo_ceiling_fan->speed( ).
    mo_ceiling_fan->medium( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Here's a command to turn a ceiling fan on low.
*&---------------------------------------------------------------------*
CLASS lcl_ceiling_fan_low_command DEFINITION FINAL
  INHERITING FROM lcl_ceiling_fan_command.
  PUBLIC SECTION.
    METHODS lif_command~execute REDEFINITION.
ENDCLASS.

CLASS lcl_ceiling_fan_low_command IMPLEMENTATION.
  METHOD lif_command~execute.
    mv_previous_speed = mo_ceiling_fan->speed( ).
    mo_ceiling_fan->low( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  And here's a command to turn it off.
*&---------------------------------------------------------------------*
CLASS lcl_ceiling_fan_off_command DEFINITION FINAL
  INHERITING FROM lcl_ceiling_fan_command.
  PUBLIC SECTION.
    METHODS lif_command~execute REDEFINITION.
ENDCLASS.

CLASS lcl_ceiling_fan_off_command IMPLEMENTATION.
  METHOD lif_command~execute.
    mv_previous_speed = mo_ceiling_fan->speed( ).
    mo_ceiling_fan->off( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  This No Command object is an example of the null object pattern.
*&---------------------------------------------------------------------*
CLASS lcl_no_command DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_command.
ENDCLASS.

CLASS lcl_no_command IMPLEMENTATION.
  METHOD lif_command~execute.
    RETURN.
  ENDMETHOD.
  METHOD lif_command~undo.
    RETURN.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  This is the invoker.
*&---------------------------------------------------------------------*
CLASS lcl_remote_control_with_undo DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor,
      " The set_command() method takes a slot position and an On and
      " Off command to be stored in that slot.
      set_command IMPORTING iv_slot        TYPE i
                            io_on_command  TYPE REF TO lif_command
                            io_off_command TYPE REF TO lif_command,
      " When an On or Off button is pressed, the hardware takes care
      " of calling either the corresponding on_button_was_pushed() of
      " off_button_was_pushed() methods.
      on_button_was_pushed IMPORTING iv_slot TYPE i,
      off_button_was_pushed IMPORTING iv_slot TYPE i,
      undo_button_was_pushed,
      " We've created method description() to print out each slot and
      " its corresponding command. You'll see us use this when we test.
      description RETURNING VALUE(rv_text) TYPE string.
  PRIVATE SECTION.
    DATA:
      " This time around the remote is going to handle seven On and Off
      " commands, which we'll hold in corresponding internal tables.
      mt_on_commands  TYPE lif_command=>tt_commands,
      mt_off_commands TYPE lif_command=>tt_commands,
      " This is where we'll stash the last command executed for
      " the undo button.
      mo_undo_command TYPE REF TO lif_command.
    METHODS get_class_name
      IMPORTING io_object      TYPE REF TO object
      RETURNING VALUE(rv_name) TYPE string.
ENDCLASS.

CLASS lcl_remote_control_with_undo IMPLEMENTATION.
  METHOD constructor.
    " In the constructor all we need to do is instantiate a No Command
    " object and initialize the on and off tables of commands.
    DATA(lo_no_command) = NEW lcl_no_command( ).
    DO 7 TIMES.
      INSERT lo_no_command INTO TABLE mt_off_commands.
      INSERT lo_no_command INTO TABLE mt_on_commands.
    ENDDO.
    " Just like the other slots, undo starts off with a No Command, so
    " pressing undo before any other button won't do anything at all.
    mo_undo_command = lo_no_command.
  ENDMETHOD.
  METHOD set_command.
    " This puts the commands in the on and off tables for later use.
    mt_on_commands[ iv_slot ] = io_on_command.
    mt_off_commands[ iv_slot ] = io_off_command.
  ENDMETHOD.
  METHOD on_button_was_pushed.
    " for ABAP 7.5 use:  mt_on_commands[ iv_slot ]->execute( ).
    CAST lif_command( mt_on_commands[ iv_slot ] )->execute( ).
    " When a button is pressed, we take the command and execute it;
    " then we save a reference to it in the mo_undo_command instance
    " variable. We do this for both "on" commands and "off" commands.
    mo_undo_command = mt_on_commands[ iv_slot ].
  ENDMETHOD.
  METHOD off_button_was_pushed.
    " for ABAP 7.5 use:  mt_off_commands[ iv_slot ]->execute( ).
    CAST lif_command( mt_off_commands[ iv_slot ] )->execute( ).
    mo_undo_command = mt_off_commands[ iv_slot ].
  ENDMETHOD.
  METHOD undo_button_was_pushed.
    " When the undo button is pressed, we invoke the undo() method
    " of the command stored in mo_undo_command. This reverses the
    " operation of the last command executed.
    mo_undo_command->undo( ).
  ENDMETHOD.
  METHOD description.
    CONSTANTS c_w TYPE i VALUE 30.
    rv_text = REDUCE string(
      INIT text = |----------------- Remote Control -----------------\n|
      FOR i = 1 UNTIL i > lines( mt_on_commands )
      NEXT text = text && |[slot { i }] | &
           |{ get_class_name( mt_on_commands[ i ] ) WIDTH = c_w }\t| &
           |{ get_class_name( mt_off_commands[ i ] ) WIDTH = c_w }\n| )
      && |[undo] { get_class_name( mo_undo_command ) WIDTH = c_w }\n|.
  ENDMETHOD.
  METHOD get_class_name.
    rv_name = to_lower( cl_abap_typedescr=>describe_by_object_ref(
      io_object )->get_relative_name( ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  This is our Client in Command Pattern-speak.
*&---------------------------------------------------------------------*
CLASS lcl_remote_loader DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS lcl_remote_loader IMPLEMENTATION.
  METHOD main.

    DATA(lo_remote) = NEW lcl_remote_control_with_undo( ).

    " Create a Light, and our new undo() enabled Light On
    " and Off commands.
    DATA(lo_living_room_light) = NEW lcl_light( |Living Room| ).
    DATA(lo_living_room_light_on) =
      NEW lcl_light_on_command( lo_living_room_light ).
    DATA(lo_living_room_light_off) =
      NEW lcl_light_off_command( lo_living_room_light ).

    " Add the light commands to the remote in slot 1
    lo_remote->set_command(
      iv_slot = 1
      io_on_command = lo_living_room_light_on
      io_off_command = lo_living_room_light_off ).

    " Turn the light on, then off and then undo.
    cl_demo_output=>write_text( |Push On/Off buttons for slot 1...| ).
    lo_remote->on_button_was_pushed( 1 ).
    lo_remote->off_button_was_pushed( 1 ).
    cl_demo_output=>write( lo_remote->description( ) ).
    cl_demo_output=>write_text( |Push the undo button...| ).
    lo_remote->undo_button_was_pushed( ).
    cl_demo_output=>line( ).

    " Then, turn the light off, back on and undo.
    cl_demo_output=>write_text( |Push Off/On buttons for slot 1...| ).
    lo_remote->off_button_was_pushed( 1 ).
    lo_remote->on_button_was_pushed( 1 ).
    cl_demo_output=>write( lo_remote->description( ) ).
    cl_demo_output=>write_text( |Push the undo button...| ).
    lo_remote->undo_button_was_pushed( ).
    cl_demo_output=>line( ).

    " Here we instantiate a Ceiling Fan and four commands:
    " high, medium, low, and off.
    DATA(lo_ceiling_fan) = NEW lcl_ceiling_fan( |Living Room| ).
    DATA(lo_ceiling_fan_high) =
      NEW lcl_ceiling_fan_high_command( lo_ceiling_fan ).
    DATA(lo_ceiling_fan_medium) =
      NEW lcl_ceiling_fan_medium_command( lo_ceiling_fan ).
    DATA(lo_ceiling_fan_low) =
      NEW lcl_ceiling_fan_low_command( lo_ceiling_fan ).
    DATA(lo_ceiling_fan_off) =
      NEW lcl_ceiling_fan_off_command( lo_ceiling_fan ).

    " Here we put low in slot 1, medium in slot 2, and high in slot 3.
    " We also load up the off command.
    lo_remote->set_command(
      iv_slot = 1
      io_on_command = lo_ceiling_fan_low
      io_off_command = lo_ceiling_fan_off ).
    lo_remote->set_command(
      iv_slot = 2
      io_on_command = lo_ceiling_fan_medium
      io_off_command = lo_ceiling_fan_off ).
    lo_remote->set_command(
      iv_slot = 3
      io_on_command = lo_ceiling_fan_high
      io_off_command = lo_ceiling_fan_off ).

    " First, turn the fan on medium. Then turn it off.
    cl_demo_output=>write_text( |Push On/Off buttons for slot 2...| ).
    lo_remote->on_button_was_pushed( 2 ).
    lo_remote->off_button_was_pushed( 2 ).
    cl_demo_output=>write( lo_remote->description( ) ).
    " Undo! It should go back to medium...
    cl_demo_output=>write_text( |Push the undo button...| ).
    lo_remote->undo_button_was_pushed( ).
    cl_demo_output=>line( ).

    " Then turn to high this time.
    cl_demo_output=>write_text( |Push On button for slot 3...| ).
    lo_remote->on_button_was_pushed( 3 ).
    cl_demo_output=>write( lo_remote->description( ) ).
    " And, one more undo; it should go back to medium.
    cl_demo_output=>write_text( |Push the undo button...| ).
    lo_remote->undo_button_was_pushed( ).
    cl_demo_output=>line( ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
    |Encapsulating Invocation - the Command Pattern (undo)| ).
  cl_demo_output=>line( ).
  lcl_remote_loader=>main( ).
  cl_demo_output=>display( ).
