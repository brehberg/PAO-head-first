*&---------------------------------------------------------------------*
*&  Sample for Encapsulating Invocation - the Command Pattern
*&    based on Head First Design Patterns: Chapter 6
*&---------------------------------------------------------------------*
REPORT yy_head_first_command.

*&---------------------------------------------------------------------*
*&  Here's the Command interface with one method called execute()
*&---------------------------------------------------------------------*
INTERFACE lif_command.
  METHODS execute.
  " This table type is used by the invoker to hold multiple Commands.
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
*&  Here's a vendor Stereo object to control.
*&---------------------------------------------------------------------*
CLASS lcl_stereo DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_location TYPE string,
      on, off, set_cd,
      set_volume IMPORTING iv_volume TYPE i.
  PRIVATE SECTION.
    DATA mv_location TYPE string.
ENDCLASS.

CLASS lcl_stereo IMPLEMENTATION.
  METHOD constructor.
    mv_location = iv_location.
  ENDMETHOD.
  METHOD on.
    cl_demo_output=>write( |{ mv_location } stereo is on| ).
  ENDMETHOD.
  METHOD off.
    cl_demo_output=>write( |{ mv_location } stereo is off| ).
  ENDMETHOD.
  METHOD set_cd.
    cl_demo_output=>write( |{ mv_location } stereo set for CD input| ).
  ENDMETHOD.
  METHOD set_volume.
    " code to set the volume with valid range: 1-11
    cl_demo_output=>write( |{ mv_location } stereo volume | &
                           |set to { iv_volume }| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Here's a command to turn a stereo on with a CD.
*&---------------------------------------------------------------------*
CLASS lcl_stereo_on_with_cd_command DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_command.
    METHODS constructor IMPORTING io_stereo TYPE REF TO lcl_stereo.
  PRIVATE SECTION.
    DATA mo_stereo TYPE REF TO lcl_stereo.
ENDCLASS.

CLASS lcl_stereo_on_with_cd_command IMPLEMENTATION.
  METHOD constructor.
    " Just like the Light On Command, we get passed the instance of the
    " stereo we are going to be controlling and we store it in a local
    " instance variable.
    mo_stereo = io_stereo.
  ENDMETHOD.
  METHOD lif_command~execute.
    " To carry out this request, we need to call three methods on the
    " stereo: first, turn it on, then set it to play a CD, and finally
    " set the volume to 11. Why 11? Well, it's better than 10, right?
    mo_stereo->on( ).
    mo_stereo->set_cd( ).
    mo_stereo->set_volume( '11' ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  And here's a command to turn a stereo off.
*&---------------------------------------------------------------------*
CLASS lcl_stereo_off_command DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_command.
    METHODS constructor IMPORTING io_stereo TYPE REF TO lcl_stereo.
  PRIVATE SECTION.
    DATA mo_stereo TYPE REF TO lcl_stereo.
ENDCLASS.

CLASS lcl_stereo_off_command IMPLEMENTATION.
  METHOD constructor.
    mo_stereo = io_stereo.
  ENDMETHOD.
  METHOD lif_command~execute.
    mo_stereo->off( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Here's a Garage Door object we would like to control.
*&---------------------------------------------------------------------*
CLASS lcl_garage_door DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_location TYPE string,
      up, down, light_on, light_off.
  PRIVATE SECTION.
    DATA mv_location TYPE string.
ENDCLASS.

CLASS lcl_garage_door IMPLEMENTATION.
  METHOD constructor.
    mv_location = iv_location.
  ENDMETHOD.
  METHOD up.
    cl_demo_output=>write( |{ mv_location } garage door is open| ).
  ENDMETHOD.
  METHOD down.
    cl_demo_output=>write( |{ mv_location } garage door is closed| ).
  ENDMETHOD.
  METHOD light_on.
    cl_demo_output=>write( |{ mv_location } garage light is on| ).
  ENDMETHOD.
  METHOD light_off.
    cl_demo_output=>write( |{ mv_location } garage light is off| ).
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
*&  Here's a vendor Ceiling Fan object to control.
*&---------------------------------------------------------------------*
CLASS lcl_ceiling_fan DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_location TYPE string,
      high, low, off.
  PRIVATE SECTION.
    DATA mv_location TYPE string.
ENDCLASS.

CLASS lcl_ceiling_fan IMPLEMENTATION.
  METHOD constructor.
    mv_location = iv_location.
  ENDMETHOD.
  METHOD high.
    " turns the ceiling fan on to high
    cl_demo_output=>write( |{ mv_location } ceiling fan is on high| ).
  ENDMETHOD.
  METHOD low.
    " turns the ceiling fan on to low
    cl_demo_output=>write( |{ mv_location } ceiling fan is on low| ).
  ENDMETHOD.
  METHOD off.
    " turns the ceiling fan off
    cl_demo_output=>write( |{ mv_location } ceiling fan is off| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Here's a command to turn a ceiling fan on high.
*&---------------------------------------------------------------------*
CLASS lcl_ceiling_fan_high_command DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_command.
    METHODS constructor IMPORTING io_fan TYPE REF TO lcl_ceiling_fan.
  PRIVATE SECTION.
    DATA mo_ceiling_fan TYPE REF TO lcl_ceiling_fan.
ENDCLASS.

CLASS lcl_ceiling_fan_high_command IMPLEMENTATION.
  METHOD constructor.
    mo_ceiling_fan = io_fan.
  ENDMETHOD.
  METHOD lif_command~execute.
    mo_ceiling_fan->high( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Here's a command to turn a ceiling fan on low.
*&---------------------------------------------------------------------*
CLASS lcl_ceiling_fan_low_command DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_command.
    METHODS constructor IMPORTING io_fan TYPE REF TO lcl_ceiling_fan.
  PRIVATE SECTION.
    DATA mo_ceiling_fan TYPE REF TO lcl_ceiling_fan.
ENDCLASS.

CLASS lcl_ceiling_fan_low_command IMPLEMENTATION.
  METHOD constructor.
    mo_ceiling_fan = io_fan.
  ENDMETHOD.
  METHOD lif_command~execute.
    mo_ceiling_fan->low( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  And here's a command to turn it off.
*&---------------------------------------------------------------------*
CLASS lcl_ceiling_fan_off_command DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_command.
    METHODS constructor IMPORTING io_fan TYPE REF TO lcl_ceiling_fan.
  PRIVATE SECTION.
    DATA mo_ceiling_fan TYPE REF TO lcl_ceiling_fan.
ENDCLASS.

CLASS lcl_ceiling_fan_off_command IMPLEMENTATION.
  METHOD constructor.
    mo_ceiling_fan = io_fan.
  ENDMETHOD.
  METHOD lif_command~execute.
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
ENDCLASS.

*&---------------------------------------------------------------------*
*&  This is the invoker.
*&---------------------------------------------------------------------*
CLASS lcl_remote_control DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor,
      " The set_command() method takes a slot position along with an
      " On and Off command to be stored in that slot.
      set_command IMPORTING iv_slot        TYPE i
                            io_on_command  TYPE REF TO lif_command
                            io_off_command TYPE REF TO lif_command,
      " When an On or Off button is pressed, the hardware takes care
      " of calling either the corresponding on_button_was_pushed() of
      " off_button_was_pushed() methods.
      on_button_was_pushed IMPORTING iv_slot TYPE i,
      off_button_was_pushed IMPORTING iv_slot TYPE i,
      " We've created method description() to print out each slot and
      " its corresponding command. You'll see us use this when we test.
      description RETURNING VALUE(rv_text) TYPE string.
  PRIVATE SECTION.
    DATA:
      " This time around the remote is going to handle seven On and Off
      " commands, which we'll hold in corresponding internal tables.
      mt_on_commands  TYPE lif_command=>tt_commands,
      mt_off_commands TYPE lif_command=>tt_commands.
    METHODS get_class_name
      IMPORTING io_object      TYPE REF TO object
      RETURNING VALUE(rv_name) TYPE string.
ENDCLASS.

CLASS lcl_remote_control IMPLEMENTATION.
  METHOD constructor.
    " In the constructor all we need to do is instantiate a No Command
    " object and initialize the on and off tables of commands.
    DATA(lo_no_command) = NEW lcl_no_command( ).
    DO 7 TIMES.
      INSERT lo_no_command INTO TABLE mt_off_commands.
      INSERT lo_no_command INTO TABLE mt_on_commands.
    ENDDO.
  ENDMETHOD.
  METHOD set_command.
    " This puts the commands in the on and off tables for later use.
    mt_on_commands[ iv_slot ] = io_on_command.
    mt_off_commands[ iv_slot ] = io_off_command.
  ENDMETHOD.
  METHOD on_button_was_pushed.
    " for ABAP 7.5 use:  mt_on_commands[ iv_slot ]->execute( ).
    CAST lif_command( mt_on_commands[ iv_slot ] )->execute( ).
  ENDMETHOD.
  METHOD off_button_was_pushed.
    " for ABAP 7.5 use:  mt_off_commands[ iv_slot ]->execute( ).
    CAST lif_command( mt_off_commands[ iv_slot ] )->execute( ).
  ENDMETHOD.
  METHOD description.
    CONSTANTS c_w TYPE i VALUE 30.
    rv_text = REDUCE #(
      INIT text = |----------------- Remote Control -----------------\n|
      FOR i = 1 UNTIL i > lines( mt_on_commands )
      NEXT text = text && |[slot { i }] | &
           |{ get_class_name( mt_on_commands[ i ] ) WIDTH = c_w }\t| &
           |{ get_class_name( mt_off_commands[ i ] ) WIDTH = c_w }\n| ).
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

    DATA(lo_remote) = NEW lcl_remote_control( ).

    " Create all the devices in their proper locations.
    DATA(lo_living_room_light) = NEW lcl_light( |Living Room| ).
    DATA(lo_kitchen_light) = NEW lcl_light( |Kitchen| ).
    DATA(lo_ceiling_fan) = NEW lcl_ceiling_fan( |Living Room| ).
    DATA(lo_garage_door) = NEW lcl_garage_door( |Front| ).
    DATA(lo_stereo) = NEW lcl_stereo( |Living Room| ).

    " Create all the Light Command objects.
    DATA(lo_living_room_light_on) =
      NEW lcl_light_on_command( lo_living_room_light ).
    DATA(lo_living_room_light_off) =
      NEW lcl_light_off_command( lo_living_room_light ).
    DATA(lo_kitchen_light_on) =
      NEW lcl_light_on_command( lo_kitchen_light ).
    DATA(lo_kitchen_light_off) =
      NEW lcl_light_off_command( lo_kitchen_light ).

    " Create the On and Off commands for the ceiling fan.
    DATA(lo_ceiling_fan_on_high) =
      NEW lcl_ceiling_fan_high_command( lo_ceiling_fan ).
    DATA(lo_ceiling_fan_on_low) =
      NEW lcl_ceiling_fan_low_command( lo_ceiling_fan ).
    DATA(lo_ceiling_fan_off) =
      NEW lcl_ceiling_fan_off_command( lo_ceiling_fan ).

    " Create the Up and Down commands for the garage.
    DATA(lo_garage_up) =
      NEW lcl_garage_door_up_command( lo_garage_door ).
    DATA(lo_garage_down) =
      NEW lcl_garage_door_down_command( lo_garage_door ).

    " Create the Stereo On and Off commands.
    DATA(lo_stereo_on_with_cd) =
      NEW lcl_stereo_on_with_cd_command( lo_stereo ).
    DATA(lo_stereo_off) =
      NEW lcl_stereo_off_command( lo_stereo ).

    " Now that we've got all our commands, we can load them
    " into the remote slots.
    lo_remote->set_command(
      iv_slot = 1
      io_on_command = lo_living_room_light_on
      io_off_command = lo_living_room_light_off ).
    lo_remote->set_command(
      iv_slot = 2
      io_on_command = lo_kitchen_light_on
      io_off_command = lo_kitchen_light_off ).
    lo_remote->set_command(
      iv_slot = 3
      io_on_command = lo_ceiling_fan_on_high
      io_off_command = lo_ceiling_fan_off ).
    lo_remote->set_command(
      iv_slot = 4
      io_on_command = lo_ceiling_fan_on_low
      io_off_command = lo_ceiling_fan_off ).
    lo_remote->set_command(
      iv_slot = 5
      io_on_command = lo_stereo_on_with_cd
      io_off_command = lo_stereo_off ).
    lo_remote->set_command(
      iv_slot = 6
      io_on_command = lo_garage_up
      io_off_command = lo_garage_down ).

    " Here's where we use our description() method to print each remote
    " slot and the command that it is assigned to.
    cl_demo_output=>write( lo_remote->description( ) ).
    cl_demo_output=>line( ).

    " All right, we are ready to roll! Now, we step through each
    " slot and push its On and Off button.
    DATA(lv_index) = 1.
    DO 7 TIMES.
      cl_demo_output=>write_text(
        |Push On/Off buttons for slot { lv_index }...| ).
      lo_remote->on_button_was_pushed( lv_index ).
      lo_remote->off_button_was_pushed( lv_index ).
      cl_demo_output=>line( ).
      ADD 1 TO lv_index.
    ENDDO.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
    |Encapsulating Invocation - the Command Pattern| ).
  cl_demo_output=>line( ).
  lcl_remote_loader=>main( ).
  cl_demo_output=>display( ).
