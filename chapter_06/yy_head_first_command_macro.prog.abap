*&---------------------------------------------------------------------*
*&  Sample for Encapsulating Invocation - the Command Pattern
*&    based on Head First Design Patterns: Chapter 6
*&---------------------------------------------------------------------*
REPORT yy_head_first_command_macro.

*&---------------------------------------------------------------------*
*&  Here's the Command interface with two methods execute() and undo()
*&---------------------------------------------------------------------*
INTERFACE lif_command.
  METHODS: execute, undo.
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
    " And here, undo() turns the light back on.
    mo_light->on( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Here's a vendor Stereo object to control.
*&---------------------------------------------------------------------*
CLASS lcl_stereo DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_location TYPE string,
      on, off, set_dvd,
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
  METHOD set_dvd.
    cl_demo_output=>write( |{ mv_location } stereo set for DVD input| ).
  ENDMETHOD.
  METHOD set_volume.
    " code to set the volume with valid range: 1-11
    cl_demo_output=>write( |{ mv_location } stereo volume | &
                           |set to { iv_volume }| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Here's a command to turn a stereo on with a DVD.
*&---------------------------------------------------------------------*
CLASS lcl_stereo_with_dvd_command DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_command.
    METHODS constructor IMPORTING io_stereo TYPE REF TO lcl_stereo.
  PRIVATE SECTION.
    DATA mo_stereo TYPE REF TO lcl_stereo.
ENDCLASS.

CLASS lcl_stereo_with_dvd_command IMPLEMENTATION.
  METHOD constructor.
    " Just like the Light On Command, we get passed the instance of the
    " stereo we are going to be controlling and we store it in a local
    " instance variable.
    mo_stereo = io_stereo.
  ENDMETHOD.
  METHOD lif_command~execute.
    " To carry out this request, we need to call three methods on the
    " stereo: first, turn it on, then set it for DVD input, and finally
    " set the volume to 11. Why 11? Well, it's better than 10, right?
    mo_stereo->on( ).
    mo_stereo->set_dvd( ).
    mo_stereo->set_volume( '11' ).
  ENDMETHOD.
  METHOD lif_command~undo.
    mo_stereo->off( ).
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
  METHOD lif_command~undo.
    mo_stereo->on( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Here's a Television object we would like to control.
*&---------------------------------------------------------------------*
CLASS lcl_tv DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_location TYPE string,
      on, off, set_input_channel.
  PRIVATE SECTION.
    DATA mv_location TYPE string.
ENDCLASS.

CLASS lcl_tv IMPLEMENTATION.
  METHOD constructor.
    mv_location = iv_location.
  ENDMETHOD.
  METHOD on.
    cl_demo_output=>write( |{ mv_location } TV is on| ).
  ENDMETHOD.
  METHOD off.
    cl_demo_output=>write( |{ mv_location } TV is off| ).
  ENDMETHOD.
  METHOD set_input_channel.
    cl_demo_output=>write( |{ mv_location } TV channel set for DVD| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Here's a command to turn a TV on.
*&---------------------------------------------------------------------*
CLASS lcl_tv_on_command DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_command.
    METHODS constructor IMPORTING io_tv TYPE REF TO lcl_tv.
  PRIVATE SECTION.
    DATA mo_tv TYPE REF TO lcl_tv.
ENDCLASS.

CLASS lcl_tv_on_command IMPLEMENTATION.
  METHOD constructor.
    mo_tv = io_tv.
  ENDMETHOD.
  METHOD lif_command~execute.
    mo_tv->on( ).
    mo_tv->set_input_channel( ).
  ENDMETHOD.
  METHOD lif_command~undo.
    mo_tv->off( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  And here's a command to turn a TV off.
*&---------------------------------------------------------------------*
CLASS lcl_tv_off_command DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_command.
    METHODS constructor IMPORTING io_tv TYPE REF TO lcl_tv.
  PRIVATE SECTION.
    DATA mo_tv TYPE REF TO lcl_tv.
ENDCLASS.

CLASS lcl_tv_off_command IMPLEMENTATION.
  METHOD constructor.
    mo_tv = io_tv.
  ENDMETHOD.
  METHOD lif_command~execute.
    mo_tv->off( ).
  ENDMETHOD.
  METHOD lif_command~undo.
    mo_tv->on( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Here's a vendor Hot tub object to control.
*&---------------------------------------------------------------------*
CLASS lcl_hottub DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      on, off, circulate, jets_on, jets_off,
      set_temperature IMPORTING iv_temp TYPE i,
      is_on RETURNING VALUE(rv_flag) TYPE abap_bool.
  PRIVATE SECTION.
    DATA:
      mv_on          TYPE abap_bool,
      mv_temperature TYPE i.
ENDCLASS.

CLASS lcl_hottub IMPLEMENTATION.
  METHOD on.
    mv_on = abap_true.
  ENDMETHOD.
  METHOD off.
    mv_on = abap_false.
  ENDMETHOD.
  METHOD circulate.
    CHECK is_on( ).
    cl_demo_output=>write( |Hottub is bubbling!| ).
  ENDMETHOD.
  METHOD jets_on.
    CHECK is_on( ).
    cl_demo_output=>write( |Hottub jets are on| ).
  ENDMETHOD.
  METHOD jets_off.
    CHECK is_on( ).
    cl_demo_output=>write( |Hottub jets are off| ).
  ENDMETHOD.
  METHOD set_temperature.
    IF iv_temp > mv_temperature.
      cl_demo_output=>write( |Hottub is heating to { iv_temp }°| ).
    ELSE.
      cl_demo_output=>write( |Hottub is cooling to { iv_temp }°| ).
    ENDIF.
    mv_temperature = iv_temp.
  ENDMETHOD.
  METHOD is_on.
    rv_flag = mv_on.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Here's a command to turn a hot tub on.
*&---------------------------------------------------------------------*
CLASS lcl_hottub_on_command DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_command.
    METHODS constructor IMPORTING io_hottub TYPE REF TO lcl_hottub.
  PRIVATE SECTION.
    DATA mo_hottub TYPE REF TO lcl_hottub.
ENDCLASS.

CLASS lcl_hottub_on_command IMPLEMENTATION.
  METHOD constructor.
    mo_hottub = io_hottub.
  ENDMETHOD.
  METHOD lif_command~execute.
    mo_hottub->on( ).
    mo_hottub->set_temperature( '104' ).
    mo_hottub->jets_on( ).
    mo_hottub->circulate( ).
  ENDMETHOD.
  METHOD lif_command~undo.
    mo_hottub->jets_off( ).
    mo_hottub->set_temperature( '98' ).
    mo_hottub->off( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  And here's a command to turn a hot tub off.
*&---------------------------------------------------------------------*
CLASS lcl_hottub_off_command DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_command.
    METHODS constructor IMPORTING io_hottub TYPE REF TO lcl_hottub.
  PRIVATE SECTION.
    DATA mo_hottub TYPE REF TO lcl_hottub.
ENDCLASS.

CLASS lcl_hottub_off_command IMPLEMENTATION.
  METHOD constructor.
    mo_hottub = io_hottub.
  ENDMETHOD.
  METHOD lif_command~execute.
    mo_hottub->jets_off( ).
    mo_hottub->set_temperature( '98' ).
    mo_hottub->off( ).
  ENDMETHOD.
  METHOD lif_command~undo.
    mo_hottub->on( ).
    mo_hottub->set_temperature( '104' ).
    mo_hottub->jets_on( ).
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
*&  A new kind of Command that can execute other Commands...
*&   and more than one of them! Pretty cool idea, huh?
*&---------------------------------------------------------------------*
CLASS lcl_macro_command DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_command.
    METHODS constructor
      IMPORTING it_commands TYPE lif_command=>tt_commands.
  PRIVATE SECTION.
    DATA mt_commands TYPE lif_command=>tt_commands.
ENDCLASS.

CLASS lcl_macro_command IMPLEMENTATION.
  METHOD constructor.
    " Take a table of Commands and store them in the Macro Command.
    mt_commands = it_commands.
  ENDMETHOD.
  METHOD lif_command~execute.
    " When the macro gets executed by the remote, execute those
    " commands one at a time.
    DATA(lv_index) = 1.
    DO lines( mt_commands ) TIMES.
      " for ABAP 7.5 use:  mt_commands[ lv_index ]->execute( ).
      CAST lif_command( mt_commands[ lv_index ] )->execute( ).
      ADD 1 TO lv_index.
    ENDDO.
  ENDMETHOD.
  METHOD lif_command~undo.
    " NOTE:  these commands have to be done backwards to ensure
    " proper undo functionality
    DATA(lv_index) = lines( mt_commands ).
    DO lines( mt_commands ) TIMES.
      " for ABAP 7.5 use:  mt_commands[ lv_index ]->undo( ).
      CAST lif_command( mt_commands[ lv_index ] )->undo( ).
      SUBTRACT 1 FROM lv_index.
    ENDDO.
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

    " Create all the devices: a light, tv, stereo, and hot tub.
    DATA(lo_light) = NEW lcl_light( |Living Room| ).
    DATA(lo_tv) = NEW lcl_tv( |Living Room| ).
    DATA(lo_stereo) = NEW lcl_stereo( |Living Room| ).
    DATA(lo_hottub) = NEW lcl_hottub( ).

    " Now create all the On commands to control them.
    DATA(lo_light_on) =  NEW lcl_light_on_command( lo_light ).
    DATA(lo_stereo_on) = NEW lcl_stereo_with_dvd_command( lo_stereo ).
    DATA(lo_tv_on) = NEW lcl_tv_on_command( lo_tv ).
    DATA(lo_hottub_on) = NEW lcl_hottub_on_command( lo_hottub ).

    " Create all the Off commands to control them.
    DATA(lo_light_off) = NEW lcl_light_off_command( lo_light ).
    DATA(lo_stereo_off) = NEW lcl_stereo_off_command( lo_stereo ).
    DATA(lo_tv_off) = NEW lcl_tv_off_command( lo_tv ).
    DATA(lo_hottub_off) = NEW lcl_hottub_off_command( lo_hottub ).

    " Create an internal table for On and a table for Off commands...
    DATA(lt_party_on) = VALUE lif_command=>tt_commands(
     ( lo_light_on ) ( lo_stereo_on )
     ( lo_tv_on ) ( lo_hottub_on ) ).
    DATA(lt_party_off) = VALUE lif_command=>tt_commands(
     ( lo_light_off ) ( lo_stereo_off )
     ( lo_tv_off ) ( lo_hottub_off ) ).

    " ...and create two corresponding macros to hold them.
    DATA(lo_party_on_macro) = NEW lcl_macro_command( lt_party_on ).
    DATA(lo_party_off_macro) = NEW lcl_macro_command( lt_party_off ).

    " Assign the macro command to a button as we would any command.
    lo_remote->set_command(
      iv_slot = 1
      io_on_command = lo_party_on_macro
      io_off_command = lo_party_off_macro ).

    cl_demo_output=>write( lo_remote->description( ) ).
    cl_demo_output=>line( ).

    " All the Commands are executed when we invoke the on macro...
    cl_demo_output=>write_text( |Push party On macro button...| ).
    lo_remote->on_button_was_pushed( 1 ).
    cl_demo_output=>line( ).

    " ...and when we invoke the off macro. It works like this.
    cl_demo_output=>write_text( |Push party Off macro button...| ).
    lo_remote->off_button_was_pushed( 1 ).
    cl_demo_output=>line( ).

    " And, one more undo; this party isn't over yet.
    cl_demo_output=>write_text( |Push the undo button...| ).
    lo_remote->undo_button_was_pushed( ).
    cl_demo_output=>line( ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
    |Encapsulating Invocation - the Command Pattern (macro)| ).
  cl_demo_output=>line( ).
  lcl_remote_loader=>main( ).
  cl_demo_output=>display( ).
