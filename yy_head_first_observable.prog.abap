*&---------------------------------------------------------------------*
*&  Sample for Keeping your Objects in the Know - the Observer Pattern
*&    based on Head First Design Patterns: Chapter 2
*&---------------------------------------------------------------------*
REPORT yy_head_first_observable.

*&---------------------------------------------------------------------*
*&  The Observable class keeps track of all your observers and notifies
*&  them for you by raising the update_observers ABAP event.
*&---------------------------------------------------------------------*
CLASS lcl_observable DEFINITION.
  PUBLIC SECTION.
    METHODS:
      " This version takes an arbitrary data object that gets passed to
      " each Observer when it is notified.
      notify_observers
        IMPORTING VALUE(io_arg) TYPE REF TO object OPTIONAL,
      set_changed,
      has_changed RETURNING VALUE(rv_changed) TYPE abap_bool.
    " This doesn't look familiar?  Hold tight, we'll get to it soon...
    EVENTS update_observers EXPORTING VALUE(io_arg) TYPE REF TO object.
  PRIVATE SECTION.
    DATA mv_changed TYPE abap_bool.
ENDCLASS.

CLASS lcl_observable IMPLEMENTATION.
  METHOD set_changed.
    " The set_changed() method sets a changed flag to true.
    mv_changed = abap_true.
  ENDMETHOD.
  METHOD has_changed.
    rv_changed = mv_changed.
  ENDMETHOD.
  METHOD notify_observers.
    " This method only updates observers if the changed flag is TRUE.
    CHECK has_changed( ).
    RAISE EVENT update_observers EXPORTING io_arg = io_arg.
    " After it notifies the observers, it sets the flag back to false.
    mv_changed = abap_false.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  We are now implementing the Observer interface as an event handler.
*&---------------------------------------------------------------------*
INTERFACE lif_observer.
  " We've changed the update() method to take both an optional data
  " argument from the event defintion and the Observable itself.
  METHODS update
    FOR EVENT update_observers OF lcl_observable
    IMPORTING io_arg sender.
  " Parameter 'io_arg' will be either the data object that was passed
  " to notify_observers() or not bound if an object wasn't specified.
  " The Subject that sent the notification is passed in as 'sender'.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Let's also create an interface for all components to implement.
*&  The display elements just need to implement a display() method.
*&---------------------------------------------------------------------*
INTERFACE lif_display_element.
  METHODS display.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Here's our subject, which we can now also call the Observable. We
*&  don't need the register(), remove(), and notify_observers() methods
*&  anymore; we inherit that behavior from the superclass.
*&---------------------------------------------------------------------*
CLASS lcl_weather_data DEFINITION INHERITING FROM lcl_observable FINAL.
  PUBLIC SECTION.
    METHODS:
      measurements_changed,
      set_measurements
        IMPORTING
          iv_temperature TYPE f
          iv_humidity    TYPE f
          iv_pressure    TYPE f,
      " These methods aren't new, but because we are going to use 'pull'
      " we thought we'd remind you they are here. The Observers will use
      " them to get at the Weather Data object's state.
      temperature RETURNING VALUE(rv_temp) TYPE f,
      humidity    RETURNING VALUE(rv_humidity) TYPE f,
      pressure    RETURNING VALUE(rv_pressure) TYPE f.
  PRIVATE SECTION.
    DATA:
      mv_temperature TYPE f,
      mv_humidity    TYPE f,
      mv_pressure    TYPE f.
    " Our class no longer needs an internal table to hold Observers.
ENDCLASS.

CLASS lcl_weather_data IMPLEMENTATION.
  METHOD measurements_changed.
    " We now first call set_changed() to indicate the state has
    " changed before calling notify_observers().
    set_changed( ).
    " Notice we aren't sending a data object with this method
    " call. That means we're using the 'pull' model.
    notify_observers( ).
  ENDMETHOD.
  METHOD set_measurements.
    mv_temperature = iv_temperature.
    mv_humidity = iv_humidity.
    mv_pressure = iv_pressure.
    measurements_changed( ).
  ENDMETHOD.
  METHOD temperature.
    rv_temp = mv_temperature.
  ENDMETHOD.
  METHOD humidity.
    rv_humidity = mv_humidity.
  ENDMETHOD.
  METHOD pressure.
    rv_pressure = mv_pressure.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Each display implements Observer so it can get changes from the
*&  Weather Data object. It also implements Display Element, because
*&  our API is going to require all components to implement this.
*&---------------------------------------------------------------------*
*&  This display element shows the current measurements from the object.
*&---------------------------------------------------------------------*
CLASS lcl_current_conditions_display DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES:
      lif_observer,
      lif_display_element.
    METHODS constructor
      IMPORTING io_observable TYPE REF TO lcl_observable.
  PRIVATE SECTION.
    DATA:
      mv_temperature  TYPE f,
      mv_humidity     TYPE f,
      mo_observable   TYPE REF TO lcl_observable.
ENDCLASS.

CLASS lcl_current_conditions_display IMPLEMENTATION.
  METHOD constructor.
    " Our constructor now takes an Observable and we use this to
    " set the current condtions object as an ABAP event handler.
    mo_observable = io_observable.
    SET HANDLER lif_observer~update FOR mo_observable.
  ENDMETHOD.
  METHOD lif_observer~update.
    " In update(), we first make sure the observable is of type Weather
    " Data and then we use its getter methods to obtain the temperature
    " and humidity measurements. After that we call display().
    " for ABAP 7.5 use: CHECK sender IS INSTANCE OF lcl_weather_data.
    CHECK cl_abap_typedescr=>describe_by_object_ref( sender ) =
          cl_abap_typedescr=>describe_by_name( 'LCL_WEATHER_DATA' ).
    DATA(lo_weather_data) = CAST lcl_weather_data( sender ).

    mv_temperature = lo_weather_data->temperature( ).
    mv_humidity = lo_weather_data->humidity( ).

    lif_display_element~display( ).
  ENDMETHOD.
  METHOD lif_display_element~display.
    " The display() method just prints out recent temp and humidity.
    cl_demo_output=>write( |Current conditions: | &
      |{ mv_temperature }°F and { mv_humidity }% humidity| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  This one keeps track of the min/max/avg temperature measurements.
*&---------------------------------------------------------------------*
CLASS lcl_statistics_display DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES:
      lif_observer,
      lif_display_element.
    METHODS constructor
      IMPORTING io_observable TYPE REF TO lcl_observable.
  PRIVATE SECTION.
    DATA:
      mv_max_temp     TYPE f VALUE 0,
      mv_min_temp     TYPE f VALUE 200,
      mv_temp_sum     TYPE f VALUE 0,
      mv_num_readings TYPE i VALUE 0,
      mo_observable   TYPE REF TO lcl_observable.
ENDCLASS.

CLASS lcl_statistics_display IMPLEMENTATION.
  METHOD constructor.
    mo_observable = io_observable.
    SET HANDLER lif_observer~update FOR mo_observable.
  ENDMETHOD.
  METHOD lif_observer~update.
    " for ABAP 7.5 use: CHECK sender IS INSTANCE OF lcl_weather_data.
    CHECK cl_abap_typedescr=>describe_by_object_ref( sender ) =
          cl_abap_typedescr=>describe_by_name( 'LCL_WEATHER_DATA' ).
    DATA(lo_weather_data) = CAST lcl_weather_data( sender ).

    DATA(lv_temp) = lo_weather_data->temperature( ).
    mv_temp_sum = mv_temp_sum + lv_temp.
    mv_num_readings = mv_num_readings + 1.

    mv_max_temp = COND #(
      WHEN lv_temp > mv_max_temp
      THEN lv_temp ELSE mv_max_temp ).
    mv_min_temp = COND #(
      WHEN lv_temp < mv_min_temp
      THEN lv_temp ELSE mv_min_temp ).

    lif_display_element~display( ).
  ENDMETHOD.
  METHOD lif_display_element~display.
    cl_demo_output=>write( |Avg/Max/Min temperature = | &
      |{ mv_temp_sum / mv_num_readings }| &
      |/{ mv_max_temp }| & |/{ mv_min_temp }| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  This display shows the weather forecast based on the barometer.
*&---------------------------------------------------------------------*
CLASS lcl_forecast_display DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES:
      lif_observer,
      lif_display_element.
    METHODS constructor
      IMPORTING io_observable TYPE REF TO lcl_observable.
  PRIVATE SECTION.
    DATA:
      mv_current_pressure TYPE f VALUE '29.92',
      mv_last_pressure    TYPE f,
      mo_observable       TYPE REF TO lcl_observable.
ENDCLASS.

CLASS lcl_forecast_display IMPLEMENTATION.
  METHOD constructor.
    mo_observable = io_observable.
    SET HANDLER lif_observer~update FOR mo_observable.
  ENDMETHOD.
  METHOD lif_observer~update.
    " for ABAP 7.5 use: CHECK sender IS INSTANCE OF lcl_weather_data.
    CHECK cl_abap_typedescr=>describe_by_object_ref( sender ) =
          cl_abap_typedescr=>describe_by_name( 'LCL_WEATHER_DATA' ).
    DATA(lo_weather_data) = CAST lcl_weather_data( sender ).

    mv_last_pressure = mv_current_pressure.
    mv_current_pressure = lo_weather_data->pressure( ).

    lif_display_element~display( ).
  ENDMETHOD.
  METHOD lif_display_element~display.
    cl_demo_output=>write( |Forecast: | && COND #(
      WHEN mv_current_pressure > mv_last_pressure
      THEN |Improving weather on the way!|
      WHEN mv_current_pressure = mv_last_pressure
      THEN |More of the same|
      WHEN mv_current_pressure < mv_last_pressure
      THEN |Watch out for cooler, rainy weather| ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  And this displays heat index based on temperature and humidity.
*&---------------------------------------------------------------------*
CLASS lcl_heat_index_display DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES:
      lif_observer,
      lif_display_element.
    METHODS constructor
      IMPORTING io_observable TYPE REF TO lcl_observable.
  PRIVATE SECTION.
    METHODS compute_heat_index
      IMPORTING iv_t TYPE f iv_rh TYPE f
      RETURNING VALUE(rv_index) TYPE f.
    DATA:
      mv_heat_index TYPE f,
      mo_observable TYPE REF TO lcl_observable.
ENDCLASS.

CLASS lcl_heat_index_display IMPLEMENTATION.
  METHOD constructor.
    mo_observable = io_observable.
    SET HANDLER lif_observer~update FOR mo_observable.
  ENDMETHOD.
  METHOD lif_observer~update.
    " for ABAP 7.5 use: CHECK sender IS INSTANCE OF lcl_weather_data.
    CHECK cl_abap_typedescr=>describe_by_object_ref( sender ) =
          cl_abap_typedescr=>describe_by_name( 'LCL_WEATHER_DATA' ).
    DATA(lo_weather_data) = CAST lcl_weather_data( sender ).

    mv_heat_index = compute_heat_index(
      iv_t = lo_weather_data->temperature( )
      iv_rh = lo_weather_data->humidity( ) ).

    lif_display_element~display( ).
  ENDMETHOD.
  METHOD lif_display_element~display.
    cl_demo_output=>write(
      |Heat index is { mv_heat_index DECIMALS = 5 }| ).
  ENDMETHOD.
  METHOD compute_heat_index.
    rv_index = ( ( CONV f( '1.6923E1' ) +
      ( CONV f( '1.85212E-1' ) * iv_t ) +
      ( CONV f( '5.37941' ) * iv_rh ) -
      ( CONV f( '1.00254E-1' ) * iv_t * iv_rh ) +
      ( CONV f( '9.41695E-3' ) * ( iv_t ** 2 ) ) +
      ( CONV f( '7.28898E-3' ) * ( iv_rh ** 2 ) ) +
      ( CONV f( '3.45372E-4' ) * ( iv_t ** 2 * iv_rh ) ) -
      ( CONV f( '8.14971E-4' ) * (  iv_t * iv_rh ** 2 ) ) +
      ( CONV f( '1.02102E-5' ) * ( iv_t ** 2 * iv_rh ** 2 ) ) -
      ( CONV f( '3.8646E-5' ) * ( iv_t ** 3 ) ) +
      ( CONV f( '2.91583E-5' ) * ( iv_rh ** 3 ) ) +
      ( CONV f( '1.42721E-6' ) * ( iv_t ** 3 * iv_rh ) ) +
      ( CONV f( '1.97483E-7' ) * ( iv_t * iv_rh ** 3 ) ) -
      ( CONV f( '2.18429E-8' ) * ( iv_t ** 3 * iv_rh ** 2 ) ) +
      ( CONV f( '8.43296E-10' ) * ( iv_t ** 2 * iv_rh ** 3 ) ) ) -
      ( CONV f( '4.81975E-11' ) * ( iv_t ** 3 * iv_rh ** 3 ) ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Next-generation weather monitoring station
*&---------------------------------------------------------------------*
CLASS lcl_weather_station DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS lcl_weather_station IMPLEMENTATION.
  METHOD main.

    " First, create the Weather Data object
    DATA(lo_weather_data) = NEW lcl_weather_data( ).

    " Create the different displays and pass them the object
    NEW lcl_current_conditions_display( lo_weather_data ).
    NEW lcl_statistics_display( lo_weather_data ).
    NEW lcl_forecast_display( lo_weather_data ).
    NEW lcl_heat_index_display( lo_weather_data ).

    " Simulate new weather measurements
    cl_demo_output=>write_text( |Simulating new weather...| ).
    lo_weather_data->set_measurements(
      iv_temperature = CONV #( '80' )
      iv_humidity = CONV #( '65.0' )
      iv_pressure = CONV #( '30.4' ) ).
    cl_demo_output=>line( ).

    cl_demo_output=>write_text( |Simulating new weather...| ).
    lo_weather_data->set_measurements(
      iv_temperature = CONV #( '82' )
      iv_humidity = CONV #( '70.0' )
      iv_pressure = CONV #( '29.2' ) ).
    cl_demo_output=>line( ).

    cl_demo_output=>write_text( |Simulating new weather...| ).
    lo_weather_data->set_measurements(
      iv_temperature = CONV #( '78' )
      iv_humidity = CONV #( '90.0' )
      iv_pressure = CONV #( '29.2' ) ).
    cl_demo_output=>line( ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
    |Keeping your Objects in the Know - the ABAP Observer Pattern| ).
  cl_demo_output=>line( ).
  lcl_weather_station=>main( ).
  cl_demo_output=>display( ).
