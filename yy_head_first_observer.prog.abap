*&---------------------------------------------------------------------*
*&  Sample for Keeping your Objects in the Know - the Observer Pattern
*&    based on Head First Design Patterns: Chapter 2
*&---------------------------------------------------------------------*
REPORT yy_head_first_observer.

*&---------------------------------------------------------------------*
*&  All our weather components implement the Observer interface.
*&  This gives the Subject a common interface to talk to when it
*&  comes time to update the observers.
*&---------------------------------------------------------------------*
INTERFACE lif_observer.
  METHODS update
    IMPORTING
      iv_temp     TYPE f
      iv_humidity TYPE f
      iv_pressure TYPE f.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Let's also create an interface for all components to implement.
*&  The display elements just need to implement a display() method.
*&---------------------------------------------------------------------*
INTERFACE lif_display_element.
  METHODS display.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Here's our Subject interface.
*&---------------------------------------------------------------------*
INTERFACE lif_subject.
  METHODS:
    " This method is called to notify all observers when the Subject's
    " state has changed.
    notify_observers,
    " Both of these methods take an Observer as an argument; that is
    " the Observer to be registered or removed.
    register_observer IMPORTING io_o TYPE REF TO lif_observer,
    remove_observer   IMPORTING io_o TYPE REF TO lif_observer.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Weather Data implements the Subject interface.
*&---------------------------------------------------------------------*
CLASS lcl_weather_data DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_subject.
    METHODS:
      measurements_changed,
      set_measurements
        IMPORTING
          iv_temperature TYPE f
          iv_humidity    TYPE f
          iv_pressure    TYPE f,
      temperature RETURNING VALUE(rv_temp) TYPE f,
      humidity    RETURNING VALUE(rv_humidity) TYPE f,
      pressure    RETURNING VALUE(rv_pressure) TYPE f.
  PRIVATE SECTION.
    DATA:
      mv_temperature TYPE f,
      mv_humidity    TYPE f,
      mv_pressure    TYPE f,
      " We've added an internal table to hold the Observers.
      mt_observers TYPE STANDARD TABLE
        OF REF TO lif_observer WITH DEFAULT KEY.
ENDCLASS.

CLASS lcl_weather_data IMPLEMENTATION.
  METHOD lif_subject~register_observer.
    " When an observer registers, we just insert it into the table.
    INSERT io_o INTO TABLE mt_observers.
  ENDMETHOD.
  METHOD lif_subject~remove_observer.
    " Likewise, when an observer unregisters, we take it off the table.
    DELETE TABLE mt_observers FROM io_o.
  ENDMETHOD.
  METHOD lif_subject~notify_observers.
    " Here's the fun part; this is where we tell all the observers
    " about the state. Because they are all Observers, we know they
    " all implment update(), so we know how to notify them.
    LOOP AT mt_observers INTO DATA(lo_observer).
      lo_observer->update(
        iv_temp = temperature( )
        iv_humidity = humidity( )
        iv_pressure = pressure( ) ).
    ENDLOOP.
  ENDMETHOD.
  METHOD measurements_changed.
    " We notify the Observers when we get updated measurements.
    lif_subject~notify_observers( ).
  ENDMETHOD.
  METHOD set_measurements.
    " Rather than reading actual weather data off a device, we're going
    " to use this method to test our display elements.
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
      IMPORTING io_weather_data TYPE REF TO lif_subject.
  PRIVATE SECTION.
    DATA:
      mv_temperature  TYPE f,
      mv_humidity     TYPE f,
      mo_weather_data TYPE REF TO lif_subject.
ENDCLASS.

CLASS lcl_current_conditions_display IMPLEMENTATION.
  METHOD constructor.
    " The constructor is passed the Weather Data object (the Subject)
    " and we use it to register this display as an observer.
    mo_weather_data = io_weather_data.
    mo_weather_data->register_observer( me ).
  ENDMETHOD.
  METHOD lif_observer~update.
    " When update() is called, we save the values and call display().
    mv_temperature = iv_temp.
    mv_humidity = iv_humidity.

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
      IMPORTING io_weather_data TYPE REF TO lif_subject.
  PRIVATE SECTION.
    DATA:
      mv_max_temp     TYPE f VALUE 0,
      mv_min_temp     TYPE f VALUE 200,
      mv_temp_sum     TYPE f VALUE 0,
      mv_num_readings TYPE i VALUE 0,
      mo_weather_data TYPE REF TO lif_subject.
ENDCLASS.

CLASS lcl_statistics_display IMPLEMENTATION.
  METHOD constructor.
    mo_weather_data = io_weather_data.
    mo_weather_data->register_observer( me ).
  ENDMETHOD.
  METHOD lif_observer~update.
    mv_temp_sum = mv_temp_sum + iv_temp.
    mv_num_readings = mv_num_readings + 1.

    mv_max_temp = COND #(
      WHEN iv_temp > mv_max_temp
      THEN iv_temp ELSE mv_max_temp ).
    mv_min_temp = COND #(
      WHEN iv_temp < mv_min_temp
      THEN iv_temp ELSE mv_min_temp ).

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
      IMPORTING io_weather_data TYPE REF TO lif_subject.
  PRIVATE SECTION.
    DATA:
      mv_current_pressure TYPE f VALUE '29.92',
      mv_last_pressure    TYPE f,
      mo_weather_data     TYPE REF TO lif_subject.
ENDCLASS.

CLASS lcl_forecast_display IMPLEMENTATION.
  METHOD constructor.
    mo_weather_data = io_weather_data.
    mo_weather_data->register_observer( me ).
  ENDMETHOD.
  METHOD lif_observer~update.
    mv_last_pressure = mv_current_pressure.
    mv_current_pressure = iv_pressure.

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
      IMPORTING io_weather_data TYPE REF TO lif_subject.
  PRIVATE SECTION.
    METHODS compute_heat_index
      IMPORTING iv_t TYPE f iv_rh TYPE f
      RETURNING VALUE(rv_index) TYPE f.
    DATA:
      mv_heat_index   TYPE f,
      mo_weather_data TYPE REF TO lif_subject.
ENDCLASS.

CLASS lcl_heat_index_display IMPLEMENTATION.
  METHOD constructor.
    mo_weather_data = io_weather_data.
    mo_weather_data->register_observer( me ).
  ENDMETHOD.
  METHOD lif_observer~update.
    mv_heat_index = compute_heat_index(
      iv_t = iv_temp iv_rh = iv_humidity ).

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
    |Keeping your Objects in the Know - the Observer Pattern| ).
  cl_demo_output=>line( ).
  lcl_weather_station=>main( ).
  cl_demo_output=>display( ).
