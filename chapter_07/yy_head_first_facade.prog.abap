*&---------------------------------------------------------------------*
*&  Sample for Being Adaptive - the Facade Pattern
*&    based on Head First Design Patterns: Chapter 7
*&---------------------------------------------------------------------*
REPORT yy_head_first_facade.

*&---------------------------------------------------------------------*
*&  Home theater component: AM/FM Tuner
*&---------------------------------------------------------------------*
CLASS lcl_tuner DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_description TYPE string,
      on, off, set_am, set_fm,
      set_frequency IMPORTING iv_frequency TYPE f,
      description RETURNING VALUE(rv_text) TYPE string.
  PRIVATE SECTION.
    DATA:
      mv_description TYPE string,
      mv_frequency   TYPE f.
ENDCLASS.

CLASS lcl_tuner IMPLEMENTATION.
  METHOD constructor.
    mv_description = iv_description.
  ENDMETHOD.
  METHOD on.
    cl_demo_output=>write( |{ mv_description } on| ).
  ENDMETHOD.
  METHOD off.
    cl_demo_output=>write( |{ mv_description } off| ).
  ENDMETHOD.
  METHOD set_am.
    cl_demo_output=>write( |{ mv_description } setting AM mode| ).
  ENDMETHOD.
  METHOD set_fm.
    cl_demo_output=>write( |{ mv_description } setting FM mode| ).
  ENDMETHOD.
  METHOD set_frequency.
    mv_frequency = iv_frequency.
    cl_demo_output=>write( |{ mv_description } setting frequency | &
                           |to { mv_frequency DECIMALS = 1 }| ).
  ENDMETHOD.
  METHOD description.
    rv_text = mv_description.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Home theater component: DVD player
*&---------------------------------------------------------------------*
CLASS lcl_dvd_player DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_description TYPE string,
      on, off, eject, stop, pause,
      play         IMPORTING iv_title TYPE string,
      play_episode IMPORTING iv_episode TYPE i,
      set_two_channel_audio,
      set_surround_audio,
      description RETURNING VALUE(rv_text) TYPE string.
  PRIVATE SECTION.
    DATA:
      mv_description     TYPE string,
      mv_title           TYPE string,
      mv_current_episode TYPE i.
ENDCLASS.

CLASS lcl_dvd_player IMPLEMENTATION.
  METHOD constructor.
    mv_description = iv_description.
  ENDMETHOD.
  METHOD on.
    cl_demo_output=>write( |{ mv_description } on| ).
  ENDMETHOD.
  METHOD off.
    cl_demo_output=>write( |{ mv_description } off| ).
  ENDMETHOD.
  METHOD eject.
    CLEAR mv_title.
    cl_demo_output=>write( |{ mv_description } eject| ).
  ENDMETHOD.
  METHOD stop.
    CLEAR mv_current_episode.
    cl_demo_output=>write( |{ mv_description } stopped| ).
  ENDMETHOD.
  METHOD pause.
    cl_demo_output=>write( |{ mv_description } paused "{ mv_title }"| ).
  ENDMETHOD.
  METHOD play.
    mv_title = iv_title.
    cl_demo_output=>write( |{ mv_description } | &
                           |playing DVD "{ mv_title }"| ).
  ENDMETHOD.
  METHOD play_episode.
    IF mv_title IS INITIAL.
      cl_demo_output=>write( |{ mv_description } can't play episode | &
                             |{ iv_episode }, no DVD inserted| ).
    ELSE.
      mv_current_episode = iv_episode.
      cl_demo_output=>write( |{ mv_description } playing | &
                             |episode { mv_current_episode }| ).
    ENDIF.
  ENDMETHOD.
  METHOD set_two_channel_audio.
    cl_demo_output=>write( |{ mv_description } set two channel audio| ).
  ENDMETHOD.
  METHOD set_surround_audio.
    cl_demo_output=>write( |{ mv_description } set surround audio| ).
  ENDMETHOD.
  METHOD description.
    rv_text = mv_description.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Home theater component: CD player
*&---------------------------------------------------------------------*
CLASS lcl_cd_player DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_description TYPE string,
      on, off, eject, stop, pause,
      play       IMPORTING iv_title TYPE string,
      play_track IMPORTING iv_track TYPE i,
      description RETURNING VALUE(rv_text) TYPE string.
  PRIVATE SECTION.
    DATA:
      mv_description   TYPE string,
      mv_title         TYPE string,
      mv_current_track TYPE i.
ENDCLASS.

CLASS lcl_cd_player IMPLEMENTATION.
  METHOD constructor.
    mv_description = iv_description.
  ENDMETHOD.
  METHOD on.
    cl_demo_output=>write( |{ mv_description } on| ).
  ENDMETHOD.
  METHOD off.
    cl_demo_output=>write( |{ mv_description } off| ).
  ENDMETHOD.
  METHOD eject.
    CLEAR mv_title.
    cl_demo_output=>write( |{ mv_description } eject| ).
  ENDMETHOD.
  METHOD stop.
    CLEAR mv_current_track.
    cl_demo_output=>write( |{ mv_description } stopped| ).
  ENDMETHOD.
  METHOD pause.
    cl_demo_output=>write( |{ mv_description } paused "{ mv_title }"| ).
  ENDMETHOD.
  METHOD play.
    mv_title = iv_title.
    cl_demo_output=>write( |{ mv_description } | &
                           |playing title "{ mv_title }"| ).
  ENDMETHOD.
  METHOD play_track.
    IF mv_title IS INITIAL.
      cl_demo_output=>write( |{ mv_description } can't play track | &
                              |{ iv_track }, no CD inserted| ).
    ELSE.
      mv_current_track = iv_track.
      cl_demo_output=>write( |{ mv_description } playing | &
                             |track { mv_current_track }| ).
    ENDIF.
  ENDMETHOD.
  METHOD description.
    rv_text = mv_description.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Home theater component: Amplifier
*&---------------------------------------------------------------------*
CLASS lcl_amplifier DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_description TYPE string,
      on, off,
      set_stereo_sound,
      set_surround_sound,
      set_volume IMPORTING iv_level TYPE i,
      set_tuner  IMPORTING io_tuner TYPE REF TO lcl_tuner,
      set_dvd    IMPORTING io_dvd TYPE REF TO lcl_dvd_player,
      set_cd     IMPORTING io_cd TYPE REF TO lcl_cd_player,
      description RETURNING VALUE(rv_text) TYPE string.
  PRIVATE SECTION.
    DATA:
      mv_description TYPE string,
      mo_tuner       TYPE REF TO lcl_tuner,
      mo_dvd         TYPE REF TO lcl_dvd_player,
      mo_cd          TYPE REF TO lcl_cd_player.
ENDCLASS.

CLASS lcl_amplifier IMPLEMENTATION.
  METHOD constructor.
    mv_description = iv_description.
  ENDMETHOD.
  METHOD on.
    cl_demo_output=>write( |{ mv_description } on| ).
  ENDMETHOD.
  METHOD off.
    cl_demo_output=>write( |{ mv_description } off| ).
  ENDMETHOD.
  METHOD set_stereo_sound.
    cl_demo_output=>write( |{ mv_description } stereo mode on| ).
  ENDMETHOD.
  METHOD set_surround_sound.
    cl_demo_output=>write( |{ mv_description } surround sound on| ).
  ENDMETHOD.
  METHOD set_volume.
    cl_demo_output=>write( |{ mv_description } setting volume | &
                           |to { iv_level }| ).
  ENDMETHOD.
  METHOD set_tuner.
    mo_tuner = io_tuner.
    cl_demo_output=>write( |{ mv_description } setting tuner | &
                           |to { mo_tuner->description( ) }| ).
  ENDMETHOD.
  METHOD set_dvd.
    mo_dvd = io_dvd.
    cl_demo_output=>write( |{ mv_description } setting DVD player | &
                           |to { mo_dvd->description( ) }| ).
  ENDMETHOD.
  METHOD set_cd.
    mo_cd = io_cd.
    cl_demo_output=>write( |{ mv_description } setting CD player | &
                           |to { mo_cd->description( ) }| ).
  ENDMETHOD.
  METHOD description.
    rv_text = mv_description.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Home theater component: Projector
*&---------------------------------------------------------------------*
CLASS lcl_projector DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_description TYPE string,
      on, off, wide_screen_mode, tv_mode,
      description RETURNING VALUE(rv_text) TYPE string.
  PRIVATE SECTION.
    DATA mv_description TYPE string.
ENDCLASS.

CLASS lcl_projector IMPLEMENTATION.
  METHOD constructor.
    mv_description = iv_description.
  ENDMETHOD.
  METHOD on.
    cl_demo_output=>write( |{ mv_description } on| ).
  ENDMETHOD.
  METHOD off.
    cl_demo_output=>write( |{ mv_description } off| ).
  ENDMETHOD.
  METHOD wide_screen_mode.
    cl_demo_output=>write( |{ mv_description } | &
                           |in widescreen mode (16x9 aspect ratio)| ).
  ENDMETHOD.
  METHOD tv_mode.
    cl_demo_output=>write( |{ mv_description } | &
                           |in tv mode (4x3 aspect ratio)| ).
  ENDMETHOD.
  METHOD description.
    rv_text = mv_description.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Home theater component: Projector Screen
*&---------------------------------------------------------------------*
CLASS lcl_screen DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_description TYPE string,
      up, down,
      description RETURNING VALUE(rv_text) TYPE string.
  PRIVATE SECTION.
    DATA mv_description TYPE string.
ENDCLASS.

CLASS lcl_screen IMPLEMENTATION.
  METHOD constructor.
    mv_description = iv_description.
  ENDMETHOD.
  METHOD up.
    cl_demo_output=>write( |{ mv_description } going up| ).
  ENDMETHOD.
  METHOD down.
    cl_demo_output=>write( |{ mv_description } going down| ).
  ENDMETHOD.
  METHOD description.
    rv_text = mv_description.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Home theater component: Theater Lights
*&---------------------------------------------------------------------*
CLASS lcl_theater_lights DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_description TYPE string,
      on, off,
      dim IMPORTING iv_level TYPE i,
      description RETURNING VALUE(rv_text) TYPE string.
  PRIVATE SECTION.
    DATA mv_description TYPE string.
ENDCLASS.

CLASS lcl_theater_lights IMPLEMENTATION.
  METHOD constructor.
    mv_description = iv_description.
  ENDMETHOD.
  METHOD on.
    cl_demo_output=>write( |{ mv_description } on| ).
  ENDMETHOD.
  METHOD off.
    cl_demo_output=>write( |{ mv_description } off| ).
  ENDMETHOD.
  METHOD dim.
    cl_demo_output=>write( |{ mv_description } | &
                           |dimming to { iv_level }%| ).
  ENDMETHOD.
  METHOD description.
    rv_text = mv_description.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Home theater component: Popcorn Popper
*&---------------------------------------------------------------------*
CLASS lcl_popcorn_popper DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_description TYPE string,
      on, off, pop,
      description RETURNING VALUE(rv_text) TYPE string.
  PRIVATE SECTION.
    DATA mv_description TYPE string.
ENDCLASS.

CLASS lcl_popcorn_popper IMPLEMENTATION.
  METHOD constructor.
    mv_description = iv_description.
  ENDMETHOD.
  METHOD on.
    cl_demo_output=>write( |{ mv_description } on| ).
  ENDMETHOD.
  METHOD off.
    cl_demo_output=>write( |{ mv_description } off| ).
  ENDMETHOD.
  METHOD pop.
    cl_demo_output=>write( |{ mv_description } popping popcorn!| ).
  ENDMETHOD.
  METHOD description.
    rv_text = mv_description.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Home Theater Facade - a unified interface that is easier to use.
*&---------------------------------------------------------------------*
*&  The Home Theater Facade manages all those subsystem components for
*&  the client. It keeps the client simple and flexible. We can upgrade
*&  the home theater components without affecting the client.
*&---------------------------------------------------------------------*
CLASS lcl_home_theater_facade DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          io_amp       TYPE REF TO lcl_amplifier
          io_tuner     TYPE REF TO lcl_tuner
          io_dvd       TYPE REF TO lcl_dvd_player
          io_cd        TYPE REF TO lcl_cd_player
          io_projector TYPE REF TO lcl_projector
          io_screen    TYPE REF TO lcl_screen
          io_lights    TYPE REF TO lcl_theater_lights
          io_popper    TYPE REF TO lcl_popcorn_popper,
      " watch_movie() follows the same sequence we had to do by hand,
      " but wraps it up in a handy method that does all the work.
      " Notice that for each task we are delegating the responsibility
      " to the corresponding component in the subsystem.
      watch_movie IMPORTING iv_movie TYPE string,
      " And end_movie() takes care of shutting everything down for us.
      " Again, each task is delegated to the appropriate component of
      " the subsystem.
      end_movie,
      listen_to_cd IMPORTING iv_cd_title TYPE string,
      end_cd,
      listen_to_radio IMPORTING iv_frequency TYPE f,
      end_radio,
      watch_tv_on_dvd IMPORTING iv_title TYPE string,
      end_tv_on_dvd.
  PRIVATE SECTION.
    " Here's the composition; these are all the components of the
    " subsystem we are going to use.
    DATA:
      mo_amp       TYPE REF TO lcl_amplifier,
      mo_tuner     TYPE REF TO lcl_tuner,
      mo_dvd       TYPE REF TO lcl_dvd_player,
      mo_cd        TYPE REF TO lcl_cd_player,
      mo_projector TYPE REF TO lcl_projector,
      mo_screen    TYPE REF TO lcl_screen,
      mo_lights    TYPE REF TO lcl_theater_lights,
      mo_popper    TYPE REF TO lcl_popcorn_popper.
ENDCLASS.

CLASS lcl_home_theater_facade IMPLEMENTATION.
  METHOD constructor.
    " The facade is passed a reference to each component of the
    " subsystem in its constructor. The facade then assigns each
    " to the corresponding instance variable.
    mo_amp = io_amp.
    mo_tuner = io_tuner.
    mo_dvd = io_dvd.
    mo_cd = io_cd.
    mo_projector = io_projector.
    mo_lights = io_lights.
    mo_screen = io_screen.
    mo_popper = io_popper.
  ENDMETHOD.
  METHOD watch_movie.
    cl_demo_output=>write_text( |Get ready to watch a movie...| ).
    " Turn on the popcorn popper and start popping ...
    mo_popper->on( ).
    mo_popper->pop( ).
    " Dim the lights to 10% ...
    mo_lights->dim( 10 ).
    " Put the screen down ...
    mo_screen->down( ).
    " Turn on the projector and put it in wide screen mode ...
    mo_projector->on( ).
    mo_projector->wide_screen_mode( ).
    " Turn on the amplifier, set it to the DVD player, put it in
    " surround sound mode and set the volume to 5 ...
    mo_amp->on( ).
    mo_amp->set_dvd( mo_dvd ).
    mo_amp->set_surround_sound( ).
    mo_amp->set_volume( 5 ).
    " Turn on the DVD player, set the audio to surround and FINALLY,
    " play the movie!
    mo_dvd->on( ).
    mo_dvd->set_surround_audio( ).
    mo_dvd->play( iv_movie ).
    " Six different classes involved!
  ENDMETHOD.
  METHOD end_movie.
    cl_demo_output=>write_text( |Shutting movie theater down...| ).
    mo_popper->off( ).
    mo_lights->on( ).
    mo_screen->up( ).
    mo_projector->off( ).
    mo_amp->off( ).
    mo_dvd->pause( ).
    mo_dvd->stop( ).
    mo_dvd->eject( ).
    mo_dvd->off( ).
  ENDMETHOD.
  METHOD listen_to_cd.
    cl_demo_output=>write_text( |Get ready for an audio experence...| ).
    mo_lights->dim( 10 ).
    mo_amp->on( ).
    mo_amp->set_volume( 5 ).
    mo_amp->set_cd( mo_cd ).
    mo_amp->set_stereo_sound( ).
    mo_cd->on( ).
    mo_cd->play( iv_cd_title ).
    mo_cd->play_track( 1 ).
  ENDMETHOD.
  METHOD end_cd.
    cl_demo_output=>write_text( |Shutting down CD...| ).
    mo_lights->on( ).
    mo_amp->off( ).
    mo_cd->pause( ).
    mo_cd->stop( ).
    mo_cd->eject( ).
    mo_cd->off( ).
  ENDMETHOD.
  METHOD listen_to_radio.
    cl_demo_output=>write_text( |Tuning in the airwaves...| ).
    mo_tuner->on( ).
    mo_tuner->set_fm( ).
    mo_tuner->set_frequency( iv_frequency ).
    mo_amp->on( ).
    mo_amp->set_volume( 5 ).
    mo_amp->set_tuner( mo_tuner ).
  ENDMETHOD.
  METHOD end_radio.
    cl_demo_output=>write_text( |Shutting down the tuner...| ).
    mo_tuner->set_am( ).
    mo_tuner->off( ).
    mo_amp->off( ).
  ENDMETHOD.
  METHOD watch_tv_on_dvd.
    cl_demo_output=>write_text( |Get ready for a TV show on DVD...| ).
    mo_screen->down( ).
    mo_projector->on( ).
    mo_projector->tv_mode( ).
    mo_amp->on( ).
    mo_amp->set_dvd( mo_dvd ).
    mo_amp->set_stereo_sound( ).
    mo_amp->set_volume( 5 ).
    mo_dvd->on( ).
    mo_dvd->set_two_channel_audio( ).
    mo_dvd->play( iv_title ).
    mo_dvd->play_episode( 1 ).
  ENDMETHOD.
  METHOD end_tv_on_dvd.
    cl_demo_output=>write_text( |Shutting down DVD...| ).
    mo_screen->up( ).
    mo_projector->off( ).
    mo_amp->off( ).
    mo_dvd->stop( ).
    mo_dvd->eject( ).
    mo_dvd->off( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  It's SHOWTIME! - this is a client of the subsystem facade
*&---------------------------------------------------------------------*
CLASS lcl_home_theater_test_drive DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS lcl_home_theater_test_drive IMPLEMENTATION.
  METHOD main.

    " Here we're creating the components right in the test drive.
    " Normally the client is given a facade; it doesn't have to
    " construct one itself.
    DATA(lo_amp) = NEW lcl_amplifier( |Top-O-Line Amplifier| ).
    DATA(lo_tuner) = NEW lcl_tuner( |Top-O-Line AM/FM Tuner| ).
    DATA(lo_dvd) = NEW lcl_dvd_player( |Top-O-Line DVD Player| ).
    DATA(lo_cd) = NEW lcl_cd_player( |Top-O-Line CD Player| ).
    DATA(lo_projector) = NEW lcl_projector( |Top-O-Line Projector| ).
    DATA(lo_screen) = NEW lcl_screen( |Theater Screen| ).
    DATA(lo_lights) = NEW lcl_theater_lights( |Theater Ceiling Light| ).
    DATA(lo_popper) = NEW lcl_popcorn_popper( |Popcorn Popper| ).

    " Instantiate the Facade with all the components in the subsystem.
    DATA(lo_home_theater) = NEW lcl_home_theater_facade(
      io_amp = lo_amp
      io_tuner = lo_tuner
      io_dvd = lo_dvd
      io_cd = lo_cd
      io_projector = lo_projector
      io_screen = lo_screen
      io_lights = lo_lights
      io_popper = lo_popper ).

    " Use the simplified interface to first start up the movie ...
    lo_home_theater->watch_movie( |Raiders of the Lost Ark| ).
    cl_demo_output=>line( ).

    " ... and then shut it down.
    lo_home_theater->end_movie( ).
    cl_demo_output=>line( ).

    lo_home_theater->listen_to_cd( |Miles Davis - Kind of Blue| ).
    cl_demo_output=>line( ).

    lo_home_theater->end_cd( ).
    cl_demo_output=>line( ).

    lo_home_theater->listen_to_radio( CONV #( '91.1' ) ).
    cl_demo_output=>line( ).

    lo_home_theater->end_radio( ).
    cl_demo_output=>line( ).

    lo_home_theater->watch_tv_on_dvd( 'M*A*S*H - Season 5' ).
    cl_demo_output=>line( ).

    lo_home_theater->end_tv_on_dvd( ).
    cl_demo_output=>line( ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
    |Being Adaptive - the Facade Pattern| ).
  cl_demo_output=>line( ).
  lcl_home_theater_test_drive=>main( ).
  cl_demo_output=>display( ).
