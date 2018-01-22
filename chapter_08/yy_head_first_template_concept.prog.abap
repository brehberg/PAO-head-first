*&---------------------------------------------------------------------*
*&  Sample for Encapsulating Algorithms - the Template Method Pattern
*&    based on Head First Design Patterns: Chapter 8
*&---------------------------------------------------------------------*
REPORT yy_head_first_template_concept.

*&---------------------------------------------------------------------*
*&  Here's our Coffee class for making coffee
*&---------------------------------------------------------------------*
CLASS lcl_coffee DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      prepare_recipe,
      " Each of these methods implements one step of the alorithm.
      " There's a method to boil water, brew the coffee, pour the
      " coffee in a cup, and add sugar and milk.
      boil_water, brew_coffee_grinds, pour_in_cup, add_sugar_and_milk.
ENDCLASS.

CLASS lcl_coffee IMPLEMENTATION.
  METHOD prepare_recipe.
    " Here's our coffee recipe, straight out of the training manual.
    " Each of the steps is implemented as a separate method.
    boil_water( ).
    brew_coffee_grinds( ).
    pour_in_cup( ).
    add_sugar_and_milk( ).
  ENDMETHOD.
  METHOD boil_water.
    cl_demo_output=>write( |Boiling water| ).
  ENDMETHOD.
  METHOD brew_coffee_grinds.
    cl_demo_output=>write( |Dripping Coffee through filter| ).
  ENDMETHOD.
  METHOD pour_in_cup.
    cl_demo_output=>write( |Pouring into cup| ).
  ENDMETHOD.
  METHOD add_sugar_and_milk.
    cl_demo_output=>write( |Adding Sugar and Milk| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  And now the Tea class for making tea
*&---------------------------------------------------------------------*
CLASS lcl_tea DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      prepare_recipe,
      " Notice that these two methods are exactly the same as they are
      " in Coffee! So we definitely have some code duplication going on.
      boil_water, pour_in_cup,
      " These two methods are specialized to Tea.
      steep_tea_bag, add_lemon.
ENDCLASS.

CLASS lcl_tea IMPLEMENTATION.
  METHOD prepare_recipe.
    " This looks very similar to the one we just implemented in Coffee;
    " the second and fourth steps are different, but it's basically
    " the same recipe.
    boil_water( ).
    steep_tea_bag( ).
    pour_in_cup( ).
    add_lemon( ).
  ENDMETHOD.
  METHOD boil_water.
    cl_demo_output=>write( |Boiling water| ).
  ENDMETHOD.
  METHOD steep_tea_bag.
    cl_demo_output=>write( |Steeping the Tea| ).
  ENDMETHOD.
  METHOD pour_in_cup.
    cl_demo_output=>write( |Pouring into cup| ).
  ENDMETHOD.
  METHOD add_lemon.
    cl_demo_output=>write( |Adding Lemon| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Let's play "coding barista" and create some coffee and tea.
*&---------------------------------------------------------------------*
CLASS lcl_barista DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS lcl_barista IMPLEMENTATION.
  METHOD main.

    DATA(lo_tea) = NEW lcl_tea( ).
    DATA(lo_coffee) = NEW lcl_coffee( ).

    cl_demo_output=>write_text( |Making tea...| ).
    lo_tea->prepare_recipe( ).
    cl_demo_output=>line( ).

    cl_demo_output=>write_text( |Making coffee...| ).
    lo_coffee->prepare_recipe( ).
    cl_demo_output=>line( ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
   |Encapsulating Algorithms - the Template Method Pattern (concept)| ).
  cl_demo_output=>line( ).
  lcl_barista=>main( ).
  cl_demo_output=>display( ).
