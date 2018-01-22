*&---------------------------------------------------------------------*
*&  Sample for Encapsulating Algorithms - the Template Method Pattern
*&    based on Head First Design Patterns: Chapter 8
*&---------------------------------------------------------------------*
REPORT yy_head_first_template_method.

*&---------------------------------------------------------------------*
*&  Caffeine Beverage is our high-level component. It has control over
*&  the algorithm for the recipe, and calls on the subclass only when
*&  they're needed for an implementation of a method.
*&---------------------------------------------------------------------*
CLASS lcl_caffeine_beverage DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      " Now, the prepare_recipe() method will be used to make both Tea
      " and Coffee. prepare_recipe() is declared final because we don't
      " want our subclasses to be able to override this method and
      " change the recipe! We've also generalized steps 2 and 4 to
      " brew() the beverage and to add_condiments().
      prepare_recipe FINAL,
      " Rememeber, we moved these into the Caffeine Beverage class.
      boil_water, pour_in_cup,
      " Because Coffee and Tea handle these methods in different ways,
      " they're going to have to be declared as abstract. Let the
      " subclasses worry about that stuff!
      brew ABSTRACT, add_condiments ABSTRACT.
ENDCLASS.

CLASS lcl_caffeine_beverage IMPLEMENTATION.
  METHOD prepare_recipe.
    " prepare_recipe() is our template method. Why?  Because:
    " (1) It is a method, after all
    " (2) It serves as a template for an algorithm, in this case, an
    " algorithm for making caffeinated beverages.
    boil_water( ).      " In the template, each step of the algorithm
    brew( ).            "   is represented by a method.
    pour_in_cup( ).     " Some methods are handled by this class...
    add_condiments( ).  " ...and some are handled by the subclass.
  ENDMETHOD.
  METHOD boil_water.
    cl_demo_output=>write( |Boiling water| ).
  ENDMETHOD.
  METHOD pour_in_cup.
    cl_demo_output=>write( |Pouring into cup| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  In our design, Tea now inherits from Caffeine Beverage. The
*&  subclasses are used simply to provide implementation details.
*&---------------------------------------------------------------------*
CLASS lcl_tea DEFINITION FINAL INHERITING FROM lcl_caffeine_beverage.
  PUBLIC SECTION.
    METHODS:
      " Tea needs to define brew() and add_condiements() - the two
      " abstract methods from Caffeine Beverage.
      brew REDEFINITION, add_condiments REDEFINITION.
ENDCLASS.

CLASS lcl_tea IMPLEMENTATION.
  METHOD brew.
    cl_demo_output=>write( |Steeping the Tea| ).
  ENDMETHOD.
  METHOD add_condiments.
    cl_demo_output=>write( |Adding Lemon| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  In our design, Coffee also inherits from Caffeine Beverage.
*&---------------------------------------------------------------------*
CLASS lcl_coffee DEFINITION FINAL INHERITING FROM lcl_caffeine_beverage.
  PUBLIC SECTION.
    METHODS:
      " Same methods as for Tea, except Coffee deals with coffee, and
      " sugar and milk instead of tea bags and lemon.
      brew REDEFINITION, add_condiments REDEFINITION.
ENDCLASS.

CLASS lcl_coffee IMPLEMENTATION.
  METHOD brew.
    cl_demo_output=>write( |Dripping Coffee through filter| ).
  ENDMETHOD.
  METHOD add_condiments.
    cl_demo_output=>write( |Adding Sugar and Milk| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Beverage with Hook is another abstract base class in our design.
*&---------------------------------------------------------------------*
CLASS lcl_beverage_with_hook DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      prepare_recipe FINAL,
      boil_water, pour_in_cup,
      brew ABSTRACT, add_condiments ABSTRACT,
      " This is a hook because the subclass can override this method,
      " but doesn't have to.
      customer_wants_condiments RETURNING VALUE(rv_flag) TYPE abap_bool.
ENDCLASS.

CLASS lcl_beverage_with_hook IMPLEMENTATION.
  METHOD prepare_recipe.
    boil_water( ).
    brew( ).
    pour_in_cup( ).
    " We've added a little conditional statement that bases its
    " success on a concrete method, customer_wants_condiments().
    IF customer_wants_condiments( ).
      " If the customer WANTS condiments, only then do we add condiments
      add_condiments( ).
    ENDIF.
  ENDMETHOD.
  METHOD boil_water.
    cl_demo_output=>write( |Boiling water| ).
  ENDMETHOD.
  METHOD pour_in_cup.
    cl_demo_output=>write( |Pouring into cup| ).
  ENDMETHOD.
  METHOD customer_wants_condiments.
    " Here're we've defined a method with a (mostly) empty default
    " implementation. This method just returns true and nothing else.
    rv_flag = abap_true.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Tea With Hook now inherits from Beverage with Hook.
*&---------------------------------------------------------------------*
CLASS lcl_tea_with_hook DEFINITION FINAL
  INHERITING FROM lcl_beverage_with_hook.
  PUBLIC SECTION.
    METHODS:
      brew REDEFINITION, add_condiments REDEFINITION,
      " Here's where you override the hook and provide your
      " own functionality
      customer_wants_condiments REDEFINITION.
  PRIVATE SECTION.
    " This code asks the user if he'd like lemon and gets
    " his input from some user interface object.
    METHODS get_user_input RETURNING VALUE(rv_answer) TYPE string.
ENDCLASS.

CLASS lcl_tea_with_hook IMPLEMENTATION.
  METHOD brew.
    cl_demo_output=>write( |Steeping the Tea| ).
  ENDMETHOD.
  METHOD add_condiments.
    cl_demo_output=>write( |Adding Lemon| ).
  ENDMETHOD.
  METHOD customer_wants_condiments.
    " Get the user's input on the condiment decision and return true
    " or false depending on the input.
    DATA(lv_answer) = get_user_input( ).
    rv_flag = xsdbool( to_lower( lv_answer(1) ) = 'y' ).
  ENDMETHOD.
  METHOD get_user_input.
    " Yes, of course we want that lemon!
    rv_answer = 'Yes'.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Coffee With Hook also inherits from Beverage with Hook.
*&---------------------------------------------------------------------*
CLASS lcl_coffee_with_hook DEFINITION FINAL
  INHERITING FROM lcl_beverage_with_hook.
  PUBLIC SECTION.
    METHODS:
      brew REDEFINITION, add_condiments REDEFINITION,
      " Here's where you override the hook and provide your
      " own functionality
      customer_wants_condiments REDEFINITION.
  PRIVATE SECTION.
    " This method asks the user if he'd like milk and sugar and gets
    " his input from some user interface object.
    METHODS get_user_input RETURNING VALUE(rv_answer) TYPE string.
ENDCLASS.

CLASS lcl_coffee_with_hook IMPLEMENTATION.
  METHOD brew.
    cl_demo_output=>write( |Dripping Coffee through filter| ).
  ENDMETHOD.
  METHOD add_condiments.
    cl_demo_output=>write( |Adding Sugar and Milk| ).
  ENDMETHOD.
  METHOD customer_wants_condiments.
    " Get the user's input on the condiment decision and return true
    " or false depending on the input.
    DATA(lv_answer) = get_user_input( ).
    rv_flag = xsdbool( to_lower( lv_answer(1) ) = 'y' ).
  ENDMETHOD.
  METHOD get_user_input.
    " We'll pass on the waistline expanding condiments.
    rv_answer = 'No'.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Let's test drive our new Coffee and Tea classes.
*&---------------------------------------------------------------------*
CLASS lcl_beverage_test_drive DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS lcl_beverage_test_drive IMPLEMENTATION.
  METHOD main.

    DATA(lo_tea) = NEW lcl_tea( ).
    DATA(lo_coffee) = NEW lcl_coffee( ).

    cl_demo_output=>write_text( |Making tea...| ).
    " The prepare_recipe() method controls the algorithm. No one can
    " change this, and it relies on subclasses to provide some or all
    " of the implementation details.
    lo_tea->prepare_recipe( ).
    cl_demo_output=>line( ).

    cl_demo_output=>write_text( |Making coffee...| ).
    lo_coffee->prepare_recipe( ).
    cl_demo_output=>line( ).

    " Create another Tea and Coffee (with hooks to get user input)
    DATA(lo_tea_with_hook) = NEW lcl_tea_with_hook( ).
    DATA(lo_coffee_with_hook) = NEW lcl_coffee_with_hook( ).

    " And call prepare_recipe() on both!
    cl_demo_output=>write_text( |Making tea...| ).
    lo_tea_with_hook->prepare_recipe( ).
    cl_demo_output=>line( ).

    cl_demo_output=>write_text( |Making coffee...| ).
    lo_coffee_with_hook->prepare_recipe( ).
    cl_demo_output=>line( ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
    |Encapsulating Algorithms - the Template Method Pattern| ).
  cl_demo_output=>line( ).
  lcl_beverage_test_drive=>main( ).
  cl_demo_output=>display( ).
