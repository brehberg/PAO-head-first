*&---------------------------------------------------------------------*
*&  Sample for Decorating Objects - the Decorator Pattern
*&    based on Head First Design Patterns: Chapter 3
*&---------------------------------------------------------------------*
REPORT yy_head_first_decorator.

*&---------------------------------------------------------------------*
*&  Beverage acts as our abstract component class.
*&---------------------------------------------------------------------*
CLASS lcl_beverage DEFINITION ABSTRACT.
  PUBLIC SECTION.
* for ABAP 7.5 use...
*    TYPES:
*      BEGIN OF ENUM ty_size,
*        tall, grande, venti,
*      END OF ENUM ty_size.
    TYPES ty_size TYPE i.
    CONSTANTS:
      BEGIN OF c_size,
        tall   TYPE ty_size VALUE 0,
        grande TYPE ty_size VALUE 1,
        venti  TYPE ty_size VALUE 2,
      END OF c_size.
    METHODS:
      set_size IMPORTING iv_size TYPE ty_size,
      size RETURNING VALUE(rv_size) TYPE ty_size,
      " the method description() is already implemented for us
      description RETURNING VALUE(rv_text) TYPE string,
      " but we need to implement cost() in the subclasses
      cost ABSTRACT RETURNING VALUE(rv_amount) TYPE decfloat16.
  PROTECTED SECTION.
    DATA:
      mv_description TYPE string VALUE 'Unknown Beverage',
      mv_size        TYPE ty_size VALUE c_size-tall.
ENDCLASS.

CLASS lcl_beverage IMPLEMENTATION.
  METHOD set_size.
    mv_size = iv_size.
  ENDMETHOD.
  METHOD size.
    rv_size = mv_size.
  ENDMETHOD.
  METHOD description.
    rv_text = mv_description.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  We use an abstract class for the Condiments (Decorator) as well
*&---------------------------------------------------------------------*
CLASS lcl_condiment_decorator DEFINITION ABSTRACT
  INHERITING FROM lcl_beverage.
  " We need to be interchangeable with a Beverage, so we inherit
  " from the Beverage class
  PUBLIC SECTION.
    METHODS:
      " Here, we're going to pass the beverage we're wrapping to the
      " decorator's constructor to set an instance variable.
      constructor IMPORTING io_beverage TYPE REF TO lcl_beverage,
      set_size FINAL REDEFINITION,
      size     FINAL REDEFINITION,
      description REDEFINITION,
      cost        REDEFINITION.
  PRIVATE SECTION.
    " This instance variable is to hold the beverage we are wrapping.
    DATA mo_beverage TYPE REF TO lcl_beverage.
ENDCLASS.

CLASS lcl_condiment_decorator IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mo_beverage = io_beverage.
  ENDMETHOD.
  METHOD set_size.
    mo_beverage->set_size( iv_size ).
  ENDMETHOD.
  METHOD size.
    rv_size = mo_beverage->size( ).
  ENDMETHOD.
  METHOD description.
    rv_text = mo_beverage->description( ).
  ENDMETHOD.
  METHOD cost.
    rv_amount = mo_beverage->cost( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  The four concrete components, one per coffee type.
*&---------------------------------------------------------------------*
*&  First we inherit the Beverage class, since this is a beverage.
*&---------------------------------------------------------------------*
CLASS lcl_espresso DEFINITION FINAL INHERITING FROM lcl_beverage.
  PUBLIC SECTION.
    METHODS: constructor, cost REDEFINITION.
    CONSTANTS c_base_cost TYPE decfloat16 VALUE '1.99'.
ENDCLASS.

CLASS lcl_espresso IMPLEMENTATION.
  METHOD constructor.
    " To take care of the description, we set this in the constructor.
    " Remember, the description instance variable is from Beverage.
    super->constructor( ).
    mv_description = |Espresso|.
  ENDMETHOD.
  METHOD cost.
    " Finally, we need to compute the cost of an Espresso. We don't
    " need to worry about adding in condiments in this class, we just
    " need to return the price of an Espresso as $1.99
    rv_amount = c_base_cost.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Okay, here's another Beverage. All we do is set the appropriate
*&  description, "House Blend Coffee," and then return the cost 89¢
*&---------------------------------------------------------------------*
CLASS lcl_house_blend DEFINITION FINAL INHERITING FROM lcl_beverage.
  PUBLIC SECTION.
    METHODS: constructor, cost REDEFINITION.
    CONSTANTS c_base_cost TYPE decfloat16 VALUE '0.89'.
ENDCLASS.

CLASS lcl_house_blend IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_description = |House Blend Coffee|.
  ENDMETHOD.
  METHOD cost.
    rv_amount = c_base_cost.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Create another Beverage class (Dark Roast) in exactly the same way.
*&---------------------------------------------------------------------*
CLASS lcl_dark_roast DEFINITION FINAL INHERITING FROM lcl_beverage.
  PUBLIC SECTION.
    METHODS: constructor, cost REDEFINITION.
    CONSTANTS c_base_cost TYPE decfloat16 VALUE '0.99'.
ENDCLASS.

CLASS lcl_dark_roast IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_description = |Dark Roast Coffee|.
  ENDMETHOD.
  METHOD cost.
    rv_amount = c_base_cost.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  And create our last Beverage class (Decaf) also in the same way.
*&---------------------------------------------------------------------*
CLASS lcl_decaf DEFINITION FINAL INHERITING FROM lcl_beverage.
  PUBLIC SECTION.
    METHODS: constructor, cost REDEFINITION.
    CONSTANTS c_base_cost TYPE decfloat16 VALUE '1.05'.
ENDCLASS.

CLASS lcl_decaf IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_description = |Decaf Coffee|.
  ENDMETHOD.
  METHOD cost.
    rv_amount = c_base_cost.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  And here are our condiment decorators; notice they need to implement
*&  not only cost() but also description(). We'll see why soon...
*&---------------------------------------------------------------------*
*&  Mocha is a decorator, so we inherit Condiment Decorator
*&---------------------------------------------------------------------*
CLASS lcl_mocha DEFINITION FINAL
  INHERITING FROM lcl_condiment_decorator.
  PUBLIC SECTION.
    METHODS: description REDEFINITION, cost REDEFINITION.
    CONSTANTS c_addin_cost TYPE decfloat16 VALUE '0.20'.
ENDCLASS.

CLASS lcl_mocha IMPLEMENTATION.
  METHOD description.
    " We want our description to not only include the beverage - say
    " 'Dark Roast' - but also to include each item decorating the
    " beverage (for instance, 'Dark Roast, Mocha'). So we first
    " delegate to the object we are decorating to get its description,
    " then append ', Mocha' to that description.
    rv_text = super->description( ) && |, Mocha|.
  ENDMETHOD.
  METHOD cost.
    " Now we need to compute the cost of our beverage with Mocha.
    " First, we delegate the call to the object we're decorating,
    " so that it can compute the base cost; then, we add the cost
    " of Mocha to the result.
    rv_amount = super->cost( ) + c_addin_cost.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Soy condiment (decorator)
*&---------------------------------------------------------------------*
CLASS lcl_soy DEFINITION FINAL INHERITING FROM lcl_condiment_decorator.
  PUBLIC SECTION.
    METHODS: description REDEFINITION, cost REDEFINITION.
    " Condiments can be charged according to size, so for instance,
    " Soy costs 10¢, 15¢, and 20¢ respectively,
    " for tall, grande, and venti coffees.
    CONSTANTS:
      BEGIN OF c_addin_cost,
        tall   TYPE decfloat16 VALUE '0.10',
        grande TYPE decfloat16 VALUE '0.15',
        venti  TYPE decfloat16 VALUE '0.20',
      END OF c_addin_cost.
ENDCLASS.

CLASS lcl_soy IMPLEMENTATION.
  METHOD  description.
    rv_text = super->description( ) && |, Soy|.
  ENDMETHOD.
  METHOD cost.
    rv_amount = super->cost( ) + SWITCH #( size( )
      WHEN c_size-tall   THEN c_addin_cost-tall
      WHEN c_size-grande THEN c_addin_cost-grande
      WHEN c_size-venti  THEN c_addin_cost-venti ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Whip condiment (decorator)
*&---------------------------------------------------------------------*
CLASS lcl_whip DEFINITION FINAL INHERITING FROM lcl_condiment_decorator.
  PUBLIC SECTION.
    METHODS: description REDEFINITION, cost REDEFINITION.
    CONSTANTS c_addin_cost TYPE decfloat16 VALUE '0.10'.
ENDCLASS.

CLASS lcl_whip IMPLEMENTATION.
  METHOD description.
    rv_text = super->description( ) && |, Whip|.
  ENDMETHOD.
  METHOD cost.
    rv_amount = super->cost( ) + c_addin_cost.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Steamed Milk condiment (decorator)
*&---------------------------------------------------------------------*
CLASS lcl_milk DEFINITION FINAL INHERITING FROM lcl_condiment_decorator.
  PUBLIC SECTION.
    METHODS: description REDEFINITION, cost REDEFINITION.
    CONSTANTS c_addin_cost TYPE decfloat16 VALUE '0.10'.
ENDCLASS.

CLASS lcl_milk IMPLEMENTATION.
  METHOD description.
    rv_text = super->description( ) && |, Milk|.
  ENDMETHOD.
  METHOD cost.
    rv_amount = super->cost( ) + c_addin_cost.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Fastest growing coffee shop around
*&---------------------------------------------------------------------*
CLASS lcl_starbuzz_coffee DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS lcl_starbuzz_coffee IMPLEMENTATION.
  METHOD main.

    DATA:
      lo_beverage1 TYPE REF TO lcl_beverage,
      lo_beverage2 TYPE REF TO lcl_beverage,
      lo_beverage3 TYPE REF TO lcl_beverage,
      lo_beverage4 TYPE REF TO lcl_beverage.

    " Order up an espresso, no condiments, and print decription and cost
    lo_beverage1 = NEW lcl_espresso( ).
    cl_demo_output=>write_text( |Beverage #1 description and cost...| ).
    cl_demo_output=>write( |{ lo_beverage1->description( ) }| &
                           | ${ lo_beverage1->cost( ) DECIMALS = 2 }| ).
    cl_demo_output=>line( ).

    lo_beverage2 = NEW lcl_dark_roast( ).          " Make a Dark Roast
    lo_beverage2 = NEW lcl_mocha( lo_beverage2 ).  " Wrap it in a Mocha
    lo_beverage2 = NEW lcl_mocha( lo_beverage2 ).  " Wrap it in a 2nd
    lo_beverage2 = NEW lcl_whip( lo_beverage2 ).   " Wrap it in a Whip
    cl_demo_output=>write_text( |Beverage #2 description and cost...| ).
    cl_demo_output=>write( |{ lo_beverage2->description( ) }| &
                           | ${ lo_beverage2->cost( ) DECIMALS = 2 }| ).
    cl_demo_output=>line( ).

    " Also, give us a Venti sized House Blend with Soy, Mocha, and Whip
    lo_beverage3 = NEW lcl_house_blend( ).
    lo_beverage3->set_size( lcl_beverage=>c_size-venti ).
    lo_beverage3 = NEW lcl_soy( lo_beverage3 ).
    lo_beverage3 = NEW lcl_mocha( lo_beverage3 ).
    lo_beverage3 = NEW lcl_whip( lo_beverage3 ).
    cl_demo_output=>write_text( |Beverage #3 description and cost...| ).
    cl_demo_output=>write( |{ lo_beverage3->description( ) }| &
                           | ${ lo_beverage3->cost( ) DECIMALS = 2 }| ).
    cl_demo_output=>line( ).

    " Finally, we need a Decaf with Soy and Milk, but make it a Grande
    lo_beverage4 = NEW lcl_decaf( ).
    lo_beverage4 = NEW lcl_soy( lo_beverage4 ).
    lo_beverage4 = NEW lcl_milk( lo_beverage4 ).
    lo_beverage4->set_size( lcl_beverage=>c_size-grande ).
    cl_demo_output=>write_text( |Beverage #4 description and cost...| ).
    cl_demo_output=>write( |{ lo_beverage4->description( ) }| &
                           | ${ lo_beverage4->cost( ) DECIMALS = 2 }| ).
    cl_demo_output=>line( ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
    |Decorating Objects - the Decorator Pattern| ).
  cl_demo_output=>line( ).
  lcl_starbuzz_coffee=>main( ).
  cl_demo_output=>display( ).
