*&---------------------------------------------------------------------*
*&  Sample for Baking with OO Goodness - the Factory Pattern
*&    based on Head First Design Patterns: Chapter 4
*&---------------------------------------------------------------------*
REPORT yy_head_first_factory_simple.

*&---------------------------------------------------------------------*
*&  This is the product of the factory: pizza!
*&---------------------------------------------------------------------*
*&  We've defined Pizza as an abstract class with some helpful
*&  implementations that can be overridden.
*&---------------------------------------------------------------------*
CLASS lcl_pizza DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      name RETURNING VALUE(rv_name) TYPE string,
      prepare, bake, cut, box,
      description RETURNING VALUE(rv_text) TYPE string.
  PROTECTED SECTION.
    DATA:
      " Each pizza has a name, a type of dough, a type of sauce,
      " and a set of toppings.
      mv_name     TYPE string,
      mv_dough    TYPE string,
      mv_sauce    TYPE string,
      mt_toppings TYPE table_of_strings.
ENDCLASS.

CLASS lcl_pizza IMPLEMENTATION.
  METHOD name.
    rv_name = mv_name.
  ENDMETHOD.
  METHOD prepare.
    cl_demo_output=>write( |Preparing { mv_name }...| ).
  ENDMETHOD.
  METHOD bake.
    cl_demo_output=>write( |Baking { mv_name }...| ).
  ENDMETHOD.
  METHOD cut.
    cl_demo_output=>write( |Cutting { mv_name }...| ).
  ENDMETHOD.
  METHOD box.
    cl_demo_output=>write( |Boxing { mv_name }...| ).
  ENDMETHOD.
  METHOD description.
    rv_text = |---- { mv_name } ----\n| &
      |{ mv_dough }\n| & |{ mv_sauce }\n| &&
      concat_lines_of( table = mt_toppings sep = |\n| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete product: Cheese pizza
*&---------------------------------------------------------------------*
*&  Each product needs to implement the Pizza interface (which in
*&  this case means 'inherit from the abstract Pizza class') and
*&  also be concrete. As long as that's the case, it can be created
*&  by the factory and handed back to the client.
*&---------------------------------------------------------------------*
CLASS lcl_cheese_pizza DEFINITION FINAL INHERITING FROM lcl_pizza.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

CLASS lcl_cheese_pizza IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_name = |Cheese Pizza|.
    mv_dough = |Regular Crust|.
    mv_sauce = |Marinara Pizza Sauce|.
    mt_toppings = VALUE #(
      ( |Fresh Mozzarella| )
      ( |Grated Parmesan Cheese| ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete product: Classic pepperoni pizza
*&---------------------------------------------------------------------*
CLASS lcl_pepperoni_pizza DEFINITION FINAL INHERITING FROM lcl_pizza.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

CLASS lcl_pepperoni_pizza IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_name = |Pepperoni Pizza|.
    mv_dough = |Crust|.
    mv_sauce = |Marinara sauce|.
    mt_toppings = VALUE #(
      ( |Sliced Pepperoni| )
      ( |Sliced Onion| )
      ( |Grated Parmesan Cheese| ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete product: Trendy clam pizza
*&---------------------------------------------------------------------*
CLASS lcl_clam_pizza DEFINITION FINAL INHERITING FROM lcl_pizza.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

CLASS lcl_clam_pizza IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_name = |Clam Pizza|.
    mv_dough = |Thin crust|.
    mv_sauce = |White garlic sauce|.
    mt_toppings = VALUE #(
      ( |Clams| )
      ( |Grated Parmesan Cheese| ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete product: Veggie pizza
*&---------------------------------------------------------------------*
CLASS lcl_veggie_pizza DEFINITION FINAL INHERITING FROM lcl_pizza.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

CLASS lcl_veggie_pizza IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_name = |Veggie Pizza|.
    mv_dough = |Crust|.
    mv_sauce = |Marinara sauce|.
    mt_toppings = VALUE #(
      ( |Diced Onion| )
      ( |Sliced Mushrooms| )
      ( |Sliced Red pepper| )
      ( |Sliced Black olives| )
      ( |Shredded Mozzarella| )
      ( |Grated Parmesan Cheese| ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  This is the factory where we create pizzas; it should be the only
*&  part of our application that refers to concrete Pizza classes.
*&---------------------------------------------------------------------*
CLASS lcl_simple_pizza_factory DEFINITION FINAL.
  PUBLIC SECTION.
    " We define a create_pizza() method in the factory. This is the
    " method all clients will use to instantiate new objects.
    METHODS create_pizza
      " This code is parameterized by the type of the pizza, just
      " like our Pizza Store order_pizza() method is.
      IMPORTING iv_type TYPE string
      RETURNING VALUE(ro_pizza) TYPE REF TO lcl_pizza.
ENDCLASS.

CLASS lcl_simple_pizza_factory IMPLEMENTATION.
  METHOD create_pizza.
    " Based on the type of pizza, we instantiate the correct concrete
    " class and assign it to the pizza return variable. Note that each
    " pizza here has to implement the Pizza interface.
    ro_pizza = SWITCH #( iv_type
      WHEN 'cheese'    THEN NEW lcl_cheese_pizza( )
      WHEN 'pepperoni' THEN NEW lcl_pepperoni_pizza( )
      WHEN 'clam'      THEN NEW lcl_clam_pizza( )
      WHEN 'veggie'    THEN NEW lcl_veggie_pizza( ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  This is the client of the factory. Pizza Store goes through the
*&  Simple Pizza Factory to get intances of pizza.
*&---------------------------------------------------------------------*
CLASS lcl_pizza_store DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      " Pizza Store gets the factory passed to it in the constructor.
      constructor
        IMPORTING io_factory TYPE REF TO lcl_simple_pizza_factory,
      " We're passing in the type of pizza to order_pizza()
      order_pizza
        IMPORTING iv_type TYPE string
        RETURNING VALUE(ro_pizza) TYPE REF TO lcl_pizza.
  PRIVATE SECTION.
    " We give Pizza Store a reference to a Simple Pizza Factory
    DATA mo_factory TYPE REF TO lcl_simple_pizza_factory.
ENDCLASS.

CLASS lcl_pizza_store IMPLEMENTATION.
  METHOD constructor.
    mo_factory = io_factory.
  ENDMETHOD.
  METHOD order_pizza.
    " And the order_pizza() method uses the factory to create its
    " pizzas by simply passing on the type of the order.
    ro_pizza = mo_factory->create_pizza( iv_type ).
    " Once we have a Pizza, we prepare it (you know, roll the dough,
    " put on the sauce and add the toppings & cheese), then we bake
    " it, cut it, and box it. Each Pizza subtype (Cheese Pizza, Veggie
    " Pizza, etc.) knows how to prepare itself!
    ro_pizza->prepare( ).
    ro_pizza->bake( ).
    ro_pizza->cut( ).
    ro_pizza->box( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Cutting-edge pizza store demonstration
*&---------------------------------------------------------------------*
CLASS lcl_pizza_test_drive DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS lcl_pizza_test_drive IMPLEMENTATION.
  METHOD main.

    DATA(lo_factory) = NEW lcl_simple_pizza_factory( ).
    DATA(lo_store) = NEW lcl_pizza_store( lo_factory ).

    DATA(lo_pizza) = lo_store->order_pizza( |cheese| ).
    cl_demo_output=>write_text( |We ordered a | && lo_pizza->name( ) ).

    cl_demo_output=>write( lo_pizza->description( ) ).
    cl_demo_output=>line( ).

    lo_pizza = lo_store->order_pizza( |veggie| ).
    cl_demo_output=>write_text( |We ordered a | && lo_pizza->name( ) ).

    cl_demo_output=>write( lo_pizza->description( ) ).
    cl_demo_output=>line( ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
    |Baking with OO Goodness - the Simple Factory Pattern| ).
  cl_demo_output=>line( ).
  lcl_pizza_test_drive=>main( ).
  cl_demo_output=>display( ).
