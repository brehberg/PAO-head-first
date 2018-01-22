*&---------------------------------------------------------------------*
*&  Sample for Baking with OO Goodness - the Factory Pattern
*&    based on Head First Design Patterns: Chapter 4
*&---------------------------------------------------------------------*
REPORT yy_head_first_factory_method.

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
      prepare, bake, cut, box.
  PROTECTED SECTION.
    " Each pizza has a name, a type of dough, a type of sauce,
    " and a set of toppings.
    DATA:
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
    " Preparation follows a number of steps in a particular sequence.
    cl_demo_output=>write( |Preparing { mv_name }...| ).
    cl_demo_output=>write( |Tossing { mv_dough }...| ).
    cl_demo_output=>write( |Adding { mv_sauce }...| ).
    cl_demo_output=>write( REDUCE string(
      INIT text = |Adding toppings... \n|
      FOR topping IN mt_toppings
      NEXT text = text && |\t{ topping }\n| ) ).
  ENDMETHOD.
  METHOD bake.
    " The abstract class provides some basic defaults for baking, ...
    cl_demo_output=>write( |Bake for 25 minutes at 350| ).
  ENDMETHOD.
  METHOD cut.                                      " ... cutting, ...
    cl_demo_output=>write( |Cut the pizza into diagonal slices| ).
  ENDMETHOD.
  METHOD box.                                      " ... and boxing.
    cl_demo_output=>write( |Place pizza in official Pizza Store box| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Each product needs to implement the Pizza interface (which in
*&  this case means 'inherit from the abstract Pizza class') and
*&  also be concrete. As long as that's the case, it can be created
*&  by the factory and handed back to the client.
*&---------------------------------------------------------------------*
*&  Concrete product: NY style cheese pizza
*&---------------------------------------------------------------------*
CLASS lcl_ny_style_cheese_pizza DEFINITION FINAL
  INHERITING FROM lcl_pizza.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

CLASS lcl_ny_style_cheese_pizza IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_name = |NY Style Sauce and Cheese Pizza|.
    " The NY Pizza has its own marinara style sauce and thin crust.
    mv_dough = |Thin Crust Dough|.
    mv_sauce = |Marinara Sauce|.
    " And one topping, reggiano cheese!
    mt_toppings = VALUE #( ( |Grated Reggiano Cheese| ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete product: Chicago style cheese pizza
*&---------------------------------------------------------------------*
CLASS lcl_chicago_cheese_pizza DEFINITION FINAL
  INHERITING FROM lcl_pizza.
  PUBLIC SECTION.
    METHODS: constructor, cut REDEFINITION.
ENDCLASS.

CLASS lcl_chicago_cheese_pizza IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_name = |Chicago Deep Dish Cheese Pizza|.
    " This pizza uses plum tomatoes as a sauce with extra-thick crust.
    mv_dough = |Extra Thick Crust Dough|.
    mv_sauce = |Plum Tomato Sauce|.
    " The Chicago style deep dish pizza has lots of mozzarella cheese!
    mt_toppings = VALUE #( ( |Shredded Mozzarella Cheese| ) ).
  ENDMETHOD.
  METHOD cut.
    " Chicago style pizza also overrides the cut() method.
    cl_demo_output=>write( |Cut the pizza into square slices| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete product: NY style pepperoni pizza
*&---------------------------------------------------------------------*
CLASS lcl_ny_style_pepperoni_pizza DEFINITION FINAL
  INHERITING FROM lcl_pizza.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

CLASS lcl_ny_style_pepperoni_pizza IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_name = |New York Style Pepperoni Pizza|.
    mv_dough = |Thin Crust Dough|.
    mv_sauce = |Marinara Sauce|.
    mt_toppings = VALUE #(
      ( |Garlic| )
      ( |Onions| )
      ( |Mushrooms| )
      ( |Red Pepper| )
      ( |Sliced Pepperoni| )
      ( |Grated Reggiano Cheese| ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete product:  NY style clam pizza
*&---------------------------------------------------------------------*
CLASS lcl_ny_style_clam_pizza DEFINITION FINAL
  INHERITING FROM lcl_pizza.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

CLASS lcl_ny_style_clam_pizza IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_name = |New York Style Clam Pizza|.
    mv_dough = |Thin Crust Dough|.
    mv_sauce = |Marinara Sauce|.
    mt_toppings = VALUE #(
      ( |Fresh Clams from Long Island Sound| )
      ( |Grated Reggiano Cheese| ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete product: NY style veggie pizza
*&---------------------------------------------------------------------*
CLASS lcl_ny_style_veggie_pizza DEFINITION FINAL
  INHERITING FROM lcl_pizza.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

CLASS lcl_ny_style_veggie_pizza IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_name = |New York Style Veggie Pizza|.
    mv_dough = |Thin Crust Dough|.
    mv_sauce = |Marinara Sauce|.
    mt_toppings = VALUE #(
      ( |Garlic| )
      ( |Onions| )
      ( |Mushrooms| )
      ( |Red Pepper| )
      ( |Grated Reggiano Cheese| ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete product: Chicago style pepperoni pizza
*&---------------------------------------------------------------------*
CLASS lcl_chicago_pepperoni_pizza DEFINITION FINAL
  INHERITING FROM lcl_pizza.
  PUBLIC SECTION.
    METHODS: constructor, cut REDEFINITION.
ENDCLASS.

CLASS lcl_chicago_pepperoni_pizza IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_name = |Chicago Style Pepperoni Pizza|.
    mv_dough = |Extra Thick Crust Dough|.
    mv_sauce = |Plum Tomato Sauce|.
    mt_toppings = VALUE #(
      ( |Black Olives| )
      ( |Spinach| )
      ( |Eggplant| )
      ( |Sliced Pepperoni| )
      ( |Shredded Mozzarella Cheese| ) ).
  ENDMETHOD.
  METHOD cut.
    cl_demo_output=>write( |Cut the pizza into square slices| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete product: Chicago style clam pizza
*&---------------------------------------------------------------------*
CLASS lcl_chicago_clam_pizza DEFINITION FINAL
  INHERITING FROM lcl_pizza.
  PUBLIC SECTION.
    METHODS: constructor, cut REDEFINITION.
ENDCLASS.

CLASS lcl_chicago_clam_pizza IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_name = |Chicago Style Clam Pizza|.
    mv_dough = |Extra Thick Crust Dough|.
    mv_sauce = |Plum Tomato Sauce|.
    mt_toppings = VALUE #(
      ( |Frozen Clams from Chesapeake Bay| )
      ( |Shredded Mozzarella Cheese| ) ).
  ENDMETHOD.
  METHOD cut.
    cl_demo_output=>write( |Cut the pizza into square slices| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete product: Chicago style veggie pizza
*&---------------------------------------------------------------------*
CLASS lcl_chicago_veggie_pizza DEFINITION FINAL
  INHERITING FROM lcl_pizza.
  PUBLIC SECTION.
    METHODS: constructor, cut REDEFINITION.
ENDCLASS.

CLASS lcl_chicago_veggie_pizza IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_name = |Chicago Deep Dish Veggie Pizza|.
    mv_dough = |Extra Thick Crust Dough|.
    mv_sauce = |Plum Tomato Sauce|.
    mt_toppings = VALUE #(
      ( |Black Olives| )
      ( |Spinach| )
      ( |Eggplant| )
      ( |Shredded Mozzarella Cheese| ) ).
  ENDMETHOD.
  METHOD cut.
    cl_demo_output=>write( |Cut the pizza into square slices| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  This is our abstract creator class. It defines an abstract factory
*&  method that the subclasses implement to produce products.
*&---------------------------------------------------------------------*
*&  Often the creator contains code that depends on an abstract product,
*&  which is produced by a subclass. The creator never really knows
*&  which concrete product was produced.
*&---------------------------------------------------------------------*
CLASS lcl_pizza_store DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS order_pizza FINAL
      IMPORTING iv_type TYPE string
      RETURNING VALUE(ro_pizza) TYPE REF TO lcl_pizza.
  PROTECTED SECTION.
    " All the responsibility for instantiating Pizzas has been moved
    " into a method that acts as a factory.
    METHODS create_pizza ABSTRACT
      IMPORTING iv_item TYPE string
      RETURNING VALUE(ro_pizza) TYPE REF TO lcl_pizza.
ENDCLASS.

CLASS lcl_pizza_store IMPLEMENTATION.
  METHOD order_pizza.
    " The subclasses of Pizza Store handle object instantiation for us
    " in the create_pizza() method.
    ro_pizza = create_pizza( iv_type ).
    cl_demo_output=>write_text(
      |------ Making a { ro_pizza->name( ) } ------| ).
    ro_pizza->prepare( ).
    ro_pizza->bake( ).
    ro_pizza->cut( ).
    ro_pizza->box( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Classes that produce products are called concrete creators.
*&---------------------------------------------------------------------*
*&  The NY Pizza Store is a subclass of Pizza Store, so it inherits the
*&  order_pizza() method (among others).
*&---------------------------------------------------------------------*
CLASS lcl_ny_pizza_store DEFINITION FINAL
  INHERITING FROM lcl_pizza_store.
  PROTECTED SECTION.
    " This is our factory method. We've got to implement create_pizza()
    " here, since it is abstract in Pizza Store.
    METHODS create_pizza REDEFINITION.
ENDCLASS.

CLASS lcl_ny_pizza_store IMPLEMENTATION.
  METHOD create_pizza.
    " Here's where we create our concrete classes. For each type of
    " Pizza we create the NY style.
    ro_pizza = SWITCH #( iv_item
      WHEN 'cheese'    THEN NEW lcl_ny_style_cheese_pizza( )
      WHEN 'pepperoni' THEN NEW lcl_ny_style_pepperoni_pizza( )
      WHEN 'clam'      THEN NEW lcl_ny_style_clam_pizza( )
      WHEN 'veggie'    THEN NEW lcl_ny_style_veggie_pizza( ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Since each franchise gets its own subclass of Pizza Store, it's
*&  free to create its own style of pizza by implementing create_pizza()
*&---------------------------------------------------------------------*
CLASS lcl_chicago_pizza_store DEFINITION FINAL
  INHERITING FROM lcl_pizza_store.
  PROTECTED SECTION.
    METHODS create_pizza REDEFINITION.
ENDCLASS.

CLASS lcl_chicago_pizza_store IMPLEMENTATION.
  METHOD create_pizza.
    " Note that the order_pizza() method in the superclass has no clue
    " which pizza we are creating; it just knows it can prepare, bake,
    " cut, and box it!
    ro_pizza = SWITCH #( iv_item
      WHEN 'cheese'    THEN NEW lcl_chicago_cheese_pizza( )
      WHEN 'pepperoni' THEN NEW lcl_chicago_pepperoni_pizza( )
      WHEN 'clam'      THEN NEW lcl_chicago_clam_pizza( )
      WHEN 'veggie'    THEN NEW lcl_chicago_veggie_pizza( ) ).
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

    DATA(lt_pizza_types) = VALUE table_of_strings(
      ( |cheese| ) ( |pepperoni| ) ( |clam| ) ( |veggie| ) ).

    " First we create two different stores.
    DATA(lo_ny_store) = NEW lcl_ny_pizza_store( ).
    DATA(lo_chicago_store) = NEW lcl_chicago_pizza_store( ).

    " Then for each known type of pizza...
    LOOP AT lt_pizza_types INTO DATA(lv_pizza_type).

      " ... we use one store to make Ed's order ...
      DATA(lo_pizza) = lo_ny_store->order_pizza( lv_pizza_type ).
      cl_demo_output=>write_text( |Ed ordered | && lo_pizza->name( ) ).
      cl_demo_output=>line( ).

      " ... and the other for Jay's.
      lo_pizza = lo_chicago_store->order_pizza( lv_pizza_type ).
      cl_demo_output=>write_text( |Jay ordered | && lo_pizza->name( ) ).
      cl_demo_output=>line( ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
    |Baking with OO Goodness - the Factory Method Pattern| ).
  cl_demo_output=>line( ).
  lcl_pizza_test_drive=>main( ).
  cl_demo_output=>display( ).
