*&---------------------------------------------------------------------*
*&  Sample for Baking with OO Goodness - the Factory Pattern
*&    based on Head First Design Patterns: Chapter 4
*&---------------------------------------------------------------------*
REPORT yy_head_first_factory_abstract.

*&---------------------------------------------------------------------*
*&  Abstract pizza ingredient: type of dough
*&---------------------------------------------------------------------*
*&  For pizza ingredients, we've got the same product families (dough,
*&  sauce, cheese, seafood, meat, and veggie toppings) but different
*&  implementations based on each region.
*&---------------------------------------------------------------------*
INTERFACE lif_dough.
  METHODS name RETURNING VALUE(rv_name) TYPE string.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Abstract pizza ingredient: type of sauce
*&---------------------------------------------------------------------*
INTERFACE lif_sauce.
  METHODS name RETURNING VALUE(rv_name) TYPE string.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Abstract pizza ingredient: type of topping
*&---------------------------------------------------------------------*
INTERFACE lif_topping.
  METHODS name RETURNING VALUE(rv_name) TYPE string.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Abstract pizza ingredient: type of cheese topping
*&---------------------------------------------------------------------*
INTERFACE lif_cheese.
  INTERFACES lif_topping.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Abstract pizza ingredient: type of seafood topping
*&---------------------------------------------------------------------*
INTERFACE lif_seafood.
  INTERFACES lif_topping.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Abstract pizza ingredient: type of meat topping
*&---------------------------------------------------------------------*
INTERFACE lif_meat.
  INTERFACES lif_topping.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Abstract pizza ingredient: type of veggie topping
*&---------------------------------------------------------------------*
INTERFACE lif_veggie.
  INTERFACES lif_topping.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Concrete pizza ingredient: type of dough - thin crust
*&---------------------------------------------------------------------*
*&  We will have lots of new classes here, one per ingredient.
*&---------------------------------------------------------------------*
CLASS lcl_thin_crust_dough DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_dough.
ENDCLASS.

CLASS lcl_thin_crust_dough IMPLEMENTATION.
  METHOD lif_dough~name.
    rv_name = |Thin Crust Dough|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete pizza ingredient: type of dough - thick crust
*&---------------------------------------------------------------------*
CLASS lcl_thick_crust_dough DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_dough.
ENDCLASS.

CLASS lcl_thick_crust_dough IMPLEMENTATION.
  METHOD lif_dough~name.
    rv_name = |Extra Thick Crust Dough|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete pizza ingredient: type of sauce - marinara
*&---------------------------------------------------------------------*
CLASS lcl_marinara_sauce DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_sauce.
ENDCLASS.

CLASS lcl_marinara_sauce IMPLEMENTATION.
  METHOD lif_sauce~name.
    rv_name = |Marinara Sauce|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete pizza ingredient: type of sauce - plum tomato
*&---------------------------------------------------------------------*
CLASS lcl_plum_tomato_sauce DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_sauce.
ENDCLASS.

CLASS lcl_plum_tomato_sauce IMPLEMENTATION.
  METHOD lif_sauce~name.
    rv_name = |Plum Tomato Sauce|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete pizza ingredient: type of cheese - mozzarella
*&---------------------------------------------------------------------*
CLASS lcl_mozzarella_cheese DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_cheese.
ENDCLASS.

CLASS lcl_mozzarella_cheese IMPLEMENTATION.
  METHOD lif_topping~name.
    rv_name = |Shredded Mozzarella Cheese|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete pizza ingredient: type of cheese - reggiano
*&---------------------------------------------------------------------*
CLASS lcl_reggiano_cheese DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_cheese.
ENDCLASS.

CLASS lcl_reggiano_cheese IMPLEMENTATION.
  METHOD lif_topping~name.
    rv_name = |Grated Reggiano Cheese|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete pizza ingredient: type of seafood - fresh clams
*&---------------------------------------------------------------------*
CLASS lcl_fresh_clams DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_seafood.
ENDCLASS.

CLASS lcl_fresh_clams IMPLEMENTATION.
  METHOD lif_topping~name.
    rv_name = |Fresh Clams from Long Island Sound|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete pizza ingredient: type of seafood - frozen clams
*&---------------------------------------------------------------------*
CLASS lcl_frozen_clams DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_seafood.
ENDCLASS.

CLASS lcl_frozen_clams IMPLEMENTATION.
  METHOD lif_topping~name.
    rv_name = |Frozen Clams from Chesapeake Bay|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete pizza ingredient: type of meat - sliced pepperoni
*&---------------------------------------------------------------------*
CLASS lcl_sliced_pepperoni DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_meat.
ENDCLASS.

CLASS lcl_sliced_pepperoni IMPLEMENTATION.
  METHOD lif_topping~name.
    rv_name = |Sliced Pepperoni|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete pizza ingredient: type of veggie - black olives
*&---------------------------------------------------------------------*
CLASS lcl_black_olives DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_veggie.
ENDCLASS.

CLASS lcl_black_olives IMPLEMENTATION.
  METHOD lif_topping~name.
    rv_name = |Black Olives|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete pizza ingredient: type of veggie - eggplant
*&---------------------------------------------------------------------*
CLASS lcl_eggplant DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_veggie.
ENDCLASS.

CLASS lcl_eggplant IMPLEMENTATION.
  METHOD lif_topping~name.
    rv_name = |Eggplant|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete pizza ingredient: type of veggie - garlic
*&---------------------------------------------------------------------*
CLASS lcl_garlic DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_veggie.
ENDCLASS.

CLASS lcl_garlic IMPLEMENTATION.
  METHOD lif_topping~name.
    rv_name = |Garlic|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete pizza ingredient: type of veggie - mushrooms
*&---------------------------------------------------------------------*
CLASS lcl_mushroom DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_veggie.
ENDCLASS.

CLASS lcl_mushroom IMPLEMENTATION.
  METHOD lif_topping~name.
    rv_name = |Mushrooms|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete pizza ingredient: type of veggie - onions
*&---------------------------------------------------------------------*
CLASS lcl_onion DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_veggie.
ENDCLASS.

CLASS lcl_onion IMPLEMENTATION.
  METHOD lif_topping~name.
    rv_name = |Onions|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete pizza ingredient: type of veggie - red pepper
*&---------------------------------------------------------------------*
CLASS lcl_red_pepper DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_veggie.
ENDCLASS.

CLASS lcl_red_pepper IMPLEMENTATION.
  METHOD lif_topping~name.
    rv_name = |Red Pepper|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete pizza ingredient: type of veggie - spinach
*&---------------------------------------------------------------------*
CLASS lcl_spinach DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_veggie.
ENDCLASS.

CLASS lcl_spinach IMPLEMENTATION.
  METHOD lif_topping~name.
    rv_name = |Spinach|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  The abstract Pizza Ingredient Factory is the interface that
*&  defines how to make a family of related products - all of the
*&  different ingredients we need to make a pizza in each region.
*&---------------------------------------------------------------------*
*&  If we'd had some common 'machinery' to implement in each instance
*&  of the factory, we could have made this an abstract class instead.
*&---------------------------------------------------------------------*
INTERFACE lif_pizza_ingredient_factory.
  TYPES tt_veggies TYPE STANDARD TABLE
    OF REF TO lif_veggie WITH EMPTY KEY.
  METHODS:
    " For each ingredient we define a create method in our interface.
    create_dough     RETURNING VALUE(ro_dough) TYPE REF TO lif_dough,
    create_sauce     RETURNING VALUE(ro_sauce) TYPE REF TO lif_sauce,
    create_cheese    RETURNING VALUE(ro_cheese) TYPE REF TO lif_cheese,
    create_clams     RETURNING VALUE(ro_clams) TYPE REF TO lif_seafood,
    create_pepperoni RETURNING VALUE(ro_meat) TYPE REF TO lif_meat,
    create_veggies   RETURNING VALUE(rt_veggies) TYPE tt_veggies.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  The NY ingredient factory implements the same inteface that is
*&  used for all pizza ingredient factories.
*&---------------------------------------------------------------------*
*&  The job of the concrete pizza factory is to make pizza ingredients.
*&  Each factory knows how to create the right objects for its region.
*&---------------------------------------------------------------------*
CLASS lcl_ny_ingredient_factory DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_pizza_ingredient_factory.
ENDCLASS.

CLASS lcl_ny_ingredient_factory IMPLEMENTATION.
  METHOD lif_pizza_ingredient_factory~create_dough.
    " For each ingredient in the family, we create the New York version.
    ro_dough = NEW lcl_thin_crust_dough( ).
  ENDMETHOD.
  METHOD lif_pizza_ingredient_factory~create_sauce.
    " This method returns the sauce that is used in its region.
    " For the NY ingredient factory we get marinara sauce.
    ro_sauce = NEW lcl_marinara_sauce( ).
  ENDMETHOD.
  METHOD lif_pizza_ingredient_factory~create_cheese.
    ro_cheese = NEW lcl_reggiano_cheese( ).
  ENDMETHOD.
  METHOD lif_pizza_ingredient_factory~create_clams.
    " New York is on the coast; it gets fresh clams.
    ro_clams = NEW lcl_fresh_clams( ).
  ENDMETHOD.
  METHOD lif_pizza_ingredient_factory~create_pepperoni.
    " The best sliced pepperoni. This is shared between NY and Chicago.
    ro_meat = NEW lcl_sliced_pepperoni( ).
  ENDMETHOD.
  METHOD lif_pizza_ingredient_factory~create_veggies.
    " For veggies, we return an internal table of Veggie instances.
    " Here we've hardcoded the veggie types. We could make this more
    " sophisticated, but that doesn't really add anything to learning
    " the factory pattern, so we'll keep it simple.
    rt_veggies = VALUE #(
      ( NEW lcl_garlic( ) )
      ( NEW lcl_onion( ) )
      ( NEW lcl_mushroom( ) )
      ( NEW lcl_red_pepper( ) ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  The Chicago ingredient factory also implements the same inteface
*&  that is used for all pizza ingredient factories.
*&---------------------------------------------------------------------*
CLASS lcl_chicago_ingredient_factory DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_pizza_ingredient_factory.
ENDCLASS.

CLASS lcl_chicago_ingredient_factory IMPLEMENTATION.
  METHOD lif_pizza_ingredient_factory~create_dough.
    " For each ingredient in the family, we create the Chicago version.
    ro_dough = NEW lcl_thick_crust_dough( ).
  ENDMETHOD.
  METHOD lif_pizza_ingredient_factory~create_sauce.
    " For the Chicago ingredient factory we get plum tomato sauce.
    ro_sauce = NEW lcl_plum_tomato_sauce( ).
  ENDMETHOD.
  METHOD lif_pizza_ingredient_factory~create_cheese.
    ro_cheese = NEW lcl_mozzarella_cheese( ).
  ENDMETHOD.
  METHOD lif_pizza_ingredient_factory~create_clams.
    " Chicago has to settle for frozen clams.
    ro_clams = NEW lcl_frozen_clams( ).
  ENDMETHOD.
  METHOD lif_pizza_ingredient_factory~create_pepperoni.
    " Make sure to use the same sliced pepperoni in the Chicago factory.
    ro_meat = NEW lcl_sliced_pepperoni( ).
  ENDMETHOD.
  METHOD lif_pizza_ingredient_factory~create_veggies.
    rt_veggies = VALUE #(
      ( NEW lcl_black_olives( ) )
      ( NEW lcl_spinach( ) )
      ( NEW lcl_eggplant( ) ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Encapsulated pizza cutting behavior stategies
*&---------------------------------------------------------------------*
*&  We would also like to change how the pizza is cut for each region.
*&---------------------------------------------------------------------*
INTERFACE lif_cut_pizza_behavior.
  METHODS do_cut.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Here's the default implementation of cutting standard pizzas
*&---------------------------------------------------------------------*
CLASS lcl_cut_pizza_standard DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_cut_pizza_behavior.
ENDCLASS.

CLASS lcl_cut_pizza_standard IMPLEMENTATION.
  METHOD lif_cut_pizza_behavior~do_cut.
    cl_demo_output=>write( |Cut the pizza into diagonal slices| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  And here's the implementation for cutting deep dish pizzas
*&---------------------------------------------------------------------*
CLASS lcl_cut_pizza_deep_dish DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_cut_pizza_behavior.
ENDCLASS.

CLASS lcl_cut_pizza_deep_dish IMPLEMENTATION.
  METHOD lif_cut_pizza_behavior~do_cut.
    cl_demo_output=>write( |Cut the pizza into square slices| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  We're still defining Pizza as an abstract class with some helpful
*&  implementations that can be overridden.
*&---------------------------------------------------------------------*
CLASS lcl_pizza DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_name TYPE string,
      name RETURNING VALUE(rv_name) TYPE string,
      " We've now made the prepare method abstract. This is where we
      " are going to collect the ingredients needed for the pizza,
      " which of cource will come from the ingredient factory.
      prepare ABSTRACT
        " To make a pizza now, we need a factory to provide the
        " ingredients. So each Pizza class gets a factory passed
        " into its prepare method.
        IMPORTING io_factory TYPE REF TO lif_pizza_ingredient_factory,
      " Other methods remain the same, with the exception of prepare().
      bake, cut, box,
      set_cut_behavior
        IMPORTING io_cb TYPE REF TO lif_cut_pizza_behavior.
  PROTECTED SECTION.
    DATA:
      " Each pizza has a name, a set of ingredients that are used in
      " its preparation, and a cutting behavior strategy.
      mv_name     TYPE string,
      mo_dough    TYPE REF TO lif_dough,
      mo_sauce    TYPE REF TO lif_sauce,
      mt_toppings TYPE STANDARD TABLE
        OF REF TO lif_topping WITH EMPTY KEY,
      mo_cut_behavior TYPE REF TO lif_cut_pizza_behavior.
ENDCLASS.

CLASS lcl_pizza IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_name.
    mo_cut_behavior = NEW lcl_cut_pizza_standard( ). " default behavior
  ENDMETHOD.
  METHOD name.
    rv_name = mv_name.
  ENDMETHOD.
  METHOD bake.
    cl_demo_output=>write( |Bake for 25 minutes at 350| ).
  ENDMETHOD.
  METHOD cut.
    mo_cut_behavior->do_cut( ).    " delegate to the cut behavior class
  ENDMETHOD.
  METHOD box.
    cl_demo_output=>write( |Place pizza in official Pizza Store box| ).
  ENDMETHOD.
  METHOD set_cut_behavior.
    mo_cut_behavior = io_cb.
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
    METHODS prepare REDEFINITION.     " Here's where the magic happens!
ENDCLASS.

CLASS lcl_cheese_pizza IMPLEMENTATION.
  METHOD prepare.
    " The prepare() method steps through creating a pizza, and each
    " time it needs an ingredient, it asks the factory to produce it.
    cl_demo_output=>write( |Preparing { mv_name }...| ).
    mo_dough = io_factory->create_dough( ).
    cl_demo_output=>write( |Tossing { mo_dough->name( ) }...| ).
    mo_sauce = io_factory->create_sauce( ).
    cl_demo_output=>write( |Adding { mo_sauce->name( ) }...| ).

    mt_toppings = VALUE #( ( io_factory->create_cheese( ) ) ).
    cl_demo_output=>write( REDUCE string(
      INIT text = |Adding toppings... \n|
      FOR topping IN mt_toppings
      NEXT text = text && |\t{ topping->name( ) }\n| ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete product: Clam pizza
*&---------------------------------------------------------------------*
CLASS lcl_clam_pizza DEFINITION FINAL INHERITING FROM lcl_pizza.
  PUBLIC SECTION.
    METHODS prepare REDEFINITION.
ENDCLASS.

CLASS lcl_clam_pizza IMPLEMENTATION.
  METHOD prepare.
    " To make a clam pizza, the prepare method collects the right
    " ingredients from its imported factory.
    cl_demo_output=>write( |Preparing { mv_name }...| ).
    mo_dough = io_factory->create_dough( ).
    cl_demo_output=>write( |Tossing { mo_dough->name( ) }...| ).
    mo_sauce = io_factory->create_sauce( ).
    cl_demo_output=>write( |Adding { mo_sauce->name( ) }...| ).

    mt_toppings = VALUE #(
      " If it's a New York factory, the clams will be fresh;
      " if it's a Chicago factory, they'll be frozen.
      ( io_factory->create_clams( ) )
      ( io_factory->create_cheese( ) ) ).
    cl_demo_output=>write( REDUCE string(
      INIT text = |Adding toppings... \n|
      FOR topping IN mt_toppings
      NEXT text = text && |\t{ topping->name( ) }\n| ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete product: Pepperoni pizza
*&---------------------------------------------------------------------*
CLASS lcl_pepperoni_pizza DEFINITION FINAL INHERITING FROM lcl_pizza.
  PUBLIC SECTION.
    METHODS prepare REDEFINITION.
ENDCLASS.

CLASS lcl_pepperoni_pizza IMPLEMENTATION.
  METHOD prepare.
    cl_demo_output=>write( |Preparing { mv_name }...| ).
    mo_dough = io_factory->create_dough( ).
    cl_demo_output=>write( |Tossing { mo_dough->name( ) }...| ).
    mo_sauce = io_factory->create_sauce( ).
    cl_demo_output=>write( |Adding { mo_sauce->name( ) }...| ).

    mt_toppings = VALUE #(
      BASE io_factory->create_veggies( )
      ( io_factory->create_pepperoni( ) )
      ( io_factory->create_cheese( ) ) ).
    cl_demo_output=>write( REDUCE string(
      INIT text = |Adding toppings... \n|
      FOR topping IN mt_toppings
      NEXT text = text && |\t{ topping->name( ) }\n| ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Concrete product: Veggie pizza
*&---------------------------------------------------------------------*
CLASS lcl_veggie_pizza DEFINITION FINAL INHERITING FROM lcl_pizza.
  PUBLIC SECTION.
    METHODS prepare REDEFINITION.
ENDCLASS.

CLASS lcl_veggie_pizza IMPLEMENTATION.
  METHOD prepare.
    cl_demo_output=>write( |Preparing { mv_name }...| ).
    mo_dough = io_factory->create_dough( ).
    cl_demo_output=>write( |Tossing { mo_dough->name( ) }...| ).
    mo_sauce = io_factory->create_sauce( ).
    cl_demo_output=>write( |Adding { mo_sauce->name( ) }...| ).

    mt_toppings = VALUE #(
      BASE io_factory->create_veggies( )
      ( io_factory->create_cheese( ) ) ).
    cl_demo_output=>write( REDUCE string(
      INIT text = |Adding toppings... \n|
      FOR topping IN mt_toppings
      NEXT text = text && |\t{ topping->name( ) }\n| ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  The clients of the Abstract Factory are the two instances of our
*&  Pizza Store, NY Pizza Store and Chicago Pizza Store.
*&---------------------------------------------------------------------*
CLASS lcl_pizza_store DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS order_pizza FINAL
      IMPORTING iv_type TYPE string
      RETURNING VALUE(ro_pizza) TYPE REF TO lcl_pizza.
  PROTECTED SECTION.
    " Remember, all the responsibility for instantiating Pizzas is in
    " the factory method create_pizza()
    METHODS create_pizza ABSTRACT
      IMPORTING iv_item TYPE string
      RETURNING VALUE(ro_pizza) TYPE REF TO lcl_pizza.
    " This is our ingredient factory. A store doesn't care which factory
    " is used, as long as it is a pizza ingredient factory.
    DATA mo_ingredient_factory TYPE REF TO lif_pizza_ingredient_factory.
ENDCLASS.

CLASS lcl_pizza_store IMPLEMENTATION.
  METHOD order_pizza.
    ro_pizza = create_pizza( iv_type ).
    cl_demo_output=>write_text(
      |------ Making a { ro_pizza->name( ) } ------| ).
    " We now pass each pizza the factory that should be used to produce
    " its ingredients during the preparation steps.
    ro_pizza->prepare( mo_ingredient_factory ).
    ro_pizza->bake( ).
    ro_pizza->cut( ).
    ro_pizza->box( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  The NY Pizza Store is a subclass of Pizza Store, so it inherits the
*&  order_pizza() method (among others).
*&---------------------------------------------------------------------*
CLASS lcl_ny_pizza_store DEFINITION FINAL
  INHERITING FROM lcl_pizza_store.
  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS create_pizza REDEFINITION.
ENDCLASS.

CLASS lcl_ny_pizza_store IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    " The NY Store is composed with a NY pizza ingredient factory.
    " This will be used to produce ingredients for NY style pizzas.
    mo_ingredient_factory = NEW lcl_ny_ingredient_factory( ).
  ENDMETHOD.
  METHOD create_pizza.
    " For each type of Pizza, we instantiate a new Pizza object and
    " give it a descriptive name.
    ro_pizza = SWITCH #( iv_item
      WHEN 'cheese'
        THEN NEW lcl_cheese_pizza( |NY Style Sauce and Cheese Pizza| )
      WHEN 'pepperoni'
        THEN NEW lcl_pepperoni_pizza( |New York Style Pepperoni Pizza| )
      WHEN 'clam'
        THEN NEW lcl_clam_pizza( |New York Style Clam Pizza| )
      WHEN 'veggie'
        THEN NEW lcl_veggie_pizza( |New York Style Veggie Pizza| ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Since each franchise gets its own subclass of Pizza Store, it's
*&  free to create its own style of pizza by intantiating the correct
*&  pizza ingredient factory and implementing create_pizza()
*&---------------------------------------------------------------------*
CLASS lcl_chicago_pizza_store DEFINITION FINAL
  INHERITING FROM lcl_pizza_store.
  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS create_pizza REDEFINITION.
ENDCLASS.

CLASS lcl_chicago_pizza_store IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    " The Chicago Store is composed with a Chicago ingredient factory.
    mo_ingredient_factory = NEW lcl_chicago_ingredient_factory( ).
  ENDMETHOD.
  METHOD create_pizza.
    " For each type of Pizza, we instantiate a new Pizza object and
    " give it a descriptive name.
    ro_pizza = SWITCH #( iv_item
      WHEN 'cheese'
        THEN NEW lcl_cheese_pizza( |Chicago Deep Dish Cheese Pizza| )
      WHEN 'pepperoni'
        THEN NEW lcl_pepperoni_pizza( |Chicago Style Pepperoni Pizza| )
      WHEN 'clam'
        THEN NEW lcl_clam_pizza( |Chicago Style Clam Pizza| )
      WHEN 'veggie'
        THEN NEW lcl_veggie_pizza( |Chicago Deep Dish Veggie Pizza| ) ).
    " Then invoke the pizza's inherited cutting behavior setter method
    " to dynamically change its behavior to cut into square slices.
    ro_pizza->set_cut_behavior( NEW lcl_cut_pizza_deep_dish( ) ).
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
    |Baking with OO Goodness - the Abstract Factory Pattern| ).
  cl_demo_output=>line( ).
  lcl_pizza_test_drive=>main( ).
  cl_demo_output=>display( ).
