*&---------------------------------------------------------------------*
*&  Sample for Well-Managed Collections - the Composite Pattern
*&    based on Head First Design Patterns: Chapter 9
*&---------------------------------------------------------------------*
REPORT yy_head_first_composite.

*&---------------------------------------------------------------------*
*&  All components must implement the Menu Component interface; however,
*&  because leaves and nodes have different roles we can't always define
*&  a default implementation that makes sense for each method. Sometimes
*&  the best we can do is throw a runtime exception.
*&---------------------------------------------------------------------*
CLASS lcx_unsupported_operation DEFINITION FINAL
  INHERITING FROM cx_no_check.
  PUBLIC SECTION.
    METHODS get_text REDEFINITION.
ENDCLASS.

CLASS lcx_unsupported_operation IMPLEMENTATION.
  METHOD get_text.
    result = |This Menu Component does not support this method|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  This Menu Component represents the interface for both Menu Items
*&  and Menus. We've used an abstract class here because we want to
*&  provide default implementations for every one of these methods.
*&---------------------------------------------------------------------*
*&  Because some of these methods only make sense for Menu Items, and
*&  some only make sense for Menus, the default implementation is to
*&  raise an Unsupported Operation exception. That way, if Menu Item or
*&  Menu doesn't support an operation, they don't have to do anything;
*&  they can just inherit the default implementation.
*&---------------------------------------------------------------------*
CLASS lcl_menu_component DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      " We have some of the same method you'll remember from our
      " previous versions of Menu Item and Menu, and we've added
      " print(), add(), remove() and child(). We'll describe those
      " soon, when we implement our new Menu and Menu Item classes.
      name          RETURNING VALUE(rv_name) TYPE string,
      text          RETURNING VALUE(rv_text) TYPE string,
      price         RETURNING VALUE(rv_price) TYPE decfloat16,
      is_vegetarian RETURNING VALUE(rv_flag) TYPE abap_bool,
      print,
      " Here are the methods for manipulating the components.
      " The components are Menu Item and Menu.
      add    IMPORTING io_menu_component TYPE REF TO lcl_menu_component,
      remove IMPORTING io_menu_component TYPE REF TO lcl_menu_component,
      child  IMPORTING iv_index TYPE i
             RETURNING VALUE(ro_child) TYPE REF TO lcl_menu_component.
ENDCLASS.

CLASS lcl_menu_component IMPLEMENTATION.
  " We've grouped together the "composite" methods - that is,
  " methods to add, remove and get Menu Components.
  METHOD add.
    RAISE EXCEPTION TYPE lcx_unsupported_operation.
  ENDMETHOD.
  METHOD remove.
    RAISE EXCEPTION TYPE lcx_unsupported_operation.
  ENDMETHOD.
  METHOD child.
    RAISE EXCEPTION TYPE lcx_unsupported_operation.
  ENDMETHOD.
  " Here are the "operation" methods; these are used by the Menu
  " Items. It turns out we can also use a couple of them in the
  " Menus too, as you'll see later on when we look at the code.
  METHOD name.
    RAISE EXCEPTION TYPE lcx_unsupported_operation.
  ENDMETHOD.
  METHOD text.
    RAISE EXCEPTION TYPE lcx_unsupported_operation.
  ENDMETHOD.
  METHOD price.
    RAISE EXCEPTION TYPE lcx_unsupported_operation.
  ENDMETHOD.
  METHOD is_vegetarian.
    RAISE EXCEPTION TYPE lcx_unsupported_operation.
  ENDMETHOD.
  " print() is an "operation" method that both our Menus and Menu
  " Items will implement, but we provide a default operation here.
  METHOD print.
    RAISE EXCEPTION TYPE lcx_unsupported_operation.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  First we need to inherit from the Menu Component interface.
*&---------------------------------------------------------------------*
*&  Menu Item overrides those methods that make sense, and uses the
*&  default implementations in Menu Component for those that don't
*&  make sense (like add() - it doesn't make sense to add a component
*&  to a Menu Item... we can only add components to a Menu).
*&---------------------------------------------------------------------*
CLASS lcl_menu_item DEFINITION FINAL INHERITING FROM lcl_menu_component.
  PUBLIC SECTION.
    METHODS:
      " A Menu Item consists of a name, some text, a flag to indicate
      " if the item is vegetarian, and a price. You pass all these
      " values into the constructor to initialize the Menu Item.
      constructor IMPORTING iv_name       TYPE string
                            iv_text       TYPE string
                            iv_vegetarian TYPE abap_bool
                            iv_price      TYPE decfloat16,
      name          REDEFINITION,
      text          REDEFINITION,
      price         REDEFINITION,
      is_vegetarian REDEFINITION,
      print         REDEFINITION.
  PRIVATE SECTION.
    DATA:
      mv_name       TYPE string,
      mv_text       TYPE string,
      mv_vegetarian TYPE abap_bool,
      mv_price      TYPE decfloat16.
ENDCLASS.

CLASS lcl_menu_item IMPLEMENTATION.
  " The constructor just takes the name, text, etc. and keeps a copy of
  " them all. This is pretty much like our old menu item implementation.
  METHOD constructor.
    super->constructor( ).
    mv_name = iv_name.
    mv_text = iv_text.
    mv_vegetarian = iv_vegetarian.
    mv_price = iv_price.
  ENDMETHOD.
  " Here's our getter methods - just like our previous implementation.
  METHOD name.
    rv_name = mv_name.
  ENDMETHOD.
  METHOD text.
    rv_text = mv_text.
  ENDMETHOD.
  METHOD price.
    rv_price = mv_price.
  ENDMETHOD.
  METHOD is_vegetarian.
    rv_flag = mv_vegetarian.
  ENDMETHOD.
  " This is different from the previous implementation. Here we're
  " overridding the print() method in the Menu Component class. For
  " Menu Item this method prints the complete menu entry: name, text,
  " price and wheter or not it's veggie.
  METHOD print.
    cl_demo_output=>write(
      |{ name( ) && COND #( WHEN is_vegetarian( ) THEN |(v)| ) }| &
      |, ${ price( ) DECIMALS = 2 }\n\t-- { text( ) }| ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Menu is also a Menu Component, just like Menu Item.
*&---------------------------------------------------------------------*
*&  Menu also overrides the methods that make sense, like a way to
*&  add and remove menu items (or other menus!) from its internal
*&  table mt_menu_components. In addition, we'll use the name() and
*&  text() methods to return the name and description of the menu.
*&---------------------------------------------------------------------*
CLASS lcl_menu DEFINITION FINAL INHERITING FROM lcl_menu_component.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_name       TYPE string
                            iv_text       TYPE string,
      name   REDEFINITION,
      text   REDEFINITION,
      print  REDEFINITION, " Both Menu Item and Menu override print().
      add    REDEFINITION,
      remove REDEFINITION,
      child  REDEFINITION.
  PRIVATE SECTION.
    DATA:
      mv_menu_name       TYPE string,
      mv_menu_text       TYPE string,
      mt_menu_components TYPE STANDARD TABLE
        OF REF TO lcl_menu_component WITH DEFAULT KEY.
ENDCLASS.

CLASS lcl_menu IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_menu_name = iv_name.
    mv_menu_text = iv_text.
  ENDMETHOD.
  " Here's how you add Menu Items or other Menus to a Menu. Because
  " both Menu Items and Menus are Menu Components, we just need one
  " method to do both. You can also remove a Menu Component or get
  " a contained Menu Component.
  METHOD add.
    INSERT io_menu_component INTO TABLE mt_menu_components.
  ENDMETHOD.
  METHOD remove.
    DELETE TABLE mt_menu_components FROM io_menu_component.
  ENDMETHOD.
  METHOD child.
    ro_child = VALUE #( mt_menu_components[ iv_index ] OPTIONAL ).
  ENDMETHOD.
  " Here are the getter methods for getting the name and text. Notice,
  " we aren't overriding price() or is_vegetarian() because those don't
  " make sense for a Menu (although you could argue that is_vegetarian()
  " might make sense). If someone tries to call those methods on a Menu,
  " they'll get an Unsupported Operation exception.
  METHOD name.
    rv_name = mv_menu_name.
  ENDMETHOD.
  METHOD text.
    rv_text = mv_menu_text.
  ENDMETHOD.
  METHOD print.
    " To print the Menu, we print the Menu's name and description.
    cl_demo_output=>write_text( |\n{ name( ) }, { text( ) }\n| &
      |------------------------------------------------------| ).
    " Look! We get to use our internal table for polymorphism.
    " We use it to loop through all the Menu's components...
    " ...those could be other Menus, or they could be Menu Items.
    LOOP AT mt_menu_components INTO DATA(lo_menu_component).
      " Since both Menus and Menu Items implement print(),
      " we just call print() and the rest is up to them.
      lo_menu_component->print( ).
    ENDLOOP.
    " NOTE: If, during this loop, we encounter another Menu object,
    " its print() method will start another loop, and so on.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  The Waitress is going to use the Menu Component interface to
*&  access both Menus and Menu Items.
*&---------------------------------------------------------------------*
CLASS lcl_waitress DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING io_all_menus TYPE REF TO lcl_menu_component,
      print_menu.
  PRIVATE SECTION.
    DATA mo_all_menus TYPE REF TO lcl_menu_component.
ENDCLASS.

CLASS lcl_waitress IMPLEMENTATION.
  METHOD constructor.
    " Yup! The Waitress code really is this simple. Now we just hand
    " her the top-level menu component, the one that contains all the
    " other menus. We've called that variable All Menus.
    mo_all_menus = io_all_menus.
  ENDMETHOD.
  METHOD print_menu.
    " All she has to do to print the entire menu hierarchy - all
    " the menus, and all the menu items - is call print() on the
    " top level menu. We're gonna have one happy Waitress.
    mo_all_menus->print( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Here's some test drive code to see how the Waitress works...
*&---------------------------------------------------------------------*
CLASS lcl_menu_test_drive DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS lcl_menu_test_drive IMPLEMENTATION.
  METHOD main.

    " Let's first create all the menu objects.
    DATA(lo_pancake_menu) = NEW lcl_menu(
      iv_name = |PANCAKE HOUSE MENU|
      iv_text = |Breakfast| ).
    DATA(lo_diner_menu) = NEW lcl_menu(
      iv_name = |DINER MENU|
      iv_text = |Lunch| ).
    DATA(lo_cafe_menu) = NEW lcl_menu(
      iv_name = |CAFE MENU|
      iv_text = |Dinner| ).
    DATA(lo_dessert_menu) = NEW lcl_menu(
      iv_name = |DESSERT MENU|
      iv_text = |Dessert of course!| ).
    DATA(lo_coffee_menu) = NEW lcl_menu(
      iv_name = |COFFEE MENU|
      iv_text = |Stuff to go with your afternoon coffee| ).

    " We also need a top-level menu that we'll name All Menus.
    DATA(lo_all_menus) = NEW lcl_menu(
      iv_name = |ALL MENUS|
      iv_text = |All menus combined| ).

    " We're using the Composite add() method to add each menu to the
    " top-level menu, lo_all_menus.
    lo_all_menus->add( lo_pancake_menu ).
    lo_all_menus->add( lo_diner_menu ).
    lo_all_menus->add( lo_cafe_menu ).

    " Now we need to add all the menu items. Here's somes examples.
    lo_pancake_menu->add( NEW lcl_menu_item(
      iv_name = |K&B's Pancake Breakfast|
      iv_text = |Pancakes with scrambled eggs, and toast|
      iv_vegetarian = abap_true iv_price = CONV #( '2.99' ) ) ).
    lo_pancake_menu->add( NEW lcl_menu_item(
      iv_name = |Regular Pancake Breakfast|
      iv_text = |Pancakes with fried eggs, sausage|
      iv_vegetarian = abap_false iv_price = CONV #( '2.99' ) ) ).
    lo_pancake_menu->add( NEW lcl_menu_item(
      iv_name = |Blueberry Pancakes|
      iv_text = |Pancakes made with fresh blueberries|
      iv_vegetarian = abap_true iv_price = CONV #( '3.49' ) ) ).
    lo_pancake_menu->add( NEW lcl_menu_item(
      iv_name = |Waffles|
      iv_text = |Waffles, with choice of blueberries or strawberries|
      iv_vegetarian = abap_true iv_price = CONV #( '3.59' ) ) ).

    lo_diner_menu->add( NEW lcl_menu_item(
      iv_name = |Vegetarian BLT|
      iv_text = |(Fakin') Bacon with lettuce & tomato on whole wheat|
      iv_vegetarian = abap_true iv_price = CONV #( '2.99' ) ) ).
    lo_diner_menu->add( NEW lcl_menu_item(
      iv_name = |BLT|
      iv_text = |Bacon with lettuce & tomato on whole wheat|
      iv_vegetarian = abap_false iv_price = CONV #( '2.99' ) ) ).
    lo_diner_menu->add( NEW lcl_menu_item(
      iv_name = |Soup of the day|
      iv_text = |Soup of the day, with a side of potato salad|
      iv_vegetarian = abap_false iv_price = CONV #( '3.29' ) ) ).
    lo_diner_menu->add( NEW lcl_menu_item(
      iv_name = |Hotdog|
      iv_text = |A hot dog, with saurkraut, relish, topped with cheese|
      iv_vegetarian = abap_false iv_price = CONV #( '3.05' ) ) ).
    lo_diner_menu->add( NEW lcl_menu_item(
      iv_name = |Steamed Veggies and Brown Rice|
      iv_text = |Steamed vegetables over brown rice|
      iv_vegetarian = abap_true iv_price = CONV #( '3.99' ) ) ).
    lo_diner_menu->add( NEW lcl_menu_item(
      iv_name = |Pasta|
      iv_text = |Spaghetti with Marinara Sauce, and a slice of bread|
      iv_vegetarian = abap_true iv_price = CONV #( '3.89' ) ) ).

    " And we're also adding a menu to a menu. All lo_diner_menu
    " cares about is that everything it holds, whether it's a
    " menu item or a menu, is a Menu Component.
    lo_diner_menu->add( lo_coffee_menu ).

    lo_coffee_menu->add( NEW lcl_menu_item(
      iv_name = |Coffee Cake|
      iv_text = |Crumbly cake topped with cinnamon and walnuts|
      iv_vegetarian = abap_true iv_price = CONV #( '1.59' ) ) ).
    lo_coffee_menu->add( NEW lcl_menu_item(
      iv_name = |Bagel|
      iv_text = |Flavors include sesame, poppyseed, cinnamon, pumpkin|
      iv_vegetarian = abap_false iv_price = CONV #( '0.69' ) ) ).
    lo_coffee_menu->add( NEW lcl_menu_item(
      iv_name = |Biscotti|
      iv_text = |Three almond or hazelnut biscotti cookies|
      iv_vegetarian = abap_true iv_price = CONV #( '0.89' ) ) ).

    lo_cafe_menu->add( NEW lcl_menu_item(
      iv_name = |Veggie Burger and Air Fries|
      iv_text = |Veggie burger on wheat bun, lettuce, tomato, and fries|
      iv_vegetarian = abap_true iv_price = CONV #( '3.99' ) ) ).
    lo_cafe_menu->add( NEW lcl_menu_item(
      iv_name = |Soup of the day|
      iv_text = |A cup of the soup of the day, with a side salad|
      iv_vegetarian = abap_false iv_price = CONV #( '3.69' ) ) ).
    lo_cafe_menu->add( NEW lcl_menu_item(
      iv_name = |Burrito|
      iv_text = |A large burrito, with pinto beans, salsa, guacamole|
      iv_vegetarian = abap_true iv_price = CONV #( '4.29' ) ) ).

    lo_cafe_menu->add( lo_dessert_menu ).

    " Add some tasty treats to the dessert menu...
    lo_dessert_menu->add( NEW lcl_menu_item(
      iv_name = |Apple Pie|
      iv_text = |Apple pie with a flakey crust, topped with icecream|
      iv_vegetarian = abap_true iv_price = CONV #( '1.59' ) ) ).
    lo_dessert_menu->add( NEW lcl_menu_item(
      iv_name = |Cheesecake|
      iv_text = |Creamy New York cheesecake, with a graham crust|
      iv_vegetarian = abap_true iv_price = CONV #( '1.99' ) ) ).
    lo_dessert_menu->add( NEW lcl_menu_item(
      iv_name = |Sorbet|
      iv_text = |A scoop of raspberry and a scoop of lime|
      iv_vegetarian = abap_true iv_price = CONV #( '1.89' ) ) ).

    " Once we've constructed our entire menu hierarchy, we hand the
    " whole thing to the Waitress, and as you've seen, it's as easy
    " as apple pie for her to print it out.
    DATA(lo_waitress) = NEW lcl_waitress( lo_all_menus ).
    lo_waitress->print_menu( ).
    cl_demo_output=>line( ).

    "We're out of apple pie. Let's remove it from the dessert menu.
    DATA(lo_apple_pie) = lo_dessert_menu->child( 1 ).
    lo_dessert_menu->remove( lo_apple_pie ).
    lo_dessert_menu->print( ).
    cl_demo_output=>line( ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section(
    |Well-Managed Collections - the Composite Pattern| ).
  cl_demo_output=>line( ).
  lcl_menu_test_drive=>main( ).
  cl_demo_output=>display( ).
