*&---------------------------------------------------------------------*
*&  Sample for Well-Managed Collections - the Iterator Pattern
*&    based on Head First Design Patterns: Chapter 9
*&---------------------------------------------------------------------*
REPORT yy_head_first_iterator_without.

*&---------------------------------------------------------------------*
*&  Common Menu Item implementation. The Diner menu has lots of lunch
*&  items, while the Pancake House menu consists of breakfast items.
*&---------------------------------------------------------------------*
CLASS lcl_menu_item DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      " A Menu Item consists of a name, some text, a flag to indicate
      " if the item is vegetarian, and a price. You pass all these
      " values into the constructor to initialize the Menu Item.
      constructor IMPORTING iv_name       TYPE string
                            iv_text       TYPE string
                            iv_vegetarian TYPE abap_bool
                            iv_price      TYPE decfloat16,
      " These getter methods let you access the fields of the menu item.
      name          RETURNING VALUE(rv_name) TYPE string,
      text          RETURNING VALUE(rv_text) TYPE string,
      price         RETURNING VALUE(rv_price) TYPE decfloat16,
      is_vegetarian RETURNING VALUE(rv_flag) TYPE abap_bool,
      description   RETURNING VALUE(rv_text) TYPE string.
  PRIVATE SECTION.
    DATA:
      mv_name       TYPE string,
      mv_text       TYPE string,
      mv_vegetarian TYPE abap_bool,
      mv_price      TYPE decfloat16.
ENDCLASS.

CLASS lcl_menu_item IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_name.
    mv_text = iv_text.
    mv_vegetarian = iv_vegetarian.
    mv_price = iv_price.
  ENDMETHOD.
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
  METHOD description.
    rv_text = |{ name( ) }, ${ price( ) DECIMALS = 2 }\n\t{ text( ) }|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Here's the implementation of the Pancake House menu.
*&---------------------------------------------------------------------*
CLASS lcl_pancake_menu DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_named_menu_item,
        name TYPE string,
        item TYPE REF TO lcl_menu_item,
      END OF ts_named_menu_item,
      " It is using a Sorted Internal Table to store the menu items
      " ordered alphabetically by the primary key name.
      tt_sorted_menu_items TYPE SORTED TABLE
        OF ts_named_menu_item WITH UNIQUE KEY name.
    METHODS:
      constructor,
      add_item IMPORTING iv_name       TYPE string
                         iv_text       TYPE string
                         iv_vegetarian TYPE abap_bool
                         iv_price      TYPE decfloat16,
      menu_items RETURNING VALUE(rt_items) TYPE tt_sorted_menu_items.
    " other menu methods here
  PRIVATE SECTION.
    DATA mt_menu_items TYPE tt_sorted_menu_items.
ENDCLASS.

CLASS lcl_pancake_menu IMPLEMENTATION.
  METHOD constructor.
    " Each menu item is added to the Sorted Table here, in the
    " constructor, and has a name, some descriptive text, whether
    " or not it's a vegetarian item, and the price.
    add_item( iv_name = |K&B's Pancake Breakfast|
      iv_text = |Pancakes with scrambled eggs, and toast|
      iv_vegetarian = abap_true iv_price = CONV #( '2.99' ) ).
    add_item( iv_name = |Regular Pancake Breakfast|
      iv_text = |Pancakes with fried eggs, sausage|
      iv_vegetarian = abap_false iv_price = CONV #( '2.99' ) ).
    add_item( iv_name = |Blueberry Pancakes|
      iv_text = |Pancakes made with fresh blueberries|
      iv_vegetarian = abap_true iv_price = CONV #( '3.49' ) ).
    add_item( iv_name = |Waffles|
      iv_text = |Waffles, with choice of blueberries or strawberries|
      iv_vegetarian = abap_true iv_price = CONV #( '3.59' ) ).
  ENDMETHOD.
  METHOD add_item.
    " To add a menu item, create a new Menu Item object, passing in
    " each argument, and then insert it into the Sorted Table.
    DATA(lo_menu_item) = NEW lcl_menu_item(
                           iv_name = iv_name
                           iv_text = iv_text
                           iv_vegetarian = iv_vegetarian
                           iv_price = iv_price ).
    INSERT VALUE #( name = lo_menu_item->name( )
                    item = lo_menu_item ) INTO TABLE mt_menu_items.
  ENDMETHOD.
  METHOD menu_items.
    " The menu_items() method returns the sorted table of menu items.
    rt_items = mt_menu_items.
  ENDMETHOD.
  " There is a bunch of other menu code that depends on the Sorted Table
  " implementation. We don't want to have to rewrite all that code!
ENDCLASS.

*&---------------------------------------------------------------------*
*&  And here's the implementation of the Diner menu.
*&---------------------------------------------------------------------*
CLASS lcl_diner_menu DEFINITION FINAL.
  PUBLIC SECTION.
    " It takes a different approach, using a Standard Internal Table
    " without a key, in order to control the max size of the menu.
    TYPES tt_menu_items TYPE STANDARD TABLE
      OF REF TO lcl_menu_item WITH EMPTY KEY.
    METHODS:
      constructor,
      add_item IMPORTING iv_name       TYPE string
                         iv_text       TYPE string
                         iv_vegetarian TYPE abap_bool
                         iv_price      TYPE decfloat16,
      menu_items RETURNING VALUE(rt_items) TYPE tt_menu_items.
    " other menu methods here
  PRIVATE SECTION.
    CONSTANTS c_max_items TYPE i VALUE 6.
    DATA:
      mv_number_of_items TYPE i VALUE 0,
      mt_menu_items      TYPE tt_menu_items.
ENDCLASS.

CLASS lcl_diner_menu IMPLEMENTATION.
  METHOD constructor.
    mt_menu_items = VALUE #( FOR i = 1 UNTIL i > c_max_items ( ) ).
    " Like before, we create the menu items in the constructor,
    " using the add_item() helper method.
    add_item( iv_name = |Vegetarian BLT|
      iv_text = |(Fakin') Bacon with lettuce & tomato on whole wheat|
      iv_vegetarian = abap_true iv_price = CONV #( '2.99' ) ).
    add_item( iv_name = |BLT|
      iv_text = |Bacon with lettuce & tomato on whole wheat|
      iv_vegetarian = abap_false iv_price = CONV #( '2.99' ) ).
    add_item( iv_name = |Soup of the day|
      iv_text = |Soup of the day, with a side of potato salad|
      iv_vegetarian = abap_false iv_price = CONV #( '3.29' ) ).
    add_item( iv_name = |Hotdog|
      iv_text = |A hot dog, with saurkraut, relish, topped with cheese|
      iv_vegetarian = abap_false iv_price = CONV #( '3.05' ) ).
    add_item( iv_name = |Steamed Veggies and Brown Rice|
      iv_text = |Steamed vegetables over brown rice|
      iv_vegetarian = abap_true iv_price = CONV #( '3.99' ) ).
    add_item( iv_name = |Pasta|
      iv_text = |Spaghetti with Marinara Sauce, and a slice of bread|
      iv_vegetarian = abap_true iv_price = CONV #( '3.89' ) ).
  ENDMETHOD.
  METHOD add_item.
    " add_item() takes all the parameters necessary to create a Menu
    " Item and instantiates one. It also checks to make sure we haven't
    " hit the menu size limit.
    IF mv_number_of_items > c_max_items.
      " We specifically want to keep this menu under a certain size.
      cl_demo_output=>write( |Sorry, menu is full!  | &
                             |Can't add item to menu| ).
      RETURN.
    ENDIF.
    DATA(lo_menu_item) = NEW lcl_menu_item(
                           iv_name = iv_name
                           iv_text = iv_text
                           iv_vegetarian = iv_vegetarian
                           iv_price = iv_price ).
    ADD 1 TO mv_number_of_items.
    mt_menu_items[ mv_number_of_items ] = lo_menu_item.
  ENDMETHOD.
  METHOD menu_items.
    " menu_items() returns the internal table of menu items.
    rt_items = mt_menu_items.
  ENDMETHOD.
  " Like before, there is a bunch of code that depends on the
  " implementation of this menu being a Standard Table. We're
  " too busy cooking to rewrite all of this.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  The specification for an ABAP-enabled Waitress: code-name "Alice"
*&    print_menu()
*&      - prints every item on the menu
*&    print_vegetarian_menu()
*&      - prints all vegetarian menu items
*&    is_item_vegetarian( iv_name )
*&      - given the name of an item, return true if the item is
*&        vegetarian, otherwise, return false
*&---------------------------------------------------------------------*
CLASS lcl_waitress DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING io_pancake_menu TYPE REF TO lcl_pancake_menu
                            io_diner_menu   TYPE REF TO lcl_diner_menu,
      print_menu,
      print_vegetarian_menu,
      is_item_vegetarian IMPORTING iv_name TYPE string
                         RETURNING VALUE(rv_flag) TYPE abap_bool.
  PRIVATE SECTION.
    DATA:
      mo_pancake_menu TYPE REF TO lcl_pancake_menu,
      mo_diner_menu   TYPE REF TO lcl_diner_menu.
ENDCLASS.

CLASS lcl_waitress IMPLEMENTATION.
  METHOD constructor.
    mo_pancake_menu = io_pancake_menu.
    mo_diner_menu = io_diner_menu.
  ENDMETHOD.
  METHOD print_menu.
    " The methods look the same, but these calls are returning
    " different table types.
    DATA(lt_pancake_menu_items) = mo_pancake_menu->menu_items( ).
    DATA(lt_diner_menu_items) = mo_diner_menu->menu_items( ).

    " Now, we have to implement two different loops to step through the
    " two implementations of the menu items ...
    cl_demo_output=>write_text( |MENU\n---------\n\nBREAKFAST| ).
    LOOP AT lt_pancake_menu_items INTO DATA(ls_menu_entry).
      " ... one loop for the Sorted Table ...
      cl_demo_output=>write( ls_menu_entry-item->description( ) ).
    ENDLOOP.

    cl_demo_output=>write_text( |\nLUNCH| ).
    LOOP AT lt_diner_menu_items INTO DATA(lo_menu_item).
      " ... and another for the Standard Table.
      cl_demo_output=>write( lo_menu_item->description( ) ).
    ENDLOOP.
  ENDMETHOD.
  METHOD print_vegetarian_menu.
    " Every other method in the Waitress is a variation of this theme.
    " We're always going to need to get both menus and use two loops
    " to iterate through their items. If another menu with a different
    " implementation is acquired then we'll have three loops.
    cl_demo_output=>write_text( |VEGETARIAN MENU\n----------------\n| ).
    LOOP AT mo_pancake_menu->menu_items( ) INTO DATA(ls_menu_entry).
      CHECK ls_menu_entry-item->is_vegetarian( ).
      cl_demo_output=>write( ls_menu_entry-item->description( ) ).
    ENDLOOP.

    LOOP AT mo_diner_menu->menu_items( ) INTO DATA(lo_menu_item).
      CHECK lo_menu_item->is_vegetarian( ).
      cl_demo_output=>write( lo_menu_item->description( ) ).
    ENDLOOP.
  ENDMETHOD.
  METHOD is_item_vegetarian.
    DATA(lt_breakfast_items) = mo_pancake_menu->menu_items( ).
    DATA(lt_lunch_items) = mo_diner_menu->menu_items( ).

    " The implementation is showing through; breakfast items are in a
    " Sorted Table with key, and lunch items are in a Standard Table.
    DATA(ls_menu_entry) = VALUE #(
      lt_breakfast_items[ name = iv_name ] OPTIONAL ).
    IF ls_menu_entry IS NOT INITIAL.
      rv_flag = ls_menu_entry-item->is_vegetarian( ).
      RETURN.
    ENDIF.

    LOOP AT lt_lunch_items INTO DATA(lo_menu_item).
      CHECK lo_menu_item->name( ) = iv_name.
      rv_flag = lo_menu_item->is_vegetarian( ).
      RETURN.
    ENDLOOP.
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

    " First we create the new menus.
    DATA(lo_pancake_menu) = NEW lcl_pancake_menu( ).
    DATA(lo_diner_menu) = NEW lcl_diner_menu( ).

    " Then we create a Waitress and pass her the menus.
    DATA(lo_waitress) = NEW lcl_waitress(
                          io_pancake_menu = lo_pancake_menu
                          io_diner_menu   = lo_diner_menu ).

    " Then we print them.
    lo_waitress->print_menu( ).
    cl_demo_output=>line( ).

    lo_waitress->print_vegetarian_menu( ).
    cl_demo_output=>line( ).

    cl_demo_output=>write_text(
      |\nCustomer asks, is the Hotdog vegetarian?| ).
    cl_demo_output=>write( |Waitress says: { COND #(
      WHEN lo_waitress->is_item_vegetarian( |Hotdog| )
      THEN |Yes| ELSE |No| ) }| ).
    cl_demo_output=>line( ).

    cl_demo_output=>write_text(
      |\nCustomer asks, are the Waffles vegetarian?| ).
    cl_demo_output=>write( |Waitress says: { COND #(
      WHEN lo_waitress->is_item_vegetarian( |Waffles| )
      THEN |Yes| ELSE |No| ) }| ).
    cl_demo_output=>line( ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section( |Well-Managed Collections | &
    |- the Iterator Pattern (using internal tables)| ).
  cl_demo_output=>line( ).
  lcl_menu_test_drive=>main( ).
  cl_demo_output=>display( ).
