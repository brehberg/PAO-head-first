*&---------------------------------------------------------------------*
*&  Sample for Well-Managed Collections - the Iterator Pattern
*&    based on Head First Design Patterns: Chapter 9
*&---------------------------------------------------------------------*
REPORT yy_head_first_iterator.

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
*&  Here is the Iterator interface with two methods.
*&---------------------------------------------------------------------*
*&  We'll decouple Waitress from the implementation of the menus, so
*&  now we can use an Iterator to iterate over any table of menu items
*&  without having to know how the table of items is implemented. This
*&  solves the problem of the Waitress depending on the implementation
*&  of the Menu Items internal table type.
*&---------------------------------------------------------------------*
INTERFACE lif_iterator.
  METHODS:
    " The has_next() method returns a boolean indicating whether or not
    " there are more elements to iterate over...
    has_next RETURNING VALUE(rv_flag) TYPE abap_bool,
    " ...and the next() method returns the next element.
    next RETURNING VALUE(ro_menu_item) TYPE REF TO lcl_menu_item.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Here's our new Menu interface with one method, create_iterator().
*&---------------------------------------------------------------------*
*&  This is a simple interface that just lets the clients get an
*&  Iterator for the items in a menu. This solves the problem of
*&  the Waitress depending on the concrete Menu class types.
*&---------------------------------------------------------------------*
INTERFACE lif_menu.
  " We're returning the Iterator interface. The client doesn't need to
  " know how the Menu Items are maintained in the Diner Menu, nor does
  " it need to know how the Diner Menu Iterator is implemented. It just
  " needs to use the iterator to step through the items in the menu.
  METHODS create_iterator
    RETURNING VALUE(ro_iterator) TYPE REF TO lif_iterator.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  Here's the implementation of the Pancake House menu.
*&---------------------------------------------------------------------*
*&  Pancake Menu now implements the Menu interface, which means it
*&  needs to implement the new create_iterator() method.
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
    " We implement the Menu interface.
    INTERFACES lif_menu.
     METHODS:
      constructor,
      add_item IMPORTING iv_name       TYPE string
                         iv_text       TYPE string
                         iv_vegetarian TYPE abap_bool
                         iv_price      TYPE decfloat16.
    " other menu methods here
  PRIVATE SECTION.
    DATA mt_menu_items TYPE tt_sorted_menu_items.
    " We're not going to need the public menu_items() method anymore
    " and in fact, we want to mark it private to hide it because it
    " exposes our internal implementation!
    METHODS menu_items
      RETURNING VALUE(rt_items) TYPE tt_sorted_menu_items.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  The Pancake Menu Iterator is an implementation of Iterator that
*&  knows how to iterate over a sorted internal table of Menu Items.
*&---------------------------------------------------------------------*
CLASS lcl_pancake_menu_iterator DEFINITION FINAL.
  PUBLIC SECTION.
    " We implement the Iterator interface.
    INTERFACES lif_iterator.
    " The constructor takes the internal table of menu items we are
    " going to iterate over.
    METHODS constructor
      IMPORTING it_items TYPE lcl_pancake_menu=>tt_sorted_menu_items.
  PRIVATE SECTION.
    DATA:
      mt_menu_items TYPE lcl_pancake_menu=>tt_sorted_menu_items,
      " Instance variable mv_position maintains the current position
      " of the iteration over the internal table.
      mv_position TYPE i VALUE 1.
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
    rt_items = mt_menu_items.
  ENDMETHOD.
  METHOD lif_menu~create_iterator.
    " Here's the new create_iterator() method. It creates a Pancake Menu
    " Iterator from the Menu Items table and returns it to the client.
    ro_iterator = NEW lcl_pancake_menu_iterator( menu_items( ) ).
  ENDMETHOD.
  " There is a bunch of other menu code that depends on the Sorted Table
  " implementation. We don't want to have to rewrite all that code!
ENDCLASS.

CLASS lcl_pancake_menu_iterator IMPLEMENTATION.
  METHOD constructor.
    mt_menu_items = it_items.
  ENDMETHOD.
  METHOD lif_iterator~next.
    " The next() method returns the next item in the sorted table
    " and increments the position.
    ro_menu_item = mt_menu_items[ mv_position ]-item.
    ADD 1 TO mv_position.
  ENDMETHOD.
  METHOD lif_iterator~has_next.
    " The has_next() method checks to see if we've seen all the elements
    " of the table and returns true if there are more to iterate through
    rv_flag = xsdbool( NOT mv_position > lines( mt_menu_items ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  And here's the implementation of the Diner menu.
*&---------------------------------------------------------------------*
*&  Each concrete Menu is responsible for creating the appropriate
*&  concrete Menu Item Iterator class.
*&---------------------------------------------------------------------*
CLASS lcl_diner_menu DEFINITION FINAL.
  PUBLIC SECTION.
    " It takes a different approach, using a Standard Internal Table
    " without a key, in order to control the max size of the menu.
    TYPES tt_menu_items TYPE STANDARD TABLE
      OF REF TO lcl_menu_item WITH EMPTY KEY.
    " We implement the Menu interface.
    INTERFACES lif_menu.
    METHODS:
      constructor,
      add_item IMPORTING iv_name       TYPE string
                         iv_text       TYPE string
                         iv_vegetarian TYPE abap_bool
                         iv_price      TYPE decfloat16.
    " other menu methods here
  PRIVATE SECTION.
    CONSTANTS c_max_items TYPE i VALUE 6.
    DATA:
      mv_number_of_items TYPE i VALUE 0,
      mt_menu_items      TYPE tt_menu_items.
    " We're not going to need the public menu_items() method anymore
    " and in fact, we want to mark it private to hide it because it
    " exposes our internal implementation!
    METHODS menu_items RETURNING VALUE(rt_items) TYPE tt_menu_items.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  The Diner Menu Iterator is an implementation of Iterator that knows
*&  how to iterate over a standard internal table of Menu Items.
*&---------------------------------------------------------------------*
CLASS lcl_diner_menu_iterator DEFINITION FINAL.
  PUBLIC SECTION.
    " We implement the Iterator interface.
    INTERFACES lif_iterator.
    " The constructor takes the internal table of menu items we are
    " going to iterate over.
    METHODS constructor
      IMPORTING it_items TYPE lcl_diner_menu=>tt_menu_items.
  PRIVATE SECTION.
    DATA:
      mt_menu_items TYPE lcl_diner_menu=>tt_menu_items,
      " Instance variable mv_position maintains the current position
      " of the iteration over the internal table.
      mv_position TYPE i VALUE 1.
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
    rt_items = mt_menu_items.
  ENDMETHOD.
  METHOD lif_menu~create_iterator.
    " Here's the new create_iterator() method. It creates a Diner Menu
    " Iterator from the Menu Items table and returns it to the client.
    ro_iterator = NEW lcl_diner_menu_iterator( menu_items( ) ).
  ENDMETHOD.
  " Like before, there is a bunch of code that depends on the
  " implementation of this menu being a Standard Table. We're
  " too busy cooking to rewrite all of this.
ENDCLASS.

CLASS lcl_diner_menu_iterator IMPLEMENTATION.
  METHOD constructor.
    mt_menu_items = it_items.
  ENDMETHOD.
  METHOD lif_iterator~next.
    " The next() method returns the next item in the standard table
    " and increments the position.
    ro_menu_item = mt_menu_items[ mv_position ].
    ADD 1 TO mv_position.
  ENDMETHOD.
  METHOD lif_iterator~has_next.
    " The has_next() method checks to see if we've seen all the elements
    " of the table and returns true if there are more to iterate through
    rv_flag = xsdbool( NOT ( mv_position > lines( mt_menu_items ) OR
    " Because the Diner Menu went ahead and allocate a max sized table,
    " we need to check not only if we are at the end of the table, but
    " also if the next item is null, which indicates there are no more.
                       mt_menu_items[ mv_position ] IS INITIAL ) ).
  ENDMETHOD.
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
*&  New and improved, now Waitress only needs to be concerned with Menus
*&  and Iterators. The two menus implement the exact same methods from
*&  the Menu interface. This frees the Waitress from any dependencies
*&  on concrete Menus. The Iterator allows the Waitress to be decoupled
*&  from the actual implementation of the concrete classes. She doesn't
*&  need to know if a Menu is implemented with a Standard Table, Sorted
*&  Table, or whatever. All she cares is that she can get an Iterator.
*&---------------------------------------------------------------------*
CLASS lcl_waitress DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      " In the constructor the Waitress takes the two menus. We need
      " to replace the concrete Menu classes with the Menu Interface.
      constructor IMPORTING io_pancake_menu TYPE REF TO lif_menu
                            io_diner_menu   TYPE REF TO lif_menu,
      print_menu,
      print_vegetarian_menu,
      is_item_vegetarian IMPORTING iv_name TYPE string
                         RETURNING VALUE(rv_flag) TYPE abap_bool.
  PRIVATE SECTION.
    METHODS:
      " The private print_menu_items() method uses the Iterator to
      " step through the menu items and print them.
      print_menu_items
        IMPORTING io_iterator TYPE REF TO lif_iterator,
      print_vegetarian_items
        IMPORTING io_iterator TYPE REF TO lif_iterator,
      is_vegetarian
        IMPORTING io_iterator TYPE REF TO lif_iterator
                  iv_name     TYPE string
        RETURNING VALUE(rv_flag) TYPE abap_bool.
    DATA:
      mo_pancake_menu TYPE REF TO lif_menu,
      mo_diner_menu   TYPE REF TO lif_menu.
ENDCLASS.

CLASS lcl_waitress IMPLEMENTATION.
  METHOD constructor.
    mo_pancake_menu = io_pancake_menu.
    mo_diner_menu = io_diner_menu.
  ENDMETHOD.
  METHOD print_menu.
    " The print_menu() method now creates two iterators, one per menu.
    DATA(lo_pancake_iterator) = mo_pancake_menu->create_iterator( ).
    DATA(lo_diner_iterator) = mo_diner_menu->create_iterator( ).

    " And then calls the private print_menu_items() with each iterator.
    cl_demo_output=>write_text( |MENU\n---------\n\nBREAKFAST| ).
    print_menu_items( lo_pancake_iterator ).
    cl_demo_output=>write_text( |\nLUNCH| ).
    print_menu_items( lo_diner_iterator ).
  ENDMETHOD.
  METHOD print_menu_items.
    WHILE io_iterator->has_next( ). " Test if there are any more items.
      DATA(lo_menu_item) = io_iterator->next( ).   " Get the next item.
      " Use the item to get the description and print it.
      cl_demo_output=>write( lo_menu_item->description( ) ).
    ENDWHILE.  " Note that we're down to one loop.
  ENDMETHOD.
  METHOD print_vegetarian_menu.
    cl_demo_output=>write_text( |VEGETARIAN MENU\n----------------\n| ).
    print_vegetarian_items( mo_pancake_menu->create_iterator( ) ).
    print_vegetarian_items( mo_diner_menu->create_iterator( ) ).
  ENDMETHOD.
  METHOD is_item_vegetarian.
    DATA(lo_breakfast_items) = mo_pancake_menu->create_iterator( ).
    DATA(lo_lunch_items) = mo_diner_menu->create_iterator( ).

    rv_flag = xsdbool(
      is_vegetarian( iv_name = iv_name
                     io_iterator = lo_breakfast_items ) OR
      is_vegetarian( iv_name = iv_name
                     io_iterator = lo_lunch_items ) ).
  ENDMETHOD.
  METHOD print_vegetarian_items.
    WHILE io_iterator->has_next( ).
      DATA(lo_menu_item) = io_iterator->next( ).
      CHECK lo_menu_item->is_vegetarian( ).
      cl_demo_output=>write( lo_menu_item->description( ) ).
    ENDWHILE.
  ENDMETHOD.
  METHOD is_vegetarian.
    WHILE io_iterator->has_next( ).
      DATA(lo_menu_item) = io_iterator->next( ).
      IF lo_menu_item->name( ) = iv_name.
        rv_flag = lo_menu_item->is_vegetarian( ).
        RETURN.
      ENDIF.
    ENDWHILE.
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
  cl_demo_output=>begin_section(
    |Well-Managed Collections - the Iterator Pattern| ).
  cl_demo_output=>line( ).
  lcl_menu_test_drive=>main( ).
  cl_demo_output=>display( ).
