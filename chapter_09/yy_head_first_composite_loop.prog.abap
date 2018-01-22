*&---------------------------------------------------------------------*
*&  Sample for Well-Managed Collections - the Composite Pattern
*&    based on Head First Design Patterns: Chapter 9
*&---------------------------------------------------------------------*
REPORT yy_head_first_composite_loop.

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
*&  Here is a generic Iterator interface with two methods.
*&---------------------------------------------------------------------*
INTERFACE lif_iterator.
  METHODS:
    " The has_next() method returns a boolean indicating whether or not
    " there are more elements to iterate over...
    has_next RETURNING VALUE(rv_flag) TYPE abap_bool,
    " ...and the next() method returns the next element.
    next RETURNING VALUE(ro_object) TYPE REF TO object.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*&  The Standard Iterator is an implementation of the Iterator that
*&  knows how to iterate over a standard internal table of objects.
*&---------------------------------------------------------------------*
CLASS lcl_standard_iterator DEFINITION FINAL.
  PUBLIC SECTION.
    " We implement the Iterator interface.
    INTERFACES lif_iterator.
    " The constructor takes the internal table of objects we are
    " going to iterate over.
    METHODS constructor IMPORTING it_objects TYPE table_of_objects.
  PRIVATE SECTION.
    DATA:
      mt_objects TYPE table_of_objects,
      " Instance variable mv_position maintains the current position
      " of the iteration over the internal table.
      mv_position TYPE i VALUE 1.
ENDCLASS.

CLASS lcl_standard_iterator IMPLEMENTATION.
  METHOD constructor.
    mt_objects = it_objects.
  ENDMETHOD.
  METHOD lif_iterator~next.
    " The next() method returns the next item in the standard table
    " and increments the position.
    ro_object = mt_objects[ mv_position ].
    ADD 1 TO mv_position.
  ENDMETHOD.
  METHOD lif_iterator~has_next.
    " The has_next() method checks to see if we've seen all the elements
    " of the table and returns true if there are more to iterate through
    rv_flag = xsdbool( NOT mv_position > lines( mt_objects ) ).
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
             RETURNING VALUE(ro_child) TYPE REF TO lcl_menu_component,
      " We've added a create_iterator() method to the Menu Component.
      " This means that each Menu and Menu Item will need to implement
      " this method. It also means that calling create_iterator() on
      " a composite should apply to all children of the composite.
      create_iterator ABSTRACT
        RETURNING VALUE(ro_iterator) TYPE REF TO lif_iterator.
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
*&  This Composite Iterator is a serious iterator. It's got the job of
*&  iterating over the Menu Items in the components, and of making sure
*&  all the child Menus (and child child Menus, and so on) are included.
*&  Watch out: Recursion zone ahead.
*&---------------------------------------------------------------------*
CLASS lcl_composite_iterator DEFINITION FINAL.
  PUBLIC SECTION.
    " Like all iterators, we're implementing the Iterator interface.
    INTERFACES lif_iterator.
    METHODS constructor IMPORTING io_iterator TYPE REF TO lif_iterator.
  PRIVATE SECTION.
    CLASS-DATA gv_composite_depth TYPE i VALUE 0.
    DATA mt_iterator_stack TYPE STANDARD TABLE
      OF REF TO lif_iterator WITH EMPTY KEY.
ENDCLASS.

CLASS lcl_composite_iterator IMPLEMENTATION.
  METHOD constructor.
    " The iterator of the top-level composite we're going to iterate
    " over is passed in. We throw that in a standard table that is
    " used as an internal stack data structure.
    INSERT io_iterator INTO mt_iterator_stack INDEX 1.
  ENDMETHOD.
  METHOD lif_iterator~next.
    " Okay, when the client wants to get the next element we first
    " make sure there is one by calling has_next() ...
    IF NOT lif_iterator~has_next( ).
      RETURN.
    ENDIF.
    " If there is a next element, we increase the recursion counter,
    " get the current iterator off the stack and get its next element.
    ADD 1 TO gv_composite_depth.
    DATA(lo_current_iterator) = mt_iterator_stack[ 1 ].
    DATA(lo_next_component) = CAST lcl_menu_component(
                                lo_current_iterator->next( ) ).
    " When at the top recursion level, we then throw that component's
    " iterator on the stack. If the component is a Menu, it will
    " iterate over all its items. If the component is a Menu Item,
    " we get the Null Iterator, and no iteration happens.
    IF gv_composite_depth = 1.
      INSERT lo_next_component->create_iterator( )
        INTO mt_iterator_stack INDEX 1.
    ENDIF.
    " Then we reduce the recursion counter and return the component.
    SUBTRACT 1 FROM gv_composite_depth.
    ro_object = lo_next_component.
  ENDMETHOD.
  METHOD lif_iterator~has_next.
    IF lines( mt_iterator_stack ) = 0.
      " To see if there is a next element, we check to see if the
      " stack is empty; if so, then there isn't one.
      rv_flag = abap_false.
      RETURN.
    ENDIF.
    " Otherwise, we get the iterator off the top of the stack
    " and see if it has a next element.
    DATA(lo_iterator) = mt_iterator_stack[ 1 ].
    IF NOT lo_iterator->has_next( ).
      " If it doesn't we pop it off the stack
      " and call has_next() recursively.
      DELETE mt_iterator_stack INDEX 1.
      rv_flag = xsdbool( lif_iterator~has_next( ) ).
      RETURN.
    ENDIF.
    " Otherwise there is a next element and we return true.
    rv_flag = abap_true.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  The Null Iterator is the laziest Iterator implementation you've
*&  ever seen. At every step of the way it simply punts.
*&  NOTE: This is an example of the Null Object Design Pattern.
*&---------------------------------------------------------------------*
CLASS lcl_null_iterator DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_iterator.
ENDCLASS.

CLASS lcl_null_iterator IMPLEMENTATION.
  METHOD lif_iterator~next.
    RETURN.      " When next() is called, we return nothing.
  ENDMETHOD.
  METHOD lif_iterator~has_next.
    " Most importantly when has_next() is called we always return false.
    rv_flag = abap_false.
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
      print         REDEFINITION,
      create_iterator REDEFINITION.
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
  METHOD create_iterator.
    " Now for the Menu Item Iterator...
    " Whoa! What's this Null Iterator? See the definition above.
    ro_iterator = NEW lcl_null_iterator( ).
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
      child  REDEFINITION,
      create_iterator REDEFINITION.
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
    " Look! Now we use our new Standard Iterator for polymorphism.
    DATA lo_iterator TYPE REF TO lif_iterator.
    lo_iterator = NEW lcl_standard_iterator( mt_menu_components ).
    " We use it to iterate through all the Menu's components...
    " ...those could be other Menus, or they could be Menu Items.
    WHILE lo_iterator->has_next( ).
      " Since both Menus and Menu Items implement print(),
      " we just call print() and the rest is up to them.
      CAST lcl_menu_component( lo_iterator->next( ) )->print( ).
    ENDWHILE.
    " NOTE: If, during this iteration, we encounter another Menu object,
    " its print() method will start another iteration, and so on.
  ENDMETHOD.
  METHOD create_iterator.
    " Here we're using a new iterator called Composite Iterator. It
    " knows how to iterate over any composite. We pass it a standard
    " iterator for the current composite's table of menu components.
    ro_iterator = NEW lcl_composite_iterator(
                    NEW lcl_standard_iterator( mt_menu_components ) ).
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
      print_menu,
      print_vegetarian_menu,
      is_item_vegetarian IMPORTING iv_name TYPE string
                         RETURNING VALUE(rv_flag) TYPE abap_bool.
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
  METHOD print_vegetarian_menu.
    " The print_vegetarian_menu() method takes the mo_all_menus
    " composite object and gets its iterator. That will be our
    " Composite Iterator.
    DATA(lo_iterator) = mo_all_menus->create_iterator( ).

    cl_demo_output=>write_text( |\nVEGETARIAN MENU\n---------------| ).
    WHILE lo_iterator->has_next( ).
      " Iterate through every element of the composite.
      DATA(lo_menu_component) = CAST lcl_menu_component(
                                  lo_iterator->next( ) ).
      TRY.
          " Call each element's is_vegetarian() method and if true,
          " we call its print() method
          IF lo_menu_component->is_vegetarian( ).
            " print() is only called on Menu Items, never composites.
            lo_menu_component->print( ).
          ENDIF.
        CATCH lcx_unsupported_operation.
          " We implemented is_vegetarian() on the Menus to always throw
          " an exception. If that happens we catch the exception, but
          " continue with our iteration.
          CONTINUE.
      ENDTRY.
    ENDWHILE.
  ENDMETHOD.
  METHOD is_item_vegetarian.
    DATA(lo_iterator) = mo_all_menus->create_iterator( ).
    WHILE lo_iterator->has_next( ).
      DATA(lo_item) = CAST lcl_menu_component( lo_iterator->next( ) ).
      IF lo_item->name( ) = iv_name.
        rv_flag = lo_item->is_vegetarian( ).
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
    |Well-Managed Collections - the Composite Iterator Pattern| ).
  cl_demo_output=>line( ).
  lcl_menu_test_drive=>main( ).
  cl_demo_output=>display( ).
