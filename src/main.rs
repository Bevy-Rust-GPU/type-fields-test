//mod do_notation_monadic;
mod free_arrow;

fn main() {
    /*
    println!("Testing Monadic Do...");
    do_notation_monadic::test_chain();
    do_notation_monadic::test_sequence();
    do_notation_monadic::test_fold();
    do_notation_monadic::test_api();
    */

    println!("Testing Free Arrow...");
    free_arrow::test_free_arrow();
}
