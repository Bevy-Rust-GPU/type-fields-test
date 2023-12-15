use std::{collections::HashMap, fmt::Display};

use type_fields::{
    macros::{
        category::{Compose, Id},
        Closure,
    },
    t_funk::{
        category::Compose, ComposeL, function::Const, Analyze, Closure, Composed, Curry2,
        Curry2A, Effect, EvalA, Function, Pointed, Sum,
    },
};

type Dict = HashMap<String, String>;

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct GetLine;

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct PutLine;

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Prompt(String);

#[derive(Debug, Default, Clone, PartialEq, Eq)]
struct Dictionary(Dict);

fn get_line() -> Effect<GetLine> {
    Effect(GetLine)
}

fn put_line() -> Effect<PutLine> {
    Effect(PutLine)
}

fn prompt(message: String) -> Effect<Prompt> {
    Effect(Prompt(message))
}

fn dictionary(dict: Dict) -> Effect<Dictionary> {
    Effect(Dictionary(dict))
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Closure)]
struct NumPrints;

impl Function<GetLine> for NumPrints {
    type Output = Sum<usize>;

    fn call(_: GetLine) -> Self::Output {
        Sum(0)
    }
}

impl Function<PutLine> for NumPrints {
    type Output = Sum<usize>;

    fn call(_: PutLine) -> Self::Output {
        Sum(1)
    }
}

impl Function<Prompt> for NumPrints {
    type Output = Sum<usize>;

    fn call(_: Prompt) -> Self::Output {
        Sum(1)
    }
}

impl Function<Dictionary> for NumPrints {
    type Output = Sum<usize>;

    fn call(_: Dictionary) -> Self::Output {
        Sum(0)
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Closure)]
struct Interpret;

#[derive(
    Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Closure, Id, Compose,
)]
struct ReadLine;

impl<T> Function<T> for ReadLine {
    type Output = String;

    fn call(_: T) -> Self::Output {
        let mut buf = Default::default();
        std::io::stdin().read_line(&mut buf).unwrap();

        buf.replace("\n", "")
    }
}

impl Function<GetLine> for Interpret {
    type Output = ReadLine;

    fn call(_: GetLine) -> Self::Output {
        ReadLine
    }
}

#[derive(
    Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Closure, Id, Compose,
)]
struct Print;

impl<T> Function<T> for Print
where
    T: Display,
{
    type Output = ();

    fn call(t: T) -> Self::Output {
        println!("{}", t);
    }
}

impl Function<PutLine> for Interpret {
    type Output = Print;

    fn call(_: PutLine) -> Self::Output {
        Print
    }
}

impl Function<Prompt> for Interpret {
    type Output = Composed<Print, Curry2A<Const, String>>;

    fn call(input: Prompt) -> Self::Output {
        Print.compose(Const.prefix2(input.0))
    }
}

#[derive(
    Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Closure, Id, Compose,
)]
struct Get;

impl Function<(HashMap<String, String>, String)> for Get {
    type Output = String;

    fn call((mut t, k): (HashMap<String, String>, String)) -> Self::Output {
        t.remove(&k).unwrap_or("I don't know that one".to_string())
    }
}

impl Function<Dictionary> for Interpret {
    type Output = Curry2A<Get, HashMap<String, String>>;

    fn call(input: Dictionary) -> Self::Output {
        Get.prefix2(input.0)
    }
}

pub fn test_free_arrow() {
    let translator = prompt("Hello".to_string())
        .compose_l(prompt("Enter an English word to translate".to_string()))
        .compose_l(get_line())
        .compose_l(dictionary(
            [
                ("apple".to_string(), "manzana".to_string()),
                ("blue".to_string(), "azul".to_string()),
                ("hello".to_string(), "hola".to_string()),
                ("goodbye".to_string(), "adios".to_string()),
            ]
            .into_iter()
            .collect(),
        ))
        .compose_l(put_line());

    let num_prints = Analyze::<NumPrints, Sum<usize>>::point(NumPrints).call(translator.clone());

    println!(
        "The Free Arrow program will print a total of {} times",
        num_prints.0
    );

    let prog = EvalA(Interpret).call(translator);
    prog.call(());
}
