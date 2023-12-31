use core::{marker::PhantomData, ops::Shl};

use type_fields::{
    macros::{category::Compose, Closure},
    t_funk::{
        bifunctor::FirstF,
        closure::Compose,
        hlist::{Here, Next, ToTList},
        tlist::ToHList,
        CallF, Chain, ChainF, Closure, ComposeL, Composed, CopointF, Copointed, Curry, Flip,
        Flipped, Fmap, Foldl, Function, Pointed, Curry2B, applicative::Pure, SequenceA, Snd, Spread,
        Spreaded, State, Suffixed, Tagged, ThenF, Tuple,
    },
};

#[derive(Closure, Compose)]
struct StateGet<M, T, P>(PhantomData<(M, T, P)>);

impl<M, T, P> Default for StateGet<M, T, P> {
    fn default() -> Self {
        StateGet(PhantomData)
    }
}

impl<M, T, P> Clone for StateGet<M, T, P> {
    fn clone(&self) -> Self {
        StateGet(PhantomData)
    }
}

impl<M, T, P> Copy for StateGet<M, T, P> {}

impl<C, M, T, P> Function<C> for StateGet<M, T, P>
where
    C: type_fields::t_funk::hlist::Get<Tagged<M, T>, P>,
{
    type Output = T;

    fn call(ctx: C) -> Self::Output {
        ctx.get().copoint()
    }
}

#[derive(Closure)]
struct StateSet<U, P>(PhantomData<(U, P)>);

impl<U, P> Default for StateSet<U, P> {
    fn default() -> Self {
        StateSet(PhantomData)
    }
}

impl<U, P> Clone for StateSet<U, P> {
    fn clone(&self) -> Self {
        StateSet(PhantomData)
    }
}

impl<U, P> Copy for StateSet<U, P> {}

impl<U, P, C, T> Function<(T, C)> for StateSet<U, P>
where
    C: type_fields::t_funk::hlist::Set<Tagged<U, T>, P>,
{
    type Output = ((), C);

    fn call((t, ctx): (T, C)) -> Self::Output {
        ((), ctx.set(Tagged::point(t)))
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Closure)]
struct Test;

impl Function<(i32, f32, &str)> for Test {
    type Output = f32;

    fn call((a, b, _): (i32, f32, &str)) -> Self::Output {
        a as f32 + b
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Closure)]
struct Push;

impl<F, I> Function<(F, I)> for Push
where
    F: Compose<Suffixed<Tuple, I>>,
{
    type Output = State<
        Composed<
            Suffixed<FirstF, <F as ComposeL<Suffixed<Tuple, I>>>::ComposeL>,
            Spreaded<Flipped<Tuple>>,
        >,
    >;

    fn call((f, i): (F, I)) -> Self::Output {
        State::point(
            Tuple
                .flip()
                .spread()
                .compose_l(FirstF.suffix(f.compose_l(Tuple.suffix(i)))),
        )
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Closure)]
struct Run;

impl<F, I> Function<(F, I)> for Run
where
    F: Closure<I::TList>,
    I: ToTList,
{
    type Output = State<Curry2B<Tuple, F::Output>>;

    fn call((f, i): (F, I)) -> Self::Output {
        State::point(Tuple.prefix(f.call(i.to_tlist())))
    }
}

#[derive(Closure)]
struct Set<M, P>(PhantomData<(M, P)>);

impl<M, P> Default for Set<M, P> {
    fn default() -> Self {
        Set(PhantomData)
    }
}

impl<M, P> Clone for Set<M, P> {
    fn clone(&self) -> Self {
        Set(PhantomData)
    }
}

impl<M, P, I> Function<I> for Set<M, P> {
    type Output = State<Curry2B<StateSet<M, P>, I>>;

    fn call(input: I) -> Self::Output {
        State::point(StateSet::<M, P>::default().prefix(input))
    }
}

#[derive(Closure)]
struct Take<M, T, P>(PhantomData<(M, T, P)>);

impl<M, T, P> Default for Take<M, T, P> {
    fn default() -> Self {
        Take(PhantomData)
    }
}

impl<M, T, P> Clone for Take<M, T, P> {
    fn clone(&self) -> Self {
        Take(PhantomData)
    }
}

impl<M, T, P, I> Function<I> for Take<M, T, P> {
    type Output = State<Curry2B<StateGet<M, T, P>, I>>;

    fn call(input: I) -> Self::Output {
        State::point(StateGet::<M, T, P>::default().prefix(input))
    }
}

trait ContextPush<T, P> {
    type StateGet<M>
    where
        Self: type_fields::t_funk::hlist::Get<Tagged<M, T>, P>;

    fn get<M>(&self) -> Self::StateGet<M>
    where
        Self: type_fields::t_funk::hlist::Get<Tagged<M, T>, P>;
}

impl<T, U, P> ContextPush<U, P> for T {
    type StateGet<M> = Curry2B<Push, StateGet<M, U, P>>

        where
            T: type_fields::t_funk::hlist::Get<Tagged<M, U>, P>;

    fn get<M>(&self) -> Self::StateGet<M>
    where
        T: type_fields::t_funk::hlist::Get<Tagged<M, U>, P>,
    {
        Push.prefix(StateGet::<M, U, P>::default())
    }
}

trait ContextRun<F> {
    fn run(&self, f: F) -> Curry2B<Run, F> {
        Run.prefix(f)
    }
}

impl<T, F> ContextRun<F> for T {}

trait ContextSet<P> {
    fn set<M>(&self) -> Set<M, P> {
        Default::default()
    }
}

impl<T, P> ContextSet<P> for T {}

trait ContextTake<I, P> {
    fn take<M>(&self) -> Take<M, I, P> {
        Default::default()
    }
}

impl<T, I, P> ContextTake<I, P> for T {}

struct Int;
struct Float;
struct Str;

pub fn test_chain() {
    let ctx = (
        Tagged::<Int, i32>::point(1),
        Tagged::<Float, f32>::point(2.0),
        Tagged::<Str, &str>::point("three"),
    )
        .to_hlist();

    let then = State::<()>::pure(())
        .chain(ctx.get::<Str>())
        .chain(ctx.get::<Float>())
        .chain(ctx.get::<Int>())
        .chain(ctx.run(Test))
        .chain(ctx.set::<Float>())
        .chain(ctx.get::<Float>());

    let res = then.copoint().call(ctx);

    assert_eq!(
        res,
        (
            (3.0,).to_hlist(),
            (
                Tagged::<Int, _>::point(1),
                Tagged::<Float, _>::point(3.0),
                Tagged::<Str, _>::point("three")
            )
                .to_hlist()
        )
    );
}

pub fn test_sequence() {
    let ctx = (
        Tagged::<Int, i32>::point(1),
        Tagged::<Float, f32>::point(2.0),
        Tagged::<Str, &str>::point("three"),
    )
        .to_hlist();

    let then = State::<()>::pure(())
        .chain(ctx.get::<Str>())
        .chain(ctx.get::<Float>())
        .chain(ctx.get::<Int>())
        .chain(ctx.run(Test))
        .chain(ctx.set::<Float>())
        .chain(ctx.get::<Float>());

    let seq = (then.clone(), then.clone(), then).to_hlist();
    let seq = SequenceA::<State<()>>::sequence_a(seq);

    let res = seq.copoint().call(ctx);

    assert_eq!(
        res,
        (
            ((3.0,).to_hlist(), (4.0,).to_hlist(), (5.0,).to_hlist()).to_hlist(),
            (
                Tagged::<Int, _>::point(1),
                Tagged::<Float, _>::point(5.0),
                Tagged::<Str, _>::point("three")
            )
                .to_hlist()
        )
    );
}

pub fn test_fold() {
    let ctx = (
        Tagged::<Int, i32>::point(1),
        Tagged::<Float, f32>::point(2.0),
        Tagged::<Str, &str>::point("three"),
    )
        .to_hlist();

    let then = State::<()>::pure(())
        .chain(ctx.get::<Str>())
        .chain(ctx.get::<Float>())
        .chain(ctx.get::<Int>())
        .chain(ctx.run(Test))
        .chain(ctx.set::<Float>())
        .chain(ctx.get::<Float>());

    let seq = (then.clone(), then.clone(), then).to_hlist();

    let foo = CallF.flip().compose_l(Snd);
    let res = seq.fmap(CopointF).foldl(foo, ctx);

    assert_eq!(
        res,
        (
            Tagged::<Int, _>::point(1),
            (
                Tagged::<Float, _>::point(5.0),
                (Tagged::<Str, _>::point("three"), ())
            )
        ),
    );
}

pub fn test_api() {
    impl<Rhs> Shl<Rhs> for Float
    where
        Rhs: ToHList,
        Rhs::HList: Foldl<ChainF, State<Curry2B<Tuple, ()>>>,
        <Rhs::HList as Foldl<ChainF, State<Curry2B<Tuple, ()>>>>::Foldl:
            Chain<Set<Self, (Next, (Here, ()))>>,
    {
        type Output =
            <<Rhs::HList as Foldl<ChainF, State<Curry2B<Tuple, ()>>>>::Foldl as Chain<
                Set<Self, (Next, (Here, ()))>,
            >>::Chain;

        fn shl(self, rhs: Rhs) -> Self::Output {
            rhs.to_hlist()
                .foldl(ChainF, State::<()>::pure(()))
                .chain(Set::<Self, (Next, (Here, ()))>::default())
        }
    }

    let ctx = (
        Tagged::<Int, i32>::point(1),
        Tagged::<Float, f32>::point(2.0),
        Tagged::<Str, &str>::point("three"),
    )
        .to_hlist();

    let baz = (Float << (Test, Int, Float, Str),);

    let baz = baz.to_hlist().foldl(ThenF, State::<()>::pure(()));
}

