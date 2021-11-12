// [Y combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed-point_combinators_in_lambda_calculus)

pub mod Func {

    // fixed-point combinator for Fn(&T1) -> R
    trait Apply1<T1, R> {
        fn apply(&self, f: &dyn Apply1<T1, R>, t1: &T1) -> R;
    }
    impl<T1, R, F> Apply1<T1, R> for F
            where F: Fn(&dyn Apply1<T1, R>, &T1) -> R {
        fn apply(&self, f: &dyn Apply1<T1, R>, t1: &T1) -> R {
            self(f, t1)
        }
    }
    pub fn fix1<T1, R>(f: impl Fn(
        &dyn Fn(&T1) -> R, &T1) -> R) ->
        impl Fn(&T1) -> R {
        move |t1|
            (&| x: &dyn Apply1<T1, R>, y1: &T1 | x.apply(x, y1))
            (&| x: &dyn Apply1<T1, R>, y1: &T1 | f(&|z1| x.apply(x, z1), y1), t1)
    }

    // fixed-point combinator for Fn(&T1, &T2) -> R
    trait Apply2<T1, T2, R> {
        fn apply(&self, f:
            &dyn Apply2<T1, T2, R>, t1: &T1, t2: &T2) -> R;
    }
    impl<T1, T2, R, F>
    Apply2<T1, T2, R> for F where F: Fn(
            &dyn Apply2<T1, T2, R>, &T1, &T2) -> R {
        fn apply(&self, f:
            &dyn Apply2<T1, T2, R>, t1: &T1, t2: &T2) -> R {
            self(f, t1, t2)
        }
    }
    pub fn fix2<T1, T2, R>(f: impl Fn(
        &dyn Fn(&T1, &T2) -> R, &T1, &T2) -> R) ->
        impl Fn(&T1, &T2) -> R {
        move |t1, t2|
            (&| x: &dyn Apply2<T1, T2, R>, y1: &T1, y2: &T2 |
                x.apply(x, y1, y2))
            (&| x: &dyn Apply2<T1, T2, R>, y1: &T1, y2: &T2 |
                f(&|z1, z2| x.apply(x, z1, z2), y1, y2), t1, t2)
    }

    // fixed-point combinator for Fn(&T1, &T2, &T3) -> R
    trait Apply3<T1, T2, T3, R> {
        fn apply(&self, f:
            &dyn Apply3<T1, T2, T3, R>, t1: &T1, t2: &T2, t3: &T3) -> R;
    }
    impl<T1, T2, T3, R, F>
    Apply3<T1, T2, T3, R> for F where F: Fn(
            &dyn Apply3<T1, T2, T3, R>, &T1, &T2, &T3) -> R {
        fn apply(&self, f:
            &dyn Apply3<T1, T2, T3, R>, t1: &T1, t2: &T2, t3: &T3) -> R {
            self(f, t1, t2, t3)
        }
    }
    pub fn fix3<T1, T2, T3, R>(f: impl Fn(
        &dyn Fn(&T1, &T2, &T3) -> R,
                &T1, &T2, &T3) -> R) ->
        impl Fn(&T1, &T2, &T3) -> R {
        move |t1, t2, t3|
            (&| x: &dyn Apply3<T1, T2, T3, R>, y1: &T1, y2: &T2, y3: &T3 |
                x.apply(x, y1, y2, y3))
            (&| x: &dyn Apply3<T1, T2, T3, R>, y1: &T1, y2: &T2, y3: &T3 |
                f(&|z1, z2, z3| x.apply(x, z1, z2, z3), y1, y2, y3), t1, t2, t3)
    }

    // fixed-point combinator for Fn(&T1, &T2, &T3, &T4) -> R
    trait Apply4<T1, T2, T3, T4, R> {
        fn apply(&self, f:
            &dyn Apply4<T1, T2, T3, T4, R>,
            t1: &T1, t2: &T2, t3: &T3, t4: &T4) -> R;
    }
    impl<T1, T2, T3, T4, R, F>
    Apply4<T1, T2, T3, T4, R> for F where F: Fn(
            &dyn Apply4<T1, T2, T3, T4, R>,
            &T1, &T2, &T3, &T4) -> R {
        fn apply(&self, f:
            &dyn Apply4<T1, T2, T3, T4, R>,
            t1: &T1, t2: &T2, t3: &T3, t4: &T4) -> R {
            self(f, t1, t2, t3, t4)
        }
    }
    pub fn fix4<T1, T2, T3, T4, R>(f: impl Fn(
        &dyn Fn(&T1, &T2, &T3, &T4) -> R,
                &T1, &T2, &T3, &T4) -> R) ->
        impl Fn(&T1, &T2, &T3, &T4) -> R {
        move |t1, t2, t3, t4|
            (&| x: &dyn Apply4<T1, T2, T3, T4, R>,
                y1: &T1, y2: &T2, y3: &T3, y4: &T4 |
                x.apply(x, y1, y2, y3, y4))
            (&| x: &dyn Apply4<T1, T2, T3, T4, R>,
                y1: &T1, y2: &T2, y3: &T3, y4: &T4 |
                f(&|z1, z2, z3, z4| x.apply(x,
                    z1, z2, z3, z4),
                    y1, y2, y3, y4),
                    t1, t2, t3, t4)
    }

    // fixed-point combinator for Fn(&T1, &T2, &T3, &T4, &T5) -> R
    trait Apply5<T1, T2, T3, T4, T5, R> {
        fn apply(&self, f:
            &dyn Apply5<T1, T2, T3, T4, T5, R>,
            t1: &T1, t2: &T2, t3: &T3, t4: &T4, t5: &T5) -> R;
    }
    impl<T1, T2, T3, T4, T5, R, F>
    Apply5<T1, T2, T3, T4, T5, R> for F where F: Fn(
            &dyn Apply5<T1, T2, T3, T4, T5, R>,
            &T1, &T2, &T3, &T4, &T5) -> R {
        fn apply(&self, f:
            &dyn Apply5<T1, T2, T3, T4, T5, R>,
            t1: &T1, t2: &T2, t3: &T3, t4: &T4, t5: &T5) -> R {
            self(f, t1, t2, t3, t4, t5)
        }
    }
    pub fn fix5<T1, T2, T3, T4, T5, R>(f: impl Fn(
        &dyn Fn(&T1, &T2, &T3, &T4, &T5) -> R,
                &T1, &T2, &T3, &T4, &T5) -> R) ->
        impl Fn(&T1, &T2, &T3, &T4, &T5) -> R {
        move |t1, t2, t3, t4, t5|
            (&| x: &dyn Apply5<T1, T2, T3, T4, T5, R>,
                y1: &T1, y2: &T2, y3: &T3, y4: &T4, y5: &T5 |
                x.apply(x, y1, y2, y3, y4, y5))
            (&| x: &dyn Apply5<T1, T2, T3, T4, T5, R>,
                y1: &T1, y2: &T2, y3: &T3, y4: &T4, y5: &T5 |
                f(&|z1, z2, z3, z4, z5| x.apply(x,
                    z1, z2, z3, z4, z5),
                    y1, y2, y3, y4, y5),
                    t1, t2, t3, t4, t5)
    }

    // fixed-point combinator for Fn(&T1, &T2, &T3, &T4, &T5, &T6) -> R
    trait Apply6<T1, T2, T3, T4, T5, T6, R> {
        fn apply(&self, f:
            &dyn Apply6<T1, T2, T3, T4, T5, T6, R>,
            t1: &T1, t2: &T2, t3: &T3, t4: &T4, t5: &T5, t6: &T6) -> R;
    }
    impl<T1, T2, T3, T4, T5, T6, R, F>
    Apply6<T1, T2, T3, T4, T5, T6, R> for F where F: Fn(
            &dyn Apply6<T1, T2, T3, T4, T5, T6, R>,
            &T1, &T2, &T3, &T4, &T5, &T6) -> R {
        fn apply(&self, f:
            &dyn Apply6<T1, T2, T3, T4, T5, T6, R>,
            t1: &T1, t2: &T2, t3: &T3, t4: &T4, t5: &T5, t6: &T6) -> R {
            self(f, t1, t2, t3, t4, t5, t6)
        }
    }
    pub fn fix6<T1, T2, T3, T4, T5, T6, R>(f: impl Fn(
        &dyn Fn(&T1, &T2, &T3, &T4, &T5, &T6) -> R,
                &T1, &T2, &T3, &T4, &T5, &T6) -> R) ->
        impl Fn(&T1, &T2, &T3, &T4, &T5, &T6) -> R {
        move |t1, t2, t3, t4, t5, t6|
            (&| x: &dyn Apply6<T1, T2, T3, T4, T5, T6, R>,
                y1: &T1, y2: &T2, y3: &T3, y4: &T4, y5: &T5, y6: &T6 |
                x.apply(x, y1, y2, y3, y4, y5, y6))
            (&| x: &dyn Apply6<T1, T2, T3, T4, T5, T6, R>,
                y1: &T1, y2: &T2, y3: &T3, y4: &T4, y5: &T5, y6: &T6 |
                f(&|z1, z2, z3, z4, z5, z6| x.apply(x,
                    z1, z2, z3, z4, z5, z6),
                    y1, y2, y3, y4, y5, y6),
                    t1, t2, t3, t4, t5, t6)
    }

    // fixed-point combinator for Fn(&T1, &T2, &T3, &T4, &T5, &T6, &T7) -> R
    trait Apply7<T1, T2, T3, T4, T5, T6, T7, R> {
        fn apply(&self, f:
            &dyn Apply7<T1, T2, T3, T4, T5, T6, T7, R>,
            t1: &T1, t2: &T2, t3: &T3, t4: &T4, t5: &T5, t6: &T6, t7: &T7) -> R;
    }
    impl<T1, T2, T3, T4, T5, T6, T7, R, F>
    Apply7<T1, T2, T3, T4, T5, T6, T7, R> for F where F: Fn(
            &dyn Apply7<T1, T2, T3, T4, T5, T6, T7, R>,
            &T1, &T2, &T3, &T4, &T5, &T6, &T7) -> R {
        fn apply(&self, f:
            &dyn Apply7<T1, T2, T3, T4, T5, T6, T7, R>,
            t1: &T1, t2: &T2, t3: &T3, t4: &T4, t5: &T5, t6: &T6, t7: &T7) -> R {
            self(f, t1, t2, t3, t4, t5, t6, t7)
        }
    }
    pub fn fix7<T1, T2, T3, T4, T5, T6, T7, R>(f: impl Fn(
        &dyn Fn(&T1, &T2, &T3, &T4, &T5, &T6, &T7) -> R,
                &T1, &T2, &T3, &T4, &T5, &T6, &T7) -> R) ->
        impl Fn(&T1, &T2, &T3, &T4, &T5, &T6, &T7) -> R {
        move |t1, t2, t3, t4, t5, t6, t7|
            (&| x: &dyn Apply7<T1, T2, T3, T4, T5, T6, T7, R>,
                y1: &T1, y2: &T2, y3: &T3, y4: &T4, y5: &T5, y6: &T6, y7: &T7 |
                x.apply(x, y1, y2, y3, y4, y5, y6, y7))
            (&| x: &dyn Apply7<T1, T2, T3, T4, T5, T6, T7, R>,
                y1: &T1, y2: &T2, y3: &T3, y4: &T4, y5: &T5, y6: &T6, y7: &T7 |
                f(&|z1, z2, z3, z4, z5, z6, z7| x.apply(x,
                    z1, z2, z3, z4, z5, z6, z7),
                    y1, y2, y3, y4, y5, y6, y7),
                    t1, t2, t3, t4, t5, t6, t7)
    }

    // fixed-point combinator for Fn(&T1, &T2, &T3, &T4, &T5, &T6, &T7, &T8) -> R
    trait Apply8<T1, T2, T3, T4, T5, T6, T7, T8, R> {
        fn apply(&self, f:
            &dyn Apply8<T1, T2, T3, T4, T5, T6, T7, T8, R>,
            t1: &T1, t2: &T2, t3: &T3, t4: &T4, t5: &T5, t6: &T6, t7: &T7, t8: &T8) -> R;
    }
    impl<T1, T2, T3, T4, T5, T6, T7, T8, R, F>
    Apply8<T1, T2, T3, T4, T5, T6, T7, T8, R> for F where F: Fn(
            &dyn Apply8<T1, T2, T3, T4, T5, T6, T7, T8, R>,
            &T1, &T2, &T3, &T4, &T5, &T6, &T7, &T8) -> R {
        fn apply(&self, f:
            &dyn Apply8<T1, T2, T3, T4, T5, T6, T7, T8, R>,
            t1: &T1, t2: &T2, t3: &T3, t4: &T4, t5: &T5, t6: &T6, t7: &T7, t8: &T8) -> R {
            self(f, t1, t2, t3, t4, t5, t6, t7, t8)
        }
    }
    pub fn fix8<T1, T2, T3, T4, T5, T6, T7, T8, R>(f: impl Fn(
        &dyn Fn(&T1, &T2, &T3, &T4, &T5, &T6, &T7, &T8) -> R,
                &T1, &T2, &T3, &T4, &T5, &T6, &T7, &T8) -> R) ->
        impl Fn(&T1, &T2, &T3, &T4, &T5, &T6, &T7, &T8) -> R {
        move |t1, t2, t3, t4, t5, t6, t7, t8|
            (&| x: &dyn Apply8<T1, T2, T3, T4, T5, T6, T7, T8, R>,
                y1: &T1, y2: &T2, y3: &T3, y4: &T4, y5: &T5, y6: &T6, y7: &T7, y8: &T8 |
                x.apply(x, y1, y2, y3, y4, y5, y6, y7, y8))
            (&| x: &dyn Apply8<T1, T2, T3, T4, T5, T6, T7, T8, R>,
                y1: &T1, y2: &T2, y3: &T3, y4: &T4, y5: &T5, y6: &T6, y7: &T7, y8: &T8 |
                f(&|z1, z2, z3, z4, z5, z6, z7, z8| x.apply(x,
                    z1, z2, z3, z4, z5, z6, z7, z8),
                    y1, y2, y3, y4, y5, y6, y7, y8),
                    t1, t2, t3, t4, t5, t6, t7, t8)
    }

    // fixed-point combinator for Fn(&T1, &T2, &T3, &T4, &T5, &T6, &T7, &T8, &T9) -> R
    trait Apply9<T1, T2, T3, T4, T5, T6, T7, T8, T9, R> {
        fn apply(&self, f:
            &dyn Apply9<T1, T2, T3, T4, T5, T6, T7, T8, T9, R>,
            t1: &T1, t2: &T2, t3: &T3, t4: &T4, t5: &T5, t6: &T6, t7: &T7, t8: &T8, t9: &T9) -> R;
    }
    impl<T1, T2, T3, T4, T5, T6, T7, T8, T9, R, F>
    Apply9<T1, T2, T3, T4, T5, T6, T7, T8, T9, R> for F where F: Fn(
            &dyn Apply9<T1, T2, T3, T4, T5, T6, T7, T8, T9, R>,
            &T1, &T2, &T3, &T4, &T5, &T6, &T7, &T8, &T9) -> R {
        fn apply(&self, f:
            &dyn Apply9<T1, T2, T3, T4, T5, T6, T7, T8, T9, R>,
            t1: &T1, t2: &T2, t3: &T3, t4: &T4, t5: &T5, t6: &T6, t7: &T7, t8: &T8, t9: &T9) -> R {
            self(f, t1, t2, t3, t4, t5, t6, t7, t8, t9)
        }
    }
    pub fn fix9<T1, T2, T3, T4, T5, T6, T7, T8, T9, R>(f: impl Fn(
        &dyn Fn(&T1, &T2, &T3, &T4, &T5, &T6, &T7, &T8, &T9) -> R,
                &T1, &T2, &T3, &T4, &T5, &T6, &T7, &T8, &T9) -> R) ->
        impl Fn(&T1, &T2, &T3, &T4, &T5, &T6, &T7, &T8, &T9) -> R {
        move |t1, t2, t3, t4, t5, t6, t7, t8, t9|
            (&| x: &dyn Apply9<T1, T2, T3, T4, T5, T6, T7, T8, T9, R>,
                y1: &T1, y2: &T2, y3: &T3, y4: &T4, y5: &T5, y6: &T6, y7: &T7, y8: &T8, y9: &T9 |
                x.apply(x, y1, y2, y3, y4, y5, y6, y7, y8, y9))
            (&| x: &dyn Apply9<T1, T2, T3, T4, T5, T6, T7, T8, T9, R>,
                y1: &T1, y2: &T2, y3: &T3, y4: &T4, y5: &T5, y6: &T6, y7: &T7, y8: &T8, y9: &T9 |
                f(&|z1, z2, z3, z4, z5, z6, z7, z8, z9| x.apply(x,
                    z1, z2, z3, z4, z5, z6, z7, z8, z9),
                    y1, y2, y3, y4, y5, y6, y7, y8, y9),
                    t1, t2, t3, t4, t5, t6, t7, t8, t9)
    }

}
