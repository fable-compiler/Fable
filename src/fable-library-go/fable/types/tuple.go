package types

type Tuple0 struct{}

type Tuple1[T any] struct {
	Item1 T
}

type Tuple2[T1 any, T2 any] struct {
	Item1 T1
	Item2 T2
}

type Tuple3[T1 any, T2 any, T3 any] struct {
	Item1 T1
	Item2 T2
	Item3 T3
}

type Tuple4[T1 any, T2 any, T3 any, T4 any] struct {
	Item1 T1
	Item2 T2
	Item3 T3
	Item4 T4
}

type Tuple5[T1 any, T2 any, T3 any, T4 any, T5 any] struct {
	Item1 T1
	Item2 T2
	Item3 T3
	Item4 T4
	Item5 T5
}

type Tuple6[T1 any, T2 any, T3 any, T4 any, T5 any, T6 any] struct {
	Item1 T1
	Item2 T2
	Item3 T3
	Item4 T4
	Item5 T5
	Item6 T6
}
