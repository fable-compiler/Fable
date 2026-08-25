from fable_library.union import Union, tagged_union


class _Shape(Union):
    @staticmethod
    def cases() -> list[str]:
        return ["Circle", "Boxed"]


@tagged_union(0)
class Shape_Circle(_Shape):
    radius: int


@tagged_union(1)
class Shape_Boxed(_Shape):
    inner: _Shape


def test_union_case_is_hashable() -> None:
    # dataclass() generates __eq__, which sets __hash__ to None unless it is
    # restored -- union values must keep supporting the hash protocol.
    assert isinstance(hash(Shape_Circle(1)), int)


def test_equal_unions_have_equal_hashes() -> None:
    assert Shape_Circle(2) == Shape_Circle(2)
    assert hash(Shape_Circle(2)) == hash(Shape_Circle(2))


def test_nested_union_is_hashable() -> None:
    # A union hashes over its fields, so a union nested as a field of another
    # union must be hashable as well.
    nested = Shape_Boxed(Shape_Circle(3))
    assert nested.GetHashCode() == Shape_Boxed(Shape_Circle(3)).GetHashCode()
    assert hash(nested) == hash(Shape_Boxed(Shape_Circle(3)))


def test_union_works_in_sets() -> None:
    # set([...]) instead of a set literal: type checkers model tagged_union case
    # classes as non-frozen dataclasses (statically unhashable), while the
    # decorator restores __hash__ at runtime.
    distinct = set([Shape_Circle(1), Shape_Circle(1), Shape_Boxed(Shape_Circle(1))])
    assert len(distinct) == 2
    assert Shape_Circle(1) in distinct
