import pytest
from fable_library.reg_exp import (
    EMPTY_MATCH,
    create,
    get_group,
    group_success,
    match_groups,
    match_or_empty,
    next_match_or_empty,
)


def test_accepted_options_compile() -> None:
    ignore_case = 1
    compiled = 8
    non_backtracking = 1024
    regex = create("ab", ignore_case | compiled | non_backtracking)

    assert regex.search("xABx") is not None


def test_unsupported_option_raises() -> None:
    right_to_left = 64

    with pytest.raises(Exception, match="RegexOptions only supports"):
        create("ab", right_to_left)


def test_failed_match_is_empty_match() -> None:
    m = match_or_empty(create("(a)(?<name>b)"), "xyz")

    assert m is EMPTY_MATCH
    assert not m
    assert str(m) == ""
    assert not group_success(m)
    assert not group_success(None)
    assert group_success("")
    assert m.start() == 0
    assert m[0] == ""
    assert next_match_or_empty(m) is EMPTY_MATCH
    assert len(match_groups(m)) == 1
    assert get_group(match_groups(m), 0) is None
    assert get_group(match_groups(m), 2) is None
    assert get_group(match_groups(m), "name") is None


def test_next_match_or_empty_after_last_match() -> None:
    m = match_or_empty(create("(a)"), "a")

    assert m[0] == "a"
    assert get_group(match_groups(m), 1) == "a"
    assert get_group(match_groups(m), 5) is None
    assert next_match_or_empty(m) is EMPTY_MATCH
