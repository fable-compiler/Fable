import pytest
from fable_library.reg_exp import create


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
