import unicodedata

from enum import IntEnum
from typing import Dict, Union


class UnicodeCategory(IntEnum):
    """Defines the Unicode category of a character.

    https://docs.microsoft.com/en-us/dotnet/api/system.globalization.unicodecategory?view=net-6.0
    """

    UppercaseLetter = 0
    LowercaseLetter = 1
    TitlecaseLetter = 2
    ModifierLetter = 3
    OtherLetter = 4
    NonSpacingMark = 5
    SpacingCombiningMark = 6
    EnclosingMark = 7
    DecimalDigitNumber = 8
    LetterNumber = 9
    OtherNumber = 10
    SpaceSeparator = 11
    LineSeparator = 12
    ParagraphSeparator = 13
    Control = 14
    Format = 15
    Surrogate = 16
    PrivateUse = 17
    ConnectorPunctuation = 18
    DashPunctuation = 19
    OpenPunctuation = 20
    ClosePunctuation = 21
    InitialQuotePunctuation = 22
    FinalQuotePunctuation = 23
    OtherPunctuation = 24
    MathSymbol = 25
    CurrencySymbol = 26
    ModifierSymbol = 27
    OtherSymbol = 28
    OtherNotAssigned = 29


unicode_category_2_python: Dict[str, UnicodeCategory] = {
    "Ll": UnicodeCategory.LowercaseLetter,
    "Lu": UnicodeCategory.UppercaseLetter,
    "Nd": UnicodeCategory.DecimalDigitNumber,
    "Cc": UnicodeCategory.Control,
    "Pd": UnicodeCategory.DashPunctuation,
    "Sc": UnicodeCategory.CurrencySymbol,
    "Pc": UnicodeCategory.ConnectorPunctuation,
    "Cf": UnicodeCategory.Format,
    "Pi": UnicodeCategory.InitialQuotePunctuation,
    "Po": UnicodeCategory.OtherPunctuation,
    "Nl": UnicodeCategory.LetterNumber,
    "Zl": UnicodeCategory.LineSeparator,
    "Zs": UnicodeCategory.SpaceSeparator,
    "Sm": UnicodeCategory.MathSymbol,
    "Sk": UnicodeCategory.ModifierSymbol,
    "Mn": UnicodeCategory.NonSpacingMark,
    "Lo": UnicodeCategory.OtherLetter,
    "No": UnicodeCategory.OtherLetter,
}


def char_code_at(s: str, index: int) -> int:
    if index >= 0 and index < len(s):
        return ord(s[index : index + 1])
    else:
        raise ValueError("Index out of range.")


def get_unicode_category(s: str, index: int = 0) -> UnicodeCategory:
    category = unicodedata.category(s[index : index + 1])
    ret = unicode_category_2_python.get(category)
    if ret is not None:
        return ret
    raise ValueError(f"Fable error, unknown Unicode category: {category}")


IS_LETTER_MASK = (
    0
    | 1 << UnicodeCategory.UppercaseLetter
    | 1 << UnicodeCategory.LowercaseLetter
    | 1 << UnicodeCategory.TitlecaseLetter
    | 1 << UnicodeCategory.ModifierLetter
    | 1 << UnicodeCategory.OtherLetter
)
IS_PUNCTUATION_MASK = (
    0
    | 1 << UnicodeCategory.ConnectorPunctuation
    | 1 << UnicodeCategory.DashPunctuation
    | 1 << UnicodeCategory.OpenPunctuation
    | 1 << UnicodeCategory.ClosePunctuation
    | 1 << UnicodeCategory.InitialQuotePunctuation
    | 1 << UnicodeCategory.FinalQuotePunctuation
    | 1 << UnicodeCategory.OtherPunctuation
)

IS_SEPARATOR_MASK = (
    0
    | 1 << UnicodeCategory.SpaceSeparator
    | 1 << UnicodeCategory.LineSeparator
    | 1 << UnicodeCategory.ParagraphSeparator
)
IS_NUMBER_MASK = (
    0
    | 1 << UnicodeCategory.DecimalDigitNumber
    | 1 << UnicodeCategory.LetterNumber
    | 1 << UnicodeCategory.OtherNumber
)
IS_SYMBOL_MASK = (
    0
    | 1 << UnicodeCategory.MathSymbol
    | 1 << UnicodeCategory.CurrencySymbol
    | 1 << UnicodeCategory.ModifierSymbol
    | 1 << UnicodeCategory.OtherSymbol
)
IS_WHITESPACE_MASK = (
    0
    | 1 << UnicodeCategory.SpaceSeparator
    | 1 << UnicodeCategory.LineSeparator
    | 1 << UnicodeCategory.ParagraphSeparator
)

IS_DIGIT_MASK = 1 << UnicodeCategory.DecimalDigitNumber
IS_CONTROL_MASK = 1 << UnicodeCategory.Control
IS_LETTER_OR_DIGIT_MASK = IS_LETTER_MASK | IS_DIGIT_MASK
IS_UPPER_MASK = 1 << UnicodeCategory.UppercaseLetter
IS_LOWER_MASK = 1 << UnicodeCategory.LowercaseLetter


def is_digit(s: str, index: int = 0) -> bool:
    return s[index : index + 1].isdigit()


def is_letter(s: str, index: int = 0) -> bool:
    return s[index : index + 1].isalpha()


def is_letter_or_digit(s: str, index: int = 0) -> bool:
    test = 1 << get_unicode_category(s, index)
    return bool(test & IS_LETTER_OR_DIGIT_MASK)


def is_control(s: str, index: int = 0):
    test = 1 << get_unicode_category(s, index)
    return bool(test & IS_CONTROL_MASK)


def is_upper(s: str, index: int = 0) -> bool:
    test = 1 << get_unicode_category(s, index)
    return bool(test & IS_UPPER_MASK)


def is_lower(s: str, index: int = 0) -> bool:
    test = 1 << get_unicode_category(s, index)
    return bool(test & IS_LOWER_MASK)


def is_punctuation(s: str, index: int = 0) -> bool:
    test = 1 << get_unicode_category(s, index)
    return bool(test & IS_PUNCTUATION_MASK)


def is_separator(s: str, index: int = 0) -> bool:
    test = 1 << get_unicode_category(s, index)
    return bool(test & IS_SEPARATOR_MASK)


def is_number(s: str, index: int = 0) -> bool:
    test = 1 << get_unicode_category(s, index)
    return bool(test & IS_NUMBER_MASK)


def is_symbol(s: str, index: int = 0) -> bool:
    test = 1 << get_unicode_category(s, index)
    return bool(test & IS_SYMBOL_MASK)


def is_white_space(s: str, index: int = 0) -> bool:
    test = 1 << get_unicode_category(s, index)
    if test & IS_WHITESPACE_MASK:
        return True

    cp = char_code_at(s, index)
    return (0x09 <= cp <= 0x0D) or cp == 0x85 or cp == 0xA0


def is_high_surrogate(s: str, index: int = 0) -> bool:
    cp = char_code_at(s, index)
    return 0xD800 <= cp <= 0xDBFF


def is_low_surrogate(s: str, index: int = 0) -> bool:
    cp = char_code_at(s, index)
    return 0xDC00 <= cp <= 0xDFFF


def is_surrogate(s: str, index: int = 0) -> bool:
    cp = char_code_at(s, index)
    return 0xD800 <= cp <= 0xDFFF


def is_surrogate_pair(s: str, index: Union[str, int]) -> bool:
    if isinstance(index, int):
        return is_high_surrogate(s, index) and is_low_surrogate(s, index + 1)

    return is_high_surrogate(s) and is_low_surrogate(index)


def parse(input: str) -> str:
    if len(input) == 1:
        return input[0]
    else:
        raise ValueError("String must be exactly one character long.")


__all__ = [
    "char_code_at",
    "get_unicode_category",
    "is_control",
    "is_letter_or_digit",
    "is_letter",
    "is_digit",
    "is_upper",
    "is_lower",
    "is_separator",
    "is_punctuation",
    "is_number",
    "is_symbol",
    "is_separator",
    "is_white_space",
    "is_surrogate",
    "is_low_surrogate",
    "is_high_surrogate",
    "is_surrogate_pair",
    "parse",
]
