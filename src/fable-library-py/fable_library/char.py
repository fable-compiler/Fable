import unicodedata
from enum import Enum
from typing import Dict, Union


class UnicodeCategory(Enum):
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


unicodeCategory2Python: Dict[str, UnicodeCategory] = {
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


def char_code_at(s: str, index: int):
    if index >= 0 and index < len(s):
        return ord(s[index : index + 1])
    else:
        raise ValueError("Index out of range.")


def get_unicode_category(s: str, index: int = 0):
    ret = unicodeCategory2Python.get(unicodedata.category(s[index : index + 1]))
    if ret:
        return ret.value
    raise ValueError("Fable error")


IS_LETTER_MASK = (
    0
    | 1 << UnicodeCategory.UppercaseLetter.value
    | 1 << UnicodeCategory.LowercaseLetter.value
    | 1 << UnicodeCategory.TitlecaseLetter.value
    | 1 << UnicodeCategory.ModifierLetter.value
    | 1 << UnicodeCategory.OtherLetter.value
)
IS_PUNCTUATION_MASK = (
    0
    | 1 << UnicodeCategory.ConnectorPunctuation.value
    | 1 << UnicodeCategory.DashPunctuation.value
    | 1 << UnicodeCategory.OpenPunctuation.value
    | 1 << UnicodeCategory.ClosePunctuation.value
    | 1 << UnicodeCategory.InitialQuotePunctuation.value
    | 1 << UnicodeCategory.FinalQuotePunctuation.value
    | 1 << UnicodeCategory.OtherPunctuation.value
)

IS_SEPARATOR_MASK = (
    0
    | 1 << UnicodeCategory.SpaceSeparator.value
    | 1 << UnicodeCategory.LineSeparator.value
    | 1 << UnicodeCategory.ParagraphSeparator.value
)
IS_NUMBER_MASK = (
    0
    | 1 << UnicodeCategory.DecimalDigitNumber.value
    | 1 << UnicodeCategory.LetterNumber.value
    | 1 << UnicodeCategory.OtherNumber.value
)
IS_SYMBOL_MASK = (
    0
    | 1 << UnicodeCategory.MathSymbol.value
    | 1 << UnicodeCategory.CurrencySymbol.value
    | 1 << UnicodeCategory.ModifierSymbol.value
    | 1 << UnicodeCategory.OtherSymbol.value
)
IS_WHITESPACE_MASK = (
    0
    | 1 << UnicodeCategory.SpaceSeparator.value
    | 1 << UnicodeCategory.LineSeparator.value
    | 1 << UnicodeCategory.ParagraphSeparator.value
)

IS_DIGIT_MASK = 1 << UnicodeCategory.DecimalDigitNumber.value
IS_CONTROL_MASK = 1 << UnicodeCategory.Control.value
IS_LETTER_OR_DIGIT_MASK = IS_LETTER_MASK | IS_DIGIT_MASK
IS_UPPER_MASK = 1 << UnicodeCategory.UppercaseLetter.value
IS_LOWER_MASK = 1 << UnicodeCategory.LowercaseLetter.value


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
