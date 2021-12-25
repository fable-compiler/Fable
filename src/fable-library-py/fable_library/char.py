from enum import Enum


class UnicodeCategory(Enum):
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


def unicodeCategoryFunc(cp):
    raise NotImplementedError


def char_code_at(s: str, index: int):
    if index >= 0 and index < len(s):
        return ord(s[index])
    else:
        raise ValueError("Index out of range.")


def getUnicodeCategory2(s: str, index: int):
    cp = char_code_at(s, index)
    return unicodeCategoryFunc(cp)


is_letter_mask = (
    0
    | 1 << UnicodeCategory.UppercaseLetter.value
    | 1 << UnicodeCategory.LowercaseLetter.value
    | 1 << UnicodeCategory.TitlecaseLetter.value
    | 1 << UnicodeCategory.ModifierLetter.value
    | 1 << UnicodeCategory.OtherLetter.value
)

is_digit_mask = 1 << UnicodeCategory.DecimalDigitNumber.value


def is_letter2(s: str, index: int) -> bool:
    test = 1 << getUnicodeCategory2(s, index)
    return (test & is_letter_mask) != 0


def is_digit2(s: str, index: int) -> bool:
    test = 1 << getUnicodeCategory2(s, index)
    return (test & is_digit_mask) != 0


def is_digit(s: str) -> bool:
    return s.isdigit()


def is_letter(s: str) -> bool:
    return s.isalpha()
